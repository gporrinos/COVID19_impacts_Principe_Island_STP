

################################################################################
###-------------------------  COVID-19 ASSESSMENT ---------------------------###
################################################################################



if(!require(ggplot2)) install.packages("ggplot2", repos = "https://cloud.r-project.org")
library(ggplot2)
if(!require(ggpubr))  install.packages("ggpubr", repos = "https://cloud.r-project.org")
library(ggpubr)



              #################################################
              #-------------- WORKING DIRECTORY --------------#
              #################################################


## SET WORK DIRECTORIES
r.scripts <- paste(getwd(),"/DATA_PROCESSING/R.SCRIPTS",sep="")
data.dir  <- paste(getwd(),"/DATA",sep="")
figures   <- paste(getwd(),"/DATA_PROCESSING/FIGURES/",sep="")
tables   <- paste(getwd(),"/DATA_PROCESSING/TABLES/",sep="")






             ##################################################
             # BASE FUNCTIONS (TEXT TO LIST AND LIST TO TEXT) #
             ##################################################

# INDIVIDUAL LIVELIHOODS AS LIST
livlist <- function(livlist)
      lapply(livlist, function(x){
          commas = unlist(gregexpr(', ', x))
          return(  unlist(lapply(c(1:length(commas)), function(y){
                   if(y == 1) {
                      liv = substr(x,1,commas[y]-1)
                        } else {
                      liv = substr(x,commas[y-1]+2,commas[y]-1)
                        }
                    }))
                )
      })

# INDIVIDUAL LIVELIHOODS AGGREGATED TO A SINGLE CHARACTER STRING
livtext <- function(livlist){
    dat = unlist(
      lapply(livlist, function(x){
          for(i in 1:length(x)){
              if(i == 1) {temp = paste(x[i],", ",sep="")} else {
                temp = paste(temp,x[i],", ",sep="")}
           }
           if(temp == "NA, , ") {temp = ""}
           return(temp)
       }))
     return(dat)
     }



# COUNT OCCUPATIONS
count_livs       <- function(y){unlist(lapply(livlist(y),function(x){if(length(x)> 1) {length(x)} else {if(x=="") {0} else {1}}}))}






###############################################################################
#--------------------------- PART 0: DATA CLEANING ---------------------------#
###############################################################################




             #------------------- LOAD DATA ------------------#


### STEP 1: INSTALL PACKAGES
if(!require(readxl)) install.packages("readxl", repos = "https://cloud.r-project.org")
library(readxl)


### STEP 2: READ EXCEL FILE
livelihood_types <- read.csv(paste(data.dir,"livtypes.csv",sep="/"),header=TRUE,sep=",")
for(i in 1:11){
    temp = as.data.frame(read_excel(paste(data.dir,"covid-19.xlsx",sep = "/"),sheet = i))
    if(i==1) {covid = temp} else {covid = cbind(covid,temp)}}
colnames(covid) <- gsub("-","_",colnames(covid))     

covid <- covid[,-which(colnames(covid)=="CODE")[-1]]






             #-- LIVELIHOODS PRESENCES IN A SINGLE VARIABLE --#


livelihoods_1 <- c("agricultura", "criacao", "caca", "buzio", "carvao","lenha","mel")
livelihoods_2 <- c("ter","ocu","tur_tipo")

# Add occupations from livelihood_1
livelihood_list <- 
           lapply(
             livelihoods_1, function(livelihood){
                  dat = covid[,which(colnames(covid) == livelihood)]
                  dat = as.character(dat)
                  dat[which(dat=="sim")] = paste(livelihood,", ",sep="")
                  dat[which(dat=="nao")] = ""
                  dat[which(dat=="nao-sabe")] = ""
                  return(dat)
             })

livelihoods <- livelihood_list[[1]]
for(i in 1:(length(livelihood_list)-1)){
         livelihoods <- paste(livelihoods,livelihood_list[[i+1]],sep="")
     }

# Add palaiê (fish trader)
covid$pal <- paste(covid$pal,", ",sep="")
covid$pal[which(covid$pal=="NA, ")] <- ""
livelihoods <- paste(livelihoods,covid$pal,sep="")

# Add fishers
covid$pes <- paste(covid$pes,", ",sep="")
covid$pes[which(covid$pes=="NA, ")] <- ""
livelihoods <- paste(livelihoods,covid$pes,sep="")

# Add occupations from livelihoods_2
covid$ter <- paste(covid$ter," ",sep=",")
covid$ter[which(covid$ter =="nao-fazem, ")] <- ""
covid$ter[which(covid$ter =="nao-fazem,")] <- ""
covid$ter <- gsub("nao-fazem, ","",covid$ter)

covid$ocu <- paste(covid$ocu," ",sep=",")
covid$ocu[which(covid$ocu=="NA, ")] <- ""
covid$ocu[which(covid$ocu=="nao-fazem, ")] <- ""
covid$ocu[which(covid$ocu=="nao-fazem,")] <- ""
covid$ocu <- gsub("nao-fazem, ","",covid$ocu)

covid$tur_tipo <- paste(covid$tur_tipo," ",sep=",")
covid$tur_tipo[which(covid$tur_tipo=="nao-turismo, ")] <- ""
covid$tur_tipo[which(covid$tur_tipo=="NA, ")] <- ""

for(i in 1:length(livelihoods_2)){
         livelihood_list <- covid[,which(colnames(covid)==livelihoods_2[i])]
         livelihoods     <- paste(livelihoods,livelihood_list,sep="")
     }

covid$livelihoods <- livelihoods    








             #---- COUNT NUMBER OF LIVELIHOODS AND ASSETS ----#

covid$n_livelihoods <- count_livs(covid$livelihoods)
covid$assets        <- paste(covid$propriedade,", ",sep = "")
covid$assets        <- gsub("nao-proprietario, ","",covid$assets)
covid$n_assets      <- count_livs(covid$assets)







             #------------- SUBSISTENCE / INCOME -------------#

livelihoods_1short <- c("agr","cri","cac","buz","car","len","mel")
covid$car_subs_rend<- ""
covid$car_subs_rend[which(covid$carvao == 'sim')]<- "venda"

extract_subsrend <- function(subsrend){
       lapply(c(1:nrow(covid)),function(j){
       output = unlist(lapply(c(1:9),function(i){
             var = c(covid[,which(colnames(covid) == 
                                    paste(livelihoods_1short[i],"subs_rend",sep="_"))])
             if(grepl(subsrend,var[j])) {output = livelihoods_1[i]} else {output = NA}
             return(output)
            }))
       output = output[!is.na(output)]
        })}

##### PART 1: CREATE INCOME DATABASE
income      <- extract_subsrend("venda")

# Extract fishing activities for income from database
covid$pes_inc <- paste(covid$pes_inc,", ",sep="")
covid$pes_inc[which(covid$pes_inc == "NA, ")] <- ""

# Add fish trade activities to income
pal_type <- livlist(covid$pal)
for(i in 1:nrow(covid)){
        if(pal_type[[i]][1] == "") {} else {
            if(class(income[[i]]) == "logical") {
               income[[i]] = pal_type[[i]]
                        } else {
             income[[i]] = c(income[[i]],pal_type[[i]])
         }}}

# Add fishing activities to income
pes_inc <- livlist(covid$pes_inc)
for(i in 1:nrow(covid)){
        if(pes_inc[[i]][1] == "") {} else {
            if(class(income[[i]]) == "logical") {
               income[[i]] = pes_inc[[i]]
                        } else {
             income[[i]] = c(income[[i]],pes_inc[[i]])
         }}}

ter_inc <- livlist(paste0(covid$ter_renda, ", "))
for(i in 1:nrow(covid)){
        if(ter_inc[[i]][1] == "" | ter_inc[[i]][1] == "NA") {} else {
            if(class(income[[i]]) == "logical") {
               income[[i]] = ter_inc[[i]]
                        } else {
             income[[i]] = c(income[[i]],ter_inc[[i]])
         }}}



# Add rest of income activities
income_activities <- c("carpinteiro", "comercio", "construcao","funcionario-empresa", "funcionario-ong", 
"funcionario-publico", "guarda", "guia", "hoteis", "jardineiro", "limpeza", "lojista", "madeira",
"motoqueiro", "outra", "aluger-de-carro", "costura", "eletricista", "pao",
"produtos", "produtos-turisticos", "professor", "refeicao", "restaurante", "sapateiro")      

for(i in 1:nrow(covid)){
     for(j in 1:length(income_activities)){
        activity = income_activities[j]
        if(grepl(activity,livelihoods[i],fixed=TRUE)) {
             if(class(income[[i]]) == "logical") {
                  income[[i]] = c(activity)
                        } else {
                  income[[i]] <- c(income[[i]],activity)
                }}}}

covid$income <- livtext(income)

# Count income activities
covid$income_n  <- count_livs(covid$income)


##### PART 2: CREATE SUBSISTENCE DATABASE
subsistence <- extract_subsrend("Consumo")

# Extract fishing activities for subsistence from database
covid$pes_subs <- paste(covid$pes_subs,", ",sep="")
covid$pes_subs[which(covid$pes_subs == "NA, ")] <- ""

# Add fishing activities to subsistence
pes_subs <- livlist(covid$pes_subs)
for(i in 1:nrow(covid)){
        if(pes_subs[[i]][1] == "") {} else {
            if(class(subsistence[[i]]) == "logical") {
               subsistence[[i]] = pes_subs[[i]]
                        } else {
             subsistence[[i]] = c(subsistence[[i]],pes_subs[[i]])
         }}}



ter_subs <- livlist(paste0(covid$ter_subs, ", "))
for(i in 1:nrow(covid)){
        if(ter_subs[[i]][1] == "" | ter_subs[[i]][1] == "NA") {} else {
            if(class(subsistence[[i]]) == "logical") {
               subsistence[[i]] = ter_subs[[i]]
                        } else {
             subsistence[[i]] = c(subsistence[[i]],ter_subs[[i]])
         }}}





# Add rest of subsistence activities ("domestica")
for(i in 1:nrow(covid)){
        activity = "domestica"
        if(grepl(activity,livelihoods[i],fixed=TRUE)) {
             if(class(subsistence[[i]]) == "logical") {
                  subsistence[[i]] = c(activity)
                        } else {
                  subsistence[[i]] <- c(subsistence[[i]],activity)
             }}}

# Count subsistence activities
covid$subsistence                   <- livtext(subsistence)    
covid$subsistence_n                 <- count_livs(covid$subsistence)
covid$subsistence_minus_domestica_n <- count_livs(sub("domestica, ",  "", covid$subsistence))









             #------------ CHANGES IN LIVELIHOODS ------------#

### STEP 1: REMOVE NAs FROM VARIABLES THAT LIST LIVELIHOOD CHANGES

clean_livchanges <- function(variable) {
       output = paste(variable,", ",sep="")
       output = sub("NA, ",     "",    output)
       output = sub("nao-mudou, ",     "",    output)
       output = sub("nao-mais, ",     "",    output)
       output = sub("nao-menos, ",     "",    output)
       return(output)}

covid$pal_mais <- clean_livchanges(covid$pal_mais)
covid$pes_mais <- clean_livchanges(covid$pes_mais)
covid$ter_mais <- clean_livchanges(covid$ter_mais)
covid$ocu_mais <- clean_livchanges(covid$ocu_mais)


covid$pal_menos <- clean_livchanges(covid$pal_menos)
covid$pes_menos <- clean_livchanges(covid$pes_menos)
covid$ter_menos <- clean_livchanges(covid$ter_menos)
covid$ocu_menos <- clean_livchanges(covid$ocu_menos)


### PART 2: FUNCTION TO COUNT OCCUPATIONS THAT INCREASED AND DECREASED THEIR ACTIVITY
changes <- function(change){
      output = rep("",nrow(covid))
      liv_1 = c("pal","pes","ter","ocu")
      for(i in c(1:length(liv_1))){
          temp = covid[,which(colnames(covid)==paste(liv_1[i],change,sep="_"))]
          output = paste(output,temp,sep="")
               }
      for(i in 1:length(livelihoods_1short)){
          temp = covid[,which(colnames(covid)==paste(livelihoods_1short[i],"trabalho_mudanca",sep="_"))]
          output = paste(output,
                         unlist(lapply(temp,function(x){   if(grepl(change,x)){paste(livelihoods_1[i],", ",sep="")} else {""}  })),
                         sep="")}
      if(change == "menos"){output = paste(output,covid$tur_tipo,sep="")}
      return(output)}

covid$all_more <- changes("mais")
covid$all_less <- changes("menos")
covid$all_unchanged <- gsub("remove, ",   "",
                        livtext(lapply(c(1:nrow(covid)),function(i){
                            unlist(lapply(livlist(covid$livelihoods)[[i]],function(x){
                                if(grepl(x,covid$all_more[i])) {"remove"} else {
                                if(grepl(x,covid$all_less[i])) {"remove"} else {x}}
                                 }))
                             }))
                       )

changes <- function(variable,change){
           variable = covid[,which(colnames(covid) == variable)]
           variable[which(variable=="")] = "delete, "
           change   = covid[,which(colnames(covid) == paste("all",change,sep="_"))]
           return(
             gsub("delete, ",  "",
                 livtext(lapply(c(1:nrow(covid)),function(i){
                     unlist(lapply(livlist(variable)[[i]],function(x){if(grepl(x,change[i]))   {x}   else   {"delete"}   }))
                           })))
                  )}

covid$income_more <- changes("income","more")
covid$income_less <- changes("income","less")
covid$income_unchanged <- changes("income","unchanged")

covid$subsistence_more <- changes("subsistence","more")
covid$subsistence_less <- changes("subsistence","less")
covid$subsistence_unchanged <- changes("subsistence","unchanged")

covid$subsistence_minus_domestica_more      <- sub("domestica, ",  "", covid$subsistence_more)
covid$subsistence_minus_domestica_less      <- sub("domestica, ",  "", covid$subsistence_less)
covid$subsistence_minus_domestica_unchanged <- sub("domestica, ",  "", covid$subsistence_unchanged)






###############################################################################
#--------- PART 1A: LIVELIHOOD DIVERSITY (EFFECT OF AGE AND GENDER) ----------#
###############################################################################




             #------- LIST OF OCCUPATIONS AND COUNTS -------#



if(!require(dplyr)) install.packages("dplyr", repos = "https://cloud.r-project.org")
library(dplyr)
if(!require(plyr)) install.packages("plyr", repos = "https://cloud.r-project.org")
library(plyr)

occupations <- levels(as.factor(unlist(livlist(covid$livelihoods))))
countoccupations <- function(dat,type) unlist(lapply(occupations, function(occupation)length(which(unlist(livlist(dat[,which(colnames(dat)== type)]))==occupation))))

occupationtable <- data.frame(code_pt = occupations, 
           all = countoccupations(covid,"livelihoods"), 
           women = countoccupations(covid[which(covid$genero == "m"),],"livelihoods"),
           men = countoccupations(covid[which(covid$genero == "h"),],"livelihoods"), 
           income = countoccupations(covid,"income"), 
           subsistence = countoccupations(covid,"subsistence"))
write.csv(livelihood_types %>% inner_join(occupationtable,by="code_pt"),
          paste(tables,"TableS1_occupation_list.csv"))






             #------------ OCCUPATION DIVERSITY ------------#



if(!require(lmtest)) install.packages("lmtest", repos = "https://cloud.r-project.org")
library(lmtest)
if(!require(MASS)) install.packages("MASS", repos = "https://cloud.r-project.org")
library(MASS)


### STEP 0: FUNCTION TO CREATE TABLE SUMMARY OF OCCUPATIONS BY GENDER, AGE, AND COMMUNITY TYPE
summarynlivelihoods <- function(variablename){
    dat               = covid[,which(colnames(covid)==variablename)]
    mod               = get(paste("mod",variablename,sep="_"))
    labels = data.frame(name  = c("n_assets","n_livelihoods","income_n","subsistence_n","subsistence_minus_domestica_n"),
                        label = c("Number of key assets","Number of occupations","Number of income occupations","Number of subsistence occupations","Number of subsistence occupations (excluding domestic tasks)"))
    variablename      = labels[which(labels[,1]==variablename),2]
    lrtest_gender     = lrtest(mod,update(mod, ~.-genero)) 
    lrtest_age        = lrtest(mod,update(mod, ~.-idade))  
    lrtest_comunidade = lrtest(mod,update(mod, ~.-tipo_comunidade))    
    av.sd = function(x){paste(round(mean(na.omit(x)),1)," (",round(sd(na.omit(x)),1),")",sep="")}
    testresults = function(lrtest){
         p = as.data.frame(lrtest)[2,5]
         if(p<0.01){p = "p < 0.01"} else {p = paste("p =",round(p,2))} 
         paste("LRT, Chisq = ", round(lrtest[2,4],2), ", df = 1, ", p, sep="")}
    output = data.frame(variablename = variablename,
                   av.sd = av.sd(dat),
                   av.sdW = av.sd(dat[which(covid$genero == "m")]),
                   av.sdM = av.sd(dat[which(covid$genero == "h")]),
                   av.sdinland = av.sd(dat[which(covid$tipo_comunidade == "interior")]),
                   av.sdcoastal = av.sd(dat[which(covid$tipo_comunidade == "costeira")]),
                   family = summary(mod)$family[1]$family,
                   estimate.GenderF = round(mod$coefficients[2],4),
                   LRT.gender = testresults(lrtest_gender),
                   estimate.age = round(mod$coefficients[3],4),
                   LRT.age = testresults(lrtest_age),
                   estimate.comunidadeinterior = round(mod$coefficients[4],4),
                   LRT.comunidade = testresults(lrtest_comunidade))
    return(output)}

### STEP 1: KEY ASSETS
        mod               = glm(n_assets ~ genero + idade + tipo_comunidade, family = "poisson", data=covid)
       (dispersion.parameter = sum(residuals(mod,type ="pearson")^2)/mod$df.residual) # Data is underdispersed
        mod_n_assets      = mod
       (lrtest(mod,update(mod, ~.-genero)))            # Gender affects the number of key assets
       (lrtest(mod,update(mod, ~.-idade)))             # Age affects the number of key assets
       (lrtest(mod,update(mod, ~.-tipo_comunidade)))   # Coastal/inland does not affect

### STEP 2: ALL OCCUPATIONS
        mod = glm(n_livelihoods ~ genero + idade + tipo_comunidade, family = "poisson", data=covid)
       (dispersion.parameter = sum(residuals(mod,type ="pearson")^2)/mod$df.residual) # Data is overdispersed
        mod = glm.nb(n_livelihoods ~ genero + idade + tipo_comunidade, data=covid)
        mod_n_livelihoods = mod
       (lrtest(mod,update(mod, ~.-genero)))            # Gender does not affect number of occupations
       (lrtest(mod,update(mod, ~.-idade)))             # Age does not affect number of occupations
       (lrtest(mod,update(mod, ~.-tipo_comunidade)))   # Coastal/inland does not affect n of occupations

### STEP 3: INCOME OCCUPATIONS
        mod = glm(income_n ~ genero + idade + tipo_comunidade, family = "poisson", data=covid)
       (dispersion.parameter = sum(residuals(mod,type ="pearson")^2)/mod$df.residual) # Data is overdispersed
        mod = glm.nb(income_n ~ genero + idade + tipo_comunidade, data=covid)
        mod_income_n  = mod
       (lrtest(mod,update(mod, ~.-genero)))            # Gender does not affect number of income occupations
       (lrtest(mod,update(mod, ~.-idade)))             # Age does not affect number of income occupations
       (lrtest(mod,update(mod, ~.-tipo_comunidade)))   # Coastal/inland does not affect n of income occupations

### STEP 4: SUBSISTENCE OCCUPATIONS
        mod = glm(subsistence_n ~ genero + idade + tipo_comunidade, family = "poisson", data=covid)
       (dispersion.parameter = sum(residuals(mod,type ="pearson")^2)/mod$df.residual)  # Data is underdispersed
        mod_subsistence_n  = mod
       (lrtest(mod,update(mod, ~.-genero)))            # Gender affect number of subsistence occupations
       (lrtest(mod,update(mod, ~.-idade)))             # Age does not affect number of subsistence occupations
       (lrtest(mod,update(mod, ~.-tipo_comunidade)))   # Coastal inland affects n of subsistence occupations


### STEP 5: SUBSISTENCE OCCUPATIONS MINUS DOMESTIC TASKS
        mod = glm(subsistence_minus_domestica_n ~ genero + idade + tipo_comunidade, family = "poisson", data=covid)
       (dispersion.parameter = sum(residuals(mod,type ="pearson")^2)/mod$df.residual)
        mod_subsistence_minus_domestica_n  = mod
       (lrtest(mod,update(mod, ~.-genero)))            # Gender affect number of subsistence occupations
       (lrtest(mod,update(mod, ~.-idade)))             # Age does not affect number of subsistence occupations
       (lrtest(mod,update(mod, ~.-tipo_comunidade)))   # Coastal inland affects n of subsistence occupations

write.csv(rbind(summarynlivelihoods("n_assets"),summarynlivelihoods("n_livelihoods"),
                summarynlivelihoods("income_n"),summarynlivelihoods("subsistence_n"),
                summarynlivelihoods("subsistence_minus_domestica_n")),
                paste(tables,"TableS2_GLMs_n_occupations.csv",sep="/"))




          #------------ VIOLIN PLOT OF OCCUPATION DIVSERITY ------------#



### STEP 1: FUNCTION FOR SPLIT VIOLIN
# from "https://stackoverflow.com/a/45614547"
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
  data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
  grp <- data[1, "group"]
  newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
  newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
  newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])

  if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
    stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
      1))
    quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
    aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
    aesthetics$alpha <- rep(1, nrow(quantiles))
    both <- cbind(quantiles, aesthetics)
    quantile_grob <- GeomPath$draw_panel(both, ...)
    ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
  }
  else {
    ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
  }
})

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}


### STEP 2: DATA
dat = rbind(data.frame(y = covid$n_livelihoods, x = "all", genero = covid$genero, tipo_comunidade = covid$tipo_comunidade),
            data.frame(y = covid$income_n, x = "income", genero = covid$genero, tipo_comunidade = covid$tipo_comunidade),
            data.frame(y = covid$subsistence_n, x = "subs", genero = covid$genero, tipo_comunidade = covid$tipo_comunidade),
            data.frame(y = covid$subsistence_minus_domestica_n, x = "subs_minus_dom", genero = covid$genero, tipo_comunidade = covid$tipo_comunidade)
)


### STEP 3: FUNCTION TO PRINT VIOLIN PLOT
violinplot <- function(variable,n){
      p = unlist(lapply(c("n_livelihoods","income_n","subsistence_n","subsistence_minus_domestica_n"), function(x){
                  mod = get(paste("mod",x,sep="_"))
                  if(variable == "tipo_comunidade") {p = lrtest(mod,update(mod,~.-tipo_comunidade))[2,5]}
                  if(variable == "genero")          {p = lrtest(mod,update(mod,~.-genero))[2,5]}
                  return(p)}))
      pfont    = unlist(lapply(p,function(p){if(p<0.05){"bold"} else {"plain"}}))
      p        = unlist(lapply(p,function(p){if(p<0.01){"p < 0.01"} else {paste("p =",round(p,2))}}))
      x        = which(c("tipo_comunidade","genero")==variable)
      lab      = c("Community type","Gender")[x]
      values   = list(c("lightskyblue","olivedrab"),c("lightgrey","dimgrey"))[[x]]
      labels   = list(c("Coastal","Inland"),c("Men","Women"))[[x]]
      dat$fill = dat[,which(colnames(dat)==variable)]
      output = ggplot(dat, aes(y=y, x=x, fill = fill)) + geom_split_violin(linewidth = 0.1*n) +
            scale_x_discrete(limits = c("all","income","subs","subs_minus_dom"), breaks = c("all","income","subs","subs_minus_dom"), 
                             labels = c("All occupations","Income occupations","Subsistence occupations", "Subs. occ. (exc. domest. tasks)")) +
            scale_fill_manual(values=values,labels=labels) +
            scale_y_continuous(limits=c(0,22.4),expand=c(0,0)) +
            xlab("Type of occupation") + ylab("Number of occupations") + labs(fill = lab) +
            theme_classic() + theme(
                   panel.grid = element_blank(),
                   axis.text.x   = element_text(size = 2*n, colour="black"),
                   axis.text.y   = element_text(size = 2*n, colour="black"),
                   axis.title.x  = element_text(size = 2.2*n, face="bold",colour="black"),    
                   axis.title.y  = element_text(size = 2.2*n, face="bold",colour="black"),
                   legend.text   = element_text(size = 1.8*n),
                   legend.title  = element_text(size = 2*n,face = "bold",colour="black"))
     for(i in 1:4){
        output = output + 
                 annotate("rect", xmin = i-0.15, xmax = i+0.15, ymin = 3.25, ymax = 4.45, fill = "white", alpha = .7) +
                 annotate("text",x=i,y=3.95,size = 0.75*n, label= p[i],fontface = pfont[i])}
     return(output)}
       




### STEP 4: PRINT VIOLIN PLOT
n=6
genero     <- violinplot("genero",n) + 
                   theme(plot.margin     = margin(t = 3, r = 3, b = 13, l = 3, unit = "pt"),
                         legend.position = c(0.9,0.8))
comunidade <- violinplot("tipo_comunidade",n) + 
                   theme(plot.margin     = margin(t = 13, r = 3, b = 3, l = 3, unit = "pt"),
                         legend.position = c(0.925,0.8))


png(filename=paste(figures,"/figS1_occupations.png",sep=""),width = 900,height=700)
ggarrange(genero,comunidade,labels = c("A","B"),
          font.label = list(size = n*4,face="bold"),
          ncol = 1, nrow = 2)
dev.off()

m=1.2
svg(filename=paste(figures,"/figS1_occupations.svg",sep=""),width = 9*m,height=7*m)
ggarrange(genero,comunidade,labels = c("A","B"),
          font.label = list(size = n*3,face="bold"),
          ncol = 1, nrow = 2)
dev.off()






###############################################################################
#------------------------ PART 1B: CLUSTER ANALYSIS --------------------------#
###############################################################################


if(!require(cluster)) install.packages("cluster", repos = "https://cloud.r-project.org")
library(cluster)

if(!require(ade4))    install.packages("ade4", repos = "https://cloud.r-project.org")
library(ade4)

if(!require(fpc))     install.packages("fpc", repos = "https://cloud.r-project.org")
library(fpc)

if(!require(vegan))   install.packages("vegan", repos = "https://cloud.r-project.org")
library(vegan)



### STEP 1: CREATE PRESENCE / ABSENCE MATRIX
presabs.matrix <- function(data){
       livs    = levels(as.factor(unlist(livlist(data))))
       presabs = data.frame(livelihoods = data)[,-1]
       for(i in 1:length(livs)){
           presabs = data.frame(presabs,unlist(lapply(livlist(data),function(x){if(length(which(x==livs[i]))==0) {0} else {1}})))}
       assets = levels(as.factor(unlist(livlist(covid$assets))))[-1]
       for(i in 1:length(assets)){
               presabs = cbind(presabs,
                   unlist(lapply(covid$assets,function(x){
                      if(grepl(assets[i],x)) {x=1} else {x=0}
                      return(x)}))
              )}
       colnames(presabs) = c(livs,assets)
       return(presabs)}
presabs  <- presabs.matrix(covid$livelihoods)

adonis2(presabs ~ genero + idade + tipo_comunidade, data = covid, perm=999)



### STEP 2: RUN CLUSTER ANALYSIS
distance <- dist.binary(presabs, method = 1, diag = FALSE, upper = FALSE)
fit      <- hclust(distance, method="ward.D") 

clusters=4


svg(filename=paste(figures,"/figS2_clusteranalysis.svg",sep=""),width = 10,height=7)
plot(fit)
rect.hclust(fit, k=clusters)
dev.off()


png(filename=paste(figures,"/figS2_clusteranalysis.png",sep=""),width = 1000,height=700)
plot(fit)
rect.hclust(fit, k=clusters)
dev.off()


groups <- cutree(fit, k=clusters)
jaccardindicator = clusterboot(distance, B=1000, clustermethod=hclustCBI,
            method="ward.D", k=clusters, count=FALSE)$bootmean
covid$cluster <- as.character(groups)
clusternames <- c("Fisher","Farmers landowners","Fish traders","Farmers non landowners","fishing")
for(i in c(1:5)){
     covid$cluster[covid$cluster == as.character(i)] = clusternames[i]}


### STEP 3: SUMMARISE CHARACTERISTICS EXOGENOUS TO THE CLUSTER VARIABLES
         temp    <- c()
         for(i in 1:clusters) {
               dat  = covid[which(groups == i),which(colnames(covid)=="genero")]
               temp = c(temp,length(dat))}
         dat     <- rbind(round(jaccardindicator,2),temp)
         perc    <- function(x,y){temp=c()
              for(i in 1:clusters) {
                 dat  = covid[which(groups == i),which(colnames(covid)==x)]
                 temp = c(temp,
                          paste(round((length(dat[which(dat==y)])/length(dat))*100,0),"%",sep=""))}
              return(temp)}
         dat     <- rbind(dat,perc("genero","m"),perc("tipo_comunidade","interior"))
         average <- function(x) {temp=c()
           for(i in 1:clusters) {
                dat  = covid[which(groups == i),which(colnames(covid)==x)]
                temp = c(temp,paste(round(mean(dat),1)," (",round(sd(dat),1),")",sep=""))}
           return(temp)}
         dat     <-rbind(dat,average("idade"),average("n_assets"),average("n_livelihoods"),average("income_n"),average("subsistence_n"))
         average <- function(x) {temp=c()
           for(i in 1:clusters) {
                if(grepl("all",x)) {n=covid$n_livelihoods}
                if(grepl("income",x)) {n=covid$income_n}
                if(grepl("subsistence",x)) {n=covid$subsistence_n}
                dat  = covid[which(groups == i),which(colnames(covid)==x)]
                dat  = na.omit(count_livs(dat)/n[which(groups == i)])
                temp = c(temp,paste(round(mean(dat)*100,0),"% (",round(sd(dat)*100,0),"%)",sep=""))}
           return(temp)}
         dat     <-rbind(dat,average("all_more"),average("all_less"),average("income_more"),average("income_less"),average("subsistence_more"),average("subsistence_less"))
         rownames(dat) <- c("Cluster Stability Index", "n","% women","% inland villages","Age (mean, SD)","Key assets (mean, SD)","Occupations (mean, SD)","Income occupations (mean, SD)","Subsistence occupations (mean, SD)",
                             "% occ. with higher activity","% occ. with higher activity","income_more","income_less","subsistence_more","subsistence_less")

reclass_livelihoods <- function(dat,reclassify){
      dat = paste(" ", dat,sep="")
      reclassify = livelihood_types[,which(colnames(livelihood_types)==reclassify)]
      for(i in 1:nrow(livelihood_types)){
          dat = gsub(paste(" ", livelihood_types[i,1],",",sep=""),
                     paste(" ", reclassify[i],",",sep=""),     dat)}
      dat = unlist(lapply(dat,function(x){substr(x,2,nchar(x))}))
  return(dat)}


summary.group.livelihoods <- function(presabs){
         temp = presabs[1,][-1,]
         for(i in 1:clusters) {
              dat = presabs[which(groups == i),]
              temp = rbind(temp,unlist(lapply(c(1:length(dat)),function(x){
                                        paste(round(mean(dat[,x])*100,0),"%",sep="")})))
               }
         colnames(temp) = colnames(presabs)
         temp = t(temp)
         return(temp)}

write.csv(rbind(dat,summary.group.livelihoods(presabs.matrix(reclass_livelihoods(covid$livelihoods,"simplified")))),
          paste(tables,"TableS3_clusteroutput.csv",sep="/"))




###############################################################################
#----------------- PART 1B: CHANGES IN OCCUPATION DIVERSITY ------------------#
###############################################################################

             #------------ GENERAL LINEAR MODELS ------------#


### STEP 0: FUNCTION TO SUMMARISE GLMS INTO A TABLE
summarylivchange <- function(dependent,change){
          total.n = count_livs(covid[,which(colnames(covid)==paste(dependent,"more",sep="_"))]) + 
                    count_livs(covid[,which(colnames(covid)==paste(dependent,"less",sep="_"))]) + 
                    count_livs(covid[,which(colnames(covid)==paste(dependent,"unchanged",sep="_"))])
          x   = count_livs(covid[,which(colnames(covid)==paste(dependent,change,sep="_"))])
          mod.data = data.frame(response = x,  total.n = total.n,  gender = covid$genero, age = covid$idade, coast_inland = covid$tipo_comunidade)
          mod = glm(cbind(response, total.n - response) ~ gender + age + coast_inland, family = "binomial", data = mod.data)
          lrtest_gender     = lrtest(mod,update(mod, ~.-gender))
          lrtest_age        = lrtest(mod,update(mod, ~.-age))
          lrtest_comunidade = lrtest(mod,update(mod, ~.-coast_inland))
          testresults = function(lrtest){
               p = as.data.frame(lrtest)[2,5]
               if(p<0.01){p = "p < 0.01"} else {p = paste("p =",round(p,2))} 
               paste("LRT, Chisq = ", round(lrtest[2,4],2), ", df = 1, ", p, sep="")}
          av.sd = function(x){paste(round(mean(na.omit(x))*100,0),"% (",round(sd(na.omit(x))*100,0),"%)",sep="")}
          output = data.frame(variablename = dependent,change = change,
                              av.sd = av.sd(x/total.n),
                              av.sdW = av.sd((x/total.n)[which(covid$genero == "m")]),
                              av.sdM = av.sd((x/total.n)[which(covid$genero == "h")]),
                              av.sdCoast = av.sd((x/total.n)[which(covid$tipo_comunidade == "costeira")]),
                              av.sdInland = av.sd((x/total.n)[which(covid$tipo_comunidade == "interior")]),
                              family = "Binomial",
                              estimate.GenderF = round(mod$coefficients[2],4),
                              LRT.gender = testresults(lrtest_gender),
                              estimate.age = round(mod$coefficients[3],4),
                              LRT.age = testresults(lrtest_age),
                              estimate.coast.inland = round(mod$coefficients[4],5),
                              LRT.coast.inland = testresults(lrtest_comunidade))
          return(output)}


### STEP 1: ALL LIVELIHOODS
# Livelihoods with increased activity
          mod = glm(cbind(count_livs(all_more), count_livs(all_unchanged)+count_livs(all_less)) ~ genero + idade + tipo_comunidade, family = "binomial", data = covid)
         (lrtest(mod,update(mod, ~.-genero)))            # Gender has NO SIGNIFICANT EFFECT on the degree of activity increase (all livelihoods)
         (lrtest(mod,update(mod, ~.-idade)))             # Age has NO SIGNIFICANT EFFECT on the degree of activity increase(all livelihoods)
         (lrtest(mod,update(mod, ~.-tipo_comunidade)))   # Coast / inland has NO SIGNIFICANT EFFECT on the degree of activity increase(all livelihoods)

# Livelihoods with decreased activity
          mod = glm(cbind(count_livs(all_less), count_livs(all_unchanged)+count_livs(all_more)) ~ genero + idade + tipo_comunidade, family = "binomial", data = covid)
         (lrtest(mod,update(mod, ~.-genero)))            # Gender HAS A SIGNIFICANT EFFECT on the degree of activity reduction (all livelihoods)
         (lrtest(mod,update(mod, ~.-idade)))             # Age has NO SIGNIFICANT EFFECT on the degree of activity reduction (all livelihoods)
         (lrtest(mod,update(mod, ~.-tipo_comunidade)))   # Coast/inland has NO SIGNIFICANT EFFECT on the degree of activity reduction (all livelihoods)



### STEP 2: INCOME LIVELIHOODS
# Livelihoods with increased activity
          mod = glm(cbind(count_livs(income_more), count_livs(income_unchanged)+count_livs(income_less)) ~ genero + idade + tipo_comunidade, family = "binomial", data = covid)
         (lrtest(mod,update(mod, ~.-genero)))            # Gender has NO SIGNIFICANT EFFECT on the degree of activity increase (income)
         (lrtest(mod,update(mod, ~.-idade)))             # Age has NO SIGNIFICANT EFFECT on the degree of activity increase(income)
         (lrtest(mod,update(mod, ~.-tipo_comunidade)))   # Coast / inland has NO SIGNIFICANT EFFECT on the degree of activity increase(income)

# Livelihoods with decreased activity
          mod = glm(cbind(count_livs(income_less), count_livs(income_unchanged)+count_livs(income_more)) ~ genero + idade + tipo_comunidade, family = "binomial", data = covid)
         (lrtest(mod,update(mod, ~.-genero)))            # Gender HAS A SIGNIFICANT EFFECT on the degree of activity reduction (income)
         (lrtest(mod,update(mod, ~.-idade)))             # Age has NO SIGNIFICANT EFFECT on the degree of activity reduction (income)
         (lrtest(mod,update(mod, ~.-tipo_comunidade)))   # Coast/inland has NO SIGNIFICANT EFFECT on the degree of activity reduction (income)



### STEP 3: SUBSISTENCE LIVELIHOODS
# Livelihoods with increased activity
          mod = glm(cbind(count_livs(subsistence_more), count_livs(subsistence_unchanged)+count_livs(subsistence_less)) ~ genero + idade + tipo_comunidade, family = "binomial", data = covid)
         (lrtest(mod,update(mod, ~.-genero)))            # Gender has NO SIGNIFICANT EFFECT on the degree of activity increase (subsistence)
         (lrtest(mod,update(mod, ~.-idade)))             # Age has NO SIGNIFICANT EFFECT on the degree of activity increase(subsistence)
         (lrtest(mod,update(mod, ~.-tipo_comunidade)))   # Coast / inland has NO SIGNIFICANT EFFECT on the degree of activity increase(subsistence)

# Livelihoods with decreased activity
          mod = glm(cbind(count_livs(subsistence_less), count_livs(subsistence_unchanged)+count_livs(subsistence_more)) ~ genero + idade + tipo_comunidade, family = "binomial", data = covid)
         (lrtest(mod,update(mod, ~.-genero)))            # Gender has NO SIGNIFICANT EFFECT on the degree of activity reduction (subsistence)
         (lrtest(mod,update(mod, ~.-idade)))             # Age has NO SIGNIFICANT EFFECT on the degree of activity reduction (subsistence)
         (lrtest(mod,update(mod, ~.-tipo_comunidade)))   # Coast/inland has NO SIGNIFICANT EFFECT on the degree of activity reduction (subsistence)

### STEP 4: SUBSISTENCE LIVELIHOODS MINUS DOMESTIC TASKS
# Livelihoods with increased activity
          mod = glm(cbind(count_livs(subsistence_minus_domestica_more), count_livs(subsistence_minus_domestica_unchanged)+count_livs(subsistence_minus_domestica_less)) ~ genero + idade + tipo_comunidade, family = "binomial", data = covid)
         (lrtest(mod,update(mod, ~.-genero)))            # Gender has NO SIGNIFICANT EFFECT on the degree of activity increase (subsistence minus domestic)
         (lrtest(mod,update(mod, ~.-idade)))             # Age has NO SIGNIFICANT EFFECT on the degree of activity increase(subsistence minus domestic)
         (lrtest(mod,update(mod, ~.-tipo_comunidade)))   # Coast / inland has NO SIGNIFICANT EFFECT on the degree of activity increase(subsistence minus domestic)

# Livelihoods with decreased activity
          mod = glm(cbind(count_livs(subsistence_minus_domestica_less), count_livs(subsistence_minus_domestica_unchanged)+count_livs(subsistence_minus_domestica_more)) ~ genero + idade + tipo_comunidade, family = "binomial", data = covid)
         (lrtest(mod,update(mod, ~.-genero)))            # Gender has NO SIGNIFICANT EFFECT on the degree of activity reduction (subsistence minus domestic)
         (lrtest(mod,update(mod, ~.-idade)))             # Age has NO SIGNIFICANT EFFECT on the degree of activity reduction (subsistence minus domestic)
         (lrtest(mod,update(mod, ~.-tipo_comunidade)))   # Coast/inland has NO SIGNIFICANT EFFECT on the degree of activity reduction (subsistence minus domestic)

dat <-rbind(summarylivchange("all","more"),summarylivchange("all","less"),
            summarylivchange("income","more"),summarylivchange("income","less"),
            summarylivchange("subsistence","more"),summarylivchange("subsistence","less"),
            summarylivchange("subsistence_minus_domestica","more"),summarylivchange("subsistence_minus_domestica","less"))


write.csv(dat,paste(tables,"TableS4_GLM_proportion_occupation_impacted.csv",sep="/"))



       #------------ Plot changes in occupation diversity ------------#


summary.change <- function(type,grouping_variable){
       output = data.frame(type = "",x="",change = "", mean = 0.1, se = 0.1,min=0.1)[-1,]
       all = covid[,which(colnames(covid) %in% paste(type,c("more","less","unchanged"),sep="_"))]
       for(i in 1:3){all[,i] = count_livs(all[,i])}
       colnames(all) = sub(paste(type,"_",sep=""),"",colnames(all))
       type         = c("Income","All occupations","Subsistence","Subs. (exc. domest. tasks)")[which(c("income","all","subsistence","subsistence_minus_domestica")==type)]
       colnames_all = colnames(all)
       all_count = all[,1]+all[,2]+all[,3]
       all       = 100*(all/all_count)
       grupos = covid[,which(colnames(covid)==grouping_variable)]
       for(i in 1:3){
            change = c("less","unchanged","more")[i]
            dat  = all[,which(colnames(all)==change)]
            for(j in 1:2){
                temp = dat[grupos == levels(as.factor(grupos))[j]]
                se = sd(temp[!is.na(temp)])/length(temp[!is.na(temp)])
                if(i == 1) {se  = se + mean(temp[!is.na(temp)])} else {se  = se + mean(temp[!is.na(temp)])+ output$min[(2*(i-1))-c(1,0)[j]]}
                if(i == 1) {min = mean(temp[!is.na(temp)])} else {min = mean(temp[!is.na(temp)])+ output$min[(2*(i-1))-c(1,0)[j]]}
                temp = data.frame(type = type, x = levels(as.factor(grupos))[j],change = c("3-less","2-unchanged","1-more")[i], mean = mean(temp[!is.na(temp)]), se = se,min=min)
                output   = rbind(output,temp)}}
        return(output)}

plotchanges <- function(grouping_variable,n){
        dat = rbind(summary.change("all",grouping_variable),
                    summary.change("income",grouping_variable),
                    summary.change("subsistence",grouping_variable),
                    summary.change("subsistence_minus_domestica",grouping_variable))
        dat$type    = factor(dat$type,   levels=c("All occupations","Income","Subsistence","Subs. (exc. domest. tasks)"))
        pvalues     = data.frame(x=0.1,mean=0.1,type = "",lab="",pfont="",change="")[-1,]
        for(i in 1:4){ for(j in 1:2){
          dependent = c("all","income","subsistence","subsistence_minus_domestica")[i]
          change    = c("more","less")[j]
          if(dependent == "all") {total.n = covid$n_livelihoods} else {total.n = covid[,which(colnames(covid)==paste(dependent,"n",sep="_"))]}
          x   = count_livs(covid[,which(colnames(covid)==paste(dependent,change,sep="_"))])
          mod.data = data.frame(response = x,  total.n = total.n,  gender = covid$genero, age = covid$idade, coast_inland = covid$tipo_comunidade)
          mod = glm(cbind(response, total.n - response) ~ gender + age + coast_inland, family = "binomial", data = mod.data)
          if(grouping_variable == "genero")          {p = lrtest(mod,update(mod, ~.-gender))[2,5]}
          if(grouping_variable == "tipo_comunidade") {p = lrtest(mod,update(mod, ~.-coast_inland))[2,5]}
          if(p<0.05){pfont = "bold"} else {pfont = "plain"}
          if(p<0.01){p = "p < 0.01"} else {p = paste("p =",round(p,2))}
          pvalues = rbind(pvalues,data.frame(x = 1.5, mean = c(90,10)[j], type = c("All occupations","Income","Subsistence","Subs. (exc. domest. tasks)")[i],
                               lab = p,pfont = pfont,change=paste(1+(j*(j-1)),change,sep="-")))
           }}
        pvalues$type =  factor(pvalues$type, levels=c("All occupations","Income","Subsistence","Subs. (exc. domest. tasks)"))
        output = ggplot(data = dat,aes(x=x,y=mean,fill=change)) + 
               geom_bar(stat="identity",color="black") +
               geom_errorbar(aes(ymax=se+1,  ymin=min), width=0.15,color = "black") + 
               facet_grid(~type) + 
               scale_y_continuous(limits=c(0, 102), expand = c(0, 0)) + 
               ylab("% of individual occupations \nimpacted by the pandemics") + 
               scale_fill_manual(values = c("forestgreen","ghostwhite","firebrick"),
                                 labels = c("Positively impacted","Unchanged","Negatively impacted")) +
               labs(fill = "Reported impacts on \noccupations") +
               theme_minimal() + theme(  
                  axis.line     = element_blank(),
                  axis.text   = element_text(size = 2*n, colour="black"),
                  axis.title  = element_text(size = 2.2*n, face="bold",colour="black"),    
                  legend.text   = element_text(size = 1.8*n),
                  legend.title  = element_text(size = 2*n,face = "bold",colour="black"),
                  strip.text.x  = element_text(size = 2*n,face="bold",colour="black"),
                  panel.grid = element_blank() 
                   ) + annotate("rect", xmin = 1.2, xmax = 1.8, ymin = 6.5, ymax = 12.5, fill = "white", alpha = .7) +
                       annotate("rect", xmin = 1.2, xmax = 1.8, ymin = 86.5, ymax = 92.5, fill = "white", alpha = .7) 
        for(i in 1:nrow(pvalues)){output = output + geom_text(data = pvalues[i,-5],aes(x=x, y=mean, label=lab),size = 0.65*n,fontface=pvalues$pfont[i])}
       return(output)}

n=10
genero     <- plotchanges("genero",n) + xlab("Gender") + 
                  scale_x_discrete(limits = c("h","m"), breaks = c("h","m"), labels=c("Men","Women")) + 
                  theme(plot.margin = margin(t = 5, r = 210, b = 20, l = 3, unit = "pt"),
                        legend.position = c(1.1,-0.1))

comunidade <- plotchanges("tipo_comunidade",n) + xlab("Type of community") + 
                  scale_x_discrete(limits = c("costeira","interior"), breaks = c("costeira","interior"), labels=c("Coastal","Inland")) + 
                  theme(plot.margin = margin(t = 20, r = 220, b = 5, l = 3, unit = "pt"),
                        legend.position = 'none')

png(filename=paste(figures,"/fig2_change_of_occupations.png",sep=""),width = 1350,height=900)
ggarrange(genero, comunidade , labels=c("A","B"),
          font.label = list(size = n*2.5,face="bold"),
          ncol = 1, nrow = 2)
dev.off()

m = 1.4
svg(filename=paste(figures,"/fig2_change_of_occupations.svg",sep=""),width = 13.5*m,height=9*m)
ggarrange(genero, comunidade , labels=c("A","B"),
          font.label = list(size = n*2.5,face="bold"),
          ncol = 1, nrow = 2)
dev.off()


pdf(paste(figures,"/fig2.pdf",sep=""),width = 13.5*m,height=9*m)
ggarrange(genero, comunidade , labels=c("A","B"),
          font.label = list(size = n*2.5,face="bold"),
          ncol = 1, nrow = 2)
dev.off()





###############################################################################
#-------------------------- PART 3: IMPACTS OF COVID -------------------------#
###############################################################################


if(!require(DiagrammeR)) install.packages("DiagrammeR", repos = "https://cloud.r-project.org")
library(DiagrammeR)


covid$impact_codes <- paste(covid$impact_codes," ",sep="")
covid_impact_codes <- sub("  "," ",covid$impact_codes)
covid$impact_codes <- paste(covid$impact_codes,",",sep="")
covid_impact_codes <- sub(", ,",", ",covid$impact_codes)
impactlist <- levels(as.factor(unlist(livlist(covid$impact_codes))))[-1]
impactsfunction <- function(genero = c("h","m"),   tipo_comunidade = c("costeira", "interior")  ) {
     impacts = data.frame(impact="blah",count=0)
     impact_codes = covid$impact_codes[which(covid$genero %in% genero & covid$tipo_comunidade %in% tipo_comunidade)]
     for(i in 1:length(impactlist)){
             impacts = rbind(impacts,data.frame(
                                 impact = impactlist[i],
                                 count  = length(which(unlist(livlist(impact_codes))==impactlist[i])
                                    )))         }
     impacts = impacts %>% arrange(desc(count))
     impacts = impacts[-which(impacts$count == 0)]
     return(impacts)}

### Are changes different across groups? --> NO
impactsfunction()
impactsfunction(tipo_comunidade = "interior", genero = "m")
impactsfunction(tipo_comunidade = "interior", genero = "h")
impactsfunction(tipo_comunidade = "costeira", genero = "m")
impactsfunction(tipo_comunidade = "costeira", genero = "h")






covid$links <- paste(covid$links," ",sep="")
covid_links <- sub("  "," ",covid$links)
covid$links <- paste(covid$links,",",sep="")
covid_links <- sub(", ,",", ",covid$links)
covid_links <- sub("--","-",covid$links)


linklist <- levels(as.factor(unlist(livlist(covid$links))))[-1]
links = data.frame(impact="blah",count=0)
for(i in 1:length(linklist)){
        links = rbind(links,data.frame(
                                 impact = linklist[i],
                                 count  = length(which(unlist(livlist(covid$links))==linklist[i])
                                    )))         }
links %>% arrange(desc(count))


library(DiagrammeR)


grViz("
digraph boxes_and_circles {

  # a 'graph' statement
  graph [overlap = true, fontsize = 10]

  # several 'node' statements
  node [shape = box,
        fontname = Helvetica]
Children_are_idle; Disruptions_on_interisland_transport;Family_stranded_on_São_Tomé;Food_insecurity;
Higher_cost_of_living;Hotels_closed;Increased_competition;Isolation;Lack_of_tourists;Lockdown;Loss_of_savings;Low_demand;Lower_income;Lower_price_of_produce;
Lower_supply_of_imported_products;Movement_restrictions;Not_able_to_sell_products;
Salary_reduction_formally_employed;Schools_closed;Shutdown_of_businesses;
Social_distancing;Time_restrictions;Travel_ban;Unemployment   

  # several 'edge' statements
Disruptions_on_interisland_transport->Lower_supply_of_imported_products Higher_cost_of_living->Lower_income                                    
Hotels_closed->Low_demand                                               Hotels_closed->Unemployment                                            
Lack_of_tourists->Low_demand                                            Lack_of_tourists->Unemployment                                         
Lockdown->Isolation                                                     Lockdown->Time_restrictions                                            
Low_demand->Lower_income                                                Low_demand->Lower_price_of_produce                                    
Low_demand->Not_able_to_sell_products                                   Lower_income                                                           
Lower_income->Food_insecurity                                           Lower_income->Loss_of_savings                                          
Lower_income->Low_demand                                                Lower_price_of_produce->Lower_income                                   
Lower_supply_of_imported_products->Lower_income                         Movement_restrictions->Not_able_to_sell_products                       
Not_able_to_sell_products->Lower_income                                 Unemployment->Theft_of_produce
Not_able_to_sell_products->Lower_price_of_produce                       Salary_reduction_formally_employed->Lower_income                       
Schools_closed->Children_are_idle                                       Schools_closed->Unemployment                                           
Shutdown_of_businesses->Unemployment                                    Social_distancing->Isolation                                           
Social_distancing->Isolation                                            Social_distancing->Low_demand                                          
Social_distancing->Schools_closed                                       Time_restrictions->Increased_competition                               
Time_restrictions->Isolation                                            Time_restrictions->Not_able_to_sell_products                           
Travel_ban->Family_stranded_on_São_Tomé                                 Travel_ban->Lack_of_tourists                                           
Unemployment->Food_insecurity                                           Unemployment->Lower_income                                             
            
}
")                      







###############################################################################
#----------------------- PART 4: CHANGES IN LIVELIHOODS ----------------------#
###############################################################################


if(!require(cowplot)) install.packages(cowplot, repos = "https://cloud.r-project.org")
library(cowplot)

reclassify = "code_pt"
occupplot <- function(type = "all",reclassify = "simplified", tipo_comunidade = c("costeira","interior"), genero = c("m","h"), main = ""){
  subselection = which(covid$tipo_comunidade %in% tipo_comunidade & covid$genero %in% genero)
  reclass = function(type = "all",change,reclassify)
               reclass_livelihoods(covid[subselection,which(colnames(covid) == paste(type,change,sep="_"))],reclassify)
  dat = unlist(lapply(c("more","less","unchanged"),function(change) {reclass(type,change, reclassify)}))
  dat = unlist(livlist(dat[-which(dat == "")]))
  occups = levels(as.factor(dat))
  dat = lapply(c("more","less","unchanged"),function(change) {unlist(lapply(occups,function(occup)   
                                                               length(which(unlist(livlist(c(reclass(type,change, reclassify)))) == occup))
                                                               ))})
  dat = data.frame(occup = occups, more = dat[[1]], less = dat[[2]], unchanged = dat[[3]], n = dat[[1]] + dat[[2]] + dat[[3]])
  dat = (dat %>% arrange(n))[,c("occup","more","less","unchanged")]
  dat$occup = factor(levels = dat$occup, x = dat$occup)
  dat = rbind(data.frame(occup = dat$occup, change = "Negatively impacted", n = dat$less),
              data.frame(occup = dat$occup, change = "Unchanged", n = dat$unchanged),
              data.frame(occup = dat$occup, change = "Positively impacted", n = dat$more))
  dat$change = factor(x=dat$change, levels = c("Negatively impacted","Unchanged","Positively impacted"))
  dat$facet = main
  ggplot(data = dat, aes(x = occup, y = n, fill = change)) + 
            geom_bar(stat='identity', colour = "black") + coord_flip() +
            scale_fill_manual(values = c("firebrick","ghostwhite","forestgreen")) + 
            theme_bw() + facet_wrap(~facet) + xlab("") + 
            theme(panel.grid.major.x = element_line(),    
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  legend.position = "none")}

temp = list(occupplot(type = "subsistence", genero = "m",main = "Subsistence, women"), 
            occupplot(type = "subsistence", genero = "h",main = "Subsistence, men"),
            occupplot(type = "income", genero = "m",main = "Income, women") + theme(legend.position = c(1.2,-0.25)), 
            occupplot(type = "income", genero = "h",main = "Income, men"))
 


svg(filename = paste(figures,"/fig4_change_of_occupations.svg",sep=""),width = 7.5,height=8)
plot_grid(plotlist=temp, align = 'v', ncol = 2, nrow = 3,   rel_heights = c(1,2,0.5) )
dev.off()

pdf(paste(figures,"/fig4.pdf",sep=""),width = 7.5,height=8)
plot_grid(plotlist=temp, align = 'v', ncol = 2, nrow = 3,   rel_heights = c(1,2,0.5) )
dev.off()








###############################################################################
#---------------------- PART 5 CHANGES TO SELLLNG POINTS ---------------------#
###############################################################################





if(!require("ggalluvial")) install.packages("ggalluvial")
library(ggalluvial)

## ALLUVIAL PLOT
temp = lapply(c("agr","cri","pes","pal"), function(occupation){
  before = paste0(covid[,which(colnames(covid) == paste(occupation,"marketbefore", sep="_"))], ", ")
  after  = paste0(covid[,which(colnames(covid) == paste(occupation,"marketnow", sep="_"))], ", ")
  
  links = c("Village", "Trader", "Market", "Hotel", "São Tomé", "Peddling", "Not selling")
  links = as.data.frame(expand.grid(source = links, target = links, value = 0))
  links$value = unlist(lapply(1:nrow(links),function(i){
    
    positions = which(grepl(links$source[i],before, fixed = TRUE) & 
                        grepl(links$target[i],after, fixed = TRUE) )
    return(sum(
      unlist(lapply(after[positions],function(x) length(gregexpr(",",x,fixed = TRUE)[[1]]))) / 
        unlist(lapply(before[positions],function(x) length(gregexpr(",",x,fixed = TRUE)[[1]])))))
  }))
  links$Occupation = occupation
  
  links$Occupation = factor(links$Occupation, 
                            levels = c("cri", "agr", "pal", "pes"), 
                            labels = c("Livestock producers", "Agriculture producers", "Fish traders", "Fishers"))
  links$source = factor(links$source, 
                        levels = c("Village", "Trader", "Market", "Hotel", "São Tomé", "Peddling", "Not selling"),
                        labels = c("Community", "Trader", "Market", "Hotel", "São Tomé", "Peddling", "Not selling"))
  links$target = factor(links$target, 
                        levels = c("Village", "Trader", "Market",  "São Tomé","Hotel", "Peddling", "Not selling"),
                        labels = c("Community", "Trader", "Market",  "São Tomé","Hotel", "Peddling", "Not selling\nanymore"))
  return(links)
})

for(i in 1:4)  if(i == 1){links = temp[[i]] } else {links = rbind(links,temp[[i]])}

links <- links[-which(links$Occupation == "Fish traders" & links$source == "Trader"),]

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}           


p <- ggplot(links,
            aes(y = value, axis1 = source, axis2 = target, fill = source)) +
  geom_alluvium(width = 0.3) +
  geom_stratum(width = 0.3, fill = NA ) +
  geom_text(stat = "stratum", colour = "black", 
            aes(label = after_stat(stratum)))+
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(breaks = c(1,2), 
                     labels = c("Before COVID-19", "During COVID-19")) +
  facet_wrap(Occupation ~., scales = "free")+
  scale_fill_manual(breaks = c("Community", "Trader", "Market", "Hotel", "São Tomé", "Peddling"),
                    values = gg_color_hue(7)) +
  ylab("Number of respondents") +
  labs(fill = "Selling points") +
  theme(
    axis.ticks.x = element_line(colour = "white"),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 13),
    strip.background = element_blank(),
    axis.text = element_text(colour = "black", size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    panel.spacing = unit(1.5, "lines"))


svg(filename = paste(figures,"/fig5_selling_points.svg",sep=""), width = 9.5,height=11)
p
dev.off()

pdf(paste(figures,"/fig5.pdf",sep=""), width = 9.5,height=11)
p
dev.off()


  
  


## SUPPLEMENTARY BARPLOT
temp = lapply(c("agr","pes","pal","cri"), function(occupation){
  before = paste0(covid[,which(colnames(covid) == paste(occupation,"marketbefore", sep="_"))], ", ")
  after  = paste0(covid[,which(colnames(covid) == paste(occupation,"marketnow", sep="_"))], ", ")
  markets = c("Village", "Trader", "Market", "Hotel", "São Tomé", "Peddling", "Not selling")
  output = data.frame(Occupation = occupation,Market = markets)
  before = unlist(lapply(markets,function(market) length(which(grepl(market,before, fixed = TRUE)))))
  after  = unlist(lapply(markets,function(market) length(which(grepl(market,after, fixed = TRUE)))))
  output = rbind(data.frame(output, when = "Before COVID-19", count = before),
                 data.frame(output, when = "During COVID-19", count = after))
  return(output)
})



#### OPTION A ####
for(i in c(1:4)){
  if(i == 1){dat = temp[[i]] } else {dat = rbind(dat,temp[[i]])}
}

dat$Occupation = factor(dat$Occupation, 
                        levels = c("cri", "agr", "pal", "pes"), 
                        labels = paste(c("Livestock producers", "Agriculture producers", "Fish traders", "Fishers"), "        "))
dat$Market = factor(dat$Market, 
                    levels = c("Village", "Trader", "Market", "Hotel", "São Tomé", "Peddling", "Not selling"),
                    labels = c("Community", "Trader", "Market", "Hotel", "São Tomé", "Peddling", "Not selling"))


if(!require(lemon)) install.packages("lemon")
library(lemon)

svg(filename = paste(figures,"/figS4_change_of_marketsA.svg",sep=""), width = 8,height=5)

ggplot(data = dat, aes(x = Market, y = count, fill = when)) +
  geom_bar(stat = "identity", position = "dodge", colour = "grey9", width = 0.7, linewidth = 0.3) + 
  facet_wrap(~Occupation, scales = "free",
             strip.position = "left") +
  scale_y_continuous(limits = c(0,35), breaks = c(0,10,20,30), expand = c(0,1) ) +
  xlab(NULL) + ylab(NULL) + 
  scale_fill_manual(values = c("grey90", "grey15")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 7.5, colour = "black"),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_text(size = 10, colour = "black"),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        axis.line.y = element_line(),
        strip.placement = "outside",
        strip.text = element_text(face = "bold"))+ 
  coord_capped_cart(left='both') 

dev.off()




#### OPTION B ####
for(i in c(1:4)){
  if(i == 1){dat = temp[[i]] } else {dat = rbind(dat,temp[[i]])}
}

dat$Occupation = factor(dat$Occupation, 
                        levels = c("cri", "agr", "pal", "pes"), 
                        labels = c("Livestock producers", "Agriculture producers", "Fish traders", "Fishers"))
dat$Market = factor(dat$Market, 
                    levels = c("Village", "Trader", "Market", "Hotel", "São Tomé", "Peddling", "Not selling"),
                    labels = c("Community", "Trader", "Market", "Hotel", "São Tomé", "Peddling", "Not selling"))


svg(filename = paste(figures,"/figS4_change_of_marketsB.svg",sep=""), width = 8,height=5)

ggplot(data = dat, aes(x = Market, y = count, fill = when)) +
  geom_bar(stat = "identity", position = "dodge", colour = "grey9", width = 0.7, linewidth = 0.4) + 
  facet_wrap(~Occupation, scales = "free") +
  scale_y_continuous(limits = c(0,35), expand = c(0,1) ) +
  xlab(NULL) + ylab("Nº of respondents") + 
  scale_fill_manual(values = c("grey90", "grey15")) +
  theme_bw() + theme(panel.grid = element_blank(),
                     axis.text = element_text(size = 7.5, colour = "black"),
                     legend.title = element_blank(),
                     legend.position = "bottom",
                     axis.title.y = element_text(size = 10, colour = "black"),
                     axis.ticks.x = element_blank())

dev.off()










    

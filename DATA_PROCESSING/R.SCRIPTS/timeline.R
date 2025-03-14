

marketplace    <- as.data.frame(
                  rbind(c(date = "00/03/2020", dateend = "08/04/2020", status = "No restrictions",start=24-6.5, end = 24-19),
                        c(date = "08/04/2020", dateend = "03/05/2020", status = "8h30m - 17h",start=24-8.5, end = 24-17),
                        c(date = "03/05/2020", dateend = "01/06/2020", status = "8h30m - 15h",start=24-8.5, end = 24-15),
                        c(date = "01/06/2020", dateend = "30/06/2020", status = "06h30m - 17h",start=24-6.5, end = 24-17)))

lockdown       <- as.data.frame(
                  rbind(c(date = "20/04/2020", dateend = "03/05/2020", status = "",  start= 24-19,  end = 24-24),
                        c(date = "20/04/2020", dateend = "03/05/2020", status = "",  start= 24-0,   end = 24-5),
                        c(date = "03/05/2020", dateend = "01/06/2020", status = "",  start= 24-15,  end = 24-24),
                        c(date = "03/05/2020", dateend = "01/06/2020", status = "",  start= 24-0,   end = 24-7),
                        c(date = "01/06/2020", dateend = "30/06/2020", status = "",  start= 24-19,  end = 24-24),
                        c(date = "01/06/2020", dateend = "30/06/2020", status = "",  start= 24-0,   end = 24-5)))


survey       <- as.data.frame(
                  rbind(c(date = "05/06/2020", dateend = "18/06/2020", status = "",  start= 32,  end = 31)))


travel_ban       <- as.data.frame(
                  rbind(c(date = "15/03/2020", dateend = "30/06/2020", status = "",  start= 30,  end = 29)))


distancing       <- as.data.frame(
                  rbind(c(date = "19/03/2020", dateend = "30/06/2020", status = "",  start= 28,  end = 27)))

st   <- 37
dates         <- as.data.frame(
                  rbind(c(date = "00/03/2020", dateend = "00/04/2020", status = "",  start= st,   end = st-2),
                        c(date = "00/04/2020", dateend = "00/05/2020", status = "",  start= st,   end = st-2),
                        c(date = "00/05/2020", dateend = "00/06/2020", status = "",  start= st,   end = st-2),
                        c(date = "00/06/2020", dateend = "00/07/2020", status = "",  start= st,   end = st-2)))




rectangles <- function(timeline,row,border,col,density,angle){
     rect(xright   = as.numeric(substr(timeline[row,2],4,5))+as.numeric(substr(timeline[row,2],1,2))/30, 
          xleft  = as.numeric(substr(timeline[row,1],4,5))+as.numeric(substr(timeline[row,1],1,2))/30, 
          ybottom = timeline[row,which(colnames(timeline)=="end")], 
          ytop    = timeline[row,which(colnames(timeline)=="start")],border=border,col=col,
          density=density, angle=angle)
          }








n=1.8

png(filename=paste0(getwd(),"/DATA_PROCESSING/FIGURES/timeline/name.png"),
    width = 1000,height=480)



par(mar=c(0.3,0,0.3,0),mgp=c(2,0.5,0),family="serif",font.axis=1,font.lab=1,cex.lab=1,cex.axis=0.8,col="black",mfrow=c(1,1))
plot(0,4,col="white",xaxt="n",yaxt="n",ylim=c(-2.5,39),xlim = c(2.7,8.7),bty="n",xlab='',ylab='')

    rect(xright   = 8.7, xleft  = 2.7, ybottom = -2.5, ytop    = 39, col="gray95",border = NA)
    rect(xright   = 7, xleft  = 3, ybottom = 0, ytop    = 24, col="gray89",border = NA)

    for(i in 3:7){  ## LINES OF MONTHS
        lines(x=c(i,i),y=c(0,36),lty = "dashed",col="black")}
    for(i in 1:4){  ## HEADER OF MONTHS
        rectangles(dates,i,"white","black",NULL,NULL)
        text(i+2.5,36,c("March 2020","April 2020","May 2020","June 2020")[i],cex=0.8*n,col = "white")}


    text(4+20/30,32,"*",col="green3",cex=1.3*n,family="serif")             ## COV19 CASES ST
    text(5+3/30,32,"*",col="green4",cex=1.3*n,family="serif")              ## COV19 CASES P
    text(5+19/30,32,"x ",col="indianred3",cex=0.8*n,family="sans",font=2)  ## BANKRUPTCY OF RESORTS
    rectangles(travel_ban,1,FALSE,"lightblue",NULL,NULL)                 ## TRAVEL BAN
    rectangles(survey,1,FALSE,"orange",NULL,NULL)                        ## SURVEY PERIOD
    rectangles(distancing,1,FALSE,"dodgerblue3",NULL,NULL)               ## SOCIAL DISTANCING


    for(i in 1:4){
        rectangles(marketplace,i,"grey","grey",NULL,NULL)}
    for(i in 1:6){
        rectangles(lockdown,i,"gray89","gray89",NULL,NULL)}
    for(i in 1:6){
        rectangles(lockdown,i,FALSE,"black",15,45)}

    
    lab_pos <- c(24,0,8,16)
    lab     <- c("0h","24h","16h","8h")    
    rect(xright   = 3, xleft  = 3, ybottom = 0, ytop    = 24, col="black",lwd = 2)
    for(i in 1:4){
       rect(xright   = 3, xleft  = 2.95, ybottom = lab_pos[i], ytop    = lab_pos[i], col="black",lwd = 1.5)
       text(2.95,lab_pos[i],lab[i],cex=0.7*n,pos=2)}


    #### LEGEND
    text(7.9,30,"KEY",cex=0.8*n,col = "black",font=2)

    start = 27
    start_text  = 7.45
    start_symbol = start_text-0.08
    text(start_symbol,start,"*",col="green3",cex=1.3,family="serif")
    text(start_text,27,"1st case of COV-19 (ST)",cex=0.7*n,pos=4)

    text(start_symbol,start-(2.5),"*",col="green4",cex=1.3,family="serif")
    text(start_text,start-(2.5),"1st case of COV-19 (P)",cex=0.7*n,pos=4)

    text(start_symbol+0.02,start-(2.5*2),"x ",col="indianred3",cex=0.8,family="sans",font=2)
    text(start_text,start-(2.5*2),"Resorts bankruptcy",cex=0.7*n,pos=4)

    startx = start_symbol-0.07
    endx   = start_symbol+0.07
    leg    = as.data.frame(rbind(
               c("orange",NA,NA,"Survey period"),
               c("lightblue",NA,NA,"Travel ban"),
               c("dodgerblue3",NA,NA,"Social distancing"),
               c("grey",NA,NA,"Market / business hours"),
               c("black",20,45,"Lockdown hours")))
    colnames(leg) = c("col","density","angle","label")


    for(i in 1:nrow(leg)){
       rect(xright   = endx, xleft = startx, ytop = start-(2.5*(2+i))+0.5, ybottom = start-(2.5*(2+i))-0.5,
            border="black",col=leg$col[i],
            density = if(is.na(leg$density[i])) {NULL} else {as.numeric(leg$density[i])},
            angle = if(is.na(leg$angle[i])) {NULL} else {as.numeric(leg$angle[i])})
       text(start_text,start-(2.5*(2+i)),leg$label[i],cex=0.7*n,pos=4)
    }

dev.off()



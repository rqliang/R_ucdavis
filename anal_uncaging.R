args <- commandArgs(TRUE)
fn <- normalizePath(args[1])
fnbase <- substr(fn,1,nchar(fn)-4)
#print(fn)
datafn <- paste(fnbase,"-data.txt",sep='')
eventfn <- paste(fnbase,"-event.txt",sep='')
timefn <- paste(fnbase,"-time.txt",sep='')
pdffn <- paste(fnbase,"-plot.pdf",sep='')

mydata <- read.table(datafn,header=T)
mytime <- read.table(timefn,header=T)
bleach <- read.table(eventfn,skip=1)
mycolors = c('red','green','blue','grey','orange')
myncol = dim(mydata)[2]
nROI = floor((myncol)/4)
myMean = 0
for (i in seq(1,nROI)) {
  F0 = mydata[1,i*4-2]
  dFF = mydata[,i*4-2]/F0 -1
  mydata = cbind(mydata,dFF)
  myMean = myMean + dFF/nROI
}
mydata = cbind(mydata, myMean)
mydata$Time = mytime$Timestamps
myMin = min(mydata[,(myncol+1):(myncol+nROI)])
myMax = max(mydata[,(myncol+1):(myncol+nROI)])
head(mydata)
write.table(mydata,datafn, row.names = F)
pdf(pdffn)
if (nROI == 1) {
  plot((myMean - myMin * 1.05) ~ Time, data=mydata, xlab='Time (s)', 
       ylab = expression(paste(Delta, "F/F")), ylim=c(0,(myMax-myMin)) * 1.05,
       type = 'l', lwd = 2)
} else {
  plot((myMean - myMin * 1.05) ~ Time, data=mydata, xlab='Time (s)', 
       ylab = expression(paste(Delta, "F/F")), ylim=c(0,(myMax-myMin)) * 1.05,
       type = 'l', lwd = 2)
  for (i in seq(1,nROI)){
    lines(mydata[,myncol+i] - myMin * 1.05 ~ mydata$Time, ylim=c(0,(myMax-myMin)) * 1.05,
          type = 'l', lwd = 2,lty=3,col=mycolors[i])
  }
}

bleach$V2=(myMax - myMin)/25
points(cbind(bleach$V1,bleach$V2),type='h',col='red')
dev.off()

## An R script for plotting deltaF over F0. You need to manually set up the 
## the time interval
## ruqiang.liang@gmail.com

#fn <- file.choose()
args <- commandArgs(TRUE)
fn <- normalizePath(args[2])
interval <- as.numeric(args[1])
assay <- strsplit(fn,split='/')[[1]]
assayname <- sprintf("%s-%s",assay[length(assay)-1], strsplit(assay[length(assay)],split='.txt')[[1]][1])
mydata <- read.table(fn,skip=1)
ch1 <- seq(1, dim(mydata)[1], 1)
ch2 <- ch1+1
myMean <- seq(3,dim(mydata)[2],4)
#interval <- 0.7850181818184865
# intv <- readline('Please tell me how much is the interval: ')
# interval <- as.numeric(intv)
timex <- seq(0,(dim(mydata)[1]-1)*interval, interval)
ch1data <- mydata[ch1, myMean]
for (i in 1:dim(ch1data)[2]){
  ch1data[,i] <- ch1data[,i]/ch1data[1,i]-1
}
ch2data <- mydata[ch2,myMean]
for (i in 1:dim(ch2data)[2]){
  ch2data[,i] <- ch2data[,i]/ch2data[1,i]-1
}
ch1mean <- rowMeans(ch1data)
ch1std <- apply(ch1data,1,sd)

#ch2mean <- rowMeans(ch2data)
#ch2std <- apply(ch2data,1,sd)

newdata <- cbind(timex,mydata[ch1,myMean], ch1data, ch1mean, ch1std) #, mydata[ch2, myMean], ch2data, ch2mean,ch2std)
#newdata <- newdata[,1:13]
write.table(newdata, paste(fn,'-dF.txt',sep=''))
mydata <- newdata
pdf(paste(fn,'-plot.pdf',sep=''))
plot(mydata[,1],mydata$ch1mean,type='l', ylim=2* c(min(ch1mean),max(ch1mean)),
     lwd=2,xlab='Time (s)', ylab=expression (paste(Delta, 'F/F'[0])), main=assayname)
for (i in 1:5){
  lines(mydata[,1], mydata[,6+i], col=i+1,lt=3)
}
dev.off()
subset(newdata,ch1mean == max (ch1mean),select=c('timex','ch1mean'))
subset(newdata,ch1mean == min (ch1mean),select=c('timex','ch1mean'))

args <- commandArgs(TRUE)
if (length(args)<3){
  print('Usage: Rscript bleach-correction.R data.txt startpoint endpoint')
  quit()
}
fn <- normalizePath(args[1])
st <- as.numeric(args[2])
ed <- as.numeric(args[3])
a = read.table(fn,header=T)
mysel = c(1:st,ed:10)
y = a[mysel,7]
x = a[mysel,8]
epm = lm(log(y+1) ~ x)
ts = a[,8]
test = exp(predict(epm, list(x=ts)))-1
a[,9] = a[,7]-test
pdf(sub('.txt','.pdf',fn))
plot(V9~Time, data=a, ylim = c(-.1, .5), type='l', xlab='Time (s)', ylab=expression(paste(Delta,'F/F'),sep=''))
dev.off()
print(summary(epm))
write.table(round(a,digits = 5),fn,quote=F,row.names = F,col.names = T)

Snow<-read.csv('SnowStrip_test_Snow.csv', skip=7)
  #read.csv('SnowStrip2_Snow.csv', skip=7)
  #read.csv('SnowStrip_test_Snow.csv', skip=7)
LST<-read.csv('SnowStrip_test_DayST.csv', skip=7)
  #read.csv('SnowStrip2_DayST_crop.csv', skip=7)
  #read.csv('SnowStrip_test_DayST.csv', skip=7)



LST[LST==9999]<-NaN
Snow[Snow==9999]<-NaN

LST<-LST[2:nrow(LST),]
Snow<-Snow[2:nrow(Snow),]

for (i in c(1:10,40:46)){
  plot(LST[,i+6]~Snow[,i+6], main=paste("Date", i), 
       ylim=c(260,290), xlim=c(0,1), ylab="LST", 
       xlab="Snow Probability")
  
  line<-lm(LST[,i+6]~Snow[,i+6])
  abline(unname(line[[1]]))
  print(i)
  print(line)
  
}

#means

for (j in c(1:10, 40:46)){
  
snows<-sort(unique(Snow[,j+6]))
snowtemp=c(1:length(snows))

for (i in 1:length(snows)){
  LSTDate=LST[,j+6]
  SnowDate=Snow[,j+6]
  
  snowtemp[i]<-mean(LSTDate[SnowDate==snows[i]], na.rm=TRUE) 
  if(length(LSTDate[SnowDate==snows[i]])<100){
    snowtemp[i]<-NaN
  }
  
}
plot(snowtemp~snows, main=paste("Date",j),ylim=c(260,290), 
     xlim=c(0,1), ylab="LST", xlab="Snow Probability")
condensed<-lm(snowtemp~snows)
print(condensed)
if(is.finite(unname(condensed[[1]][2]))){
abline(unname(condensed[[1]]))

}
}






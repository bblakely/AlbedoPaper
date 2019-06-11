nst.mod.raw<-read.csv('STnight_Model.csv', skip=7)
nst.dat.raw<-read.csv('STNight_Data.csv', skip=7)

nst.dat<-nst.dat.raw[2:24535,7:52]
nst.mod<-nst.mod.raw[2:24535,7:52]


nst.dat[nst.dat>350]<-NA
nst.mod[nst.mod>350]<-NA


nst.dat[nst.dat<240]<-NA
nst.mod[nst.mod<240]<-NA



monthlab<-c('jan',0,0,0,'feb',0,0,0,'mar',0,0,0,'april',0,0,0,'may',0,0,0,'jun',0,0,0,'jul',0,0,0,'aug',0,0,0,'sep',0,0,0,'oct',0,0,0,'nov',0,0,0,'dec')
par(mfrow=c(2,2))
par(mar=c(4,4,3,2))
for (i in seq(from=1,to=46, by=4)){
  smoothScatter(y=nst.dat[,i], x=nst.mod[,i], main=monthlab[i], ylim=c(250,300), xlim=c(250,300),
                colramp=colorRampPalette(c('white','forest green', 'black')), ylab='MODIS', xlab='Lowess')
  abline(0,1, col='red')
}

for (i in seq(from=1,to=46, by=4)){
  plot(y=nst.dat[,i], x=nst.mod[,i], main=monthlab[i],  ylim=c(250,300), xlim=c(250,300),
       ylab='MODIS', xlab='Lowess')
  abline(0,1, col='red')
}




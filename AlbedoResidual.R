dat.raw<-read.csv('MODISalbedo.csv', skip=7)
mod.raw<-read.csv('ModelAlbedo.csv', skip=7)


dat<-dat.raw[2:24535,7:52]
mod<-mod.raw[2:24535,7:52]


dat[dat>1]<-NA
mod[mod>1]<-NA


dat[mod<0.01]<-NA
mod[mod<0.01]<-NA

plot(dat$B23, mod$B23)
abline(0,1, col='red')

library(smoothScatter)
monthlab<-c('jan',0,0,0,'feb',0,0,0,'mar',0,0,0,'april',0,0,0,'may',0,0,0,'jun',0,0,0,'jul',0,0,0,'aug',0,0,0,'sep',0,0,0,'oct',0,0,0,'nov',0,0,0,'dec')
par(mfrow=c(2,2))
par(mar=c(4,4,3,2))
for (i in seq(from=1,to=46, by=4)){
smoothScatter(y=dat[,i], x=mod[,i], main=monthlab[i], ylim=c(0.1,0.8), xlim=c(0.1,0.65),
              colramp=colorRampPalette(c('white','forest green', 'black')), ylab='MODIS', xlab='Lowess')
abline(0,1, col='red')
}

vegtype<-read.csv('ModUTM.csv')[,2]
colvec<-rep('black', 24534)
colvec[vegtype==12]<-'yellow'
colvec[vegtype==14]<-'orange'
colvec[vegtype==5]<-'dark blue'
colvec[vegtype==4]<-'forest green'
colvec[vegtype==1]<-'purple4'
colvec[vegtype==26]<-'dark red'


for (i in seq(from=1,to=46, by=4)){
  #resid<-mod[,i]-dat[,i]
  #cutoff<-2.5*sd(resid, na.rm=TRUE)
  #colvec[(mod[,i]-dat[,i]>cutoff | mod[,i]-dat[,i]<(-cutoff))]<-'red'
  dat[is.na(vegtype),i]<-NaN
  mod[is.na(vegtype),i]<-NaN
  plot(y=dat[,i], x=mod[,i], main=monthlab[i], ylim=c(0.1,0.8), xlim=c(0.1,0.65),
        ylab='MODIS', xlab='Lowess', col=colvec)
  abline(0,1, col='red')
}



#Where are residuals?
diffs<-dat-mod

geo<-dat.raw[2:24535,5:6]
feb.diffs<-cbind(geo,diffs[,5])
feb.diffs.lg<-febdiffs[(feb.diffs[,3]>0.15 | feb.diffs[,3]<(-0.15)),]
feb.diffs.lg<-feb.diffs.lg[!is.na(feb.diffs.lg[,3]),]

write.csv(feb.diffs.lg,'FebDiverge.csv')

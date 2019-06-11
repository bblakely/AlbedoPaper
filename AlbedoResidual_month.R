dat.raw<-read.csv('MODISalbedo.csv', skip=7)
mod.raw<-read.csv('ModelAlbedo.csv', skip=7)


dat<-dat.raw[2:24535,7:52]
mod<-mod.raw[2:24535,7:52]


dat[dat>1]<-NA
mod[mod>1]<-NA


dat[mod<0.01]<-NA
mod[mod<0.01]<-NA


vegtype<-read.csv('ModUTM.csv')[,2]
colvec<-rep('black', 24534)
colvec[vegtype==12]<-'yellow'
colvec[vegtype==14]<-'orange'
colvec[vegtype==5]<-'dark blue'
colvec[vegtype==4]<-'forest green'
colvec[vegtype==1]<-'purple4'
colvec[vegtype==26]<-'dark red'

dat[is.na(vegtype),]<-NaN
mod[is.na(vegtype),]<-NaN

#Group by month, data
dat.dat<-dat
dat.jan<-rowMeans(dat.dat[1:4])
dat.feb<-rowMeans(dat.dat[5:8])
dat.mar<-rowMeans(dat.dat[9:12])
dat.apr<-rowMeans(dat.dat[13:15])
dat.may<-rowMeans(dat.dat[16:19])
dat.jun<-rowMeans(dat.dat[20:23])
dat.jul<-rowMeans(dat.dat[24:27])
dat.aug<-rowMeans(dat.dat[28:31])
dat.sep<-rowMeans(dat.dat[32:35])
dat.oct<-rowMeans(dat.dat[36:38])
dat.nov<-rowMeans(dat.dat[39:42])
dat.dec<-rowMeans(dat.dat[43:46])


dat.month<-data.frame(cbind(dat.jan,dat.feb,dat.mar,dat.apr,dat.may,dat.jun,
                            dat.jul,dat.aug,dat.sep,dat.oct,dat.nov,dat.dec))

dat.win<-rowMeans(dat.month[,c(1:2, 12)])
dat.spr<-rowMeans(dat.month[,3:5])
dat.sum<-rowMeans(dat.month[,6:8])
dat.fal<-rowMeans(dat.month[,9:11])

dat.seas<-cbind(dat.win,dat.spr,dat.sum,dat.fal)


#Group by month, model
mod.dat<-mod
mod.jan<-rowMeans(mod.dat[1:4])
mod.feb<-rowMeans(mod.dat[5:8])
mod.mar<-rowMeans(mod.dat[9:12])
mod.apr<-rowMeans(mod.dat[13:15])
mod.may<-rowMeans(mod.dat[16:19])
mod.jun<-rowMeans(mod.dat[20:23])
mod.jul<-rowMeans(mod.dat[24:27])
mod.aug<-rowMeans(mod.dat[28:31])
mod.sep<-rowMeans(mod.dat[32:35])
mod.oct<-rowMeans(mod.dat[36:38])
mod.nov<-rowMeans(mod.dat[39:42])
mod.dec<-rowMeans(mod.dat[43:46])


mod.month<-data.frame(cbind(mod.jan,mod.feb,mod.mar,mod.apr,mod.may,mod.jun,
                            mod.jul,mod.aug,mod.sep,mod.oct,mod.nov,mod.dec))

mod.win<-rowMeans(mod.month[,c(1:2, 12)])
mod.spr<-rowMeans(mod.month[,3:5])
mod.sum<-rowMeans(mod.month[,6:8])
mod.fal<-rowMeans(mod.month[,9:11])

mod.seas<-cbind(mod.win,mod.spr,mod.sum,mod.fal)


#Monthly Plots
library(smoothScatter)
monthlab<-c('jan','feb','mar','april','may','jun','jul','aug','sep','oct','nov','dec')

  #c('jan',0,0,0,'feb',0,0,0,'mar',0,0,0,'april',0,0,0,'may',0,0,0,'jun',0,0,0,'jul',0,0,0,'aug',0,0,0,'sep',0,0,0,'oct',0,0,0,'nov',0,0,0,'dec')
par(mfrow=c(2,2))
par(mar=c(4,4,3,2))
for (i in 1:12){
smoothScatter(y=dat.month[,i], x=mod.month[,i], main=monthlab[i], ylim=c(0.1,0.8), xlim=c(0.1,0.65),
              colramp=colorRampPalette(c('white','forest green', 'black')), ylab='MODIS', xlab='Lowess')
abline(0,1, col='black')
}


for (i in 1:12){
  #resid<-mod[,i]-dat[,i]
  #cutoff<-2.5*sd(resid, na.rm=TRUE)
  #colvec[(mod[,i]-dat[,i]>cutoff | mod[,i]-dat[,i]<(-cutoff))]<-'red'
  dat[is.na(vegtype),i]<-NaN
  mod[is.na(vegtype),i]<-NaN
  plot(y=dat.month[,i], x=mod.month[,i], main=monthlab[i], ylim=c(0.1,0.7), xlim=c(0.1,0.7),
        ylab='MODIS', xlab='Lowess', col=colvec)
  abline(0,1, col='black')
}

##Seasonal plots
seaslab<-c('Winter', 'Spring', 'Summer','Fall')
par(mfrow=c(2,2))
par(mar=c(3,3,2,0))
#par(mar=c(4,4,3,2))
for (i in 1:4){
  #smoothScatter(y=dat.seas[,i], x=mod.seas[,i], main=seaslab[i], ylim=c(0,0.7), xlim=c(0,0.7),
                #colramp=colorRampPalette(c('white','forest green', 'black')),
                #font=2, font.lab=2, ylab='', xlab='')
  thin<-sample(1:nrow(dat.seas), 4907)
  smoothScatter(y=dat.seas[,i], x=mod.seas[,i], main=seaslab[i], ylim=c(0,0.7), xlim=c(0,0.7),
  colramp=colorRampPalette(c('white', 'black')),nrpoints=0,
  font=2, font.lab=2, ylab='', xlab='')
  colvec.s<-colvec
  colvec.s[colvec=='dark blue']<-'blue'
  colvec.s[colvec=='purple4']<-'purple'
  colvec.s[colvec=='dark red']<-'red'
  points(y=dat.seas[thin,i], x=mod.seas[thin,i],col=colvec.s[thin], pch='.', cex=1.5)
  # ylab='MODIS', xlab='Lowess',
  abline(0,1, col='black')
  box(lwd=2)
}

for (i in 1:4){

  plot(y=dat.seas[,i], x=mod.seas[,i], main=seaslab[i], ylim=c(0,0.7), xlim=c(0,0.7),
       ylab='', xlab='',col=colvec, font=2, font.lab=2, pch='.')
  #ylab='MODIS', xlab='Lowess',
  abline(0,1, col='black')
  box(lwd=2)
}



#Where are residuals?
# diffs<-dat-mod
# diffs.seas<-dat.seas - mod.seas
# 
# geo<-dat.raw[2:24535,5:6]
# feb.diffs<-cbind(geo,diffs[,5])
# feb.diffs.lg<-febdiffs[(feb.diffs[,3]>0.15 | feb.diffs[,3]<(-0.15)),]
# feb.diffs.lg<-feb.diffs.lg[!is.na(feb.diffs.lg[,3]),]
# 
# write.csv(feb.diffs.lg,'FebDiverge.csv')

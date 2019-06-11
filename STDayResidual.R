dst.mod.raw<-read.csv('STDay_Modeled.csv', skip=7)
dst.dat.raw<-read.csv('STDay_MODIS.csv', skip=7)

dst.dat<-dst.dat.raw[2:24535,7:52]
dst.mod<-dst.mod.raw[2:24535,7:52]


dst.dat[dst.dat>350]<-NA
dst.mod[dst.mod>350]<-NA


dst.dat[dst.dat<240]<-NA
dst.mod[dst.mod<240]<-NA

dst.dat[is.na(vegtype),]<-NaN
dst.mod[is.na(vegtype),]<-NaN

#Group to months
#Group by month, data
dst.dat<-dst.dat
dat.jan<-rowMeans(dst.dat[1:4])
dat.feb<-rowMeans(dst.dat[5:8])
dat.mar<-rowMeans(dst.dat[9:12])
dat.apr<-rowMeans(dst.dat[13:15])
dat.may<-rowMeans(dst.dat[16:19])
dat.jun<-rowMeans(dst.dat[20:23])
dat.jul<-rowMeans(dst.dat[24:27])
dat.aug<-rowMeans(dst.dat[28:31])
dat.sep<-rowMeans(dst.dat[32:35])
dat.oct<-rowMeans(dst.dat[36:38])
dat.nov<-rowMeans(dst.dat[39:42])
dat.dec<-rowMeans(dst.dat[43:46])


dst.dat.month<-data.frame(cbind(dat.jan,dat.feb,dat.mar,dat.apr,dat.may,dat.jun,
                            dat.jul,dat.aug,dat.sep,dat.oct,dat.nov,dat.dec))

dat.win<-rowMeans(dst.dat.month[,c(1:2, 12)])
dat.spr<-rowMeans(dst.dat.month[,3:5])
dat.sum<-rowMeans(dst.dat.month[,6:8])
dat.fal<-rowMeans(dst.dat.month[,9:11])

dst.dat.seas<-cbind(dat.win,dat.spr,dat.sum,dat.fal)


#Group by month, model
dst.mod<-dst.mod
mod.jan<-rowMeans(dst.mod[1:4])
mod.feb<-rowMeans(dst.mod[5:8])
mod.mar<-rowMeans(dst.mod[9:12])
mod.apr<-rowMeans(dst.mod[13:15])
mod.may<-rowMeans(dst.mod[16:19])
mod.jun<-rowMeans(dst.mod[20:23])
mod.jul<-rowMeans(dst.mod[24:27])
mod.aug<-rowMeans(dst.mod[28:31])
mod.sep<-rowMeans(dst.mod[32:35])
mod.oct<-rowMeans(dst.mod[36:38])
mod.nov<-rowMeans(dst.mod[39:42])
mod.dec<-rowMeans(dst.mod[43:46])


dst.mod.month<-data.frame(cbind(mod.jan,mod.feb,mod.mar,mod.apr,mod.may,mod.jun,
                            mod.jul,mod.aug,mod.sep,mod.oct,mod.nov,mod.dec))

mod.win<-rowMeans(dst.mod.month[,c(1:2, 12)])
mod.spr<-rowMeans(dst.mod.month[,3:5])
mod.sum<-rowMeans(dst.mod.month[,6:8])
mod.fal<-rowMeans(dst.mod.month[,9:11])

dst.mod.seas<-cbind(mod.win,mod.spr,mod.sum,mod.fal)


#Monthly plots
library(smoothScatter)
monthlab<-c('jan','feb','mar','april','may','jun','jul','aug','sep','oct','nov','dec')
par(mfrow=c(2,2))
par(mar=c(4,4,3,2))
for (i in 1:12){
  smoothScatter(y=dst.dat.month[,i], x=dst.mod.month[,i], main=monthlab[i], ylim=c(260,305), xlim=c(260,305),
                colramp=colorRampPalette(c('white','forest green', 'black')), ylab='MODIS', xlab='Lowess')
  abline(0,1, col='black')
}

for (i in 1:12){
  plot(y=dst.dat.month[,i], x=dst.mod.month[,i], main=monthlab[i],  ylim=c(260,305), xlim=c(260,305),
       ylab='MODIS', xlab='Lowess', col=colvec, pch='.')
  abline(0,1, col='black')
}


#Seasonal Plots

seaslab<-c('Winter','Spring', 'Summer', 'Fall')
par(mfrow=c(2,2))
par(mar=c(4,4,3,2))
for (i in 1:4){
  # smoothScatter(y=dst.dat.seas[,i], x=dst.mod.seas[,i], main=seaslab[i], ylim=c(260,305), xlim=c(260,305),
  #               colramp=colorRampPalette(c('white','forest green', 'black')), ylab='MODIS', xlab='Lowess',
  #               font=2, font.lab=2)
  
  smoothScatter(y=dst.dat.seas[,i], x=dst.mod.seas[,i], main=seaslab[i], ylim=c(255,305), xlim=c(255,305),
                colramp=colorRampPalette(c('white', 'black')), ylab='MODIS', xlab='Lowess',
                font=2, font.lab=2, nrpoints=0)
  
  thin<-sample(1:nrow(dst.dat.seas), 4907)
  colvec.s<-colvec
  colvec.s[colvec=='dark blue']<-'blue'
  colvec.s[colvec=='purple4']<-'purple'
  colvec.s[colvec=='dark red']<-'red'
  points(y=dst.dat.seas[thin,i], x=dst.mod.seas[thin,i],col=colvec.s[thin], pch='.', cex=1.5)
  
  
  abline(0,1, col='black')
  box(lwd=2)
}

for (i in 1:4){
  dst.dat[is.na(vegtype),i]<-NaN
  dst.mod[is.na(vegtype),i]<-NaN
  plot(y=dst.dat.seas[,i], x=dst.mod.seas[,i], main=seaslab[i],  ylim=c(260,305), xlim=c(260,305),
       ylab='MODIS', xlab='Lowess', col=colvec, pch='.', font=2, font.lab=2)
  abline(0,1, col='black')
  box(lwd=2)
}

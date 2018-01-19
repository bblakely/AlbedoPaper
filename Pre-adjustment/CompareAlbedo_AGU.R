ModernAlbRaw<-read.csv('Albedo_modern.csv',skip=7)
PaleoAlbRaw<-read.csv('Albedo_paleo.csv',skip=7)


ModernAlb<-ModernAlbRaw[2:nrow(ModernAlbRaw),7:52]
PaleoAlb<-PaleoAlbRaw[2:nrow(PaleoAlbRaw),7:52]

ModernAlb[PaleoAlb==9999]<-9999
PaleoAlb[ModernAlb==9999]<-9999

PaleoAlb[PaleoAlb==9999]<-NaN
ModernAlb[ModernAlb==9999]<-NaN

TableDiffsAlb<-ModernAlb-PaleoAlb
DiffMeans<-colMeans(TableDiffsAlb, na.rm=TRUE)

library('insol')

nmins<-1440
JulTab<-matrix(nrow=365,ncol=nmins)
PosTab<-array(dim=c(nmins,3,365))
angles<-array(dim=c(nmins,2,365))
SUN<-array(dim=c(nmins,2,365))
insol<-matrix(nrow=365,ncol=nmins)
for (i in 1:365){
  JulTab[i,]<-seq(from=(i-0.5), to=(i+0.5), length.out=nmins)
  PosTab[,,i]<-sunvector(jd=JulTab[i,], latitude=44.2, longitude=-91,timezone=-6)
  angles[,,i]<-sunpos(PosTab[,,i])
  SUN[,,i]<-insolation(zenith=angles[,2,i],jd=JulTab[i,],height=200,visibility=10,RH=50,tempK=280,O3=.003,alphag=0)
  insol[i,]<-SUN[,1,i]+SUN[,2,i]
}


insol[insol<0.2]<-NaN
Dayinsols<-(rowMeans(insol, na.rm=TRUE))
plot(Dayinsols, type='l')

Insolation<-Dayinsols

InsolPicks=Insolation[round(seq(from=1, to=365,length.out=46))]


###Old Insolation
# Julian=seq(from=1,to=365)
# sunvector(jd=Julian, latitude=44.2, longitude=-91,timezone=-6)->position
# sunpos(position)->angles
# SUN<-insolation(zenith=angles[,2],jd=Julian, height=200,visibility=10,RH=50,tempK=280,O3=.003,alphag=0)
# Insolation=SUN[,1]+SUN[,2]
# plot(Insolation)
# InsolPicksold=Insolation[round(seq(from=1, to=365,length.out=46))]


RFPixels<-(-TableDiffsAlb/4)

rfpixels<-RFPixels

TabInsol<-matrix(nrow=24534,ncol=46, data=rep(InsolPicks, each=24534))


loquantrf=rep(0,46)
hiquantrf=rep(0,46)
for (i in 1:46){
  rfpixels[,i]=rfpixels[,i]*InsolPicks[i]*0.854
  quantrf<-quantile(rfpixels[,i], c(0.1, 0.90), na.rm=TRUE)
  loquantrf[i]<-quantrf[1]
  hiquantrf[i]<-quantrf[2]
}


RF=colMeans((rfpixels), na.rm=TRUE)

PixAvgForce<-rowMeans(rfpixels)
quantile(PixAvgForce, c(0.1, 0.90), na.rm=TRUE)
mean(PixAvgForce, na.rm=TRUE)

SummerDays=c(20:31)
WinterDays=c(1:8,43:46)

#mean(RF[SummerDays])
SummerRF<-rowMeans(rfpixels[,SummerDays])
mean(SummerRF, na.rm=TRUE)
quantile(SummerRF, c(0.1, 0.90), na.rm=TRUE)


#mean(RF[WinterDays])
WinterRF<-rowMeans(rfpixels[,WinterDays])
mean(WinterRF, na.rm=TRUE)
quantile(WinterRF, c(0.1, 0.90), na.rm=TRUE)


RFsm<-approx(RF,n=12)
hiquantrfsm<-approx(hiquantrf,n=12)
loquantrfsm<-approx(loquantrf,n=12)


par(mar=c(5,5,5,5))
plot(RFsm$y, ylim=c(-10,3), type='l',lwd='3', xaxt='n',yaxt='n',xlab='Month',ylab='λRF(W/m2)', main='Albedo RF',cex.lab=2.2, cex.main=2.5,bty="n")
polygon(x=c(1:12,12:1),y=c(hiquantrfsm$y, rev(loquantrfsm$y)), border=NA,col='gray')
abline(v=c(3.75,5.6,8.25,10.2), lty=3)
lines(RFsm$y, lwd=5)
axis(side=2, labels= seq(from=-10, to=4, by=4), at=seq(from=-10, to=4, by=4), cex.axis=1.5, font=2)
box(,lwd=3)
#lines(loquantrf, lty=4,col='red',lwd='2')
#lines(hiquantrf, lty=4,col='forest green',lwd='2')
abline(h=0,lty=2, lwd=3,col='red4')
axis(side=1,labels=c(1:12),at=seq(from=1,to=12,length.out=12),cex.axis=1.5, font=2)


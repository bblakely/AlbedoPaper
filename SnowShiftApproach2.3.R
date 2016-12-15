Modern<-read.csv('GHCN_Modern_Snowanalysis.csv', skip=7)
Early<-read.csv('GHCN_Historic_Snowanalysis.csv', skip=7)
ModAlbedo<-read.csv('Albedo_Modern_Snowanalysis.csv', skip=7)
HistAlbedo<-read.csv('Albedo_Paleo_Snowanalysis.csv', skip=7)
DatAlbedo<-read.csv('Modis_Snowanalysis.csv',skip=7)

Albedo<-HistAlbedo
Albedo<-Albedo[Albedo$Lon<(-75),]

AlbedoDAT<-Albedo[2:nrow(Albedo),5:50]
AlbedoDAT[AlbedoDAT==9999]<-NaN

Checkvec=rowSums(AlbedoDAT)

AlbedoDAT<-AlbedoDAT[!is.na(Checkvec),]
Georef<-Albedo[!is.na(Checkvec),3:4]
Georef_shift<-Georef
Georef_shift$Lon<-Georef_shift$Lon+0.2

####To months####
alb.jan<-rowMeans(AlbedoDAT[1:4],na.rm=TRUE)
alb.feb<-rowMeans(AlbedoDAT[5:8],na.rm=TRUE)
alb.mar<-rowMeans(AlbedoDAT[9:12],na.rm=TRUE)
alb.apr<-rowMeans(AlbedoDAT[13:15],na.rm=TRUE)
alb.may<-rowMeans(AlbedoDAT[16:19],na.rm=TRUE)
alb.jun<-rowMeans(AlbedoDAT[20:23],na.rm=TRUE)
alb.jul<-rowMeans(AlbedoDAT[24:27],na.rm=TRUE)
alb.aug<-rowMeans(AlbedoDAT[28:31],na.rm=TRUE)
alb.sep<-rowMeans(AlbedoDAT[32:35],na.rm=TRUE)
alb.oct<-rowMeans(AlbedoDAT[36:38],na.rm=TRUE)
alb.nov<-rowMeans(AlbedoDAT[39:42],na.rm=TRUE)
alb.dec<-rowMeans(AlbedoDAT[43:46],na.rm=TRUE)
alb.month<-data.frame(cbind(alb.jan,alb.feb,alb.mar,alb.apr,alb.may,
                              alb.jun,alb.jul,alb.aug,alb.sep,alb.oct,alb.nov,alb.dec))
######
AlbedoDAT<-alb.month

EarDAT<-Early[2:nrow(Early),5:16]
EarDAT<-EarDAT[!is.na(Checkvec),]

ModDAT<-Modern[2:nrow(Early),5:16]
ModDAT<-ModDAT[!is.na(Checkvec),]

nobs=nrow(EarDAT)

HistAlbs<-matrix(nrow=nobs,ncol=12)
plots<-sample(1:nobs,20)

for (n in 1:nobs){
  #nrow(AlbedoDAT)
  Epix<-unlist(EarDAT[n,1:12])
  Mpix<-unlist(ModDAT[n,1:12])
  Apix<-unlist(AlbedoDAT[n,1:12])
  
  Allsnow<-c(Epix,Mpix)
  Trumax<-max(Allsnow, na.rm=TRUE)

  Enorm<-Epix/max(Epix, na.rm=TRUE)  
  #Esmooth<-approx(Enorm, n=46)
  Escale<-Enorm
  #Esmooth$y

  Mnorm<-Mpix/max(Mpix, na.rm=TRUE)
  #Msmooth<-approx(Mnorm, n=46)
  Mscale<-Mnorm
    #Msmooth$y
  
  Scalars<-(Escale-Mscale)
    #((Escale-Mscale)/Escale)
  AlbRange=(max(Apix, na.rm=TRUE)-min(Apix, na.rm=TRUE))
  
  HistAlb<-Apix+(Scalars*AlbRange)
  index=c(1:2,10:12)
  #which(HistAlb <= Apix)
  HistAlb[index]<-Apix[index]
  
  HistAlbs[n,]<-HistAlb
  
  ###Plotting Commands. Commented out for speed
   if (any(n == plots)){
#   plot(Escale,type='l',lty=2, lwd=2, xaxt='n', xlab='Month', 
#        main='Albedo Shift', col='white', ylim=c(0,.6), pch=1.5,
#        ylab='Albdeo') 
#   #originally had main=n and no color=white or ylim to track during dev
#   
#   axis(1,at=seq(from=1, to=46,length.out=12),labels=c(1:12))
#   legend(15,0.5, legend=c('Historic Albedo', 'Modern Albedo'), 
#          col=c('orange', 'purple'), lwd=2)
#   #lines(Mscale, type='l',col='red', lty=2, lwd=2)
#   lines(Apix, col='purple',lty=2, lwd=2)
#   lines(HistAlb, col='orange', lwd=2)
   plot(Epix, type='l')
   lines(Mpix, col='red')
 }
}

HistAlbAvg=colMeans(HistAlbs, na.rm=TRUE)
ModAlbAvg=colMeans(AlbedoDAT[,1:46])

#####Albedo Change plots####
# #spring
# Histsm<-approx(HistAlbAvg[1:20], n=10)
# Modsm<-approx(ModAlbAvg[1:20],n=10)
# 
# plot(Histsm, type='l',ylim=c(0.13,0.36), main='Spring Albedo Shift', cex.main=2.0,ylab='Mean Albedo', xlab='Month', cex.lab=2.2,yaxt='n',xaxt='n',bty='n')
# lines(Modsm, col='red 4', lwd=5)
# xaxis<-approx(c(1:20), n=5)
# axis(side=1,labels=c(1:5),at=xaxis$y, cex.axis=1.5, font=2)
# axis(side=2, labels=seq(from=0.15, to=0.36, by=0.05), at=seq(from=0.15, to=0.36, by=0.05), cex.axis=1.5, font=2)
# box(,lwd=3)
# lines(Histsm[1:20], lwd=5)
# 
# legend(x=12.5,y=0.35,legend=c('Non-shifted','Shifted'), lwd=5, col=c('red4','black'))
# 
# #Fall
# 
# Histsm<-approx(HistAlbAvg[36:46], n=5)
# Modsm<-approx(ModAlbAvg[36:46],n=5)
# 
# plot(Histsm, type='l',ylim=c(0.13,0.3), main='Fall Albedo Shift', cex.main=2.0,ylab='Mean Albedo', xlab='Month', cex.lab=2.2,yaxt='n',xaxt='n',bty='n')
# lines(Modsm, col='red 4', lwd=5)
# xaxis<-approx(c(1:10), n=3)
# axis(side=1,labels=c(10:12),at=xaxis$y, cex.axis=1.5, font=2)
# axis(side=2, labels=seq(from=0.10, to=0.3, by=0.05), at=seq(from=0.10, to=0.3, by=0.05), cex.axis=1.5, font=2)
# box(,lwd=3)
# lines(Histsm[1:20], lwd=5)
# 
# legend(x=1.1,y=0.29,legend=c('Non-shifted','Shifted'), lwd=5, col=c('red4','black'))
# 
# 
# AlbedoDiffs<-HistAlbs-(AlbedoDAT[,1:12])
# AvgDiffs<-colMeans(AlbedoDiffs, na.rm=TRUE)
# plot(AvgDiffs, type='l', main='albedo change')
#####
####Bring in Solar####
#Bring in transmittance

Trans<-read.csv('Transmittance.csv', skip=7)
Trans<-Trans[2:nrow(Trans),]
Trans_band<-Trans[,5:124]
index.mtr<-matrix(nrow=10,ncol=12)
for(i in 1:12){
  index.mtr[,i]=seq(from=i, to=120, by=12)
}
Months.trans<-data.frame(matrix(nrow=238,ncol=12))
for(j in 1:12){
  Months.trans[,j]<-rowMeans(Trans_band[,index.mtr[,j]]) 
}
names(Months.trans)<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
Trans.avg<-colMeans(Months.trans)

#Bring in Insol
Insol<-read.csv('Insolation.csv', skip=7)
Insol<-Insol[2:nrow(Insol),]
Insol_band<-Insol[,5:124]
Months.insol<-data.frame(matrix(nrow=238,ncol=12))
for(j in 1:12){
  Months.insol[,j]<-rowMeans(Insol_band[,index.mtr[,j]])
}
names(Months.insol)<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
Insol.avg<-colMeans(Months.insol)

InsolPicks<-unname(Insol.avg)
TransPicks<-mean(Trans.avg)
#####
#Calculate RF
TabInsol<-matrix(nrow=nrow(AlbedoDAT),ncol=12, data=rep(InsolPicks, each=nrow(AlbedoDAT)))
TableRF<-(AlbedoDiffs)*TabInsol*(TransPicks^2)
TablePix<-rowMeans(TableRF)
mean(TablePix)
quantile(TablePix,c(0.1,0.9))
AvgRF<-colMeans(TableRF)

#Print RF for uncertainty
#SnowAlbForce<-cbind(Georef_shift, TableRF)
#names(SnowAlbForce)<-c("Lat","Lon","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#write.csv(SnowAlbForce,"SnowAlbedoForce.csv")


##PlotRF
top=c(1:12)
bottom=c(1:12)

for (i in 1:12){
  quant<-c(quantile(TableRF[,i], c(0.1,0.90),na.rm=TRUE))
  top[i]<-quant[2]
  bottom[i]<-quant[1]
  #top<-max(Diffs[,i], na.rm=TRUE)
  #bottom[i]<-min(Diffs[,i], na.rm=TRUE)
}

smtop<-top
  #approx(top, n=12)
smbottom<-bottom
  #approx(bottom, n=12)
smoothRF<-AvgRF
  #approx(AvgRF, n=12)

#Better uncertainty



par(mar=c(5,5,4,2))
plot(AvgRF,type='l',ylim=c(-2,9), main='Snow Albedo RF', cex.main=2.5,ylab='', xlab='', cex.lab=2.2,yaxt='n',xaxt='n',bty='n')
ylab=expression(RF~(Wm^-2))
axis(side=1,labels=c(1:12),at=c(1:12), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=-2, to=9, by=2), at=seq(from=-2, to=9, by=2), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)
box(,lwd=3)
polygon(x=c(1:12,12:1),y=c(smtop,rev(smbottom)),border=NA, col='gray')
lines(AvgRF, lwd=5)
abline(h=0, col='red4', lty=2, lwd=3)

write.csv(AvgRF,'SnowForcing1.csv')

#mean(AvgRF[AvgRF!=0])
#plot(AvgRF, type='l' )

#Snow covered season length

ModLengths<-rep(0,nrow(ModDAT))
EarLengths<-rep(0,nrow(EarDAT))

for(i in 1:nrow(ModDAT)){
  ModLengths[i]<-length(which(ModDAT[i,]!=0))
  EarLengths[i]<-length(which(EarDAT[i,]!=0))

}

mean(ModLengths)
mean(EarLengths)

#Winter and spring changes

WinterDays<-c(1,2,12)
SpringDays<-c(3:5)

mean(rowMeans(TableRF[,SpringDays]))
quantile(rowMeans(TableRF[,SpringDays]), c(0.1,0.9))
#mean(AvgDiffs[SpringDays], na.rm=TRUE)
#mean(AvgDiffs[WinterDays], na.rm=TRUE)

 



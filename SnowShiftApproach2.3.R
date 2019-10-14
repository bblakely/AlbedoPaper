
Modern<-read.csv('GHCN_Modern_Snowanalysis.csv', skip=7)
Early<-read.csv('GHCN_Historic_Snowanalysis.csv', skip=7)
ModAlbedo<-read.csv('Albedo_Modern_Snowanalysis.csv', skip=7)
HistAlbedo<-read.csv('Albedo_Paleo_Snowanalysis.csv', skip=7)
DatAlbedo<-read.csv('Modis_Snowanalysis.csv',skip=7)


usekern<-TRUE #Do you want to calculate forcing with radiative kernel?


#Clip to area of interest
Albedo<-HistAlbedo
Albedo<-Albedo[Albedo$Lon<(-75),]

AlbedoDAT<-Albedo[2:nrow(Albedo),5:50]
AlbedoDAT[AlbedoDAT==9999]<-NaN

#Clear out pixels with no data
#Currently removes pixels with *any* NA dates; using a narm on the sum would allow for some missing dates
Checkvec=rowSums(AlbedoDAT)
AlbedoDAT<-AlbedoDAT[!is.na(Checkvec),]

Georef<-Albedo[!is.na(Checkvec),3:4]
Georef_shift<-Georef
Georef_shift$Lon<-Georef_shift$Lon+0.2 #Not sure why this is done; there must be a misalignment somewhere

####Albedo to months####
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

rm('alb.jan','alb.feb','alb.mar','alb.apr','alb.may','alb.jun','alb.jul','alb.aug','alb.sep','alb.oct','alb.nov','alb.dec')


######
AlbedoDAT<-alb.month

#Clip off IDL line and georef info
EarDAT<-Early[2:nrow(Early),5:16]
EarDAT<-EarDAT[!is.na(Checkvec),]

ModDAT<-Modern[2:nrow(Early),5:16]
ModDAT<-ModDAT[!is.na(Checkvec),]

nobs=nrow(EarDAT)

HistAlbs<-matrix(nrow=nobs,ncol=12)
Shifts<-matrix(nrow=nobs,ncol=12)

#Parameters for how to calc snow forcing

#Set to 'TRUE' when you want to normalize by max total snow (across H and M) and false when you want to normalize by H and M individually
#TRUE makes larger trends
Allmax<-FALSE

#Set to 'TRUE' when you want to limit forcing to spring only. This is most useful when Allmax is false;
#When Allmax is true, Modern tends to get *darker* in winter becasue of the timing of its seasonal peak
SprOnly<-TRUE

#Set to "TRUE" when you want shift plots for diagnosis or sup. figures
Diagplot<-FALSE
#Set of plots, will only be used when Diagplot is true
plots<-sample(1:nrow(HistAlbs),20)


for (n in 1:nobs){
  
  Epix<-unlist(EarDAT[n,1:12]) #Historic swe at pixel
  Mpix<-unlist(ModDAT[n,1:12]) #Modern swe at pixel
  Apix<-unlist(AlbedoDAT[n,1:12]) #Albedo at pixel
  
  #Trumax for versions where we normalize by max total snow
  Allsnow<-c(Epix,Mpix)
  Trumax<-max(Allsnow, na.rm=TRUE)

  #Normalized historic and modern snow
  if(Allmax==TRUE){
    norm.e<-norm.m<-Trumax
    }else{
      norm.e<-max(Epix, na.rm=TRUE)
      norm.m<-max(Mpix, na.rm=TRUE)}
  
  Enorm<-Epix/norm.e 
  Escale<-Enorm #Rename, legacy of older script.

  Mnorm<-Mpix/norm.m
  Mscale<-Mnorm

  
  Scalars<-(Escale-Mscale)
  #((Escale-Mscale)/Escale) #This creates exponential behavior when Escale is small
  
  AlbRange=(max(Apix, na.rm=TRUE)-min(Apix, na.rm=TRUE))
  
  HistAlb<-Apix+(Scalars*AlbRange)
  
  if(SprOnly==TRUE){
  index=c(1:2,10:12) 
  which(HistAlb <= Apix)
  HistAlb[index]<-Apix[index]
  }
  
  HistAlbs[n,]<-HistAlb
  Shifts[n,]<-Scalars

  if(Diagplot==TRUE){
    
  start<-1 #Month to start display
  end<-6 #Month to end display

   if (any(n == plots)){
    plot(Escale,type='l',lty=2, lwd=2, xlab='Month',
       main=paste('Albedo Shift', n), col='white', ylim=c(.1,.8), pch=1.5,
       ylab='Albdeo', xlim=c(1,6),font=2, font.lab=2)

  #axis(1,at=seq(from=1, to=12,length.out=12),labels=c(1:12))
  legend(4.8,0.65, legend=c('Modern','Shifted'),
         col=c('purple', 'orange'), lwd=2, text.font=2, cex=0.8)
  #lines(Mscale, type='l',col='red', lty=2, lwda=2)
  lines(Apix, col='purple',lty=2, lwd=2)
  lines(HistAlb, col='orange', lwd=2)
  box(lwd=3)

  plot(Epix, type='l', main='SWE',xlim=c(1,6), lwd=2,xlab='Month',ylab='SWE',
       font=2, font.lab=2)
  lines(Mpix, col='red', lwd=2)
  legend(4.6,y=max(Epix), legend=c('2000 - 2010', '1900 - 1910'),
         col=c('red', 'black'), lwd=2, text.font=2, cex=0.8)
  box(lwd=3)

  #Normalized SWE
  plot(Enorm, type='l', main='Normalized SWE',xlim=c(1,6), ylab='% Maximum SWE',
        lwd=2,xlab='Month',font=2, font.lab=2)
  lines(Mnorm, col='red', lwd=2)
  legend(4.6,1, legend=c('2000 - 2010', '1900 - 1910'),
          col=c('red', 'black'), lwd=2, text.font=2, cex=0.8)
  box(lwd=3)

  #print(paste('M', Mnorm[3], n))
  #print(paste('H', Enorm[3], n))
  #print(paste('A', Apix[3], n))
  #print(paste('RG', AlbRange, n))
  #print(paste('S', HistAlb[3],n))

 }
  }
}


####Older code used to find large-magnitude plots for supplement (typical shifts were too small to see graphically)####

#shift.mag<-as.matrix(HistAlbs-AlbedoDAT) # All changes in albedo
#shift.avg<-rowMeans(shift.mag[,start:end]) #Avg changes in albdeo (per pixel)
#shift.quant<-quantile(shift.avg, .95, na.rm=TRUE) #Places with large changes in albedo
#shift.large<-which(shift.avg > shift.quant) #Create selection to make clearest plots for supplement

#demoplot<-sample(shift.large, 5)
#for (p in demoplot){
#}
#####

HistAlbAvg=colMeans(HistAlbs, na.rm=TRUE)
ModAlbAvg=colMeans(AlbedoDAT)

AlbedoDiffs<-HistAlbs-AlbedoDAT
#385 looks good
#####Albedo Change plots####
if(Diagplot==TRUE){
#spring
Histsm<-HistAlbAvg[1:6]
Modsm<-ModAlbAvg [1:6]

plot(Histsm, type='l',ylim=c(0.1,0.4), main='Spring Albedo Shift', cex.main=2.0,ylab='Mean Albedo', xlab='Month', cex.lab=2.2,yaxt='n',xaxt='n', bty='n', col='white')
lines(Modsm, col='red 4', lwd=5)
xaxis<-c(1:6)
  #approx(c(1:20), n=5)
axis(side=1,labels=c(1:6),at=c(1:6), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=0.15, to=0.4, by=0.05), at=seq(from=0.15, to=0.4, by=0.05), cex.axis=1.5, font=2)
box(lwd=3)
lines(Histsm[1:20], lwd=5, lty=2)

legend(x=3,y=0.4,legend=c('Non-shifted','Shifted'), lwd=5, col=c('red4','black'))

#Fall

Histsm<-HistAlbAvg[6:12]
Modsm<-ModAlbAvg [6:12]

plot(Histsm, type='l',ylim=c(0.1,0.4), main='Fall Albedo Shift', cex.main=2.0,ylab='Mean Albedo', xlab='Month', cex.lab=2.2,yaxt='n',xaxt='n', bty='n', col='white')
lines(Modsm, col='red 4', lwd=5)
xaxis<-c(6:12)
#approx(c(1:20), n=5)
axis(side=1,labels=c(6:12),at=c(6:12), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=0.15, to=0.4, by=0.05), at=seq(from=0.15, to=0.4, by=0.05), cex.axis=1.5, font=2)
box(lwd=3)
lines(Histsm[1:20], lwd=5, lty=2)
legend(x=3,y=0.4,legend=c('Non-shifted','Shifted'), lwd=5, col=c('red4','black'))


AlbedoDiffs<-HistAlbs-(AlbedoDAT[,1:12])
AvgDiffs<-colMeans(AlbedoDiffs, na.rm=TRUE)
plot(AvgDiffs, type='l', main='albedo change')
}
#####
#### Bring in Solar ####
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
#### Calculate RF ####

#with insolation
TabInsol<-matrix(nrow=nrow(AlbedoDAT),ncol=12, data=rep(InsolPicks, each=nrow(AlbedoDAT)))
TableRF<-(AlbedoDiffs)*TabInsol*(TransPicks^2)

#with raditive kernel
if(usekern==TRUE){
  source('RadKernel_extract.R')
  TableRF<-sweep(-AlbedoDiffs/0.01, 2, albkern,'*')
}

  TablePix<-rowMeans(TableRF)
  mean(TablePix)
  quantile(TablePix,c(0.1,0.9))
  AvgRF<-colMeans(TableRF)
  

#Print RF for uncertainty
#SnowAlbForce<-cbind(Georef_shift, TableRF)
#names(SnowAlbForce)<-c("Lat","Lon","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#write.csv(SnowAlbForce,"SnowAlbedoForce.csv")
#####

#### PLOTS ####
## PlotRF ##
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
plot(AvgRF,type='l',ylim=c(-2,10), main='Snow Albedo RF', cex.main=2.5,ylab='', xlab='', cex.lab=2.2,yaxt='n',xaxt='n',bty='n')
ylab=expression(RF~(Wm^-2))
axis(side=1,labels=seq(from=1, to=12, by=2),at=seq(from=1, to=12, by=2), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=-2, to=14, by=4), at=seq(from=-2, to=14, by=4), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)
box(lwd=3)
polygon(x=c(1:12,12:1),y=c(smtop,rev(smbottom)),border=NA, col='gray')
lines(AvgRF, lwd=5)
abline(h=0, col='red4', lty=2, lwd=3)

dev.copy(png, filename="Figures/SnowAlbedo.png", width=450, height=300);dev.off()

#TIFFTIME

par(mar=c(8,9,6,2))
plot(AvgRF,type='l',ylim=c(-2,10), main='Snow Albedo RF', cex.main=4.5,ylab='', xlab='', cex.lab=2.2,yaxt='n',xaxt='n',bty='n')
ylab=expression(RF~(Wm^-2))
axis(side=1,labels=seq(from=1, to=12, by=2),at=seq(from=1, to=12, by=2), cex.axis=3, font=2, line=0.5, tick=FALSE)
axis(side=2, labels=seq(from=-2, to=14, by=4), at=seq(from=-2, to=14, by=4), cex.axis=3, font=2)
mtext(side=1, text="Month", line=5, cex=3.5, font=2)
mtext(side=2, text=ylab, line=4.5, cex=3.5, font=2)
box(lwd=5)
polygon(x=c(1:12,12:1),y=c(smtop,rev(smbottom)),border=NA, col='gray')
lines(AvgRF, lwd=8)
abline(h=0, col='red4', lty=2, lwd=5)

dev.copy(tiff, filename="Figures/SnowAlbedo.tif", width=450*8, height=300*8, res=300);dev.off()


#####

write.csv(AvgRF,'WriteFile/SnowForcing1.csv')


AvgRF->AlbSnowRF  #Rename to keep in CombineForce

#### Misc. Details. Seasonal numbers and averages ####
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

WinterDays<-c(1,2,11,12)
SpringDays<-c(3:5)

ann.sno<-mean(rowMeans(TableRF)); print(ann.sno)
quantile(rowMeans(TableRF), c(0.05,0.95))
ann.sno.ci<-sd(rowMeans(TableRF))*2
print("albedo snow ann interval");print(c(ann.sno+ann.sno.ci, ann.sno-ann.sno.ci))

spr.sno<-mean(rowMeans(TableRF[,SpringDays])); print(spr.sno)
quantile(rowMeans(TableRF[,SpringDays]), c(0.05,0.95))
spr.sno.ci<-sd(rowMeans(TableRF[,SpringDays]))*2
print("albedo snow spr interval");print(c(spr.sno+spr.sno.ci, spr.sno-spr.sno.ci))


#mean(AvgDiffs[SpringDays], na.rm=TRUE)
#mean(AvgDiffs[WinterDays], na.rm=TRUE)

#histograms of negative and positive shifts
if(Diagplot==TRUE){
for(p in 1:12){
  #print(which(HistAlbs[,p]<0))
  print(length(which(Shifts[,p]<0))/nobs)
  hist(Shifts[,p], xlim=c(-0.5,0.5), main=p)
}
}

#####


#####Raster plotting. Seasonal lengths, monthly plots. Doesn't plot without diagplot on ####
if (Diagplot==TRUE){
#Using seasonal shift scalars
plotshift<-function(datinput, monthabbr, col=rev(colorRampPalette(c("dark red", "beige","forest green"))(10))){
  shifty<-rasterFromXYZ(cbind(Georef_shift$Lon,Georef_shift$Lat,datinput))
  plot(shifty, useRaster=FALSE, main=paste('advance in seasonal melt,', monthabbr), 
       cex.main=0.8, col=col,breaks=seq(-0.4,0.4,length.out=11))
}
par(mfrow=c(2,2))
plotshift(Shifts[,3],'Mar');plotshift(Shifts[,4],'Apr');plotshift(Shifts[,5],'May')
par(mfrow=c(1,1))
}

#Using DOYs with snow

mseas<-mdates<-rep(0,nrow(ModDAT))
eseas<-edates<-rep(0, nrow(ModDAT))

cutoff<-5 #Set to value you want to consider snow-free

#Makes the variables of interest

#Regular version, for prelim plots and analyses
for(r in 1:nrow(ModDAT)){
mvec<-(approx(as.numeric(ModDAT[r,]), n=46)$y)
evec<-approx(as.numeric(EarDAT[r,]), n=46)$y

mdates[r]<-min(which(mvec<cutoff)) #First date with 'no' snow cover
edates[r]<-min(which(evec<cutoff))

mseas[r]<-length(which(mvec>cutoff)) #Total number of dates with snow cover
eseas[r]<-length(which(evec>cutoff))

}


#It's really obnoxious to type the georef stuff repeatedly. This saves the trouble
makeraster<-function(dat){
  ras<-rasterFromXYZ(cbind(Georef_shift$Lon, Georef_shift$Lat,dat))
}

#Make all the rasters
mdates.ra<-makeraster(mdates)
edates.ra<-makeraster(edates)
mseas.ra<-makeraster(mseas)
eseas.ra<-makeraster(eseas)

#Set color palettes
palabs.red<-colorRampPalette(c("dark red","beige")) #For things on one side of zero
palabs.blue<-colorRampPalette(c("dark blue","beige"))
paldiff<-colorRampPalette(c("dark red", "beige","forest green")) #When you have a span on either side of zero, usually for diffs.

# Plots for date of melt. The *8 in all of these is to convert from MODIS dates to days (~8 days per date)
if (Diagplot==TRUE){
par(mfrow=c(1,2))
plot(edates.ra*8, useRaster=FALSE,col=rev(palabs.blue(10)), breaks=seq(10*8,20*8,length.out=11), main='DOY of melt')
plot(mdates.ra*8, useRaster=FALSE,col=rev(palabs.blue(10)), breaks=seq(10*8,20*8,length.out=11), main='DOY of melt')
par(mfrow=c(1,1))
plot((edates.ra-mdates.ra)*8, useRaster=FALSE,col=rev(palabs.red(7)), breaks=seq(0,7*8,length.out=8), main="advancement in snow melt (days)")

#Plots for length of season
par(mfrow=c(1,2))
plot(mseas.ra*8, useRaster=FALSE, col=rev(palabs.blue(9)), breaks=seq(12*8,30*8, length.out=10),main='length of snow season (days)', cex.main=0.8)
plot(eseas.ra*8, useRaster=FALSE, col=rev(palabs.blue(9)), breaks=seq(12*8,30*8, length.out=10), main='length of snow season (days)', cex.main=0.8)
par(mfrow=c(1,1))
plot((mseas.ra-eseas.ra)*8, useRaster=FALSE,col=(paldiff(14)), breaks=seq(-70,70,length.out=15), main="change in snow season length") #seq(-10*8,2*8,length.out=13)


#Steps for getting a non-gappy snow raster for plotting

#Snow data without the gaps and matching georef
EarDAT.cons<-Early[2:nrow(Early),5:16]  #Make a version not matched to albedo for ultimate plotting of snow alone
ModDAT.cons<-Modern[2:nrow(Early),5:16] #Make a version not matched to albedo for ultimate plotting of snow alone

Georef.cons<-Albedo[2:nrow(Albedo),3:4]
Georef_shift.cons<-Georef.cons
Georef_shift.cons$Lon<-Georef_shift.cons$Lon+0.2 #Not sure why this is done; there must be a misalignment somewhere


#Expanded version of product creation
mseas.cons<-rep(0, nrow(ModDAT.cons))
eseas.cons<-rep(0, nrow(EarDAT.cons))

mdates.cons<-rep(0, nrow(ModDAT.cons))
edates.cons<-rep(0, nrow(EarDAT.cons))

for (r in 1:nrow(ModDAT.cons)){
  mvec.cons<-(approx(as.numeric(ModDAT.cons[r,]), n=46)$y)
  evec.cons<-approx(as.numeric(EarDAT.cons[r,]), n=46)$y
  
  mseas.cons[r]<-length(which(mvec.cons>cutoff)) #Total number of dates with snow cover
  eseas.cons[r]<-length(which(evec.cons>cutoff))

  mdates.cons[r]<-min(which(mvec.cons<cutoff), na.rm=TRUE)
  edates.cons[r]<-min(which(evec.cons<cutoff), na.rm=TRUE)
}

seas.diff<-(mseas.cons*8-eseas.cons*8)-4 # *8 for 8 days per date, -4 to center last date. The threshhold could be crossed at any time in the 8 day period and that splits the difference.
seas.prod<-data.frame(cbind(Georef_shift.cons$Lon, Georef_shift.cons$Lat,seas.diff)); colnames(seas.prod)<-c('Lon','Lat','diff')
#write.csv(seas.prod, "WriteFile/Meltseason_diff1.csv", row.names=FALSE)

dates.diff<-(mdates.cons*8-edates.cons*8)-4
dates.prod<-data.frame(cbind(Georef_shift.cons$Lon, Georef_shift.cons$Lat,dates.diff)); colnames(dates.prod)<-c('Lon','Lat','diff')
write.csv(dates.prod, "WriteFile/Dates_diff.csv", row.names=FALSE)


#Plots with direct monthly differences (just curious)
par(mfrow=c(2,4), mar=c(2,2,3,2))
for(i in c(11:12, 1:5)){
  rast.m<-makeraster(ModDAT[i])
  rast.e<-makeraster(EarDAT[i])
  
  plot(rast.m-rast.e, useRaster=FALSE, col=paldiff(16), breaks=c(-115, seq(-70,70, length.out=15),115), main=month.name[i])
  
}


}

#####

####Reporting numbers####
if(reportnum==TRUE){
  
  #Change in snowmelt date
  mean(mseas-eseas)*8
  
  #Albedo snow forcings
  
  spr.sno<-mean(rowMeans(TableRF[,SpringDays])); print(spr.sno) #Spring snow forcing
  spr.sno.ci<-sd(rowMeans(TableRF[,SpringDays]))*2
  print("albedo snow spr interval");print(c(spr.sno+spr.sno.ci, spr.sno-spr.sno.ci))
  
  mean(rowMeans(TableRF))#Annual snow forcing
  ann.sno.ci<-sd(rowMeans(TableRF))*2
  print("albedo snow ann interval");print(c(ann.sno+ann.sno.ci, ann.sno-ann.sno.ci))

}
#####




Modern_day_raw<-read.csv('Modern_day.csv', skip=7)
Modern_night_raw<-read.csv('Modern_night.csv', skip=7)
Paleo_day_raw<-read.csv('Paleo_day.csv', skip=7)
Paleo_night_raw<-read.csv('Paleo_night.csv', skip=7)

ModernVegRaw<-read.csv('LC_GEO.csv', skip=7)
PaleoVegRaw<-read.csv('Paleo_GEO.csv',skip=7)

#More data cleanup

ModernVeg1<-ModernVegRaw[PaleoVegRaw[,5]<15,]
PaleoVeg1<-PaleoVegRaw[PaleoVegRaw[,5]<15,]

ModD1<-Modern_day_raw[PaleoVegRaw[,5]< 15,5:50]
ModN1<-Modern_night_raw[PaleoVegRaw[,5]< 15,5:50]

PalD1<-Paleo_day_raw[PaleoVegRaw[,5]< 15,5:50]
PalN1<-Paleo_night_raw[PaleoVegRaw[,5]< 15,5:50]


PaleoVeg<-PaleoVeg1[ModernVeg1[,5]< 15,]
ModernVeg<-ModernVeg1[ModernVeg1[,5]< 15,]

ModD<-ModD1[ModernVeg1[,5]<15,]
ModN<-ModN1[ModernVeg1[,5]<15,]

ModD[ModD==9999]<-NA
ModN[ModN==9999]<-NA

PalD<-PalD1[ModernVeg1[,5]<15,]
PalN<-PalN1[ModernVeg1[,5]<15,]

PalD[PalD==9999]<-NA
PalN[PalN==9999]<-NA


#Pixels with no change
nochange<- which(ModernVeg$B1==PaleoVeg$B1)

#Pixels where there was land use change
change<-length(which(ModernVeg$B1 != PaleoVeg$B1))

##Types of change
#Forests becoming more deciduous
EGtoD<-which(PaleoVeg$B1==1 & ModernVeg$B1==4)
EGtoM<-which(PaleoVeg$B1==1 & ModernVeg$B1==5)
MtoD<-which(PaleoVeg$B1==5 & ModernVeg$B1==4)

TotDecidify=c(EGtoD,EGtoM,MtoD)

#Deforestation
FtoC<-(which((PaleoVeg$B1==4|PaleoVeg$B1==5|PaleoVeg$B1==1) & ModernVeg$B1==12))
FtoM<-(which((PaleoVeg$B1==4|PaleoVeg$B1==5|PaleoVeg$B1==1) & ModernVeg$B1==14))
FtoU<-which((PaleoVeg$B1==4|PaleoVeg$B1==5|PaleoVeg$B1==1) & ModernVeg$B1==13)
MtoC<-(which(PaleoVeg$B1==14 & ModernVeg$B1==12))

TotDeforest<-c(FtoC,FtoM,FtoU,MtoC)


#Forest becoming more evergreen
DtoEG<-which(PaleoVeg$B1==4 & ModernVeg$B1==1)
MtoEG<-which(PaleoVeg$B1==5 & ModernVeg$B1==1)
DtoM<-which(PaleoVeg$B1==4 & ModernVeg$B1==5)

TotEGify<-c(DtoEG,MtoEG,DtoM)

#Afforestation
CtoF<-(which((ModernVeg$B1==4|ModernVeg$B1==5|ModernVeg$B1==1) & PaleoVeg$B1==12))
MtoF<-(which((ModernVeg$B1==4|ModernVeg$B1==5|ModernVeg$B1==1) & PaleoVeg$B1==14))
CtoM<-(which(PaleoVeg$B1==12 & ModernVeg$B1==14))

TotAfforest<-c(CtoF,MtoF,CtoM)

#Crop/mosiac to urban
CtoU<-which(PaleoVeg$B1==12|PaleoVeg$B1==14 & ModernVeg$B1==13)

TotUrbanize<-c(CtoU,FtoU)

##Surface temperature
STDiff_day<-(ModD-PalD)
STDiff_night<-(ModN-PalN)

#Day/night weighted
jd<-1:365
lat=44.2
lon=-91
tmz=(-6)

Days<-daylength(lat,lon,jd,tmz)
Daylen<-Days[,3]
dayweight=Daylen/24

useweight<-approx(dayweight, n=46)

#Seasons
SummerDays<-c(20:31)
WinterDays<-c(1:8, 43:46)
SpringDays<-c(9:19)

#More Deciduous
DCifyDiffs_D<-STDiff_day[TotDecidify,]
DCifyProf_D<-colMeans(DCifyDiffs_D, na.rm=TRUE)
plot(DCifyProf_D, type='l')

DCifyDiffs_N<-STDiff_night[TotDecidify,]
DCifyProf_N<-colMeans(DCifyDiffs_N, na.rm=TRUE)
plot(DCifyProf_N, type='l')

DCify_Avg<-(DCifyProf_D*useweight$y)+(DCifyProf_N*(1-useweight$y))

mean(DCify_Avg[c(SpringDays,WinterDays)])

#Deforestation
DefDiffs_D<-STDiff_day[TotDeforest,]
DefProf_D<-colMeans(DefDiffs_D, na.rm=TRUE)
plot(DefProf_D, type='l')

DefDiffs_N<-STDiff_night[TotDeforest,]
DefProf_N<-colMeans(DefDiffs_N, na.rm=TRUE)
plot(DefProf_N, type='l')

Def_Avg<-(DefProf_D*useweight$y)+(DefProf_N*(1-useweight$y))

mean(Def_Avg[c(SummerDays)])
mean(Def_Avg[c(WinterDays)])

#More Evergreen
EGDiffs_D<-STDiff_day[TotEGify,]
EGProf_D<-colMeans(EGDiffs_D, na.rm=TRUE)
plot(EGProf_D, type='l')

EGDiffs_N<-STDiff_night[TotEGify,]
EGProf_N<-colMeans(EGDiffs_N, na.rm=TRUE)
plot(EGProf_N, type='l')

#Afforestation
AffDiffs_D<-STDiff_day[TotAfforest,]
AffProf_D<-colMeans(AffDiffs_D, na.rm=TRUE)
plot(AffProf_D, type='l')

AffDiffs_N<-STDiff_night[TotAfforest,]
AffProf_N<-colMeans(AffDiffs_N, na.rm=TRUE)
plot(AffProf_N, type='l')

#Urbanization
UrbDiffs_D<-STDiff_day[TotUrbanize,]
UrbProf_D<-colMeans(UrbDiffs_D, na.rm=TRUE)
plot(UrbProf_D, type='l')

UrbDiffs_N<-STDiff_night[TotUrbanize,]
UrbProf_N<-colMeans(UrbDiffs_N, na.rm=TRUE)
plot(UrbProf_N, type='l')

Urb_Avg<-(UrbProf_D*useweight$y)+(UrbProf_N*(1-useweight$y))



#PLOT ALL THE THINGS

###Daytime ST plot
##Deforest only
par(mar=c(5,5,5,3))
plot(DefProf_D, ylim=c(-1.5,3), type='l', col='white', xaxt='n',cex.lab=1.5, cex.main=2.5, 
     yaxt='n',main= "Daytime ST Change", xlab='Month', ylab="ST Change (Deg.C)")
polygon(x=c(18,18,22,22),y=c(-2,4,4,-2), col='beige', border=NA)
#polygon(x=c(35,35,38,38),y=c(-3,3,3,-3), col='beige', border=NA)
abline(h=0, lty=2, lwd=2)
abline(v=c(12,20,31,39), lty=3, lwd=1)
lines(DefProf_D,col='dark orange', lwd=3)
#lines(EGProf_D, col='purple', lwd=3)
#lines(AffProf_D, col='green',lwd=3)
#Look nicer
box(lwd=2)
axis(side=2, labels= seq(from=-1.5, to=3, by=0.5), at=seq(from=-1.5, to=3, by=0.5), cex.axis=1.5, font=2)
axis(side=1, labels= seq(from=1, to=12, length.out=12), at= seq(from=1, to=46, length.out=12), cex.axis=1.5, font=2)
# legend(x=1,y=3, legend=c('Deforestation', 'Leaf Out'), 
#        col=c('dark orange','beige'),lwd=c(2,NA),
#        pch=c(NA,15),cex=0.85, pt.cex=2)
#Without line label
legend(x=1,y=3, legend=c('Leaf Out'), 
       col=c('beige'),
       pch=c(15),cex=0.85, pt.cex=2)


##Deforest and comp shift combined
plot(DefProf_D, ylim=c(-1.5,3), type='l', col='white', xaxt='n',cex.lab=1.5, cex.main=2.5, 
     yaxt='n',main= "Daytime ST Change", xlab='Month', ylab="ST Change (Deg. C)")
polygon(x=c(18,18,22,22),y=c(-2,4,4,-2), col='beige', border=NA)
#polygon(x=c(35,35,38,38),y=c(-3,3,3,-3), col='beige', border=NA)
abline(h=0, lty=2, lwd=2)
abline(v=c(12,20,31,39), lty=3, lwd=1)
#lines(DefProf_D,col='dark orange', lwd=3)
lines(DCifyProf_D, col='forest green', lwd=3)
#lines(EGProf_D, col='purple', lwd=3)
#lines(AffProf_D, col='green',lwd=3)

#Look nicer
box(lwd=2)
axis(side=2, labels= seq(from=-1.5, to=3, by=0.5), at=seq(from=-1.5, to=3, by=0.5), cex.axis=1.5, font=2)
axis(side=1, labels= seq(from=1, to=12, length.out=12), at= seq(from=1, to=46, length.out=12), cex.axis=1.5, font=2)
legend(x=1,y=3, legend=c('Leaf Out'), 
       col=c('beige'),
      pch=c(15),cex=0.85, pt.cex=2)

# legend(x=1,y=3, legend=c('Deforestation', 'Comp. Shift', 'Leaf Out'), 
#        col=c('dark orange','forest green','beige'),lwd=c(2,2, NA),
#       pch=c(NA,NA,15),cex=0.85, pt.cex=2)

###Nighttime ST plot
##Deforest only
par(mar=c(5,5,5,3))
plot(DefProf_N, ylim=c(-1.5,3), type='l', col='white',xaxt='n',cex.lab=1.5, cex.main=2.5, 
     yaxt='n',main= "Nighttime ST Change", xlab='Month', ylab="ST Change (Deg.C)")
abline(h=0, lty=2, lwd=2)
abline(v=c(12,20,31,39), lty=3, lwd=1)
lines(DefProf_N, col='dark orange', lwd=3)
#lines(EGProf_N, col='purple',lty=2)
#lines(AffProf_N, col='green',lty=2)

#Pretty!
box(lwd=2)
axis(side=2, labels= seq(from=-1.5, to=3, by=0.5), at=seq(from=-1.5, to=3, by=0.5), cex.axis=1.5, font=2)
axis(side=1, labels= seq(from=1, to=12, length.out=12), at= seq(from=1, to=46, length.out=12), cex.axis=1.5, font=2)
# legend(x=1,y=2.5, legend=c('Deforestation'), 
#        col=c('dark orange'),lwd=c(2),
#        cex=0.85, pt.cex=2)


#Deciduous
plot(DefProf_N, ylim=c(-1.5,3), type='l', col='white',xaxt='n',cex.lab=1.5, cex.main=2.5, 
     yaxt='n',main= "Nighttime ST Change", xlab='Month', ylab="ST Change (Deg. C)")
abline(h=0, lty=2, lwd=2)
abline(v=c(12,20,31,39), lty=3, lwd=1)
#lines(DefProf_N, col='dark orange', lwd=3)
lines(DCifyProf_N, col='forest green', lwd=3)
#lines(EGProf_N, col='purple',lty=2)
#lines(AffProf_N, col='green',lty=2)

#Pretty!
box(lwd=2)
axis(side=2, labels= seq(from=-1.5, to=3, by=0.5), at=seq(from=-1.5, to=3, by=0.5), cex.axis=1.5, font=2)
axis(side=1, labels= seq(from=1, to=12, length.out=12), at= seq(from=1, to=46, length.out=12), cex.axis=1.5, font=2)
# legend(x=1,y=2.5, legend=c('Deforestation', 'Comp. Shift'), 
#        col=c('dark orange','forest green'),lwd=c(2,2),
#        cex=0.85, pt.cex=2)

#BarplotExperiment
barplot(c(length(TotDeforest),length(nochange),length(TotDecidify),length(TotAfforest),length(TotEGify)), col=c('dark orange','gray','forest green','dark blue','purple 4'),names.arg=c('Deforestation','No Change/Recovered','More Decid.','Afforestation','More Evergreen'), ylab='Number of pixels')


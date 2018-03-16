setwd('F:/R/MS_Figures')
Modern_day_raw<-read.csv('Modern_day.csv', skip=7)
Modern_night_raw<-read.csv('Modern_night.csv', skip=7)
Paleo_day_raw<-read.csv('Paleo_day.csv', skip=7)
Paleo_night_raw<-read.csv('Paleo_night.csv', skip=7)

ModernVegRaw<-read.csv('LC_GEO.csv', skip=7)
PaleoVegRaw<-read.csv('Paleo_GEO.csv',skip=7)

ModernVegRaw<-ModernVegRaw[2:nrow(ModernVegRaw),]
PaleoVegRaw<-PaleoVegRaw[2:nrow(PaleoVegRaw),]
Modern_day_raw<-Modern_day_raw[2:nrow(Modern_day_raw),]
Paleo_day_raw<-Paleo_day_raw[2:nrow(Paleo_day_raw),]
Modern_night_raw<-Modern_night_raw[2:nrow(Modern_night_raw),]
Paleo_night_raw<-Paleo_night_raw[2:nrow(Paleo_night_raw),]

#More data cleanup

#Take out places where Paleo has bad values
ModernVeg1<-ModernVegRaw[PaleoVegRaw[,5]<15,]
PaleoVeg1<-PaleoVegRaw[PaleoVegRaw[,5]<15,]

ModD1<-Modern_day_raw[PaleoVegRaw[,5]< 15,5:50]
ModN1<-Modern_night_raw[PaleoVegRaw[,5]< 15,5:50]

PalD1<-Paleo_day_raw[PaleoVegRaw[,5]< 15,5:50]
PalN1<-Paleo_night_raw[PaleoVegRaw[,5]< 15,5:50]

convert.code.1<-convert.code[PaleoVegRaw[,5]< 15]

#Take out places where modern has bad values
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

convert.code.split<-convert.code.1[ModernVeg1[,5]< 15]

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


######To months, day####
d.st.jan<-rowMeans(STDiff_day[1:4],na.rm=TRUE)
d.st.feb<-rowMeans(STDiff_day[5:8],na.rm=TRUE)
d.st.mar<-rowMeans(STDiff_day[9:12],na.rm=TRUE)
d.st.apr<-rowMeans(STDiff_day[13:15],na.rm=TRUE)
d.st.may<-rowMeans(STDiff_day[16:19],na.rm=TRUE)
d.st.jun<-rowMeans(STDiff_day[20:23],na.rm=TRUE)
d.st.jul<-rowMeans(STDiff_day[24:27],na.rm=TRUE)
d.st.aug<-rowMeans(STDiff_day[28:31],na.rm=TRUE)
d.st.sep<-rowMeans(STDiff_day[32:35],na.rm=TRUE)
d.st.oct<-rowMeans(STDiff_day[36:38],na.rm=TRUE)
d.st.nov<-rowMeans(STDiff_day[39:42],na.rm=TRUE)
d.st.dec<-rowMeans(STDiff_day[43:46],na.rm=TRUE)
d.st.month<-data.frame(cbind(d.st.jan,d.st.feb,d.st.mar,d.st.apr,d.st.may,
                              d.st.jun,d.st.jul,d.st.aug,d.st.sep,d.st.oct,d.st.nov,d.st.dec))

#####
STDiff_day<-d.st.month


######To months, night####
n.st.jan<-rowMeans(STDiff_night[1:4],na.rm=TRUE)
n.st.feb<-rowMeans(STDiff_night[5:8],na.rm=TRUE)
n.st.mar<-rowMeans(STDiff_night[9:12],na.rm=TRUE)
n.st.apr<-rowMeans(STDiff_night[13:15],na.rm=TRUE)
n.st.may<-rowMeans(STDiff_night[16:19],na.rm=TRUE)
n.st.jun<-rowMeans(STDiff_night[20:23],na.rm=TRUE)
n.st.jul<-rowMeans(STDiff_night[24:27],na.rm=TRUE)
n.st.aug<-rowMeans(STDiff_night[28:31],na.rm=TRUE)
n.st.sep<-rowMeans(STDiff_night[32:35],na.rm=TRUE)
n.st.oct<-rowMeans(STDiff_night[36:38],na.rm=TRUE)
n.st.nov<-rowMeans(STDiff_night[39:42],na.rm=TRUE)
n.st.dec<-rowMeans(STDiff_night[43:46],na.rm=TRUE)
n.st.month<-data.frame(cbind(n.st.jan,n.st.feb,n.st.mar,n.st.apr,n.st.may,
                             n.st.jun,n.st.jul,n.st.aug,n.st.sep,n.st.oct,n.st.nov,n.st.dec))

#####

STDiff_night<-n.st.month

#Day/night weighted
jd<-1:365
lat=44.2
lon=-91
tmz=(-6)

Days<-daylength(lat,lon,jd,tmz)
Daylen<-Days[,3]
dayweight=Daylen/24

useweight<-approx(dayweight, n=12)

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

#####Plots#####

#PLOT ALL THE THINGS
par(xpd=FALSE)
###Daytime ST plot
#####Deforest####

#Better Uncertainty
poss<-sort(unique(convert.code.split[TotDeforest]))
convert.code.split.def<-convert.code.split[TotDeforest]
list.ind.split.def<-list()
for(i in 1:length(poss)){
  list.ind.split.def[[i]]<-which(convert.code.split.def==poss[i])
}

var.set.lst.def=matrix(nrow=12, ncol=length(poss))
count.set=rep(0, length(poss))  
for(j in 1:length(poss)){
  DefSet<-DefDiffs_D[list.ind.split.def[[j]],]
  count.set[j]<-length(list.ind.split.def[[j]])/length(which(!is.na(convert.code.split)))
  for (i in 1:ncol(DefSet)){
    var.set.lst.def[i,j]<-sd(DefSet[,i], na.rm=TRUE)
  }
}

##Scale var.set by counts
var.scl.lst.def<-sweep(var.set.lst.def,2,count.set,'*')

#Sum across veg conversions
uncertainty.lst.def<-rowSums(var.scl.lst.def, na.rm=TRUE)


top=c(1:12)
bottom=c(1:12)
for (i in 1:12){
  quant<-c(quantile(DefDiffs_D[,i], c(0.1,0.90),na.rm=TRUE))
  top[i]<-quant[2]
  bottom[i]<-quant[1]
  #top<-max(Diffs[,i], na.rm=TRUE)
  #bottom[i]<-min(Diffs[,i], na.rm=TRUE)
}

par(mar=c(5,5,5,3))
plot(DefProf_D, ylim=c(-2,5), type='l', col='white', xaxt='n',cex.lab=1.5, cex.main=2.5, 
     yaxt='n',main= "Daytime LST Change", xlab='', ylab="")
ylab<-expression(Delta~LST~(degree*C))
polygon(x=c(1:12,12:1),y=c(top,rev(bottom)),border=NA, col='gray')
lines(DefProf_D,col='dark orange', lwd=3)
box(,lwd=2)
abline(h=0, lty=2, lwd=2)
#lines(EGProf_D, col='purple', lwd=3)
#lines(AffProf_D, col='green',lwd=3)
box(lwd=2)
axis(side=2, labels= seq(from=-2, to=5, by=1), at=seq(from=-2, to=5, by=1), cex.axis=1.5, font=2)
axis(side=1, labels= seq(from=1, to=12, length.out=12), at= seq(from=1, to=12, length.out=12), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)
#####
####Comp shift####
top=c(1:12)
bottom=c(1:12)
for (i in 1:12){
  quant<-c(quantile(DCifyDiffs_D[,i], c(0.1,0.90),na.rm=TRUE))
  top[i]<-quant[2]
  bottom[i]<-quant[1]
  #top<-max(Diffs[,i], na.rm=TRUE)
  #bottom[i]<-min(Diffs[,i], na.rm=TRUE)
}
plot(DefProf_D, ylim=c(-2,5), type='l', col='white', xaxt='n',cex.lab=1.5, cex.main=2.5, 
     yaxt='n',main= "Daytime LST Change", xlab='', ylab="")
ylab<-expression(Delta~LST~(degree*C))
polygon(x=c(1:12,12:1),y=c(top,rev(bottom)),border=NA, col='gray')
abline(h=0, lty=2, lwd=2)
#lines(DefProf_D,col='dark orange', lwd=3)
lines(DCifyProf_D, col='forest green', lwd=3)
#lines(EGProf_D, col='purple', lwd=3)
#lines(AffProf_D, col='green',lwd=3)
#Look nicer
box(lwd=2)
axis(side=2, labels= seq(from=-2, to=5, by=1), at=seq(from=-2, to=5, by=1), cex.axis=1.5, font=2)
axis(side=1, labels= seq(from=1, to=12, length.out=12), at= seq(from=1, to=12, length.out=12), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)
#####


###Nighttime ST plot
####Deforest####

top=c(1:12)
bottom=c(1:12)
for (i in 1:12){
  quant<-c(quantile(DefDiffs_N[,i], c(0.1,0.90),na.rm=TRUE))
  top[i]<-quant[2]
  bottom[i]<-quant[1]
  #top<-max(Diffs[,i], na.rm=TRUE)
  #bottom[i]<-min(Diffs[,i], na.rm=TRUE)
}

par(mar=c(5,5,5,3))
plot(DefProf_N, ylim=c(-2.1,5), type='l', col='white',xaxt='n',cex.lab=1.5, cex.main=2.5, 
     yaxt='n',main= "Nighttime LST Change", xlab='', ylab='')
ylab<-expression(Delta~LST~(degree*C))
polygon(x=c(1:12,12:1),y=c(top,rev(bottom)),border=NA, col='gray')
abline(h=0, lty=2, lwd=2)
lines(DefProf_N, col='dark orange', lwd=3)
#lines(EGProf_N, col='purple',lty=2)
#lines(AffProf_N, col='green',lty=2)

#Pretty!
box(lwd=2)
axis(side=2, labels= seq(from=-2, to=5, by=1), at=seq(from=-2, to=5, by=1), cex.axis=1.5, font=2)
axis(side=1, labels= seq(from=1, to=12, length.out=12), at= seq(from=1, to=12, length.out=12), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)
#####

####Deciduous####

top=c(1:12)

bottom=c(1:12)
for (i in 1:12){
  quant<-c(quantile(DCifyDiffs_N[,i], c(0.1,0.90),na.rm=TRUE))
  top[i]<-quant[2]
  bottom[i]<-quant[1]
  #top<-max(Diffs[,i], na.rm=TRUE)
  #bottom[i]<-min(Diffs[,i], na.rm=TRUE)
}


plot(DCifyProf_N, ylim=c(-2,5), type='l', col='white',xaxt='n',cex.lab=1.5, cex.main=2.5, 
     yaxt='n',main= "Nighttime LST Change", xlab='', ylab="")
ylab<-expression(Delta~LST~(degree*C))
polygon(x=c(1:12,12:1),y=c(top,rev(bottom)),border=NA, col='gray')
abline(h=0, lty=2, lwd=2)
lines(DCifyProf_N, col='forest green', lwd=3)
#lines(EGProf_N, col='purple',lty=2)
#lines(AffProf_N, col='green',lty=2)

#Pretty!
box(lwd=2)
axis(side=2, labels= seq(from=-2, to=5, by=1), at=seq(from=-2, to=5, by=1), cex.axis=1.5, font=2)
axis(side=1, labels= seq(from=1, to=12, length.out=12), at= seq(from=1, to=12, length.out=12), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)
#####


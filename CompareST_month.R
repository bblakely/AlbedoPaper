#Read in data
Modern_day_raw<-read.csv('Modern_day.csv', skip=7)
Modern_night_raw<-read.csv('Modern_night.csv', skip=7)
Paleo_day_raw<-read.csv('Paleo_day.csv', skip=7)
Paleo_night_raw<-read.csv('Paleo_night.csv', skip=7)

source('VegConvert_GEO.R')

nobs=nrow(Modern_day_raw)

Georef<-Paleo_day_raw[2:nobs, 3:4]

ModD_Dat<-Modern_day_raw[2:nobs,5:50]
ModD_Dat[ModD_Dat==9999]<-NaN

ModN_Dat<-Modern_night_raw[2:nobs,5:50]
ModN_Dat[ModN_Dat==9999]<-NaN

PalD_Dat<-Paleo_day_raw[2:nobs,5:50]
PalD_Dat[PalD_Dat==9999]<-NaN

PalN_Dat<-Paleo_night_raw[2:nobs,5:50]
PalN_Dat[PalN_Dat==9999]<-NaN

#Vectors of Differences in temp

DayDiffs=ModD_Dat-PalD_Dat
DayDiffu=colMeans(DayDiffs, na.rm=TRUE)
plot(DayDiffu, type='l')

####Day Temp To Months####
d.temp.jan<-rowMeans(DayDiffs[1:4],na.rm=TRUE)
d.temp.feb<-rowMeans(DayDiffs[5:8],na.rm=TRUE)
d.temp.mar<-rowMeans(DayDiffs[9:12],na.rm=TRUE)
d.temp.apr<-rowMeans(DayDiffs[13:15],na.rm=TRUE)
d.temp.may<-rowMeans(DayDiffs[16:19],na.rm=TRUE)
d.temp.jun<-rowMeans(DayDiffs[20:23],na.rm=TRUE)
d.temp.jul<-rowMeans(DayDiffs[24:27],na.rm=TRUE)
d.temp.aug<-rowMeans(DayDiffs[28:31],na.rm=TRUE)
d.temp.sep<-rowMeans(DayDiffs[32:35],na.rm=TRUE)
d.temp.oct<-rowMeans(DayDiffs[36:38],na.rm=TRUE)
d.temp.nov<-rowMeans(DayDiffs[39:42],na.rm=TRUE)
d.temp.dec<-rowMeans(DayDiffs[43:46],na.rm=TRUE)
d.temp.month<-data.frame(cbind(d.temp.jan,d.temp.feb,d.temp.mar,d.temp.apr,d.temp.may,
                              d.temp.jun,d.temp.jul,d.temp.aug,d.temp.sep,d.temp.oct,d.temp.nov,d.temp.dec))
#plot(colMeans(d.temp.month, na.rm=TRUE),type='l')
#####

DayDiffs<-d.temp.month
DayDiffu=colMeans(DayDiffs, na.rm=TRUE)
lines(approx(DayDiffu, n=46)$y,col='green')

###Night
NightDiffs=ModN_Dat-PalN_Dat
NightDiffu=colMeans(NightDiffs, na.rm=TRUE)
plot(NightDiffu)


####Night Temp To Months####
n.temp.jan<-rowMeans(NightDiffs[1:4],na.rm=TRUE)
n.temp.feb<-rowMeans(NightDiffs[5:8],na.rm=TRUE)
n.temp.mar<-rowMeans(NightDiffs[9:12],na.rm=TRUE)
n.temp.apr<-rowMeans(NightDiffs[13:15],na.rm=TRUE)
n.temp.may<-rowMeans(NightDiffs[16:19],na.rm=TRUE)
n.temp.jun<-rowMeans(NightDiffs[20:23],na.rm=TRUE)
n.temp.jul<-rowMeans(NightDiffs[24:27],na.rm=TRUE)
n.temp.aug<-rowMeans(NightDiffs[28:31],na.rm=TRUE)
n.temp.sep<-rowMeans(NightDiffs[32:35],na.rm=TRUE)
n.temp.oct<-rowMeans(NightDiffs[36:38],na.rm=TRUE)
n.temp.nov<-rowMeans(NightDiffs[39:42],na.rm=TRUE)
n.temp.dec<-rowMeans(NightDiffs[43:46],na.rm=TRUE)
n.temp.month<-data.frame(cbind(n.temp.jan,n.temp.feb,n.temp.mar,n.temp.apr,n.temp.may,
                               n.temp.jun,n.temp.jul,n.temp.aug,n.temp.sep,n.temp.oct,n.temp.nov,n.temp.dec))
#plot(colMeans(n.temp.month, na.rm=TRUE),type='l')
#####
NightDiffs<-n.temp.month
NightDiffu=colMeans(NightDiffs, na.rm=TRUE)
lines(approx(NightDiffu, n=46)$y, col='green')

#Unweighted
#MeanDiffs=(NightDiffu+DayDiffu)/2

#Day/night weighted
library('insol')
jd<-1:365
lat=44.2
lon=-91
tmz=(-6)

Days<-daylength(lat,lon,jd,tmz)
Daylen<-Days[,3]
dayweight=Daylen/24

useweight_temp<-approx(dayweight, n=12)
useweight_force<-approx(dayweight, n=46)

#Tabular weighting
Tabweight<-matrix(nrow=24650,ncol=12, data=rep(useweight_temp$y, each=24650))
TableDiffs<-data.frame(DayDiffs*Tabweight+NightDiffs*(1-Tabweight))
AvgDiffs<-colMeans(TableDiffs, na.rm=TRUE)

AvgDiffs_u<-(DayDiffu*useweight_temp$y)+(NightDiffu*(1-useweight_temp$y))


SummerDays<-c(6:8)
WinterDays<-c(1:2, 12)
SpringDays<-c(3:5)
mean(AvgDiffs[SummerDays])
mean(AvgDiffs[WinterDays])

plot(AvgDiffs, type='l',ylim=c(-.5,1), main='Day and Night ST')
abline(h=0, lty=2)
lines(NightDiffu, col='forest green')
lines(DayDiffu, col='red')
legend(x=1,y=1, legend=c('Night','Day'), col=c('forest green','red'), lty=c(1,1))


#Forcing
SB=5.67e-8

HtempsDay<-PalD_Dat
HtempsNight<-PalN_Dat

MtempsDay<-ModD_Dat
MtempsNight<-ModN_Dat

#Daytime
HforceDay<-(HtempsDay^4)*SB*.96
MforceDay<-(MtempsDay^4)*SB*.96

ForcingsDay<-HforceDay-MforceDay
AdjforcingDay<-(-368/390)*ForcingsDay

AdjDay<-colMeans(AdjforcingDay, na.rm=TRUE)

#Nighttime
HforceNight<-(HtempsNight^4)*SB*.96
MforceNight<-(MtempsNight^4)*SB*.96

ForcingsNight<-HforceNight-MforceNight
AdjforcingNight<-(-368/390)*ForcingsNight

AdjNight<-colMeans(AdjforcingNight, na.rm=TRUE)

#Tabular all forcings
#No weight
#TableForce<-(AdjforcingDay+AdjforcingNight)/2

#Weighted
Tabweight<-matrix(nrow=24650,ncol=46, data=rep(useweight_force$y, each=24650))
TableForce<-data.frame(AdjforcingDay*Tabweight+AdjforcingNight*(1-Tabweight))
TableForce_long<-TableForce
plot(colMeans(TableForce_long, na.rm=TRUE), type='l')

####Radiative forcing to month####
rf.temp.jan<-rowMeans(TableForce[1:4],na.rm=TRUE)
rf.temp.feb<-rowMeans(TableForce[5:8],na.rm=TRUE)
rf.temp.mar<-rowMeans(TableForce[9:12],na.rm=TRUE)
rf.temp.apr<-rowMeans(TableForce[13:15],na.rm=TRUE)
rf.temp.may<-rowMeans(TableForce[16:19],na.rm=TRUE)
rf.temp.jun<-rowMeans(TableForce[20:23],na.rm=TRUE)
rf.temp.jul<-rowMeans(TableForce[24:27],na.rm=TRUE)
rf.temp.aug<-rowMeans(TableForce[28:31],na.rm=TRUE)
rf.temp.sep<-rowMeans(TableForce[32:35],na.rm=TRUE)
rf.temp.oct<-rowMeans(TableForce[36:38],na.rm=TRUE)
rf.temp.nov<-rowMeans(TableForce[39:42],na.rm=TRUE)
rf.temp.dec<-rowMeans(TableForce[43:46],na.rm=TRUE)
rf.temp.month<-data.frame(cbind(rf.temp.jan,rf.temp.feb,rf.temp.mar,rf.temp.apr,rf.temp.may,
                               rf.temp.jun,rf.temp.jul,rf.temp.aug,rf.temp.sep,rf.temp.oct,rf.temp.nov,rf.temp.dec))
#plot(colMeans(rf.temp.month, na.rm=TRUE),type='l')
#####

TableForce<-rf.temp.month
TabRF<-colMeans(TableForce, na.rm=TRUE)
#lines(approx(TabRF,n=46)$y, col='green')

#Write CSV for forcing standardization

#STForceDat<-cbind(Georef,TableForce)
#write.csv(STForceDat, "STForcing.csv")

TabPix<-rowMeans(TableForce, na.rm=TRUE)
quantile(TabPix, c(0.1,0.9), na.rm=TRUE)

SummerRF<-rowMeans(TableForce[,SummerDays])
mean(SummerRF, na.rm=TRUE)
quantile(SummerRF, c(0.1,0.9), na.rm=TRUE)

WinterRF<-rowMeans(TableForce[,WinterDays])
mean(WinterRF, na.rm=TRUE)
quantile(WinterRF, c(0.1,0.9), na.rm=TRUE)

#Net
#old unweighted:
#STNetForce=(AdjNight+AdjDay)/2

#Aggregate before subsetting by days (old)
#new weighted

STNetForce<-(AdjDay*useweight_force$y)+(AdjNight*(1-useweight_force$y))
STNetForceTab<-TabRF

mean(STNetForceTab[WinterDays])
mean(STNetForceTab[SummerDays])


####Apply vegetation conversion subsetting to Surface Temp####

var.set.force=matrix(nrow=12, ncol=26)
var.set.lst=matrix(nrow=12, ncol=26)
count.set=rep(0, length(poss))  

for(j in 1:length(poss)){
  LSTSet.force<-rf.temp.month[list.ind[[j]],]
  LSTSet.lst<-TableDiffs[list.ind[[j]],]
  count.set[j]<-length(list.ind[[j]])/length(which(!is.na(convert.code)))
  for (i in 1:ncol(LSTSet.force)){
    var.set.force[i,j]<-sd(LSTSet.force[,i], na.rm=TRUE)
    var.set.lst[i,j]<-sd(LSTSet.lst[,i], na.rm=TRUE)
  }
}

##Scale var.set by counts
var.scl.force<-sweep(var.set.force,2,count.set,'*')
var.scl.lst<-sweep(var.set.lst,2,count.set,'*')
colnames(var.scl.force)<-poss


#Sum across veg conversions
uncertainty.force<-rowSums(var.scl.force, na.rm=TRUE)
uncertainty.lst<-rowSums(var.scl.lst, na.rm=TRUE)


###Deforest and Comp shift figure###

STForce.veg<-cbind(TableForce, convert.code)

Deforest<-c(7:11,13,21,22,25)    #This one is EG, MX, or DC forest to C, M, or U only
Deforest.2<-c(-2,7:13,21,22,25)  #This one includes mosaic to urban and mosaic to crop
Comp<-c(-1,3,4) #This one is E to MX, E to DC, or MX to DC

STForce.def<-STForce.veg[which(STForce.veg[,13]%in%Deforest),]
STForce.avg.def<-colMeans(STForce.def[,1:12], na.rm=TRUE)
var.scl.def<-var.scl.force[,which(as.numeric(colnames(var.scl.force))%in%Deforest)]
uncertainty.def<-rowSums(var.scl.def)


STForce.comp<-STForce.veg[which(STForce.veg[,13]%in%Comp),]
STForce.avg.comp<-colMeans(STForce.comp[,1:12], na.rm=TRUE)
var.scl.comp<-var.scl.force[,which(as.numeric(colnames(var.scl.force))%in%Comp)]
uncertainty.comp<-rowSums(var.scl.comp)

plot(STForce.avg.comp, type='l', col='forest green', ylim=c(-4, 8),lwd=2)
abline(h=0)

###ALL the plots

#Deforestation forcing
par(mar=c(5,6,4,2))
ylab<-expression(RF~(Wm^-2))
plot(STForce.avg.def, type='l', col='orange', ylim=c(-4, 8), lwd=2, main="Deforestation",cex.main=2.5, ylab="", xlab="",cex.lab=2.1,yaxt='n',xaxt='n',bty='n')
axis(side=1,labels=seq(from=1, to=12, by=2),at=seq(from=1, to=12, by=2), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=-4, to=8, by=3), at=seq(from=-4, to=8, by=3), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=3, cex=2.0, font=2)
polygon(x=c(1:12,12:1),y=c(STForce.avg.def+1.96*uncertainty.def,rev(STForce.avg.def-1.96*uncertainty.def)),border=NA, col='palegoldenrod')
lines(STForce.avg.def,  col='orange', ylim=c(-12, 1), lwd=2)
box(lwd=3)
abline(h=0, col='red4', lty=2, lwd=3)


#Comp shift forcing
ylab<-expression(RF~(Wm^-2))
plot(STForce.avg.comp, type='l', col='forest green', ylim=c(-4, 8), lwd=2, main="Comp Shift",cex.main=2.5, ylab="", xlab="",cex.lab=2.1,yaxt='n',xaxt='n',bty='n')
axis(side=1,labels=seq(from=1, to=12, by=2),at=seq(from=1, to=12, by=2), cex.axis=1.5, font=2)
#axis(side=1,labels=c(1:12),at=c(1:12), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=-4, to=8, by=3), at=seq(from=-4, to=8, by=3), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=3, cex=2.0, font=2)
polygon(x=c(1:12,12:1),y=c(STForce.avg.comp+1.96*uncertainty.comp,rev(STForce.avg.comp-1.96*uncertainty.comp)),border=NA, col='darkseagreen1')
lines(STForce.avg.comp,  col='forest green', ylim=c(-12, 1), lwd=2)
box(lwd=3)
abline(h=0, col='red4', lty=2, lwd=3)

par(xpd=FALSE)

# Regional ST change
par(mar=c(5,5,4,2))
ylab<-expression(Delta~LST~(degree*C))
plot(AvgDiffs,type='l',ylim=c(-1.1,1), main='LST Change', cex.main=2.5,ylab='', xlab='', cex.lab=2.2,yaxt='n',xaxt='n',bty='n')
axis(side=1,labels=seq(from=1, to=12, by=2),at=seq(from=1, to=12, by=2), cex.axis=1.5, font=2)
#axis(side=1,labels=c(1:12),at=c(1:12), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=-1, to=1.5, by=0.5), at=seq(from=-1, to=1.5, by=0.5), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)
box(lwd=3)
polygon(x=c(1:12,12:1),y=c(AvgDiffs+1.96*uncertainty.lst,rev(AvgDiffs-1.96*uncertainty.lst)),border=NA, col='gray')
lines(AvgDiffs, lwd=5)
abline(h=0, col='red4', lty=2, lwd=3)


smoothRF<-TabRF
#Regional ST forcing
par(mar=c(5,5,4,2))
ylab<-expression(RF~(Wm^-2))
plot(smoothRF,type='l',ylim=c(-4,5), main='LST RF', cex.main=2.5,ylab='', xlab='', cex.lab=2.2,yaxt='n',xaxt='n',bty='n')
axis(side=1,labels=seq(from=1, to=12, by=2),at=seq(from=1, to=12, by=2), cex.axis=1.5, font=2)
#axis(side=1,labels=c(1:12),at=c(1:12), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=-5, to=5, by=2), at=seq(from=-5, to=5, by=2), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)
box(lwd=3)
polygon(x=c(1:12,12:1),y=c(smoothRF+1.96*uncertainty.force,rev(smoothRF-1.96*uncertainty.force)),border=NA, col='gray')
lines(smoothRF, lwd=5) 
abline(h=0, col='red4', lty=2, lwd=3)

write.csv(STNetForce,'ST_dayweight.csv')




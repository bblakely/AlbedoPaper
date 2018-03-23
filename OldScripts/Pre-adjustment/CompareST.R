#Read in data
Modern_day_raw<-read.csv('Modern_day.csv', skip=7)
Modern_night_raw<-read.csv('Modern_night.csv', skip=7)
Paleo_day_raw<-read.csv('Paleo_day.csv', skip=7)
Paleo_night_raw<-read.csv('Paleo_night.csv', skip=7)

nobs=nrow(Modern_day_raw)

ModD_Dat<-Modern_day_raw[2:nobs,5:50]
ModD_Dat[ModD_Dat==9999]<-NaN

ModN_Dat<-Modern_night_raw[2:nobs,5:50]
ModN_Dat[ModN_Dat==9999]<-NaN

PalD_Dat<-Paleo_day_raw[2:nobs,5:50]
PalD_Dat[PalD_Dat==9999]<-NaN

PalN_Dat<-Paleo_night_raw[2:nobs,5:50]
PalN_Dat[PalN_Dat==9999]<-NaN

#Vecotors of Differences in temp

DayDiffs=ModD_Dat-PalD_Dat
DayDiffu=colMeans(DayDiffs, na.rm=TRUE)
plot(DayDiffu)

NightDiffs=ModN_Dat-PalN_Dat
NightDiffu=colMeans(NightDiffs, na.rm=TRUE)
plot(NightDiffu)

library('insol')
#Unweighted
MeanDiffs=(NightDiffu+DayDiffu)/2

#Day/night weighted
jd<-1:365
lat=44.2
lon=-91
tmz=(-6)

Days<-daylength(lat,lon,jd,tmz)
Daylen<-Days[,3]
dayweight=Daylen/24

useweight<-approx(dayweight, n=46)
AvgDiffs<-(DayDiffu*useweight$y)+(NightDiffu*(1-useweight$y))

SummerDays<-c(20:31)
WinterDays<-c(1:8, 43:46)
SpringDays<-c(9:19)
mean(AvgDiffs[SummerDays])
mean(AvgDiffs[WinterDays])

plot(AvgDiffs, type='l',ylim=c(-.5,1), main='Day and Night ST')
abline(h=0, lty=2)
lines(NightDiffu, col='forest green')
lines(DayDiffu, col='red')
legend(x=0,y=1, legend=c('Night','Day'), col=c('forest green','red'), lty=c(1,1))


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
Tabweight<-matrix(nrow=24650,ncol=46, data=rep(useweight$y, each=24650))
TableForce<-(AdjforcingDay*Tabweight+AdjforcingNight*(1-Tabweight))

TabRF<-colMeans(TableForce, na.rm=TRUE)
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

STNetForce<-(AdjDay*useweight$y)+(AdjNight*(1-useweight$y))
STNetForceTab<-TabRF

mean(STNetForce[WinterDays])
mean(STNetForce[SummerDays])

#Plotting

top=c(1:46)
bottom=c(1:46)
for (i in 1:46){
  quant<-c(quantile(TableForce[,i], c(0.1,0.90),na.rm=TRUE))
  top[i]<-quant[2]
  bottom[i]<-quant[1]
  #top<-max(Diffs[,i], na.rm=TRUE)
  #bottom[i]<-min(Diffs[,i], na.rm=TRUE)
}

smtop<-approx(top, n=12)
smbottom<-approx(bottom, n=12)
smoothRF<-approx(STNetForce, n=12)


par(mar=c(5,5,4,2))
plot(smoothRF$y,type='l',ylim=c(-5,7), main='Net ST RF', cex.main=2.5,ylab='RF (W/m2)', xlab='Month', cex.lab=2.2,yaxt='n',xaxt='n',bty='n')
axis(side=1,labels=c(1:12),at=c(1:12), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=-10, to=32, by=2), at=seq(from=-10, to=32, by=2), cex.axis=1.5, font=2)
box(,lwd=3)

polygon(x=c(1:12,12:1),y=c(smtop$y,rev(smbottom$y)),border=NA, col='gray')
abline(v=c(3.75,5.6,8.25,10.2), lty=3)
lines(smoothRF$y, lwd=5)
abline(h=0, col='red4', lty=2, lwd=3)

write.csv(STNetForce,'ST_dayweight.csv')

#smooth<-approx(STNetForce, n=23)
#STNetForcePlot<-approx(smooth, n=46)
#plot(STNetForce,type='l')
#abline(h=0, lty=2)

##Day/Night weighting



#ST analyses

source('VegConvert_UTM.R')
source('RadKernel_extract.R')

#Read in data
Modern_day_raw<-read.csv('Modern_day_(v2).csv', skip=7)
Modern_night_raw<-read.csv('Modern_night_(v2).csv', skip=7)
Paleo_day_raw<-read.csv('Paleo_day_v2.csv', skip=7)
Paleo_night_raw<-read.csv('Paleo_night_v2.csv', skip=7) 

diag.plots<-FALSE #Do you want diagnostic plots?

#number of observations; to use whenever we want all the rows (pixels)
nobs=nrow(Modern_day_raw)

#Pull georef
Georef<-Paleo_day_raw[2:nobs, 5:6] 

#Clip off ENVI additional line, unneeded metadata. NAN fills.
ModD_Dat<-Modern_day_raw[2:nobs,7:52]
ModD_Dat[ModD_Dat==9999]<-NaN

ModN_Dat<-Modern_night_raw[2:nobs,7:52]
ModN_Dat[ModN_Dat==9999]<-NaN

PalD_Dat<-Paleo_day_raw[2:nobs,7:52]
PalD_Dat[PalD_Dat==9999]<-NaN

PalN_Dat<-Paleo_night_raw[2:nobs,7:52]
PalN_Dat[PalN_Dat==9999]<-NaN

#Vectors of Differences in temp

###Day
DayDiffs=ModD_Dat-PalD_Dat
DayDiffu=colMeans(DayDiffs, na.rm=TRUE)
if(diag.plots==TRUE){plot(DayDiffu, type='l')}

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
if(diag.plots==TRUE){lines(approx(DayDiffu, n=46)$y,col='green')}

###Night
NightDiffs=ModN_Dat-PalN_Dat
NightDiffu=colMeans(NightDiffs, na.rm=TRUE)
if(diag.plots==TRUE){plot(NightDiffu)}

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
if(diag.plots==TRUE){lines(approx(NightDiffu, n=46)$y, col='green')}


#Day/night weighted
library('insol') #insol has daylight hours based on lat an lon.
jd<-1:365
lat=44.2 #Intermediate values. Makes coding easier by collapsing space dimension, could change (to some small effect) if needed
lon=-91
tmz=(-6) #Timezone. GMT - 6 AKA US Central time.

Days<-daylength(lat,lon,jd,tmz) #generates daylenghts
Daylen<-Days[,3]
dayweight=Daylen/24

useweight_temp<-approx(dayweight, n=12) # Selects 12 weights, one for each month, close to the first of each month.
useweight_force<-approx(dayweight, n=46) # Selects 14 weights, one for each MODIS date, close to first of each date period

#Tabular weighting
#Tabweight is the proportion of daylight hours
Tabweight<-matrix(nrow=24534,ncol=12, data=rep(useweight_temp$y, each=24534)) #Space
Tabweight.temp<-Tabweight

# Weighted average of day ST for day hours and night for night. 
# Technically instantaneous sunrise/sunset a MODIS limitation

#Weighting with spatial dimensions intact
TableDiffs<-data.frame(DayDiffs*Tabweight+NightDiffs*(1-Tabweight))
AvgDiffs<-colMeans(TableDiffs, na.rm=TRUE)

#Some seasonal #s
SummerDays<-c(6:8)
WinterDays<-c(1:2, 12)
SpringDays<-c(3:5)
mean(AvgDiffs[SummerDays])
mean(AvgDiffs[WinterDays])

#Plot of non-collapsed weighted temp changes
if(Diagplot==TRUE){
plot(AvgDiffs, type='l',ylim=c(-.5,1.3), main='Day and Night ST')
abline(h=0, lty=2)
lines(NightDiffu, col='forest green')
lines(DayDiffu, col='red')
legend(x=1,y=1, legend=c('Night','Day'), col=c('forest green','red'), lty=c(1,1))
}

#Forcing
SB=5.67e-8

#Renaming. Legacy of when this was 2 scripts, one for ST change and one for forcing
HtempsDay<-PalD_Dat
HtempsNight<-PalN_Dat

MtempsDay<-ModD_Dat
MtempsNight<-ModN_Dat

## Radkern calcualtions of flux ##
forcing.k<-TableDiffs*stkern
forcing.k.u<-colMeans(TableDiffs*stkern, na.rm=TRUE)
forcing.k.plot<-approx(forcing.k.u, n=46)$y


## SB law calcualtions of longwave flux ##

####Set emissivities####
#Flat emi's (original); Daytime
emi.h<-0.96
emi.m<-0.96
#0.988 average from lit
#0.96 original

#Lit based emi's
emi.db<-read.csv('EMIs_dat.csv')
emi.avg<-rowMeans(emi.db[3:9])

paleo.emi<-matrix(NA, nrow(HtempsDay), ncol=46)
paleo.emi[which(!is.na(paleo.veg)),]<-mean(emi.avg[1:5])
paleo.emi[paleo.veg==12,]<-emi.avg[5]
paleo.emi[paleo.veg==14,]<-emi.avg[4]
paleo.emi[paleo.veg==4,]<-emi.avg[3]
paleo.emi[paleo.veg==5,]<-emi.avg[2]
paleo.emi[paleo.veg==1,]<-emi.avg[1]
paleo.emi[,c(1:8,39:46)]<-(paleo.emi[,c(1:8,39:46)] + emi.avg[7])/2  #Winter emi is average of snow and veg type (assumes 50% snow cover)

modern.emi<-matrix(NA, nrow(d.temp.month), ncol=46)
modern.emi[which(!is.na(modern.veg)),]<-mean(emi.avg[1:5])
modern.emi[modern.veg==12,]<-emi.avg[5]
modern.emi[modern.veg==14,]<-emi.avg[4]
modern.emi[modern.veg==4,]<-emi.avg[3]
modern.emi[modern.veg==5,]<-emi.avg[2]
modern.emi[modern.veg==1,]<-emi.avg[1]
modern.emi[,c(1:8,39:46)]<-(modern.emi[,c(1:8,39:46)] + emi.avg[7])/2  #Winter emi is average of snow and veg type (assumes 50% snow cover)
#####

#### Daytime (includes emi diagnostics) ####

#With flat emi
Hforce.o<-(HtempsDay^4)*SB*emi.h #Historic surface outgoing longwave
Mforce.o<-(MtempsDay^4)*SB*emi.m #Modern surface outgoing longwave

#With lit emi
Hforce.l<-(HtempsDay^4)*SB*paleo.emi #Historic surface outgoing longwave
Mforce.l<-(MtempsDay^4)*SB*modern.emi #Modern surface outgoing longwave

#Should probably get LWin adjusted version up here; currently written as a mean


#Assign chosen emi (...force.l vs ...force.o)
HforceDay<-Hforce.l
MforceDay<-Mforce.l

#Componenet fluxes
ForcingsDay<-HforceDay-MforceDay  #SURFACE forcings
AtmforcingDay<-(-368/390)*ForcingsDay #ATMOSPHERIC forcings
TOAforcingDay<-(22/390)*ForcingsDay #TOA forcings

AtmDay<-colMeans(AtmforcingDay, na.rm=TRUE)
TOADay<-colMeans(TOAforcingDay, na.rm=TRUE)
SURFDay<-colMeans(ForcingsDay, na.rm=TRUE)

#Plot component fluxes day
if(diag.plots==TRUE){
plot(SURFDay, ylim=c(-8,8), type='l', ylab='LW components', main='Day')
lines(TOADay, col='light blue')
lines(AtmDay, col='red')

legend(0,-2, legend = c("Surface; (-) means more heat out", 'TOA; (-) means more heat out', 'ATM; (+) means more trapped, i.e. less out'), 
       col=c('black', 'light blue', 'red'), lwd=1, cex=0.5, bty='n')
}

#Plot all emi options
forcing.l<-colMeans(Hforce.l-Mforce.l, na.rm=TRUE)
forcing.o<-colMeans(Hforce.o-Mforce.o, na.rm=TRUE)

#Forcing including longwave 'albedo'
lwin_dum<-approx(c(260,264,288,297,334,362,374,377,358,326,301,265)+15, n=46)$y #manually added LWin from syv tower data
forcing.l2<-forcing.l+(colMeans(modern.emi-paleo.emi, na.rm=TRUE)*lwin_dum)

if(diag.plots==TRUE){
plot(-forcing.o, type='l', ylim=c(-5,8), lwd=3, main='LWforcing(day)')
lines(-forcing.l, col='red', lwd=3, lty=2)
lines(-forcing.l2, col='green', lwd=3, lty=3)
lines(forcing.k.plot, col='orange', lwd=3, lty=4)
abline(h=0)
legend(15,0,legend=c('albedo-based','original (static)','veg based (lit))', "veg based+lw_in", "RadKern"), 
       lwd=2, col=c('blue','black','red', 'green', 'orange'), cex=0.5, bty='n', ncol=2)
}
mean(-forcing.o)
mean(-forcing.l)
mean(-forcing.l2)
#####

#### Nighttime, just uses chosen scheme (lit based) ####

HforceNight<-(HtempsNight^4)*SB*paleo.emi
MforceNight<-(MtempsNight^4)*SB*modern.emi

ForcingsNight<-HforceNight-MforceNight
AtmforcingNight<-(-368/390)*ForcingsNight
TOAforcingNight<-(22/390)*ForcingsNight

AtmNight<-colMeans(AtmforcingNight, na.rm=TRUE)
TOANight<-colMeans(TOAforcingNight, na.rm=TRUE)
SURFNight<-colMeans(ForcingsNight, na.rm=TRUE)


if(diag.plots==TRUE){
  plot(SURFNight, ylim=c(-8,5), type='l', ylab='LW components', main='Night')
lines(TOANight, col='light blue')
lines(AtmNight, col='red')

legend(0,-2, legend = c("Surface; (-) means more heat out", 'TOA; (-) means more heat out', 'ATM; (+) means more trapped, i.e. less out'), 
       col=c('black', 'light blue', 'red'), lwd=1, cex=0.5, bty='n')
}

#Tabular all forcings (Current)
#Weighting
Tabweight<-matrix(nrow=24534,ncol=46, data=rep(useweight_force$y, each=24534))
TableForce<-data.frame(TOAforcingDay*Tabweight+TOAforcingNight*(1-Tabweight))
TableForce_long<-TableForce
if(diag.plots==TRUE){plot(colMeans(TableForce_long, na.rm=TRUE), type='l')}
#####

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

## Summary arrays ##

TableForce<-rf.temp.month #Monthly full weighted fluxes
TabRF<-colMeans(TableForce, na.rm=TRUE)

TabPix<-rowMeans(TableForce, na.rm=TRUE) #Yearly averages for each pixel
quantile(TabPix, c(0.1,0.9), na.rm=TRUE) #And its 'uncertainty'

#Write CSV for forcing standardization
#STForceDat<-cbind(Georef,TableForce)
#write.csv(STForceDat, "WriteFile/STForcing.csv")

#Summer and winter #s. 
STNetForceTab<-TabRF
mean(STNetForceTab[WinterDays]) 
mean(STNetForceTab[SummerDays])

####Apply vegetation conversion subsetting to Surface Temp####

var.set.force=matrix(nrow=12, ncol=length(poss))
var.set.lst=matrix(nrow=12, ncol=length(poss))
count.set=rep(0, length(poss))  

#Same deal as for recalc_albedo; check there for full explanation
#General goal is to do a weighted sum of variance in each conversion to more accurately capture uncertainty
for(j in 1:length(poss)){
  LSTSet.force<-data.frame(rf.temp.month[list.ind[[j]],])
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
#####

###Deforest and Comp shift###

STForce.veg<-cbind(TableForce, convert.code) #TableForce for Trenberth ratios, forcing.k for radiative kernels
STChg.veg<-cbind(TableDiffs, convert.code)

#### Veg shift Subsetting ####

Deforest<-c(7:11,13,21,22,25)    #This one is EG, MX, or DC forest to C, M, or U only
Deforest.2<-c(-2,7:13,21,22,25)  #This one includes mosaic to urban and mosaic to crop
Comp<-c(-1,3,4) #This one is E to MX, E to DC, or MX to DC
Urb<-c(12,14,21,22,25)

## Deforestation ##
STForce.def<-STForce.veg[which(STForce.veg[,13]%in%Deforest),]
STForce.avg.def<-colMeans(STForce.def[,1:12], na.rm=TRUE)
var.scl.def<-var.scl.force[,which(as.numeric(colnames(var.scl.force))%in%Deforest)]
uncertainty.def<-rowSums(var.scl.def, na.rm=TRUE)

STChg.def<-STChg.veg[which(STChg.veg[,13]%in%Deforest),]
STChg.def.avg<-colMeans(STChg.def[1:12], na.rm=TRUE)
mean(STChg.def.avg)
mean(STChg.def.avg[c(1:2,12)])
mean(STChg.def.avg[c(6:8)])

#used to check how no-snow places act (why?)
defplace<-which(STForce.veg[,13]%in%Deforest)
nosnowplace<-which(Georef$Lat<=40)
subset.darkdef<-defplace[which(defplace%in%nosnowplace)]
STForce.nosnow<-STForce.veg[subset.darkdef,]
STForce.nosnow.avg<-colMeans(STForce.nosnow[1:12], na.rm=TRUE)

## Composition shift ##
STForce.comp<-STForce.veg[which(STForce.veg[,13]%in%Comp),]
STForce.avg.comp<-colMeans(STForce.comp[,1:12], na.rm=TRUE)
var.scl.comp<-var.scl.force[,which(as.numeric(colnames(var.scl.force))%in%Comp)]
uncertainty.comp<-rowSums(var.scl.comp)

STChg.comp<-STChg.veg[which(STChg.veg[,13]%in%Comp),]
STChg.comp.avg<-colMeans(STChg.comp[1:12], na.rm=TRUE)
mean(STChg.comp.avg)
mean(STChg.comp.avg[c(1:2,12)])
mean(STChg.comp.avg[c(6:8)])


#### Veg shift Plots ####

# These numbers should be the reverse of albedo, i.e. -2 to 6 becomes -6 to 2
l.max<-1#1
l.min<-(-1)#(-12)
span<-c(l.min, l.max)

## Deforestation ##
par(mar=c(5,6,4,2))
ylab<-expression(RF~(Wm^-2))
plot(STForce.avg.def, type='l', col='forest green', ylim=span, lwd=2, main="Deforestation",cex.main=2.5, ylab="", xlab="",cex.lab=2.1,yaxt='n',xaxt='n',bty='n')
axis(side=1,labels=seq(from=1, to=12, by=2),at=seq(from=1, to=12, by=2), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=l.min, to=l.max, by=0.5), at=seq(from=l.min, to=l.max, by=0.5), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=3, cex=2.0, font=2)
abline(h=0, col='red4', lty=2, lwd=3)
polygon(x=c(1:12,12:1),y=c(STForce.avg.def+1.96*uncertainty.def,rev(STForce.avg.def-1.96*uncertainty.def)),border=NA, col='darkseagreen1')
lines(STForce.avg.def,  col='forest green', ylim=c(-12, 1), lwd=2)
box(lwd=3)
dev.copy(png, filename="Figures/STDeforest.png", width=500, height=425);dev.off()


# lines(STForce.nosnow.avg, lwd=2)

## Composition Shift ##
ylab<-expression(RF~(Wm^-2))
plot(STForce.avg.comp, type='l', col='forest green', ylim=span, lwd=2, main="Comp Shift",cex.main=2.5, ylab="", xlab="",cex.lab=2.1,yaxt='n',xaxt='n',bty='n')
axis(side=1,labels=seq(from=1, to=12, by=2),at=seq(from=1, to=12, by=2), cex.axis=1.5, font=2)
#axis(side=1,labels=c(1:12),at=c(1:12), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=l.min, to=l.max, by=0.5), at=seq(from=l.min, to=l.max, by=0.5), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=3, cex=2.0, font=2)
abline(h=0, col='red4', lty=2, lwd=3)
polygon(x=c(1:12,12:1),y=c(STForce.avg.comp+1.96*uncertainty.comp,rev(STForce.avg.comp-1.96*uncertainty.comp)),border=NA, col='darkseagreen1')
lines(STForce.avg.comp,  col='forest green', ylim=c(-12, 1), lwd=2)
box(lwd=3)
dev.copy(png, filename="Figures/STCompshift.png", width=500, height=425);dev.off()


par(xpd=FALSE)
#####


### Regional ###

#### Regional Plots ####
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
dev.copy(png, filename="Figures/STChange.png", width=600, height=400);dev.off()


#smoothRF<-TabRF
smoothRF<-colMeans(TableDiffs, na.rm=TRUE)*(-1) ###TableDiffs for Trenberth, forcings.k for kermels; need to propegate kernel forcings throughout to get correct uncertainty
#Regional ST forcing
par(mar=c(5,5,4,2))
ylab<-expression(RF~(Wm^-2)) #was ylim -4, 6
plot(smoothRF,type='l',ylim=c(-2.1,2.1), main='LST RF', cex.main=2.5,ylab='', xlab='', cex.lab=2.2,yaxt='n',xaxt='n',bty='n')
axis(side=1,labels=seq(from=1, to=12, by=2),at=seq(from=1, to=12, by=2), cex.axis=1.5, font=2)
#axis(side=1,labels=c(1:12),at=c(1:12), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=-5, to=5, by=1), at=seq(from=-5, to=5, by=1), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)
box(lwd=3)
polygon(x=c(1:12,12:1),y=c(smoothRF+1.96*uncertainty.force,rev(smoothRF-1.96*uncertainty.force)),border=NA, col='gray')
lines(smoothRF, lwd=5) 
abline(h=0, col='red4', lty=2, lwd=3)

dev.copy(png, filename="Figures/STForcing.png", width=600, height=400);dev.off()

#####
#write.csv(STNetForce,'WriteFile/ST_dayweight.csv')

#CIs
wintermonths<-c(1:2, 12)
springmonths<-c(3:5)
summermonths<-c(6:8)
fallmonths<-c(9:11)

ST.force.ann.ci<-mean(uncertainty.force)
print("annual st interval:");print(c(mean(smoothRF)+ST.force.ann.ci,mean(smoothRF)-ST.force.ann.ci))

ST.force.w.ci<-mean(uncertainty.force[wintermonths])
print("winter st interval:");print(c(mean(smoothRF[wintermonths])+ST.force.w.ci,mean(smoothRF[wintermonths])-ST.force.w.ci))

ST.force.s.ci<-mean(uncertainty.force[summermonths])
print("summer st interval:");print(c(mean(smoothRF[summermonths])+ST.force.w.ci,mean(smoothRF[summermonths])-ST.force.w.ci))

#ST by veg change
#3 panels; compare ST profiles, yearly, gs stchange

#Profiles
par(mfcol=c(2,2), mar=c(2,4,4,1))
lablim=1.3
plot(STChg.def.avg, type='l', ylim=c(-lablim,lablim), main='Deforest', xaxt='n', ylab='ST chg (Deg. C)');abline(h=0, lty=2)
plot(STChg.comp.avg, type='l', ylim=c(-lablim,lablim), main='Comp shift', ylab='ST chg (Deg. C)', xlab='Month'); abline(h=0, lty=2)

#Bars
barplot(c(mean(STChg.def.avg), mean(STChg.comp.avg)),names.arg=c('Deforest', 'Comp Shift'), ylim=c(-lablim, lablim), main='Annual')
gsmonths<-c(5:9)
barplot(c(mean(STChg.def.avg[gsmonths]), mean(STChg.comp.avg[gsmonths])),names.arg=c('Deforest', 'Comp Shift'), ylim=c(-lablim, lablim), main='Growing season')


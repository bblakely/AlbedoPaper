##Run VegConvert_UTM, Ceres_unpack, and CalcSolar before starting
source('VegConvert_UTM.R')
source('Ceres_unpack.R')
source('CalcSolar.R')

source('RadKernel_extract.R')

reportnum<-FALSE
#Cleanup unwanted bits from CalcSolar and VegConvert
rm(list=setdiff(ls(), c("Months", "Months.insol","list.ind", "poss", "convert.code", "transmit.avg", 'albkern', 'reportnum')))

Diagplot<-FALSE
vegshift.sep<-FALSE #Do you want separate long- and shortwave forcings by veg shift
usekern<-TRUE

#Read in raw data
ModernAlbRaw<-read.csv('Albedo_Modern1.csv',skip=7)
PaleoAlbRaw<-read.csv('Albedo_Paleo_v2.csv',skip=7) #older: 'Albedo_Paleo.csv; in 'older data'
#new: 'Albedo_Paleo_v2.csv'

#Pull georeference
Georef<-ModernAlbRaw[2:nrow(ModernAlbRaw), 5:6]
Georef.utm<-ModernAlbRaw[2:nrow(ModernAlbRaw), 3:4]
#Remove comment line, unwanted metadata columns
ModernAlb<-ModernAlbRaw[2:nrow(ModernAlbRaw),7:52]
PaleoAlb<-PaleoAlbRaw[2:nrow(PaleoAlbRaw),7:52]

#Exclude pixels where only one dataset has albedo
ModernAlb[PaleoAlb==9999]<-9999
PaleoAlb[ModernAlb==9999]<-9999

#NaN so operations missing data yield NaNs
PaleoAlb[PaleoAlb==9999]<-NaN
ModernAlb[ModernAlb==9999]<-NaN

#Change in albedo
DiffsAlb<-ModernAlb-PaleoAlb
DiffMeans<-colMeans(DiffsAlb, na.rm=TRUE)

rm('ModernAlbRaw', 'PaleoAlbRaw')

#Aggregate to months. Indices are manually assigned according to MODIS dates
d.alb.jan<-rowMeans(DiffsAlb[1:4],na.rm=TRUE)
d.alb.feb<-rowMeans(DiffsAlb[5:8],na.rm=TRUE)
d.alb.mar<-rowMeans(DiffsAlb[9:12],na.rm=TRUE)
d.alb.apr<-rowMeans(DiffsAlb[13:15],na.rm=TRUE)
d.alb.may<-rowMeans(DiffsAlb[16:19],na.rm=TRUE)
d.alb.jun<-rowMeans(DiffsAlb[20:23],na.rm=TRUE)
d.alb.jul<-rowMeans(DiffsAlb[24:27],na.rm=TRUE)
d.alb.aug<-rowMeans(DiffsAlb[28:31],na.rm=TRUE)
d.alb.sep<-rowMeans(DiffsAlb[32:35],na.rm=TRUE)
d.alb.oct<-rowMeans(DiffsAlb[36:38],na.rm=TRUE)
d.alb.nov<-rowMeans(DiffsAlb[39:42],na.rm=TRUE)
d.alb.dec<-rowMeans(DiffsAlb[43:46],na.rm=TRUE)
d.alb.month<-data.frame(cbind(d.alb.jan,d.alb.feb,d.alb.mar,d.alb.apr,d.alb.may,
                              d.alb.jun,d.alb.jul,d.alb.aug,d.alb.sep,d.alb.oct,d.alb.nov,d.alb.dec))
if(Diagplot==TRUE){plot(colMeans(d.alb.month, na.rm=TRUE),type='l', ylim=c(0,0.1))}

#Regionally aggregated monthly change
AlbedoChange<-(colMeans(d.alb.month, na.rm=TRUE))

#With radiative kernel
AlbedoForce_radkern<-sweep(d.alb.month/0.01, 2, albkern, '*')
AlbedoForce_radkern.u<-((AlbedoChange/0.01)*albkern)



#Calculate forcing
#Ceres_unpack and CalcSolar need to have been run at this point
AlbForce.month<-AlbForce<-matrix(nrow=nrow(d.alb.month),ncol=12)
Sun.dyn<-Sun.sta<-rep(0,12)
insol.avg<-colMeans(Months.insol) #Months.insol is the insolation version of 'Months' below
transmit.month<-colMeans(Months) #Average regional monthly transmittance. 'Months' comes from calc solar; its the 10-yr avg monthly transmittance for each pixel

#Seasonal transmittances
trans.winter<-mean(transmit.avg[c(1,2,12)])
trans.spring<-mean(transmit.avg[c(3:5)])
trans.summer<-mean(transmit.avg[c(6:8)])
trans.fall<-mean(transmit.avg[c(9:11)])

#Combine into 12-month vector
transmit.seas<-c(rep(trans.winter,2),rep(trans.spring,3),rep(trans.summer,3),rep(trans.fall,3), rep(trans.winter,1))

rm('trans.winter','trans.spring','trans.summer','trans.fall')

#Yearly average
transmit.yr<-mean(transmit.month)

#Calcualte albedo forcings, yearly transmittance
for(i in 1:12){
Sun.sta[i]<-insol.avg[i]*((transmit.yr)^2)
AlbForce[,i]<-insol.avg[i]*((transmit.yr)^2)*d.alb.month[,i]*(-1)
}
AlbForce.avg<-colMeans(AlbForce, na.rm=TRUE)

#Calculate albedo forcings, monthly transmittance
for (i in 1:12){
Sun.dyn[i]<-insol.avg[i]*((transmit.month[i])^2)
AlbForce.month[,i]<-insol.avg[i]*((transmit.month[i])^2)*d.alb.month[,i]*(-1)
}
AlbForce.month.avg<-colMeans(AlbForce.month, na.rm=TRUE)

#Plot for comparison (remove eventually)
if(Diagplot==TRUE){
plot(AlbForce.avg, type='l', lwd=2, ylim=c(-7,1))
lines(AlbForce.month.avg, col='blue', lwd=2)
lines(AlbedoForce_radkern.u, col='purple',lwd=2)
abline(h=0, lty=2,lwd=2)
legend(9,-3, legend=c('yearly','monthly', 'rad kernels'), col=c('black','blue', 'purple'), lwd=2, cex=0.5)
}
##SUPER KEY SET THIS###
if(usekern==TRUE){AlbForce<-AlbedoForce_radkern}else{AlbForce<-AlbForce.month}

AlbForce.avg<-colMeans(AlbForce, na.rm=TRUE)
  #AlbedoForce_radkern for radiative kernels
  #AlbForce.month for Trenberth
  #Eventually should change this into a function but for now renaming so plots work

#Cleanup, add basic metadata. and write file
AlbForce.std<-data.frame(cbind(Georef,AlbForce))
names(AlbForce.std)<-c("Lat","Lon","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
AlbForce.std[is.na(AlbForce.std)]<-9999
write.csv(AlbForce.std,"WriteFile/AlbedoForcing_STD.csv")

####Apply vegetation conversion subsetting to albedo####

var.set.force=matrix(nrow=12, ncol=length(poss))
var.set.alb=matrix(nrow=12, ncol=length(poss))
count.set=rep(0, length(poss))

#remove NaNs (places missing either modern or historic veg) from convert.code
convert.code.real<-convert.code[which(!is.na(convert.code))]

##This loop yields standard deviations (uncertainty) for each month of each conversion
##These are then added (weighted by proportion of landscape having this conversion) to create uncertainty shading on plots
##Necesary because a pooled sd would mistakenly assign variation (different forcings for differen landcovers) as error 
for(j in 1:length(poss)){ #for each vegetation conversion...
  AlbSet<-data.frame(AlbForce[list.ind[[j]],]) #Albset is the subset of pixels (forcings) that has that vegetation conversion. Each row a pixel, each column a month.
  AlbSet.alb<-d.alb.month[list.ind[[j]],] #Albset.alb is the same thing but just for albedo change not forcing
  count.set[j]<-length(list.ind[[j]])/length(which(convert.code.real!=0))#Proportion of all conversions that is the conversion of interest (for weighting)
  for (i in 1:ncol(AlbSet)){  #For each months...
    var.set.force[i,j]<-sd(AlbSet[,i], na.rm=TRUE) #Var.set.force column (vegetation conversion)i row (month) j is the SD of that conversion in that month
    var.set.alb[i,j]<-sd(AlbSet.alb[,i], na.rm=TRUE) #Same thing for albedo rather than forcing.
  }
}

##Scale var.set by counts. count.set is the portion of each conversion
var.scl.force<-sweep(var.set.force,2,count.set,'*')
var.scl.alb<-sweep(var.set.alb,2,count.set,'*')

#Label
colnames(var.scl.force)<-poss

#Sum across veg conversions
uncertainty.force<-rowSums(var.scl.force, na.rm=TRUE)
uncertainty.alb<-rowSums(var.scl.alb, na.rm=TRUE)



#####Plots#####
par(mfrow=c(1,1))

#Forcing for Deforest/compshift
AlbForce.veg<-cbind(AlbForce, convert.code)
AlbChange.veg<-cbind(d.alb.month, convert.code)
Deforest<-c(7:11,13,21,22,25)    #This one is EG, MX, or DC forest to C, M, or U only
Deforest.2<-c(-2,7:13,21,22,25)  #This one includes mosaic to urban and mosaic to crop
Comp<-c(-1,3,4) #This one is E to MX, E to DC, or MX to DC

#Pull forcings/uncertainties for deforestation
AlbForce.def<-AlbForce.veg[which(AlbForce.veg[,13]%in%Deforest.2),]
AlbForce.avg.def<-colMeans(AlbForce.def[,1:12], na.rm=TRUE)
var.scl.def<-var.scl.force[,which(as.numeric(colnames(var.scl.force))%in%Deforest)]
uncertainty.def.alb<-rowSums(var.scl.def,na.rm=TRUE)

AlbChange.def<-AlbChange.veg[which(AlbForce.veg[,13]%in%Deforest),]
mean(colMeans(AlbChange.def[1:12], na.rm=TRUE)) #Yearly avg, deforested

#Set flexible plot limit params
l.max<-1
l.min<-(-14)
span<-c(l.min, l.max)

##Actual plotting
#RF for deforestation
if(vegshift.sep==TRUE){
par(mar=c(5,6,4,2))
ylab<-expression(RF~(Wm^-2))
plot(AlbForce.avg.def, type='l', col='grey20', ylim=span, lwd=2, main="Deforest",cex.main=2.5, ylab="", xlab="",cex.lab=2.1,yaxt='n',xaxt='n',bty='n')
axis(side=1,labels=seq(from=1, to=12, by=2),at=seq(from=1, to=12, by=2), cex.axis=1.5, font=2)
#axis(side=1,labels=c(1:12),at=c(1:12), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=l.min, to=l.max, by=2), at=seq(from=l.min, to=l.max, by=2), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=3, cex=2.0, font=2)
abline(h=0, col='red4', lty=2, lwd=3)
polygon(x=c(1:12,12:1),y=c(AlbForce.avg.def+1.96*uncertainty.def.alb,rev(AlbForce.avg.def-1.96*uncertainty.def.alb)),border=NA, col='gray')
lines(AlbForce.avg.def,  col='grey20', ylim=c(-14, 1), lwd=2)
box(lwd=3)
dev.copy(png, filename="Figures/AlbedoDeforest.png", width=500, height=425);dev.off()
}
#Pull forcings/uncertainties for compshift
AlbForce.comp<-AlbForce.veg[which(AlbForce.veg[,13]%in%Comp),]
AlbForce.avg.comp<-colMeans(AlbForce.comp[,1:12], na.rm=TRUE)
var.scl.comp<-var.scl.force[,which(as.numeric(colnames(var.scl.force))%in%Comp)]
uncertainty.comp.alb<-rowSums(var.scl.comp)

AlbChange.comp<-AlbChange.veg[which(AlbForce.veg[,13]%in%Comp),]
mean(colMeans(AlbChange.comp[1:12], na.rm=TRUE)) #Yearly avg, deforested



#Plot RF for compshift
if(vegshift.sep==TRUE){
par(mar=c(5,6,4,2))
ylab<-expression(RF~(Wm^-2))
plot(AlbForce.avg.comp, type='l', col='grey20', ylim=span, lwd=2, main="Comp Shift",cex.main=2.5, ylab="", xlab="",cex.lab=2.1,yaxt='n',xaxt='n',bty='n')
axis(side=1,labels=seq(from=1, to=12, by=2),at=seq(from=1, to=12, by=2), cex.axis=1.5, font=2)
#axis(side=1,labels=c(1:12),at=c(1:12), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=l.min, to=l.max, by=5), at=seq(from=l.min, to=l.max, by=5), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=3, cex=2.0, font=2)
abline(h=0, col='red4', lty=2, lwd=3)
polygon(x=c(1:12,12:1),y=c(AlbForce.avg.comp+1.96*uncertainty.comp.alb,rev(AlbForce.avg.comp-1.96*uncertainty.comp.alb)),border=NA, col='gray')
lines(AlbForce.avg.comp,  col='grey20', ylim=c(-12, 1), lwd=2)
box(lwd=3)
dev.copy(png, filename="Figures/AlbedoCompshift.png", width=500, height=425);dev.off()
}

#Fancy albedo change plot
par(xpd=FALSE)


par(mar=c(5,5,4,2))
ylab<-expression(Delta~alpha~("%"))
plot(AlbedoChange,type='l',ylim=c(-0.02,0.15), main='Albedo Change', cex.main=2.5, ylab="", xlab="",cex.lab=2.1,yaxt='n',xaxt='n',bty='n')
axis(side=1,labels=seq(from=1, to=12, by=2),at=seq(from=1, to=12, by=2), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=-2, to=16, by=4), at=seq(-0.02, to=0.16, by=0.04), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=3, cex=2.0, font=2)
box(lwd=3)
abline(h=0, col='red4', lty=2, lwd=3)
polygon(x=c(1:12,12:1),y=c(AlbedoChange+1.96*uncertainty.alb,rev(AlbedoChange-1.96*uncertainty.alb)),border=NA, col='gray')
lines(AlbedoChange, lwd=5)
dev.copy(png, filename="Figures/AlbedoChange.png", width=450, height=300);dev.off()


#Albedo RF plot
par(mar=c(5,5,4,2))
ylab<-expression(RF~(Wm^-2))
plot(AlbForce.avg,type='l',ylim=c(-10,2), main='Albedo RF', cex.main=2.5, ylab="", xlab="",cex.lab=2.1,yaxt='n',xaxt='n',bty='n')
axis(side=1,labels=seq(from=1, to=12, by=2),at=seq(from=1, to=12, by=2), cex.axis=1.5, font=2)
#axis(side=1,labels=c(1:12),at=c(1:12), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=-14, to=2, by=2), at=seq(from=-14, to=2, by=2), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)
box(lwd=3)
polygon(x=c(1:12,12:1),y=c(AlbForce.avg+1.96*uncertainty.force,rev(AlbForce.avg-1.96*uncertainty.force)),border=NA, col='gray')
#polygon(x=c(1:12,12:1),y=c(top,rev(bottom)),border=NA, col='gray')
#abline(v=c(3.75,5.6,8.25,10.2), lty=3)
lines(AlbForce.avg, lwd=5)
abline(h=0, col='red4', lty=2, lwd=3)
dev.copy(png, filename="Figures/AlbedoForcing.png", width=450, height=300);dev.off()

#Seasonal excerpts
wintermonths<-c(1:2, 12)
springmonths<-c(3:5)
summermonths<-c(6:8)
fallmonths<-c(9:11)

#CI's
Alb.force.ann.ci<-mean(uncertainty.force)
print("annual alb interval:");print(c(mean(AlbForce.avg)+Alb.force.ann.ci,mean(AlbForce.avg)-Alb.force.ann.ci))

Alb.force.w.ci<-mean(uncertainty.force[wintermonths])
print("winter alb interval:");print(c(mean(AlbForce.avg[wintermonths])+Alb.force.w.ci,mean(AlbForce.avg[wintermonths])-Alb.force.w.ci))

Alb.force.s.ci<-mean(uncertainty.force[summermonths])
print("summer alb interval:");print(c(mean(AlbForce.avg[summermonths])+Alb.force.w.ci,mean(AlbForce.avg[summermonths])-Alb.force.w.ci))

####Reporting numbers####
if(reportnum==TRUE){
  #Albedo change
  mean(AlbedoChange)#% change albedo regionwide
  mean(AlbedoChange[wintermonths]) #% change in winter
  mean(AlbedoChange[springmonths]);mean(AlbedoChange[summermonths]);mean(AlbedoChange[fallmonths]) #% change in other seasons
  
  #Forcings
  mean(AlbForce.avg) #Yearly forcing
  Alb.force.ann.ci<-mean(uncertainty.force)
  print("annual alb interval:");print(c(mean(AlbForce.avg)+Alb.force.ann.ci,mean(AlbForce.avg)-Alb.force.ann.ci))
  
  mean(AlbForce.avg[wintermonths]) #Winter forcing
  Alb.force.w.ci<-mean(uncertainty.force[wintermonths])
  print("winter alb interval:");print(c(mean(AlbForce.avg[wintermonths])+Alb.force.w.ci,mean(AlbForce.avg[wintermonths])-Alb.force.w.ci))
  
  mean(AlbForce.avg[summermonths]) #Summer forcing
  Alb.force.s.ci<-mean(uncertainty.force[summermonths])
  print("summer alb interval:");print(c(mean(AlbForce.avg[summermonths])+Alb.force.w.ci,mean(AlbForce.avg[summermonths])-Alb.force.w.ci))
  
  #Vegetation shift
  mean(colMeans(AlbChange.def[1:12], na.rm=TRUE)) #Yearly avg, deforested
  mean(colMeans(AlbChange.comp[1:12], na.rm=TRUE)) #Yearly avg, compshift
  
}
#####

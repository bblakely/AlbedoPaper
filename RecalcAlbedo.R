##Run VegConvert_UTM, Ceres_unpack, and CalcSolar before starting
source('VegConvert_UTM.R')
source('CalcSolar.R')

#Cleanup unwanted bits from CalcSolar and VegConvert
rm(list=setdiff(ls(), c("Months", "Months.insol")))


#Read in raw data
ModernAlbRaw<-read.csv('Albedo_Modern1.csv',skip=7)
PaleoAlbRaw<-read.csv('Albedo_Paleo.csv',skip=7)

#Pull georef
Georef<-ModernAlbRaw[2:nrow(ModernAlbRaw), 5:6]
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
plot(colMeans(d.alb.month, na.rm=TRUE),type='l')

#Regionally aggregated monthly change
AlbedoChange<-(colMeans(d.alb.month, na.rm=TRUE))


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
Sun.dyn<-insol.avg[i]*((transmit.month[i])^2)
AlbForce.month[,i]<-insol.avg[i]*((transmit.month[i])^2)*d.alb.month[,i]*(-1)
}
AlbForce.month.avg<-colMeans(AlbForce.month, na.rm=TRUE)

#Plot for comparison (remove eventually)
plot(AlbForce.avg, type='l', lwd=2)
lines(AlbForce.month.avg, col='blue', lwd=2)
legend(6,-2, legend=c('yearly','monthly'), col=c('black','blue'), lwd=2, cex=0.5)

####Dummy net plot####
plot(colMeans(-AlbForce, na.rm=TRUE),type='l',ylim=c(-4,0.5))
abline(h=0)

#Cleanup, add basic metadata. and write file
AlbForce.std<-data.frame(cbind(Georef,AlbForce))
names(AlbForce.std)<-c("Lat","Lon","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
AlbForce.std[is.na(AlbForce.std)]<-9999
write.csv(AlbForce.std,"AlbedoForcing_STD.csv")

#Manually copied other forcings - needs to be automated!
SToffset<-c(-0.922178822, -0.813981288, -0.287019643,  0.038124206,  1.313251843,
             2.424227792,  1.723607501,  1.194636428,  0.849527934, -0.009961376,
            -0.393786777, -0.805246905)
  #c(-0.90971734, -1.02053852, -0.45385387,  0.03406376,  1.12528311,
            #2.43204722,1.67001469,  1.15986021,  0.88614576, -0.19873367, -0.45981090, -0.71047294)

Snowoffset_approx<-c(0,0,0.48,2.67,0.078,0,0,0,0,0,0,0)
SnowSToffset_approx<-c(0.3871,0.73988,0.70798,0.65542,0.14659,0,0,0,0,0,0.075639,0.19390)

plot(colMeans(AlbForce, na.rm=TRUE),type='l',ylim=c(-5,2.5))
abline(h=0)
lines(SToffset, col='red')
lines(Snowoffset_approx, col='blue')
lines(SnowSToffset_approx, col='light blue')
lines(AlbForce.avg+SToffset+Snowoffset_approx+SnowSToffset_approx, lwd=3)
mean(AlbForce.avg+SToffset+Snowoffset_approx+SnowSToffset_approx)
abline(h=0)
######

####Apply vegetation conversion subsetting to albedo####
##Must run VegConvert_UTM first to get list.ind

var.set.force=matrix(nrow=12, ncol=26)
var.set.alb=matrix(nrow=12, ncol=26)
count.set=rep(0, length(poss))

convert.code.real<-convert.code[which(!is.na(convert.code))]

for(j in 1:length(poss)){
  AlbSet<-AlbForce[list.ind[[j]],]
  AlbSet.alb<-d.alb.month[list.ind[[j]],]
  count.set[j]<-length(list.ind[[j]])/length(which(convert.code.real!=0))#length(which(!is.na(convert.code)))
  for (i in 1:ncol(AlbSet)){
    var.set.force[i,j]<-sd(AlbSet[,i], na.rm=TRUE)
    var.set.alb[i,j]<-sd(AlbSet.alb[,i], na.rm=TRUE)
  }
}

##Scale var.set by counts
var.scl.force<-sweep(var.set.force,2,count.set,'*')
var.scl.alb<-sweep(var.set.alb,2,count.set,'*')

colnames(var.scl.force)<-poss

#Sum across veg conversions
uncertainty.force<-rowSums(var.scl.force, na.rm=TRUE)
uncertainty.alb<-rowSums(var.scl.alb, na.rm=TRUE)

#####Plots#####
par(mfrow=c(1,1))

#Forcing for Deforest/compshift
AlbForce.veg<-cbind(AlbForce, convert.code)
Deforest<-c(7:11,13,21,22,25)    #This one is EG, MX, or DC forest to C, M, or U only
Deforest.2<-c(-2,7:13,21,22,25)  #This one includes mosaic to urban and mosaic to crop
Comp<-c(-1,3,4) #This one is E to MX, E to DC, or MX to DC

AlbForce.def<-AlbForce.veg[which(AlbForce.veg[,13]%in%Deforest),]
AlbForce.avg.def<-colMeans(AlbForce.def[,1:12], na.rm=TRUE)
var.scl.def<-var.scl.force[,which(as.numeric(colnames(var.scl.force))%in%Deforest)]
uncertainty.def<-rowSums(var.scl.def)

l.max<-4
l.min<--12
span<-c(l.min, l.max)

par(mar=c(5,6,4,2))
ylab<-expression(RF~(Wm^-2))
plot(AlbForce.avg.def, type='l', col='orange', ylim=span, lwd=2, main="Deforest",cex.main=2.5, ylab="", xlab="",cex.lab=2.1,yaxt='n',xaxt='n',bty='n')
axis(side=1,labels=seq(from=1, to=12, by=2),at=seq(from=1, to=12, by=2), cex.axis=1.5, font=2)
#axis(side=1,labels=c(1:12),at=c(1:12), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=l.min, to=l.max, by=4), at=seq(from=l.min, to=l.max, by=4), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=3, cex=2.0, font=2)
abline(h=0, col='red4', lty=2, lwd=3)
polygon(x=c(1:12,12:1),y=c(AlbForce.avg.def+1.96*uncertainty.def,rev(AlbForce.avg.def-1.96*uncertainty.def)),border=NA, col='palegoldenrod')
lines(AlbForce.avg.def,  col='orange', ylim=c(-12, 1), lwd=2)
box(lwd=3)

AlbForce.comp<-AlbForce.veg[which(AlbForce.veg[,13]%in%Comp),]
AlbForce.avg.comp<-colMeans(AlbForce.comp[,1:12], na.rm=TRUE)
var.scl.comp<-var.scl.force[,which(as.numeric(colnames(var.scl.force))%in%Comp)]
uncertainty.comp<-rowSums(var.scl.comp)

par(mar=c(5,6,4,2))
ylab<-expression(RF~(Wm^-2))
plot(AlbForce.avg.comp, type='l', col='forest green', ylim=span, lwd=2, main="Comp Shift",cex.main=2.5, ylab="", xlab="",cex.lab=2.1,yaxt='n',xaxt='n',bty='n')
axis(side=1,labels=seq(from=1, to=12, by=2),at=seq(from=1, to=12, by=2), cex.axis=1.5, font=2)
#axis(side=1,labels=c(1:12),at=c(1:12), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=l.min, to=l.max, by=4), at=seq(from=l.min, to=l.max, by=4), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=3, cex=2.0, font=2)
abline(h=0, col='red4', lty=2, lwd=3)
polygon(x=c(1:12,12:1),y=c(AlbForce.avg.comp+1.96*uncertainty.comp,rev(AlbForce.avg.comp-1.96*uncertainty.comp)),border=NA, col='darkseagreen1')
lines(AlbForce.avg.comp,  col='forest green', ylim=c(-12, 1), lwd=2)
box(lwd=3)


#Fancy albedo change plot
par(xpd=FALSE)


par(mar=c(5,5,4,2))
ylab<-expression(Delta~alpha~("%"))
plot(AlbedoChange,type='l',ylim=c(-0.02,0.1), main='Albedo Change', cex.main=2.5, ylab="", xlab="",cex.lab=2.1,yaxt='n',xaxt='n',bty='n')
axis(side=1,labels=seq(from=1, to=12, by=2),at=seq(from=1, to=12, by=2), cex.axis=1.5, font=2)
axis(side=2, labels=seq(from=-2, to=10, by=4), at=seq(-0.02, to=0.1, by=0.04), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=3, cex=2.0, font=2)
box(lwd=3)
polygon(x=c(1:12,12:1),y=c(AlbedoChange+1.96*uncertainty.alb,rev(AlbedoChange-1.96*uncertainty.alb)),border=NA, col='gray')
lines(AlbedoChange, lwd=5)
abline(h=0, col='red4', lty=2, lwd=3)


par(mar=c(5,5,4,2))
ylab<-expression(RF~(Wm^-2))
plot(AlbForce.avg,type='l',ylim=c(-6,1), main='Albedo RF', cex.main=2.5, ylab="", xlab="",cex.lab=2.1,yaxt='n',xaxt='n',bty='n')
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

wintermonths<-c(1:2, 12)
springmonths<-c(3:5)
summermonths<-c(6:8)
fallmonths<-c(9:11)


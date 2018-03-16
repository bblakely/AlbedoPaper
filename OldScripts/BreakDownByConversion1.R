setwd('F:/R/MS_Figures')
##Read in data
AlbModernRaw<-read.csv('Albedo_modern.csv', skip=7)
AlbPaleoRaw<-read.csv('Albedo_Paleo.csv', skip=7)
PaleoVegRaw<-read.csv('Paleo_vegetation_UTM.csv', skip=7)
ModernVegRaw<-read.csv('Modern_vegetation_UTM.csv', skip=7)

AlbPaleoRaw$B5<-as.numeric(levels(AlbPaleoRaw$B5))[AlbPaleoRaw$B5]

ModernVegRaw<-ModernVegRaw[2:nrow(ModernVegRaw),]
PaleoVegRaw<-PaleoVegRaw[2:nrow(PaleoVegRaw),]

AlbModernRaw<-AlbModernRaw[2:nrow(AlbModernRaw),]
AlbPaleoRaw<-AlbPaleoRaw[2:nrow(AlbPaleoRaw),]

#Bit of data cleanup
AlbM<-AlbModernRaw[PaleoVegRaw[,7]>0,7:52]
AlbP<-AlbPaleoRaw[PaleoVegRaw[,7]>0,7:52]
ModernVeg<-ModernVegRaw[PaleoVegRaw[,7]>0,]
PaleoVeg<-PaleoVegRaw[PaleoVegRaw[,7]>0,]

#Modify convert.code
convert.code.split<-convert.code[PaleoVegRaw[,7]>0]


#Pixels where there was no land use change
nochange<- which(ModernVeg$B1==PaleoVeg$B1)

#Pixels where there was land use change
change<-length(which(ModernVeg$B1 != PaleoVeg$B1))

##Types of change
#Forests becoming more deciduous
EGtoD<-which(PaleoVeg$B1==1 & ModernVeg$B1==4) #list.ind[14]
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

#Crop to urban
CtoU<-which(PaleoVeg$B1==12 & ModernVeg$B1==13)

###Albedo
#Data cleanup
AlbM[AlbM==9999]<-NA
AlbP[AlbP==9999]<-NA


#Overall Differences
AlbDiff<-(AlbM-AlbP)
plot(colMeans(AlbDiff, na.rm=TRUE), type='l')

#####To months####
c.alb.jan<-rowMeans(AlbDiff[1:4],na.rm=TRUE)
c.alb.feb<-rowMeans(AlbDiff[5:8],na.rm=TRUE)
c.alb.mar<-rowMeans(AlbDiff[9:12],na.rm=TRUE)
c.alb.apr<-rowMeans(AlbDiff[13:15],na.rm=TRUE)
c.alb.may<-rowMeans(AlbDiff[16:19],na.rm=TRUE)
c.alb.jun<-rowMeans(AlbDiff[20:23],na.rm=TRUE)
c.alb.jul<-rowMeans(AlbDiff[24:27],na.rm=TRUE)
c.alb.aug<-rowMeans(AlbDiff[28:31],na.rm=TRUE)
c.alb.sep<-rowMeans(AlbDiff[32:35],na.rm=TRUE)
c.alb.oct<-rowMeans(AlbDiff[36:38],na.rm=TRUE)
c.alb.nov<-rowMeans(AlbDiff[39:42],na.rm=TRUE)
c.alb.dec<-rowMeans(AlbDiff[43:46],na.rm=TRUE)
c.alb.month<-data.frame(cbind(c.alb.jan,c.alb.feb,c.alb.mar,c.alb.apr,c.alb.may,
                              c.alb.jun,c.alb.jul,c.alb.aug,c.alb.sep,c.alb.oct,c.alb.nov,c.alb.dec))

#####
AlbDiff<-c.alb.month


#More Deciduous
DecidifyDiffs<-AlbDiff[c(EGtoD,EGtoM,MtoD),]
ToDecidDiffs<-AlbDiff[c(EGtoD,MtoD),]
DecidChange<-colMeans(DecidifyDiffs, na.rm=TRUE)
plot(DecidChange, type='l')
mean(DecidChange)

#Deforestation
DeforestDiffs<-AlbDiff[TotDeforest,]
DeforestChange<-colMeans(DeforestDiffs, na.rm=TRUE)
DefChgSm<-approx(DeforestChange,n=12)
plot(DefChgSm, type='l')
plot(DeforestChange, type='l')
mean(DeforestChange)

#Afforestation
AfforestDiffs<-AlbDiff[c(TotAfforest),]
AffProf<-colMeans(AfforestDiffs, na.rm=TRUE)
plot(AffProf, type='l')
mean(AffProf)

#More Evergreen
EGifyDiffs<-AlbDiff[c(TotEGify),]
EGprof<-colMeans(EGifyDiffs, na.rm=TRUE)
plot(EGprof, type='l')
mean(EGprof)

par(mar=c(3,5,3,2))


barplot(height=(c(length(TotDeforest),length(nochange),length(TotDecidify),length(TotAfforest),length(TotEGify))/8090*100), col=c("gray90", "gray", "gray", "gray20","gray"),
        names.arg=c('Deforestation','No Change','Shift Deciduous','Afforestation','Shift Evergreen'), 
        ylab='% Converted',ylim=c(0,45), main='Forest Change', cex.main=2.2, cex.axis=1.2,cex.names=1.2, 
        cex.lab=2, font.axis=2, font.lab=2)

abline(h=seq(from=0, to=40, by=5),col='light gray', lty=2)

#Second time to cover gridlines
barplot(height=(c(length(TotDeforest),length(nochange),length(TotDecidify),length(TotAfforest),length(TotEGify))/8090*100), col=c("gray90", "gray", "gray", "gray20","gray"),
        names.arg=c('Deforestation','No Change','Shift Deciduous','Afforestation','Shift Evergreen'), 
        ylab='% Converted',ylim=c(0,45), main='Forest Change', cex.main=2.2, cex.axis=1.2,cex.names=1.2, 
        cex.lab=2, font.axis=2, font.lab=2, add=TRUE)


#Third time around for shading
barplot(height=(c(length(TotDeforest),length(nochange),length(TotDecidify),length(TotAfforest),length(TotEGify))/8090*100), col="black", density=c(0,0,20,0,80),
        names.arg=c('Deforestation','No Change','Shift Deciduous','Afforestation','Shift Evergreen'), 
        ylab='% Converted',ylim=c(0,45), main='Forest Change', cex.main=2.2, cex.axis=1.2,cex.names=1.2, 
        cex.lab=2, font.axis=2, font.lab=2, add=TRUE)

box(lwd=3)

####Better Uncertainty (Run VegConvert first)

#For Deforestation
poss<-sort(unique(convert.code.split[TotDeforest]))
list.ind.split.def<-list()
for(i in 1:length(poss)){
  list.ind.split.def[[i]]<- which(convert.code.split==poss[i])
}

var.set.alb.def=matrix(nrow=12, ncol=length(poss))
count.set=rep(0, length(poss))  
for(j in 1:length(poss)){
  DefSet<-DeforestDiffs[list.ind.split.def[[j]],]
  count.set[j]<-length(list.ind.split.def[[j]])/length(which(!is.na(convert.code.split)))
  for (i in 1:ncol(DefSet)){
    var.set.alb.def[i,j]<-sd(DefSet[,i], na.rm=TRUE)
  }
}

##Scale var.set by counts
var.scl.alb.def<-sweep(var.set.alb.def,2,count.set,'*')

#Sum across veg conversions
uncertainty.alb.def<-rowSums(var.scl.alb.def, na.rm=TRUE)



# ###Plot all together
 par(xpd=FALSE)
##Deforest only

# top=c(1:12)
# bottom=c(1:12)
# for (i in 1:12){
#   quant<-c(quantile(DeforestDiffs[,i], c(0.1,0.90),na.rm=TRUE))
#   top[i]<-quant[2]
#   bottom[i]<-quant[1]
#   #top<-max(Diffs[,i], na.rm=TRUE)
#   #bottom[i]<-min(Diffs[,i], na.rm=TRUE)
# }


par(mar=c(5,5,5,3))
ylab=expression(Delta~Albedo)
plot(DeforestChange, type='l', ylim=c(-0.05,0.27), col='white',xaxt='n',cex.lab=2.1, cex.main=2.5, 
     yaxt='n', main= "Albedo Change", xlab='', ylab="")
abline(h=0, lty=2, lwd=2)

box(lwd=2)
polygon(x=c(1:12,12:1),y=c(DeforestChange+1.96*uncertainty.alb.def,rev(DeforestChange-1.96*uncertainty.alb.def)),border=NA, col='gray')
#polygon(x=c(1:12,12:1),y=c(top,rev(bottom)),border=NA, col='gray')
lines(DeforestChange, col='dark orange',lwd='3')
axis(side=2, labels= seq(from=-0.05, to=0.35, by=0.05), at=seq(from=-0.05, to=0.35, by=0.05), cex.axis=1.5, font=2)
axis(side=1, labels= seq(from=1, to=12, length.out=12), at= seq(from=1, to=12, length.out=12), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=3, cex=2, font=2)
#legend(x=25,y=0.14, legend=c('Deforestation'), 
       #col=c('dark orange'),lwd=c(2),
       #cex=0.85, pt.cex=2)


#Better uncertainty for deciduous
poss<-sort(unique(convert.code.split[TotDecidify]))
list.ind.split.dec<-list()
for(i in 1:length(poss)){
  list.ind.split.dec[[i]]<- which(convert.code.split==poss[i])
}

var.set.alb.dec=matrix(nrow=12, ncol=length(poss))
count.set=rep(0, length(poss))  
for(j in 1:length(poss)){
  DefSet<-DeforestDiffs[list.ind.split.dec[[j]],]
  count.set[j]<-length(list.ind.split.dec[[j]])/length(which(!is.na(convert.code.split)))
  for (i in 1:ncol(DefSet)){
    var.set.alb.dec[i,j]<-sd(DefSet[,i], na.rm=TRUE)
  }
}

##Scale var.set by counts
var.scl.alb.dec<-sweep(var.set.alb.dec,2,count.set,'*')

#Sum across veg conversions
uncertainty.alb.dec<-rowSums(var.scl.alb.dec, na.rm=TRUE)



#Decidify only
# top=c(1:12)
# bottom=c(1:12)
# for (i in 1:12){
#   quant<-c(quantile(DecidifyDiffs[,i], c(0.1,0.90),na.rm=TRUE))
#   top[i]<-quant[2]
#   bottom[i]<-quant[1]
#   #top<-max(Diffs[,i], na.rm=TRUE)
#   #bottom[i]<-min(Diffs[,i], na.rm=TRUE)
# }

plot(DecidChange, type='l', ylim=c(-0.05,0.27), col='white',xaxt='n',cex.lab=2.2, cex.main=2.5, 
     yaxt='n', main= "Albedo Change", xlab='', ylab="")
ylab=expression(Delta~Albedo)
polygon(x=c(1:12,12:1),y=c(DecidChange+1.96*uncertainty.alb.dec,rev(DecidChange-1.96*uncertainty.alb.dec)),border=NA, col='gray')
#polygon(x=c(1:12,12:1),y=c(top,rev(bottom)),border=NA, col='gray')
abline(h=0, lty=2, lwd=2)
lines(DecidChange,col='forest green', lwd=3)
#lines(approx(DeforestChange, n=23), col='dark orange',lwd='3')

box(lwd=2)
axis(side=2, labels= seq(from=-0.05, to=0.35, by=0.05), at=seq(from=-0.05, to=0.35, by=0.05), cex.axis=1.5, font=2)
axis(side=1, labels= seq(from=1, to=12, length.out=12), at= seq(from=1, to=12, length.out=12), cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=3, cex=2, font=2)
# legend(x=25,y=0.14, legend=c('Deforestation', 'Comp. Shift'), 
#        col=c('dark orange','forest green'),lwd=c(2,2),
#        cex=0.85, pt.cex=2)
#lines(EGprof, col='purple')
#lines(AffProf, col='blue')



###Subseting for mapping
M_AlbCl<-AlbModernRaw[PaleoVegRaw[,7]>0,]

#More Deciduous
AlbDecidify<-M_AlbCl[TotDecidify,]
ModLCDecidify<-ModernVeg[TotDecidify,]
PalLCDecidify<-PaleoVeg[TotDecidify,]
AlbDecidify$ModLC<-ModLCDecidify$B1
AlbDecidify$PalLC<-PalLCDecidify$B1
ChgSeed<-((AlbDecidify$ModLC)-(AlbDecidify$PalLC))
ChgSeed[ChgSeed==4]<-1
ChgSeed[ChgSeed==3]<-2
ChgSeed[ChgSeed==-1]<-3
AlbDecidify$ChgCode<-ChgSeed
write.csv(AlbDecidify, 'F:/Albedo_Decidified.csv')

#Deforestation
AlbDeforest<-M_AlbCl[TotDeforest,]
ModLCDeforest<-ModernVeg[TotDeforest,]
PalLCDeforest<-PaleoVeg[TotDeforest,]
AlbDeforest$ModLC<-ModLCDeforest$B1
AlbDeforest$PalLC<-PalLCDeforest$B1
ChgSeed<-((AlbDeforest$ModLC)-(AlbDeforest$PalLC))

#For unique difference identifiers i.e. Crop (12) - Mosaic (14) = -2
#Differences
Seednum<-c(-1,-2,7,10,11,12,13)
#Codes to assign. negatives are here to prevent double-replacement
#e.g. -2 gets replaced by 10 then is read in a later iteration as 10 and replaced by 6
#Putting negatives on the codes prevents this.They are removed after the loop
Codenum<-c(-14,-10,-8,-6, -7, -11,-4)

#Replace differences with appropriate code
for (i in 1:7){
  ChgSeed[ChgSeed==Seednum[i]]<-Codenum[i]
}
ChgSeed<-ChgSeed*(-1)

#But some transformations do not yield a unique difference
#e.g. Deciduous -> Crop (12 - 4) and Mixed to urban (13 - 4) both yield 8
#Assign codes manually in these instances:
ChgSeed[AlbDeforest$PalLC==4 & AlbDeforest$ModLC==12]<-9
ChgSeed[AlbDeforest$PalLC==4 & AlbDeforest$ModLC==13]<-13
ChgSeed[AlbDeforest$PalLC==5 & AlbDeforest$ModLC==14]<-5
ChgSeed[AlbDeforest$PalLC==5 & AlbDeforest$ModLC==13]<-12

AlbDeforest$ChgCode<-ChgSeed
write.csv(AlbDeforest, 'F:/AlbDeforest.csv')

#No change
AlbedoNC<-M_AlbCl[nochange,]
write.csv(AlbedoNC,'F:/AlbedoNC.csv')

#More EVERGREEN
AlbEGify<-M_AlbCl[TotEGify,]
ModLCEGify<-ModernVeg[TotEGify,]
PalLCEGify<-PaleoVeg[TotEGify,]
AlbEGify$ModLC<-ModLCEGify$B1
AlbEGify$PalLC<-PalLCEGify$B1

ChgSeed<-(AlbEGify$ModLC - AlbEGify$PalLC)

ChgSeed[ChgSeed==-3]<-(-2)
ChgSeed[ChgSeed==1]<-(-3)
ChgSeed[ChgSeed==-4]<-(-1)

AlbEGify$ChgCode<-ChgSeed

write.csv(AlbEGify,'F:/AlbEGify.csv')


#Afforestation
AlbAfforest<-M_AlbCl[TotAfforest,]
ModLCAfforest<-ModernVeg[TotAfforest,]
PalLCAfforest<-PaleoVeg[TotAfforest,]
AlbAfforest$ModLC<-ModLCAfforest$B1
AlbAfforest$PalLC<-PalLCAfforest$B1
ChgSeed<-((AlbAfforest$ModLC)-(AlbAfforest$PalLC))

Seednum<-c(-11,-8,2,-7,-13,-10,-9)
Codenum<-c(7,  9, 10,8, 4,  6, 5)

for(i in 1:7){
  ChgSeed[ChgSeed==Seednum[i]]<-Codenum[i]
}
ChgSeed<-ChgSeed*(-1)

AlbAfforest$ChgCode<-ChgSeed

write.csv(AlbAfforest,'F:/AlbAfforest')


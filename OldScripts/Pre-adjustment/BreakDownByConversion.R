##Read in data
AlbModernRaw<-read.csv('Albedo_modern.csv', skip=7)
AlbPaleoRaw<-read.csv('Albedo_Paleo.csv', skip=7)
PaleoVegRaw<-read.csv('Paleo_vegetation_UTM.csv', skip=7)
ModernVegRaw<-read.csv('Modern_vegetation_UTM.csv', skip=7)

#Bit of data cleanup
AlbM<-AlbModernRaw[PaleoVegRaw[,7]>0,7:52]
AlbP<-AlbPaleoRaw[PaleoVegRaw[,7]>0,7:52]
ModernVeg<-ModernVegRaw[PaleoVegRaw[,7]>0,]
PaleoVeg<-PaleoVegRaw[PaleoVegRaw[,7]>0,]


#Pixels where there was no land use change
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

#Crop to urban
CtoU<-which(PaleoVeg$B1==12 & ModernVeg$B1==13)

###Albedo
#Data cleanup
AlbM[AlbM==9999]<-NA
AlbP[AlbP==9999]<-NA

#Overall Differences
AlbDiff<-(AlbM-AlbP)
plot(colMeans(AlbDiff, na.rm=TRUE), type='l')

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

barplot(c(length(TotDeforest),length(nochange),length(TotDecidify),length(TotAfforest),length(TotEGify)), col=c('dark orange','gray','forest green','dark blue','purple 4'),
        names.arg=c('Deforestation','No Change','More Decid.','Afforestation','More Evergreen'), 
        ylab='# Pixels',ylim=c(0,3500), main='Forest Change', cex.main=2.2, cex.axis=1.2,cex.names=1.2, 
        cex.lab=2, font.axis=2, font.lab=2)
box(lwd=2)

###Plot all together
##Deforest only
par(mar=c(5,5,5,3))
plot(DeforestChange, type='l', ylim=c(-0.01,0.15), col='white',xaxt='n',cex.lab=2.1, cex.main=2.5, 
     yaxt='n', main= "Albedo Change", xlab='Month', ylab="Alb. Change")
abline(h=0, lty=2, lwd=2)
abline(v=c(12,20,31,39), lty=3, lwd=1)
lines(approx(DeforestChange, n=23), col='dark orange',lwd='3')

box(lwd=2)
axis(side=2, labels= seq(from=-0.01, to=0.15, by=0.01), at=seq(from=-0.01, to=0.15, by=0.01), cex.axis=1.5, font=2)
axis(side=1, labels= seq(from=1, to=12, length.out=12), at= seq(from=1, to=46, length.out=12), cex.axis=1.5, font=2)
#legend(x=25,y=0.14, legend=c('Deforestation'), 
       #col=c('dark orange'),lwd=c(2),
       #cex=0.85, pt.cex=2)


# comp change
plot(DeforestChange, type='l', ylim=c(-0.01,0.15), col='white',xaxt='n',cex.lab=2.2, cex.main=2.5, 
     yaxt='n', main= "Albedo Change", xlab='Month', ylab="Alb. Change")
abline(h=0, lty=2, lwd=2)
abline(v=c(12,20,31,39), lty=3, lwd=1)
lines(DecidChange,col='forest green', lwd=3)
#lines(approx(DeforestChange, n=23), col='dark orange',lwd='3')

box(lwd=2)
axis(side=2, labels= seq(from=-0.01, to=0.15, by=0.01), at=seq(from=-0.01, to=0.15, by=0.01), cex.axis=1.5, font=2)
axis(side=1, labels= seq(from=1, to=12, length.out=12), at= seq(from=1, to=46, length.out=12), cex.axis=1.5, font=2)
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

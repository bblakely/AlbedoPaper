#Experimenting with emissivities in response to R2

#Creates empirically calcualted emissivities from albedo
#Likely underestimates emissivity overall and esp. in winter

#Get veg data @ albedo res
source('VegConvert_UTM.R')
rm(list=setdiff(ls(), c("list.ind", "poss", "convert.code", "modern.veg","paleo.veg")))

#Get Albedo data
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

rm('ModernAlbRaw', 'PaleoAlbRaw')

Modern.emi<-0.99-(ModernAlb*0.16)
Modern.emi<-cbind(Modern.emi, modern.veg)

Paleo.emi<-0.99-(PaleoAlb*0.16)
Paleo.emi<-cbind(Paleo.emi, paleo.veg)

Diffs.emi<-(Modern.emi - Paleo.emi)
Diffs.veg<-cbind(Diffs.emi, convert.code)

emi.m.dyn<-colMeans(Modern.emi[,1:46], na.rm=TRUE)
emi.h.dyn<-colMeans(Paleo.emi[,1:46], na.rm=TRUE)
#Plot to show emi changes
#Modern
par(mfrow=c(1,2))
vegposs.m<-unique(Modern.emi$modern.veg)[2:7]
vegcol<-c('blue','yellow','orange','purple','green','red')
plot(colMeans(Modern.emi[,1:46], na.rm=TRUE), ylim=c(0.88,1), 
     type='l', main='mod', ylab='emissivity')
for (i in 1: length(vegposs.m)){
  veg<-colMeans(Modern.emi[Modern.emi$modern.veg==vegposs.m[i],1:46], na.rm=TRUE)
  lines(veg, col=vegcol[i])
}
abline(h=0.96)


#historic
vegposs.h<-unique(Paleo.emi$paleo.veg)[2:7]
vegcol<-c('blue','yellow','orange','purple','green','red')
plot(colMeans(Paleo.emi[,1:46], na.rm=TRUE), ylim=c(0.88,1), 
     type='l', main='pal', ylab='emissivity')
for (i in 1: length(vegposs.h)){
  veg<-colMeans(Paleo.emi[Paleo.emi$paleo.veg==vegposs.m[i],1:46], na.rm=TRUE)
  lines(veg, col=vegcol[i])
}
abline(h=0.96)


#Using lit values (which are much higher)

emi.db<-read.csv('EMIs_dat.csv')
emi.dumbavg<-unname(rowMeans(emi.db[1:6,3:9]))


#Next task is to apply to st calcs


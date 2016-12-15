
source("Residual_v_snow.R")
snow.mod.raw<-read.csv('GHCN_Snow.csv',skip=7)
snow.hist.raw<-read.csv('GHCN_Snow_hist.csv',skip=7)
lst.raw<-read.csv('LST.csv', skip=7)

Georef<-lst.raw[2:nrow(lst.raw),5:6]

#QC
MOD<-(snow.mod.raw[2:24535,5:18])
MOD[MOD>500]<-NA
MOD$B5<-0
mod.dat<-MOD[3:14]

HIST<-(snow.hist.raw[2:24535,5:18])
HIST[HIST>500]<-NA
hist.dat<-HIST[3:14]

dif.dat<-(mod.dat-hist.dat)
snow.change<-colMeans(dif.dat)


LST<-lst.raw[2:nrow(lst.raw),5:52]
lst.dat<-LST[,3:48]
lst.dat[lst.dat>350]<-NA
#compress to months
lst.jan<-rowMeans(lst.dat[,1:4],na.rm=TRUE)
lst.feb<-rowMeans(lst.dat[5:8],na.rm=TRUE)
lst.mar<-rowMeans(lst.dat[9:12],na.rm=TRUE)
lst.apr<-rowMeans(lst.dat[13:15],na.rm=TRUE)
lst.may<-rowMeans(lst.dat[16:19],na.rm=TRUE)
lst.nov<-rowMeans(lst.dat[39:42],na.rm=TRUE)
lst.dec<-rowMeans(lst.dat[43:46],na.rm=TRUE)
lst.month<-data.frame(cbind(lst.jan,lst.feb,lst.mar,lst.apr,lst.may,lst.nov,lst.dec))

# #Back of envelope forcing, wrong way but illustrative
# monthtemps<-colMeans(lst.month, na.rm=TRUE)
# snow.change<-colMeans(dif.dat)
# snow.scale<-c(0.008,0.008,0.004,0.006,0.013)
# snow.shift<-snow.change[c(1:5,11:12)]*snow.scale
# newmonth<-monthtemps+snow.shift
# 
# sb=5.67E-8
# L1<-0.95*sb*monthtemps^4
# L2<-0.95*sb*newmonth^4
# 
# Forcings<-(L1-L2)

#Pixelized forcing
#Calculate change in residual due to snow predicted by fits
snow.shift.px<-data.frame(mapply('*',dif.dat[,c(1:5,11:12)],mean(slopes[c(1:4,6:7)]),SIMPLIFY=FALSE))
#apply shift to LST
#####
#Conceptually, this is adding the additional residual due to snow to the other
#residuals and adding THAT to the model. But since the data are already model plus
#original residuals, the PIXELIZED additional residual can be added to the data
#This would not work with averages because LW emission is not a  linear function
#of temperature
#####
newmonth.px<-lst.month+snow.shift.px

sb<-5.67e-8

L1.px<-0.98*sb*lst.month^4
L2.px<-0.98*sb*newmonth.px^4

forcing.px<-L1.px-L2.px

zeros<-matrix(data=0, nrow=nrow(forcing.px), ncol=5)
forcing.year<-cbind(forcing.px[,1:5],zeros,forcing.px[,6:7])

#Write for standard forcing
Forcing.std<-data.frame(cbind(Georef, forcing.year))
names(Forcing.std)<-c("Lat","Lon","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(Forcing.std,"SnowSTForcing_STD.csv")

#Plotting
force.month.num<-colMeans(forcing.px, na.rm=TRUE)
#put in 0's for non-snow days
force.month<-c(force.month.num[1:5],rep(0,5),force.month.num[6:7])
plot(force.month, type='l')

###Apply weighted Uncertainty

var.set.force=matrix(nrow=12, ncol=26)
var.set.sst=matrix(nrow=12, ncol=26)
count.set=rep(0, length(poss))  

for(j in 1:length(poss)){
  SSTSet<-forcing.year[list.ind[[j]],]
  count.set[j]<-length(list.ind[[j]])/length(which(!is.na(convert.code)))
  for (i in 1:ncol(SSTSet)){
    var.set.force[i,j]<-sd(SSTSet[,i], na.rm=TRUE)
  }
}

##Scale var.set by counts
var.scl.force<-sweep(var.set.force,2,count.set,'*')

#Sum across veg conversions
uncertainty.force<-rowSums(var.scl.force, na.rm=TRUE)



#Fancy plot
#get quantiles (there must be a more efficient way of doing this)
# hiquant<-c(1:7)
# loquant<-c(1:7)
# 
# for (i in 1:7){
#   quants<-quantile(forcing.px[,i], c(0.1,0.9),na.rm=TRUE)
#   hiquant[i]<-quants[[2]]
#   loquant[i]<-quants[[1]]
# }
# 
# hiquant.yr<-c(hiquant[1:5],rep(0,5),hiquant[6:7])
# loquant.yr<-c(loquant[1:5],rep(0,5),loquant[6:7])

par(mar=c(5,5,5,5))
plot(force.month, ylim=c(-1.2,2.5), type='l',lwd='3', xaxt='n',yaxt='n',xlab='',ylab='', main='Snow LST RF',cex.lab=2.2, cex.main=2.5,bty="n")
ylab=expression(RF~(Wm^-2))
polygon(x=c(1:12,12:1),y=c(force.month+1.96*uncertainty.force, rev(force.month-1.96*uncertainty.force)), border=NA,col='gray')
#polygon(x=c(1:12,12:1),y=c(hiquant.yr, rev(loquant.yr)), border=NA,col='gray')
#abline(v=c(3.75,5.6,8.25,10.2), lty=3)
lines(force.month, lwd=5)
axis(side=2, labels= seq(from=-1, to=2, by=0.5), at=seq(from=-1, to=2, by=0.5), cex.axis=1.5, font=2)
box(,lwd=3)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)
#lines(loquantrf, lty=4,col='red',lwd='2')
#lines(hiquantrf, lty=4,col='forest green',lwd='2')
abline(h=0,lty=2, lwd=3,col='red4')
axis(side=1,labels=c(1:12),at=seq(from=1,to=12,length.out=12),cex.axis=1.5, font=2)

#Quantiles
quantile(forcing.px[,c(1:2,7)], c(0.1,0.9), na.rm=TRUE) #winter
quantile(forcing.px[,c(3:5)], c(0.1,0.9), na.rm=TRUE) #spring
quantile(forcing.year,c(0.1,0.9),na.rm=TRUE) #all year

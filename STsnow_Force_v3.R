#Call preliminary scripts
source("Residual_v_snow.R") #Takes a bit
source("VegConvert_UTM.R") #Was geo for some reason despite the # of records indicating we're un UTM grid here
snow.mod.raw<-read.csv('GHCN_Snow.csv',skip=7)
snow.hist.raw<-read.csv('GHCN_Snow_hist.csv',skip=7)
lst.raw<-read.csv('LST.csv', skip=7)

Georef<-lst.raw[2:nrow(lst.raw),5:6]

Diagplot<-FALSE
usekern<-TRUE

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
snow.shift.px<-data.frame(mapply('*',dif.dat[,c(1:5,11:12)],mean(-slopes[c(1:4,6:7)]),SIMPLIFY=FALSE))
#apply shift to LST
#####
#Conceptually, this is adding the additional residual due to snow to the other
#residuals and adding THAT to the model. But since the data are already model plus
#original residuals, the PIXELIZED additional residual can be added to the data
#This would not work with averages because LW emission is not a  linear function
#of temperature
#####
newmonth.px<-lst.month+snow.shift.px

#Shift plots
newmonth.mt<-as.matrix(newmonth.px)
lstmonth.mt<-as.matrix(lst.month)
mod.mt<-as.matrix(mod.dat)
hist.mt<-as.matrix(hist.dat)
no.nan<-which(!is.na(newmonth.mt[,3]))

#Pull upper quantile of effect for more dramatic demonstrative figure in supplement
nmonth<-4
smonth<-1

shift.mag<-newmonth.mt - lstmonth.mt
shift.avg<-rowMeans(shift.mag[,smonth:nmonth])
#shift.max<-apply(shift.mag,[,1:nmonth], 1, min )
shift.quant<-quantile(shift.avg, 0.005, na.rm=TRUE)
shift.large<-which(shift.avg < shift.quant)
plotnum<-sample(shift.large, 20)
  #sample(no.nan, 20)


#p=4879
if(Diagplot==TRUE){
for(p in plotnum){
  #10824 is what is used for figures as of 1-27
  plot(newmonth.mt[p, c(smonth:nmonth)], type='l', xaxt='n',ylab='LST', xlab='Month', lwd=2, font=2,font.lab=2, main=p, col='orange')
  lines(lstmonth.mt[p ,c(smonth:nmonth)], type='l', lwd=2, font=2,font.lab=2, lty=2, col='purple')
  axis(side=1, at=c(1:4), labels=c(1:4), font=2)
  box(lwd=2)
  legend(1,max(lstmonth.mt[p, c(smonth:nmonth)]-1),legend=c('Modern', 'Shifted'), lwd=2, col=c('purple', 'orange'), text.font=2, cex=0.8)

  plot(hist.mt[p,c(smonth:nmonth)], type='l', lwd=2, xlab="Month", xaxt='n',ylab="SWE (cm)", font=2, font.lab=2, 
       ylim=c(min(mod.mt[p,c(smonth:nmonth)]),max(hist.mt[p,c(smonth:nmonth)]+60)))
  axis(side=1, at=c(1:4), labels=c(1:4), font=2)
  lines(mod.mt[p,c(smonth:nmonth)], type='l',col='red', lwd=2)
  box(lwd=2)
  legend(1,max(hist.mt[p,c(smonth:nmonth)]+55),legend=c('2000 - 2010', '1900 - 1910'), lwd=2, col=c('red','black'), text.font=2, cex=0.8)
  }
}
sb<-5.67e-8

L1.px<-(0.98*sb*lst.month^4)*(-22/390)
L2.px<-(0.98*sb*newmonth.px^4)*(-22/390)

forcing.px<-L1.px-L2.px

if(usekern==TRUE){
  source("RadKernel_extract.R")
  forcing.px<-sweep(newmonth.px-lst.month, 2, stkern[c(1:5,11:12)], '*')
  }


zeros<-matrix(data=0, nrow=nrow(forcing.px), ncol=5)
forcing.year<-cbind(forcing.px[,1:5],zeros,forcing.px[,6:7])

#Write for standard forcing
Forcing.std<-data.frame(cbind(Georef, forcing.year))
names(Forcing.std)<-c("Lat","Lon","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(Forcing.std,"WriteFile/SnowSTForcing_STD.csv")

#Plotting
force.month.num<-colMeans(forcing.px, na.rm=TRUE)
#put in 0's for non-snow days
force.month<-c(force.month.num[1:5],rep(0,5),force.month.num[6:7])
#plot(force.month, type='l')

###Apply weighted Uncertainty

var.set.force=matrix(nrow=12, ncol=length(poss))
var.set.sst=matrix(nrow=12, ncol=length(poss))
count.set=rep(0, length(poss))  

for(j in 1:length(poss)){
  SSTSet<-data.frame(forcing.year[list.ind[[j]],])
  count.set[j]<-length(list.ind[[j]])/length(which(!is.na(convert.code)))
  for (i in 1:ncol(SSTSet)){
    var.set.force[i,j]<-sd(SSTSet[,i], na.rm=TRUE)
  }
}

##Scale var.set by counts
var.scl.force<-sweep(var.set.force,2,count.set,'*')

#Sum across veg conversions
uncertainty.force<-rowSums(var.scl.force, na.rm=TRUE)


par(mar=c(5,5,4,2))
plot(force.month, ylim=c(-1.1,1.1), type='l',lwd='3', xaxt='n',yaxt='n',xlab='',ylab='', main='Snow LST RF',cex.lab=2.2, cex.main=2.5,bty="n")
ylab=expression(RF~(Wm^-2))
polygon(x=c(1:12,12:1),y=c(force.month+1.96*uncertainty.force, rev(force.month-1.96*uncertainty.force)), border=NA,col='gray')
#polygon(x=c(1:12,12:1),y=c(hiquant.yr, rev(loquant.yr)), border=NA,col='gray')
#abline(v=c(3.75,5.6,8.25,10.2), lty=3)
lines(force.month, lwd=5)
axis(side=2, labels= seq(from=-1, to=2, by=1), at=seq(from=-1, to=2, by=1), cex.axis=1.5, font=2)
box(lwd=3)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)
#lines(loquantrf, lty=4,col='red',lwd='2')
#lines(hiquantrf, lty=4,col='forest green',lwd='2')
abline(h=0,lty=2, lwd=3,col='red4')
axis(side=1,labels=seq(from=1,to=12,by=2),at=seq(from=1,to=12,by=2),cex.axis=1.5, font=2)

dev.copy(png, filename="Figures/SnowLST.png", width=450, height=300);dev.off()

#Quantiles
#quantile(forcing.px[,c(1:2,7)], c(0.1,0.9), na.rm=TRUE) #winter
#quantile(forcing.px[,c(3:5)], c(0.1,0.9), na.rm=TRUE) #spring
#quantile(forcing.year,c(0.1,0.9),na.rm=TRUE) #all year

WinterDays<-c(1,6:7)#There are no data for non-snow days; 6 and 7 are nov and Dec
SpringDays<-c(3:5)

ann.sno<-mean(rowMeans(forcing.px, na.rm=TRUE), na.rm=TRUE); print(ann.sno)
ann.sno.ci<-sd(rowMeans(forcing.px, na.rm=TRUE), na.rm=TRUE)*2
print("st snow ann interval");print(c(ann.sno+ann.sno.ci, ann.sno-ann.sno.ci))

spr.sno<-mean(rowMeans(forcing.px[,SpringDays], na.rm=TRUE), na.rm=TRUE); print(spr.sno)
spr.sno.ci<-sd(rowMeans(forcing.px[,SpringDays], na.rm=TRUE), na.rm=TRUE)*2
print("st snow spr interval");print(c(spr.sno+spr.sno.ci, spr.sno-spr.sno.ci))

wnt.sno<-mean(rowMeans(forcing.px[,WinterDays], na.rm=TRUE), na.rm=TRUE); print(wnt.sno)
wnt.sno.ci<-sd(rowMeans(forcing.px[,WinterDays], na.rm=TRUE), na.rm=TRUE)*2
print("st snow wnt interval");print(c(wnt.sno+wnt.sno.ci, wnt.sno-wnt.sno.ci))



force.month->STSnowRF #Rename for combine force

####Reporting numbers####
if(reportnum==TRUE){
#Prepare SWE
dif.dat.sub<-dif.dat
dif.dat.sub[(paleo.raw$B1[2:24535]==0),]<-NA
snow.change.num<-colMeans(dif.dat.sub, na.rm=TRUE)

#SWE changes
mean(snow.change.num) #Average SWE change
min(rowMeans(dif.dat.sub[,c(1:5,11:12)], na.rm=TRUE), na.rm=TRUE) #Large SWE changes in UP

#LST change from SWE
mean(colMeans(snow.shift.px[c(7,1:2)])) #LST change, winter
mean(colMeans(snow.shift.px[c(3:5)])) #LST change, spring

#Forcings
mean(rowMeans(forcing.px[,SpringDays], na.rm=TRUE), na.rm=TRUE)
spr.sno.ci<-sd(rowMeans(forcing.px[,SpringDays], na.rm=TRUE), na.rm=TRUE)*2
print("st snow spr interval");print(c(spr.sno+spr.sno.ci, spr.sno-spr.sno.ci))

mean(rowMeans(forcing.px[,WinterDays], na.rm=TRUE), na.rm=TRUE)
wnt.sno.ci<-sd(rowMeans(forcing.px[,WinterDays], na.rm=TRUE), na.rm=TRUE)*2
print("st snow wnt interval");print(c(wnt.sno+wnt.sno.ci, wnt.sno-wnt.sno.ci))

mean(rowMeans(forcing.px, na.rm=TRUE), na.rm=TRUE) #Annual forcing
ann.sno.ci<-sd(rowMeans(forcing.px, na.rm=TRUE), na.rm=TRUE)*2
print("st snow ann interval");print(c(ann.sno+ann.sno.ci, ann.sno-ann.sno.ci))

}

#####

#Redo combine force with real data
source('RecalcAlbedo.R')
source('CompareST_month_v2.R')
source('SnowShiftApproach2.3.R')
source('STsnow_Force.R')

writefile<-FALSE #Do you want to write finalized change and forcing files?
vegplot<-TRUE #Do you want the 25-odd individual converison plots?

par<-pardefault

alb.diff<-data.frame(d.alb.month)
st.diff<-data.frame(TableDiffs)
alb.force<-data.frame(AlbForce)
st.force<-data.frame(TableForce)

snow.force.alb<-AlbSnowRF
snow.force.st<-STSnowRF

veg.force<-data.frame(alb.force+st.force)

rm(list=setdiff(ls(), c("alb.force", "st.force","alb.diff","st.diff", "veg.force",
                        "Georef", "Georef.utm", "convert.code", "writefile",
                        "Deforest","Deforest.2", "Comp", "snow.force.alb", 'snow.force.st',
                        'vegplot', "paleo.veg","modern.veg")))

#Seasonal profiles

alb<-colMeans(alb.force, na.rm=TRUE)
st<-colMeans(st.force, na.rm=TRUE)
snow.alb<-snow.force.alb
snow.st<-snow.force.st


Total<-(alb+st+snow.alb+snow.st)
offset<-(st+snow.alb+snow.st)

mean(Total)/(mean(alb))
mean(offset)/(mean(alb))

### Regional Plots ###
#### Seasonal Profiles ####
par(xpd=FALSE)
par(mar=c(5,5,5,5))
plot(alb, ylim=c(-6,4), type='l',lwd='3', xaxt='n',yaxt='n',xlab='',ylab='', main='',cex.lab=2.2, cex.main=2.5,bty="n", col='white')
ylab=expression(RF~(Wm^-2))
axis(side=2, labels= seq(from=-8, to=3, by=2), at=seq(from=-8, to=3, by=2), cex.axis=1.5, font=2)
box(lwd=3)
axis(side=1,labels=seq(from=1,to=12, by=2),at=seq(from=1,to=12,by=2),cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)

lines(st, col='forest green', lwd=4, lty=3)
lines(snow.alb, col='dark blue', lwd=4,lty=3)
lines(alb, col='gray', lwd=4, lty=3)
lines(snow.st, col='light blue', lwd=4,lty=3)
lines(Total, lwd=5)
abline(h=0,lty=2, lwd=3,col='red4')

legend(x=4,y=-2.1, legend=c('Veg. Albedo','Veg. LST','Snow Albedo','Snow LST','Total'), lty=c(3,3,3,3,1),
lwd=c(4,4,4,4,4),col=c('gray',"forest green","dark blue","light blue","black"),
ncol=2, x.intersp=0.1,y.intersp=0.6, text.width=1.5, text.font=2, bty='n', cex=1.1)
#This looks bad in the preview but is fine when zoomed up


#### Barplots ####

alb.ann<-mean(alb)
st.ann<-mean(st)
snow.st.ann<-mean(snow.st)
snow.alb.ann<-mean(snow.alb)
Tot.ann<-mean(Total)

#### Individual Forcings ####
par(mar=c(5,6,5,5))
ylab=expression(Annual~RF~(Wm^-2))
lab.albveg<-expression(Veg~alpha)
lab.albsnow<-expression(Snow~alpha)
labloc<-barplot(c(alb.ann,st.ann,snow.alb.ann,snow.st.ann))

barplot(c(alb.ann,st.ann,snow.alb.ann,snow.st.ann), col=c("gray","forest green","light blue","dark blue"), 
        font=2, ylim=c(-2.5,1), cex.axis=2,cex.names=2, ylab='',
        cex.lab=2, font.axis=2, font.lab=2, axes=FALSE)

abline(h=seq(from=-2,to=0.8,by=0.5), lty=2,col='light gray')

barplot(c(alb.ann,st.ann,snow.alb.ann,snow.st.ann), col=c("gray","forest green","light blue","dark blue"), 
        font=2,ylim=c(-2.5,1),cex.axis=2,cex.names=2, ylab='',
        cex.lab=2, font.axis=2, font.lab=2, axes=FALSE, add=T)

text(labloc,0.6,c(expression(alpha),"LST",expression(alpha),"LST"), font=2, cex=2.2)
abline(v=2.5, lwd=3, lty=3)
text(labloc[2],-1,"VEG", cex=2.5, font=2)
text(mean(c(labloc[3], labloc[4])),-1,"SNOW", cex=2.5, font=2)

axis(side=4, labels=seq(from=-2, to=0.8, by=0.5), at=seq(from=-2, to=0.8, by=0.5), cex.axis=2.2, font=2)
#mtext(side=2, text=ylab, line=2.5, cex=2.4, font=2)
abline(h=0, lwd=3)
box(lwd=3)

#When exporting, 1050 x 750 works well

#### Combined Plots ####
veg.comb<-alb.ann+st.ann
sno.comb<-snow.st.ann+snow.alb.ann
Tot.ann<-Tot.ann #Already made for little plot

#Setting type to 'inset' makes a plot where the individual plot (above) can be used as an inset;
#Otherwise it is sized to be a stand-alone plot
type<-'inset'
if(type=='inset'){ylim=c(-2,3)}else{ylim=c(-2,1)}

barplot(c(Tot.ann, veg.comb,sno.comb),col=c("black","gray","dark blue"),  
        main="Combined RF",names.arg=c("Combined","Vegetation","Snow"), font=2,
        ylim=ylim,cex.main=3, cex.axis=2,cex.names=2, ylab='',
        cex.lab=2, font.axis=2, font.lab=2, axes=FALSE)

abline(h=seq(from=-1.5,to=3,by=0.5), lty=2,col='light gray')

barplot(c(Tot.ann, veg.comb,sno.comb),col=c("black","gray","dark blue"), 
        main="Combined RF",names.arg=c("Combined","Vegetation","Snow"), font=2,
        ylim=ylim,cex.main=3, cex.axis=2,cex.names=2, ylab='',
        cex.lab=2, font.axis=2, font.lab=2, axes=FALSE, add=T)

barplot(c(Tot.ann, veg.comb,sno.comb), density=c(0,20,50),col=c("black","forest green","light blue"), 
        main="Combined RF",names.arg=c("Combined","Vegetation","Snow"), font=2,
        ylim=ylim,cex.main=3, cex.axis=2,cex.names=2, ylab='',
        cex.lab=2, font.axis=2, font.lab=2, axes=FALSE, add=TRUE)

axis(side=2, labels=seq(from=-2, to=3, by=1), at=seq(from=-2, to=3, by=1), cex.axis=1.8, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2.5, font=2)
abline(h=0, lwd=3)
box(lwd=3)

#When exporting, 800 x 640 works well

#####



#### Exporting ####

months<-c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov")
if(writefile==TRUE){
  alb.force[is.na(alb.force)]<-9999;colnames(alb.force)<-months
  st.force[is.na(alb.force)]<-9999;colnames(st.force)<-months
  veg.force[is.na(alb.force)]<-9999;colnames(veg.force)<-months
  write.csv(cbind(Georef.utm, alb.force),"WriteFile/Albedo_Forcing.csv", quote="false", row.names=FALSE)
  write.csv(cbind(Georef.utm, st.force), "WriteFile/ST_Forcing.csv")
  write.csv(cbind(Georef.utm, veg.force), "WriteFile/Total_Veg_Forcing.csv")
  
  alb.diff[is.na(alb.diff)]<-9999
  write.csv(cbind(Georef.utm, alb.diff),"WriteFile/Albedo_Change.csv", quote=FALSE, row.names=FALSE)
  write.csv(cbind(Georef.utm, st.diff), "WriteFile/ST_Change.csv")
}

#Maps created by (1) subtracting the modeled rasters in ENVI (2) cleaning in IDL, and (3) symbolizing in ArcGIS
#There is almost certainly a better way, but that works
#####



### Vegetation forcing plots ###
#### Multi-veg plots ####
if(vegplot==TRUE){
key<-read.csv('convertcode_key.csv')
par(mfrow=c(2,2))
par(mar=c(2,3,2,2))

dest<-order(key$MOD)

for(i in dest){
  plot(colMeans(alb.force[convert.code==key$Code[i],],na.rm=TRUE), ylim=c(-30,30), ylab='forcing', lty=2,lwd=2, 
       main=paste(key$PAL[i],"to",key$MOD[i]), type='l', col='dark gray', font=2)
  lines(colMeans(st.force[convert.code==key$Code[i],], na.rm=TRUE), lty=2, col='forest green', lwd=2)
  lines(colMeans(veg.force[convert.code==key$Code[i],], na.rm=TRUE), lwd=3)
  abline(h=0, col='red')
  text(9,20, paste("n=",length(which(convert.code==key$Code[i]))," (",
                    round((length(which(convert.code==key$Code[i]))/length(which(!is.na(convert.code)))),3)*100,"%",
                    ")", sep=''), cex=0.8, font=2)
  text(8,-20, paste("Annual RF:", 
                    round(mean(colMeans(veg.force[convert.code==key$Code[i],], na.rm=TRUE)),3)),
                    cex=0.8, font=2)
  box(lwd=3)
  
}
}
length(which(convert.code==0))/length(which(!is.na(convert.code))) #Proportion of no change tiles

#### Stacked veg forcing ####

#Update key to only include existing transitions
real<-unique(convert.code)[which(!is.na(unique(convert.code)))]
newkey<-key[which(key$Code%in%real),]
newkey<-newkey[order(newkey$Code),]
#probably an apply solution but loop for now
for(i in 1:nrow(newkey)){
  newkey$Count[i]<-length(which(convert.code==newkey$Code[i]))
}


##Aggregate
#For sums
allveg<-aggregate(veg.force, by=list(convert.code), FUN='sum', na.rm=TRUE)
allveg.st<-aggregate(st.force, by=list(convert.code), FUN='sum', na.rm=TRUE)
allveg.alb<-aggregate(alb.force, by=list(convert.code), FUN='sum', na.rm=TRUE)

#For means
allveg.u<-aggregate(veg.force, by=list(convert.code), FUN='mean', na.rm=TRUE)
allveg.st.u<-aggregate(st.force, by=list(convert.code), FUN='mean', na.rm=TRUE)
allveg.alb.u<-aggregate(alb.force, by=list(convert.code), FUN='mean', na.rm=TRUE)

##Combine information

conv.m2<-8000*8000/10e9  #W/m2 -> GW/cell

prepare.plot<-function(datinput){
colnames(datinput)[1]<-"Code"
datinput$PAL<-newkey$PAL;datinput$MOD<-newkey$MOD
datinput$label<-paste(newkey$PAL, "to", newkey$MOD)
datinput$counts<-newkey$Count
datinput$sums<-rowMeans(datinput[,2:13])*conv.m2 #For sums. Mean forcing (W/m2) of all (summed) cells for each conversion; units technically GW/region I guess
datinput$means<-rowMeans(datinput[,2:13]) #For means. Mean forcing (W/m2) for each conversion. Units W/m2
datinput<-datinput[order(datinput$MOD, datinput$PAL),]
return(datinput)
}

#For sums
allveg.plot<-prepare.plot(allveg)
allveg.st.plot<-prepare.plot(allveg.st)
allveg.alb.plot<-prepare.plot(allveg.alb)

#For means
allveg.plot.u<-prepare.plot(allveg.u)
allveg.st.plot.u<-prepare.plot(allveg.st.u)
allveg.alb.plot.u<-prepare.plot(allveg.alb.u)


##Group by conversion type

conversion<-list(Deforest.2, Comp, -Deforest.2,-Comp)

vegetize<-function(datin, convlist, fxn){
  ann.force<-rep(0,length(convlist))
    for (i in 1:length(convlist)){
      if(fxn=='sum'){ann.force[i]<-sum(datin$sums[datin$Code%in%convlist[[i]]], na.rm=TRUE)}
      if(fxn=='mean'){ann.force[i]<-weighted.mean(datin$means[datin$Code%in%convlist[[i]]], datin$counts[datin$Code%in%convlist[[i]]], na.rm=TRUE)}
      }
  return(ann.force)
}

#For sums
albs<-vegetize(allveg.alb.plot, conversion, 'sum')
sts<-vegetize(allveg.st.plot, conversion, 'sum')
tots<-vegetize(allveg.plot, conversion, 'sum')

#For means
albs.u<-vegetize(allveg.alb.plot.u, conversion, 'mean')
sts.u<-vegetize(allveg.st.plot.u, conversion, 'mean')
tots.u<-vegetize(allveg.plot.u, conversion, 'mean')

##Stacking (for plots)

stackplot<-function(albs, sts){
stack.conv<-which(sign(albs)==sign(sts))
albs.stack<-albs
albs.stack[stack.conv]<-albs[stack.conv]+sts[stack.conv]
return(albs.stack)
}

albs.stack<-stackplot(albs,sts) #For sums
albs.stack.u<-stackplot(albs.u, sts.u) #For means

##Plotting
#Set plot parameters
par(mfrow=c(1,1))
par(mar=c(5,5,4,4))
par(xpd=FALSE) #These plots are prone to having lines outside plot bounds

#Set labels
chglab<-c("Deforest","Shift DC", "Afforest", "Shift EG")

#Sums
barplot(albs.stack, names.arg=chglab,ylab="Total RF (GW)", font=2, font.lab=2,ylim=c(-90,45))
barplot(sts, names.arg=chglab,col='forest green', add=TRUE, font=2, font.lab=2)
barplot(tots, names.arg=chglab,density=15, col='black',add=TRUE, font=2, font.lab=2)
abline(h=0, lwd=2, col='dark red', lty=2)
legend (3,-20, legend=c('Albdeo','LST','Combined'), fill=c('gray','forest green','black'), cex=0.8)

#Means
ylab.st<-expression(Average~RF~(Wm^-2))
barplot(albs.stack.u, names.arg=chglab, font=2, font.lab=2, ylab=ylab.st)
barplot(sts.u, names.arg=chglab,col='forest green', add=TRUE, font=2, font.lab=2)
barplot(tots.u, names.arg=chglab,density=15, col='black',add=TRUE, font=2, font.lab=2)
abline(h=0, lwd=2, col='dark red', lty=2)
#legend (3,-1, legend=c('Albdeo','LST','Combined'), fill=c('gray','forest green','black'), cex=0.8)
#####


#### Back-of-envelope for deforestation ####

alb.pix<-rowMeans(alb.force)
st.pix<-rowMeans(st.force)
veg.pix<-alb.pix+st.pix


nochg<-which(convert.code==0)
chg<-which(!is.na(convert.code)&convert.code!=0)
regrow<-which(convert.code==0 &(paleo.veg==4 |paleo.veg==5|paleo.veg==1|paleo.veg==14))
def<-which(convert.code%in%Deforest.2)
af<-which(convert.code%in%-Deforest.2)
cshift<-which(convert.code%in%Comp)
eshift<-which(convert.code%in%-Comp)
urb<-which(convert.code==14)
tot<-7251

#Giant equation of death? Missing something here - estimate is off by ~ 0.04 W/m
actual<-
(length(nochg)/tot)*mean(veg.pix[nochg], na.rm=TRUE)+  #Contribution from no change
(length(def)/tot)*mean(veg.pix[def], na.rm=TRUE)+      #Deforestation
(length(cshift)/tot)*mean(veg.pix[cshift], na.rm=TRUE)+#Comp shift
(length(eshift)/tot)*mean(veg.pix[eshift], na.rm=TRUE)+#Shift to evergreen
(length(af)/tot)*mean(veg.pix[af], na.rm=TRUE)        #Afforestation
print(actual)

hypodef<-which(convert.code%in%Deforest[1:6]) #Excludes conversion to urban and mosaic -> crop (as mosaic would not happen up there); not using currently

#Giant equation of *hypothetical* death
hypothetical<-
(1258/tot)*mean(veg.pix[nochg], na.rm=TRUE)+  #Contribution from never forest (no change)
(length(regrow)/tot)*mean(veg.pix[def], na.rm=TRUE)+ #Hypothetical contribution from reforested area if deforested
(length(def)/tot)*mean(veg.pix[def], na.rm=TRUE)+      #Deforestation
(length(cshift)/tot)*mean(veg.pix[def], na.rm=TRUE)+#Hypothetical contribution from comp shift if deforested
(length(eshift)/tot)*mean(veg.pix[eshift], na.rm=TRUE)+#Shift to evergreen
(length(af)/tot)*mean(veg.pix[af], na.rm=TRUE)        #Afforestation

print(hypothetical)
(hypothetical-actual)/actual #percent change!

#####



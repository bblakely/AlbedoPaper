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
                        'vegplot')))

#Seasonal profiles

alb<-colMeans(alb.force, na.rm=TRUE)
st<-colMeans(st.force, na.rm=TRUE)

#Need to update snow ST so it just calls the script
snow.alb<-snow.force.alb
snow.st<-snow.force.st
  #c(0.3871,0.73988,0.70798,0.65542,0.14659,0,0,0,0,0,0.075639,0.19390)

Total<-(alb+st+snow.alb+snow.st)
offset<-(st+snow.alb+snow.st)

mean(Total)/(mean(alb))
mean(offset)/(mean(alb))

### Plots ###
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

#...unlike all of this stuff where I tried to get this to work in R
# test<-cbind(Georef, alb.force[,6])
# coordinates(test)=~Lon+Lat
# proj4string(test)=CRS("+init=epsg:4326")
# test2 = spTransform(test,CRS("+init=epsg:3175"))
# gridded(test2)<-TRUE
# points2grid(test, tolerance=2.5e-8)
# r<-raster(test2)
# projection(r)<-CRS("+init=epsg:3175")
# plot(r)


#veg subsetting
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

#This is not productive but I'm curious. shows total forcings.

real<-unique(convert.code)[which(!is.na(unique(convert.code)))]
newkey<-key[which(key$Code%in%real),]
newkey<-newkey[order(newkey$Code),]

allveg<-aggregate(veg.force, by=list(convert.code), FUN='sum', na.rm=TRUE)
#Combined plots?
allveg.st<-aggregate(st.force, by=list(convert.code), FUN='sum', na.rm=TRUE)
allveg.alb<-aggregate(alb.force, by=list(convert.code), FUN='sum', na.rm=TRUE)

conv.m2<-8000*8000/10e9

prepare.plot<-function(datinput){
colnames(datinput)[1]<-"Code"
datinput$PAL<-newkey$PAL;datinput$MOD<-newkey$MOD
datinput$label<-paste(newkey$PAL, "to", newkey$MOD)
datinput$sums<-rowSums(datinput[,2:13])*conv.m2
datinput<-datinput[order(datinput$MOD, datinput$PAL),]
return(datinput)
}


allveg.plot<-prepare.plot(allveg)
allveg.st.plot<-prepare.plot(allveg.st)
allveg.alb.plot<-prepare.plot(allveg.alb)

#Steps to get to what this plot is showing:
#(1)pixel summed throughout year (2) pixels in each conversion type summed again.
#This factors in both the magnitude of the change and its extent; 300 pixels of -1 W/m will show more stongly than 20 pixels of -5 W/m
par(mfrow=c(1,1))
par(mar=c(5,4,4,4))

#Cutoff for plotting; combine very small forcings into a single 'other' bin
co<-sum(abs(allveg.plot$sums))*0.02 # % of total forcings, positive or negative
draw.co<-which(abs(allveg.plot$sums)>co)

nodraw<-which(!c(1:length(allveg.plot))%in%(draw.co))
nodraw.alb<-sum(allveg.alb.plot$sums[nodraw])
nodraw.st<-sum(allveg.st.plot$sums[nodraw])
nodraw.tot<-sum(allveg.plot$sums[nodraw])

#All on one plot
barplot(allveg.alb.plot$sums, names.arg=allveg.alb.plot$label,las=2, cex.names=0.8, cex.axis=0.8, ylim=c(-40000*conv.m2,15000*conv.m2),ylab="GW", main="Total absolute forcing (annual)")
barplot(allveg.st.plot$sums, names.arg=allveg.st.plot$label,las=2, cex.names=0.8,cex.axis=0.8,col='forest green', add=TRUE)
par(lwd=2)
barplot(allveg.plot$sums, names.arg=allveg.plot$label,las=2,cex.names=0.8,cex.axis=0.8,density=15, col='black',add=TRUE)

#With minor players binned
barplot(c(allveg.alb.plot$sums[draw.co],nodraw.alb), names.arg=c(allveg.alb.plot$label[draw.co], "OTHER"),las=2, cex.names=0.8, cex.axis=0.8, ylim=c(-40000*conv.m2,15000*conv.m2),ylab="GW", main="Total absolute forcing (annual)")
barplot(c(allveg.st.plot$sums[draw.co], nodraw.st), names.arg=c(allveg.st.plot$label[draw.co],"OTHER"),las=2, cex.names=0.8,cex.axis=0.8,col='forest green', add=TRUE)
par(lwd=2)
barplot(c(allveg.plot$sums[draw.co], nodraw.tot), names.arg=c(allveg.plot$label[draw.co], "OTHER"),las=2,cex.names=0.8,cex.axis=0.8,density=15, col='black',add=TRUE)
print(paste("other includes", allveg.plot$label[nodraw]))

#Grouped by conversion type
vegetize<-function(datin, determiner){
dat.veg<-sum(datin$sums[allveg.alb.plot$Code%in%determiner])
return(dat.veg)
}

#There must be a better way...
allveg.alb.def<-vegetize(allveg.alb.plot, Deforest.2)
allveg.alb.comp<-vegetize(allveg.alb.plot, Comp)
allveg.alb.aff<-vegetize(allveg.alb.plot, -Deforest.2)
allveg.alb.rev<-vegetize(allveg.alb.plot, -Comp)

allveg.st.def<-vegetize(allveg.st.plot, Deforest.2)
allveg.st.comp<-vegetize(allveg.st.plot, Comp)
allveg.st.aff<-vegetize(allveg.st.plot, -Deforest.2)
allveg.st.rev<-vegetize(allveg.st.plot, -Comp)

allveg.def<-vegetize(allveg.plot, Deforest.2)
allveg.comp<-vegetize(allveg.plot, Comp)
allveg.aff<-vegetize(allveg.plot, -Deforest.2)
allveg.rev<-vegetize(allveg.plot, -Comp)

albs<-c(allveg.alb.def, allveg.alb.comp,allveg.alb.aff, allveg.alb.rev)
sts<-c(allveg.st.def, allveg.st.comp,allveg.st.aff, allveg.st.rev)
tots<-c(allveg.def, allveg.comp,allveg.aff, allveg.rev)

chglab<-c("Deforest","Comp shift", "Afforest", "Rev Comp Shift")

barplot(albs, names.arg=chglab, cex.names=0.8, cex.axis=0.8, ylim=c(-200000*conv.m2,15000*conv.m2),ylab="GW", main="Total absolute forcing (annual)")
barplot(sts, names.arg=chglab,cex.names=0.8,cex.axis=0.8,col='forest green', add=TRUE)
barplot(tots, names.arg=chglab,cex.names=0.8,cex.axis=0.8,density=15, col='black',add=TRUE)


# Def<-which(allveg$Code%in%Deforest)
# Def.2<-which(allveg$Code%in%Deforest.2)
# Comps<-which(allveg$Code%in%Comp)
# 
# Reforest<-(-Deforest)[1:6]
# Reforest.2<-(-Deforest.2)[1:7]
# Revcomp<-(-Comp)
# Ref<-which(allveg$Code%in%Reforest)
# Ref.2<-which(allveg$Code%in%Reforest.2)
# Revcomps<-which(allveg$Code%in%Revcomp)
# 
# par(mfrow=c(2,2))
# 
# barplot(allveg$sums[Def.2], names.arg=allveg$label[Def.2], ylim=c(-30000, 5000))
# abline(h=0, col='dark red', lwd=2)
# barplot(allveg$sums[Comps], names.arg=allveg$label[Comps], ylim=c(-30000, 5000))
# abline(h=0, col='dark red', lwd=2)
# barplot(allveg$sums[Ref.2], names.arg=allveg$label[Ref.2], ylim=c(-30000, 5000))
# abline(h=0, col='dark red', lwd=2)
# barplot(allveg$sums[Revcomps], names.arg=allveg$label[Revcomps], ylim=c(-30000, 5000))
# abline(h=0, col='dark red', lwd=2)


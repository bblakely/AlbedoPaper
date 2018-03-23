#Redo combine force with real data
source('RecalcAlbedo.R')
source('CompareST_month_v2.R')

alb.diff<-data.frame(d.alb.month)
st.diff<-data.frame(TableDiffs)

alb.force<-data.frame(AlbForce)
st.force<-data.frame(TableForce)

veg.force<-alb.force+st.force
#rm(list=setdiff(ls(), c("alb.force", "st.force","alb.diff","st.diff"))

#Seasonal profiles

alb<-colMeans(alb.force, na.rm=TRUE)
st<-colMeans(st.force, na.rm=TRUE)

#Need to updates snow so it just calls the scripts
snow.alb<-c(0,0,0.48,2.67,0.078,0,0,0,0,0,0,0)
snow.st<-c(0.3871,0.73988,0.70798,0.65542,0.14659,0,0,0,0,0,0.075639,0.19390)

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

text(labloc,0.8,c(expression(alpha),"LST",expression(alpha),"LST"), font=2, cex=2.2)
abline(v=2.5, lwd=3, lty=3)
text(labloc[2],-1,"VEG", cex=2.5, font=2)
text(mean(c(labloc[3], labloc[4])),-1,"SNOW", cex=2.5, font=2)

axis(side=4, labels=seq(from=-2, to=0.5, by=0.5), at=seq(from=-2, to=0.5, by=0.5), cex.axis=2.2, font=2)
#mtext(side=2, text=ylab, line=2.5, cex=2.4, font=2)
abline(h=0, lwd=3)
box(lwd=3)

#### Combined Plots ####
veg.comb<-alb.ann+st.ann
sno.comb<-snow.st.ann+snow.alb.ann
Tot.ann<-Tot.ann #Already made for little plot

barplot(c(Tot.ann, veg.comb,sno.comb),col=c("black","gray","dark blue"),  
        main="Combined RF",names.arg=c("Combined","Vegetation","Snow"), font=2,
        ylim=c(-2,1),cex.main=3, cex.axis=2,cex.names=2, ylab='',
        cex.lab=2, font.axis=2, font.lab=2, axes=FALSE)

abline(h=seq(from=-1.5,to=3,by=0.5), lty=2,col='light gray')

barplot(c(Tot.ann, veg.comb,sno.comb),col=c("black","gray","dark blue"), 
        main="Combined RF",names.arg=c("Combined","Vegetation","Snow"), font=2,
        ylim=c(-2,1),cex.main=3, cex.axis=2,cex.names=2, ylab='',
        cex.lab=2, font.axis=2, font.lab=2, axes=FALSE, add=T)

barplot(c(Tot.ann, veg.comb,sno.comb), density=c(0,20,50),col=c("black","forest green","light blue"), 
        main="Combined RF",names.arg=c("Combined","Vegetation","Snow"), font=2,
        ylim=c(-2,1),cex.main=3, cex.axis=2,cex.names=2, ylab='',
        cex.lab=2, font.axis=2, font.lab=2, axes=FALSE, add=TRUE)

axis(side=2, labels=seq(from=-2, to=3, by=1), at=seq(from=-2, to=3, by=1), cex.axis=1.8, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2.5, font=2)
abline(h=0, lwd=3)
box(lwd=3)
#####


#### Exporting ####
write.csv(alb, "WriteFile/Albedo_Forcing.csv")
write.csv(st, "WriteFile/ST_Forcing.csv")
write.csv(veg.force, "WriteFile/Total_Veg_Forcing.csv")


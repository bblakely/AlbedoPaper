##Net forcing graphs
source('RecalcAlbedo.R')
Alb<-colMeans(AlbForce, na.rm=TRUE)

SToffset<-c(-1.0360913,-1.0960469,-0.2991189, 0.4711990, 2.0769652,3.2174836,1.8973833,1.3350884,1.0554685,0.2082993,-0.3036339,-1.0760419)

#Old ST
#c(-0.922178822, -0.813981288, -0.287019643,  0.038124206,  1.313251843,2.424227792,  1.723607501,  1.194636428,  0.849527934, -0.009961376,-0.393786777, -0.805246905)

Snowoffset_approx<-c(0,0,0.48,2.67,0.078,0,0,0,0,0,0,0)
SnowSToffset_approx<-c(0.3871,0.73988,0.70798,0.65542,0.14659,0,0,0,0,0,0.075639,0.19390)

Total<-(AlbForce.avg+SToffset+Snowoffset_approx+SnowSToffset_approx)
offset<-(SToffset+Snowoffset_approx+SnowSToffset_approx)

par(xpd=FALSE)
#All three
par(mar=c(5,5,5,5))
plot(Alb, ylim=c(-6,4), type='l',lwd='3', xaxt='n',yaxt='n',xlab='',ylab='', main='',cex.lab=2.2, cex.main=2.5,bty="n", col='white')
ylab=expression(RF~(Wm^-2))
axis(side=2, labels= seq(from=-8, to=3, by=2), at=seq(from=-8, to=3, by=2), cex.axis=1.5, font=2)
box(lwd=3)
axis(side=1,labels=seq(from=1,to=12, by=2),at=seq(from=1,to=12,by=2),cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)

lines(SToffset, col='forest green', lwd=4, lty=3)
lines(Snowoffset_approx, col='dark blue', lwd=4,lty=3)
lines(Alb, col='gray', lwd=4, lty=3)
lines(SnowSToffset_approx, col='light blue', lwd=4,lty=3)

lines(Total, lwd=5)
abline(h=0,lty=2, lwd=3,col='red4')



#legend(x=4,y=-2.1, legend=c('Veg. Albedo','Veg. LST','Snow Albedo','Snow LST','Total'), lty=c(3,3,3,3,1),
      # lwd=c(4,4,4,4,4),col=c('gray',"forest green","dark blue","light blue","black"), cex=1.4,
       #ncol=2, x.intersp=0.1,y.intersp=0.6, text.width=1.5, text.font=2)
#This looks bad in the preview but is fine when zoomed up

###Barplots

Alb.ann<-mean(Alb)
ST.ann<-mean(SToffset)
SnST.ann<-mean(SnowSToffset_approx)
SnAlb.ann<-mean(Snowoffset_approx)

Tot.ann<-sum(Alb.ann,ST.ann,SnST.ann,SnAlb.ann)

####little plot#### 
#ylim=c(-1.8,0.8)was old version for combined plot
par(mar=c(5,6,5,5))
ylab=expression(Annual~RF~(Wm^-2))
lab.albveg<-expression(Veg~alpha)
lab.albsnow<-expression(Snow~alpha)
labloc<-barplot(c(Alb.ann,ST.ann,SnST.ann,SnAlb.ann))
                
barplot(c(Alb.ann,ST.ann,SnAlb.ann,SnST.ann), col=c("gray","forest green","light blue","dark blue"), 
         font=2, ylim=c(-2.5,1), cex.axis=2,cex.names=2, ylab='',
         cex.lab=2, font.axis=2, font.lab=2, axes=FALSE)

abline(h=seq(from=-2,to=0.8,by=0.5), lty=2,col='light gray')
barplot(c(Alb.ann,ST.ann,SnAlb.ann,SnST.ann), col=c("gray","forest green","light blue","dark blue"), 
         font=2,ylim=c(-2.5,1),cex.axis=2,cex.names=2, ylab='',
        cex.lab=2, font.axis=2, font.lab=2, axes=FALSE, add=T)

#####
#names.arg=c(expression(alpha),"LST",expression(alpha),"LST"))
#names.arg=c("Veg. Albedo","Veg. LST","Snow LST","Snow Albedo")
#main="Component RF",cex.main=2.2,
#labels=seq(from=-2, to=1, by=0.5)was old version for combo plot
#cex.names was 1.5 for combo plot
#####

text(labloc,0.8,c(expression(alpha),"LST",expression(alpha),"LST"), font=2, cex=2.2)
abline(v=2.5, lwd=3, lty=3)
text(labloc[2],-1,"VEG", cex=2.5, font=2)
text(mean(c(labloc[3], labloc[4])),-1,"SNOW", cex=2.5, font=2)

axis(side=4, labels=seq(from=-1.5, to=0.5, by=0.5), at=seq(from=-1.5, to=0.5, by=0.5), cex.axis=2.2, font=2)
#mtext(side=2, text=ylab, line=2.5, cex=2.4, font=2)
abline(h=0, lwd=3)
box(lwd=3)

#legend(x=1.15,y=-0.3, legend=c(lab.albveg,"Veg ST",lab.albsnow,"Snow ST"), fill=c("gray","forest green","light blue","dark blue"),
       #bty='n', text.font=2, ncol=2, cex=1.5)




####big plot####
veg.comb<-Alb.ann+ST.ann
sno.comb<-SnST.ann+SnAlb.ann
Tot.ann<-sum(Alb.ann,ST.ann,SnST.ann,SnAlb.ann)


#ylim=c(-2,3) is old limits for combined plot with panels
barplot(c(Tot.ann, veg.comb,sno.comb),col=c("black","gray","dark blue"),  
        main="Combined RF",names.arg=c("Combined","Vegetation","Snow"), font=2,
        ylim=c(-2,1),cex.main=3, cex.axis=2,cex.names=2, ylab='',
        cex.lab=2, font.axis=2, font.lab=2, axes=FALSE)
#names.arg=c("Combined Forcings","Veg. Forcings","Snow Forcings")
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

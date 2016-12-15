##Net forcing graphs
Alb<-colMeans(-AlbForce, na.rm=TRUE)
SToffset<-c(-0.922178822, -0.813981288, -0.287019643,  0.038124206,  1.313251843,
            2.424227792,  1.723607501,  1.194636428,  0.849527934, -0.009961376,
            -0.393786777, -0.805246905)
Snowoffset_approx<-c(0,0,0.48,2.67,0.078,0,0,0,0,0,0,0)
SnowSToffset_approx<-c(0.3871,0.73988,0.70798,0.65542,0.14659,0,0,0,0,0,0.075639,0.19390)

Total<-(-AlbForce.avg+SToffset+Snowoffset_approx+SnowSToffset_approx)
offset<-(SToffset+Snowoffset_approx+SnowSToffset_approx)

#All three
par(mar=c(5,5,5,5))
plot(Alb, ylim=c(-5,3), type='l',lwd='3', xaxt='n',yaxt='n',xlab='',ylab='', main='Seasonal RF',cex.lab=2.2, cex.main=2.5,bty="n", col='white')
ylab=expression(RF~(Wm^-2))
axis(side=2, labels= seq(from=-8, to=3, by=1), at=seq(from=-8, to=3, by=1), cex.axis=1.5, font=2)
box(,lwd=3)
axis(side=1,labels=c(1:12),at=seq(from=1,to=12,length.out=12),cex.axis=1.5, font=2)
mtext(side=1, text="Month", line=3, cex=2, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)

lines(SToffset, col='forest green', lwd=4, lty=3)
lines(Snowoffset_approx, col='dark blue', lwd=4,lty=3)
lines(Alb, col='gray', lwd=4, lty=3)
lines(SnowSToffset_approx, col='light blue', lwd=4,lty=3)

lines(Total, lwd=5)
abline(h=0,lty=2, lwd=3,col='red4')

legend(x=4,y=-2.5, legend=c('Veg. Albedo','Veg. LST','Snow Albedo','Snow LST','Total'), lty=c(3,3,3,3,1),
       lwd=c(4,4,4,4,4),col=c('gray',"forest green","dark blue","light blue","black"), cex=0.7,
       ncol=2, x.intersp=0.3,y.intersp=0.6, text.width=1, text.font=2)



###Barplots

Alb.ann<-mean(Alb)
ST.ann<-mean(SToffset)
SnST.ann<-mean(SnowSToffset_approx)
SnAlb.ann<-mean(Snowoffset_approx)

Tot.ann<-sum(Alb.ann,ST.ann,SnST.ann,SnAlb.ann)

#little plot
ylab=expression(Annual~RF~(Wm^-2))
barplot(c(Alb.ann,ST.ann,SnST.ann,SnAlb.ann), col=c("gray","forest green","light blue","dark blue"), 
        main="Component RF",names.arg=c("Veg. Albedo","Veg. LST","Snow LST","Snow Albedo"), font=2,
        ylim=c(-1.8,0.6),cex.main=2.2, cex.axis=1.2,cex.names=1.2, ylab='',
        cex.lab=2, font.axis=2, font.lab=2, axes=FALSE)

abline(h=seq(from=-1.8,to=0.5,by=0.2), lty=2,col='light gray')
barplot(c(Alb.ann,ST.ann,SnST.ann,SnAlb.ann), col=c("gray","forest green","light blue","dark blue"), 
        main="Component RF",names.arg=c("Veg. Albedo","Veg. LST","Snow LST","Snow Albedo"), font=2,
        ylim=c(-1.8,0.6),cex.main=2.2, cex.axis=1.2,cex.names=1.2, ylab='',
        cex.lab=2, font.axis=2, font.lab=2, axes=FALSE, add=T)

axis(side=2, labels=seq(from=-1.8, to=0.5, by=0.4), at=seq(from=-1.8, to=0.5, by=0.4), cex.axis=1.5, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)
abline(h=0, lwd=3)
box(lwd=3)

#big plot
veg.comb<-Alb.ann+ST.ann
sno.comb<-SnST.ann+SnAlb.ann
Tot.ann<-sum(Alb.ann,ST.ann,SnST.ann,SnAlb.ann)

barplot(c(Tot.ann, veg.comb,sno.comb),col=c("black","gray","dark blue"), 
        main="Combined RF",names.arg=c("Combined Forcings","Veg. Forcings","Snow Forcings"), font=2,
        ylim=c(-1.8,3),cex.main=2.2, cex.axis=1.2,cex.names=1.2, ylab='',
        cex.lab=2, font.axis=2, font.lab=2, axes=FALSE)

abline(h=seq(from=-1.5,to=3,by=0.5), lty=2,col='light gray')

barplot(c(Tot.ann, veg.comb,sno.comb),col=c("black","gray","dark blue"), 
        main="Combined RF",names.arg=c("Combined Forcings","Veg. Forcings","Snow Forcings"), font=2,
        ylim=c(-1.8,3),cex.main=2.2, cex.axis=1.2,cex.names=1.2, ylab='',
        cex.lab=2, font.axis=2, font.lab=2, axes=FALSE, add=T)

barplot(c(Tot.ann, veg.comb,sno.comb), density=c(0,20,50), col=c("black","forest green","light blue"), 
        main="Combined RF",names.arg=c("Combined Forcings","Veg. Forcings","Snow Forcings"), font=2,
        ylim=c(-1.5,3),cex.main=2.2, cex.axis=1.2,cex.names=1.2, ylab='',
        cex.lab=2, font.axis=2, font.lab=2, axes=FALSE, add=TRUE)

axis(side=2, labels=seq(from=-1.5, to=3, by=0.5), at=seq(from=-1.5, to=3, by=0.5), cex.axis=1.5, font=2)
mtext(side=2, text=ylab, line=2.5, cex=2, font=2)
abline(h=0, lwd=3)
box(lwd=3)

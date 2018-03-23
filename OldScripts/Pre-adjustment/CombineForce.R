STForcing<-read.csv('ST_dayweight.csv')
AlbedoForcing<-RF
SnowForcing<-read.csv('SnowForcing1.csv')
SnowLST<-read.csv('LST_SnowForcingProf.csv')
SnowLST_long<-approx(SnowLST[,2], n=46)$y

ST<-STForcing[,2]
Alb<-AlbedoForcing
Snow<-SnowForcing[,2]
LSTSnow<-SnowLST_long


plot(Alb, ylim=c(-10,10))
lines(ST)
lines(Snow)

TOTALITY_MOD<-Alb+ST+Snow
TOTALITY<-Alb+ST+Snow+SnowLST_long
plot(TOTALITY)
abline(h=0)

Totsm_pre<-approx(TOTALITY_MOD,n=12)$y
Totsm<-Totsm_pre+SnowLST[,2]
plot(Totsm, type='l',lwd=5)
abline(h=0)

STsm<-approx(ST,n=12)
Snowsm<-approx(Snow,n=12)
Albsm<-approx(Alb, n=12)
LSTSnowsm<-SnowLST[,2]

#ST and Albedo
AlbST<-Alb+ST

#Albedo and snow only
AlbTot<-Alb+Snow
Albtotsm<-approx(AlbTot,n=12)

par(mar=c(5,5,5,5))
plot(Albtotsm$y, ylim=c(-4,2), type='l',lwd='3', xaxt='n',yaxt='n',xlab='Month',ylab='RF(W/m2)', main='Albedo Forcing',cex.lab=2.2, cex.main=2.5,bty="n")
#polygon(x=c(1:12,12:1),y=c(hiquantrfsm$y, rev(loquantrfsm$y)), border=NA,col='gray')
axis(side=2, labels= seq(from=-8, to=2, by=2), at=seq(from=-8, to=2, by=2), cex.axis=1.5, font=2)
box(,lwd=3)
axis(side=1,labels=c(1:12),at=seq(from=1,to=12,length.out=12),cex.axis=1.5, font=2)

#lines(STsm$y, col='forest green', lwd=3, lty=3)
lines(Snowsm$y, col='dark blue', lwd=4,lty=3)
lines(Albsm$y, col='gray', lwd=4, lty=3)

lines(Albtotsm$y, lwd=5)
abline(h=0,lty=2, lwd=3,col='red4')

legend(x=5,y=-2, legend=c('Albedo','Snow', 'Total'), lty=c(3,3,1),lwd=c(4,4,5),col=c('gray',"dark blue","black"), cex=0.9)


#All three
par(mar=c(5,5,5,5))
plot(Totsm, ylim=c(-5,3), type='l',lwd='3', xaxt='n',yaxt='n',xlab='Month',ylab='RF(W/m2)', main='Total Forcing',cex.lab=2.2, cex.main=2.5,bty="n")
#polygon(x=c(1:12,12:1),y=c(hiquantrfsm$y, rev(loquantrfsm$y)), border=NA,col='gray')
axis(side=2, labels= seq(from=-8, to=2, by=2), at=seq(from=-8, to=2, by=2), cex.axis=1.5, font=2)
box(,lwd=3)
axis(side=1,labels=c(1:12),at=seq(from=1,to=12,length.out=12),cex.axis=1.5, font=2)

lines(STsm$y, col='forest green', lwd=4, lty=3)
lines(Snowsm$y, col='dark blue', lwd=4,lty=3)
lines(Albsm$y, col='gray', lwd=4, lty=3)
lines(LSTSnowsm, col='light blue', lwd=4,lty=3)

lines(Totsm, lwd=5)
abline(h=0,lty=2, lwd=3,col='red4')

legend(x=5,y=-2, legend=c('Albedo','Surface Temp.','Snow','Albedo Snow','Total'), lty=c(3,3,3,3,1),lwd=c(4,4,4,4,5),col=c('gray',"forest green","dark blue","light blue","black"), cex=0.7)


#Seasons!
WinterDays<-c(1:8,43:46)
SpringDays<-c(9:19)
SummerDays<-c(20:31)
FallDays<-c(31:43)

mean(TOTALITY[WinterDays])
mean(TOTALITY[SpringDays])
mean(TOTALITY[SummerDays])
mean(TOTALITY[FallDays])






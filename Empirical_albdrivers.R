#Empirical drivers of albedo change
plot.new()
source('Compshift_maps.R')



#Month-ize LAI

d.lai.jan<-rowMeans(lai.chg.dat[1:4],na.rm=TRUE)
d.lai.feb<-rowMeans(lai.chg.dat[5:8],na.rm=TRUE)
d.lai.mar<-rowMeans(lai.chg.dat[9:12],na.rm=TRUE)
d.lai.apr<-rowMeans(lai.chg.dat[13:15],na.rm=TRUE)
d.lai.may<-rowMeans(lai.chg.dat[16:19],na.rm=TRUE)
d.lai.jun<-rowMeans(lai.chg.dat[20:23],na.rm=TRUE)
d.lai.jul<-rowMeans(lai.chg.dat[24:27],na.rm=TRUE)
d.lai.aug<-rowMeans(lai.chg.dat[28:31],na.rm=TRUE)
d.lai.sep<-rowMeans(lai.chg.dat[32:35],na.rm=TRUE)
d.lai.oct<-rowMeans(lai.chg.dat[36:38],na.rm=TRUE)
d.lai.nov<-rowMeans(lai.chg.dat[39:42],na.rm=TRUE)
d.lai.dec<-rowMeans(lai.chg.dat[43:46],na.rm=TRUE)
d.lai.month<-data.frame(cbind(d.lai.jan,d.lai.feb,d.lai.mar,d.lai.apr,d.lai.may,
                              d.lai.jun,d.lai.jul,d.lai.aug,d.lai.sep,d.lai.oct,d.lai.nov,d.lai.dec))

rm('d.lai.jan','d.lai.feb','d.lai.mar','d.lai.apr','d.lai.may',
   'd.lai.jun','d.lai.jul','d.lai.aug','d.lai.sep','d.lai.oct','d.lai.nov','d.lai.dec')


LaiChange.veg<-cbind(d.lai.month, convert.code)

#rm(list=setdiff(ls(), c("AlbChange.veg", "LaiChange.veg", "alb.chg.compshift","convert.code","Deforest.2", "Comp", "Georef.utm")))

#AlbChange<-AlbChange.veg[which(!is.na(convert.code)),];LaiChange<-LaiChange.veg[which(!is.na(convert.code)),]


AlbChange.comp<-AlbChange.veg[which(convert.code%in%Comp[1:2]),]; LaiChange.comp<-LaiChange.veg[which(convert.code%in%Comp[1:2]),]; georef.comp<-Georef[which(convert.code%in%Comp[1:2]),]
AlbChange.strict<-AlbChange.veg[which(convert.code==3),]; LaiChange.strict<-LaiChange.veg[which(convert.code==3),]; georef.strict<-Georef[which(convert.code==3),]

write.csv(cbind(AlbChange.comp, georef.comp),"/Users/bethanyblakely/Desktop/Analysis/MIP/EGtoDC_Data/AlbChange.comp.csv")


plot(colMeans(AlbChange.strict[1:12], na.rm=TRUE), type='l')
lines(colMeans(AlbChange.comp[1:12], na.rm=TRUE), type='l')

par(mfrow=c(1,2))
plot(colMeans(LaiChange.strict[,1:12]), ylim=c(0,3));plot(colMeans(LaiChange.comp[,1:12]), ylim=c(0,3))

par(mar=c(4,4,4,4), mfrow=c(2,2))
summer<-c(6:9)
winter<-c(1:3, 11:12)

plot(rowMeans(AlbChange.strict[,winter], na.rm=TRUE)~rowMeans(LaiChange.strict[,winter], na.rm=TRUE), ylim=c(-0.05, 0.1), xlim=c(-1.5,2.8))
abline(coefficients(lm(rowMeans(AlbChange.strict[,winter], na.rm=TRUE)~rowMeans(LaiChange.strict[,winter], na.rm=TRUE))))
plot(1:10, col='white')
plot(rowMeans(AlbChange.strict[,summer], na.rm=TRUE)~rowMeans(LaiChange.strict[,summer], na.rm=TRUE), ylim=c(-0.05, 0.1), xlim=c(-1.5,2.8))
abline(coefficients(lm(rowMeans(AlbChange.strict[,summer], na.rm=TRUE)~rowMeans(LaiChange.strict[,summer], na.rm=TRUE))))
plot(1:10, col='white')

library(ggplot2)
library(gridExtra)

albexp<-expression(Delta~alpha);laiexp<-expression(Delta~"LAI")
#MX to DC and EG to DC

gdat.w<-data.frame(cbind(rowMeans(AlbChange.comp[,winter], na.rm=TRUE),rowMeans(LaiChange.comp[,winter], na.rm=TRUE),rowMeans(LaiChange.comp[,summer], na.rm=TRUE))); colnames(gdat.w)<-c("walb",'wlai', 'slai')
p1e<-ggplot(gdat.w, aes(x=wlai,y=walb, col=slai))+geom_point(size=2)+xlim(-2.5,2.8)+ylim(-0.06,0.075)+theme_bw()+ylab(albexp)+xlab(laiexp)+ggtitle("Winter")+geom_hline(yintercept=0, linetype="dashed", size=0.2)+scale_colour_gradient2(low='orange4', mid='antiquewhite', high="forest green", midpoint=0.5)

gdat.s<-data.frame(cbind(rowMeans(AlbChange.comp[,summer], na.rm=TRUE),rowMeans(LaiChange.comp[,summer], na.rm=TRUE))); colnames(gdat.s)<-c("salb",'slai')
p2e<-ggplot(gdat.s, aes(x=slai,y=salb, col=slai))+geom_point(size=2)+xlim(-2.5,2.8)+ylim(-0.06,0.075)+theme_bw()+ylab(albexp)+xlab(laiexp)+ggtitle("Summer")+geom_hline(yintercept=0, linetype="dashed", size=0.2)+scale_colour_gradient2(low='orange4', mid='antiquewhite', high="forest green", midpoint=0.5)

grid.arrange(p1e,p2e, nrow=2)

#STRICT
gdat.w<-data.frame(cbind(rowMeans(AlbChange.strict[,winter], na.rm=TRUE),rowMeans(LaiChange.strict[,winter], na.rm=TRUE))); colnames(gdat.w)<-c("walb",'wlai')
p1es<-ggplot(gdat.w, aes(x=wlai,y=walb, col=wlai))+geom_point(size=0.04)+xlim(-2.5,2.8)+ylim(-0.06,0.075)+theme_bw()+ylab(albexp)+xlab(laiexp)+ggtitle("Empirical")+geom_hline(yintercept=0, linetype="dashed", size=0.2)

gdat.s<-data.frame(cbind(rowMeans(AlbChange.strict[,summer], na.rm=TRUE),rowMeans(LaiChange.strict[,summer], na.rm=TRUE))); colnames(gdat.s)<-c("salb",'slai')
p2es<-ggplot(gdat.s, aes(x=slai,y=salb, col=slai))+geom_point(size=0.04)+xlim(-2.5,2.8)+ylim(-0.06,0.075)+theme_bw()+ylab(albexp)+xlab(laiexp)+ggtitle("Empirical")+geom_hline(yintercept=0, linetype="dashed", size=0.2)

grid.arrange(p1es,p2es, nrow=2)


saveRDS(p1e, "/Users/bethanyblakely/Desktop/Analysis/MIP/p1e.rds");saveRDS(p2e, "/Users/bethanyblakely/Desktop/Analysis/MIP/p2e.rds")
saveRDS(p1es, "/Users/bethanyblakely/Desktop/Analysis/MIP/p1es.rds");saveRDS(p2es, "/Users/bethanyblakely/Desktop/Analysis/MIP/p2es.rds")


source('CompareST_month.R')

LST.DecidDeforest_Rf<-TableForce[convert.code==8 | convert.code==10,]
plot(-colMeans(LST.DecidDeforest_Rf, na.rm=TRUE), type='l',ylim=c(-7,7))
abline(h=0)

LST.Decidify_Rf<-TableForce[convert.code==3 | convert.code==4 | convert.code==(-1),]
plot(colMeans(LST.Decidify_Rf, na.rm=TRUE), type='l', ylim=c(-7,7))
abline(h=0)

LST.Deforest_Rf<-TableForce[convert.code==8 | convert.code==10 |convert.code==7 | 
                          convert.code==9 | convert.code==11 | convert.code==13,]

LST.EGDeforest_Rf<-TableForce[convert.code==7 | convert.code==9 | convert.code==11 | convert.code==13,]

plot(colMeans(LST.Deforest_Rf, na.rm=TRUE), type='l', ylim=c(-7,7))
abline(h=0)

rm(list=setdiff(ls(), c("LST.Deforest_Rf","LST.Decidify_Rf","LST.DecidDeforest_Rf","LST.EGDeforest_Rf")))

source('RecalcAlbedo.R')

AlbForce<-data.frame(-AlbForce)

A.DecidDeforest_Rf<-AlbForce[convert.code==8 | convert.code==10,]
plot(-colMeans(A.DecidDeforest_Rf, na.rm=TRUE), type='l',ylim=c(-7,7))
abline(h=0)

A.Decidify_Rf<-AlbForce[convert.code==3 | convert.code==4 | convert.code==(-1),]
plot(colMeans(A.Decidify_Rf, na.rm=TRUE), type='l', ylim=c(-7,7))
abline(h=0)

A.Deforest_Rf<-AlbForce[convert.code==8 | convert.code==10 |convert.code==7 | 
                          convert.code==9 | convert.code==11 | convert.code==13,]

A.EGDeforest_Rf<-AlbForce[convert.code==7 | convert.code==9 | convert.code==11 | convert.code==13,]



###########
plot(colMeans(A.Deforest_Rf, na.rm=TRUE), type='l', ylim=c(-7,7))
abline(h=0)

rm(list=setdiff(ls(), c("LST.Deforest_Rf","LST.Decidify_Rf","LST.DecidDeforest_Rf","LST.EGDeforest_Rf",
                        "A.Deforest_Rf"," A.Decidify_Rf","A.DecidDeforest_Rf", "A.EGDeforest_Rf")))


LSTDef<-colMeans(LST.Deforest_Rf, na.rm=TRUE)
ADef<-colMeans(A.Deforest_Rf, na.rm=TRUE)
ADef[c(1:4,11:12)]<-ADef[c(1:4,11:12)]*0.1
Net.Deforest<-LSTDef+ADef
plot(Net.Deforest, type='l', ylim=c(-10,5))
abline(h=0)
mean(Net.Deforest)

LSTDC<-colMeans(LST.Decidify_Rf, na.rm=TRUE)
ADC<-colMeans(A.Decidify_Rf, na.rm=TRUE)
ADC[c(1:4,11:12)]<-0
  #ADC[c(1:4,11:12)]*0.1
Net.Decidify<-LSTDC+ADC
plot(Net.Decidify, type='l', ylim=c(-7,10))
abline(h=0)
mean(Net.Decidify)

LSTDcDf<-colMeans(LST.DecidDeforest_Rf, na.rm=TRUE)
ADcDf<-colMeans(A.DecidDeforest_Rf, na.rm=TRUE)
ADcDf[c(1:4,11:12)]<-0
  #ADcDf[c(1:4,11:12)]*0.1
Net.DecidDeforest<-LSTDcDf+ADcDf
plot(-Net.DecidDeforest, type='l', ylim=c(-7,10))
abline(h=0)
mean(Net.DecidDeforest)


LSTEgDf<-colMeans(LST.EGDeforest_Rf, na.rm=TRUE)
AEgDf<-colMeans(A.EGDeforest_Rf, na.rm=TRUE)
AEgDf[c(1:4,11:12)]<-0
  #AEgDf[c(1:4,11:12)]*0.1
Net.EGDeforest<-LSTEgDf+AEgDf
lines(-Net.EGDeforest, type='l', ylim=c(-7,10))
abline(h=0)
mean(Net.EGDeforest)



plot(-LSTEgDf, type='l', ylim=c(-10,10), col="purple4")
lines(-LSTDcDf, type='l', col="forest green")
lines(-AEgDf, col='purple4', lwd=1.5)
lines(-ADcDf, col='forest green', lwd=1.5)

lines(ADC, col="orange", lwd=1.5)
lines(LSTDC, col="orange")
abline(h=0)

plot(LSTEgDf, type='l', ylim=c(-10,10), col="purple4", lty=2)
lines(LSTDcDf, type='l', col="forest green", lty=2)
lines(AEgDf, col='purple4', lwd=1.5,lty=2)
lines(ADcDf, col='forest green', lwd=1.5, lty=2)

lines(ADC, col="orange", lwd=1.5)
lines(LSTDC, col="orange")

abline(h=0)


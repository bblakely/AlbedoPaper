#vegetation symbology
default<-rasterFromXYZ(cbind(Georef.utm, convert.code))
bkco<-'black'
defco<-'gray30'
plot(trim(default), col=bkco, legend=FALSE)
#deforestation
defrast<-default
defrast[which(convert.code%in%-Deforest.2 | convert.code%in%-Comp | convert.code%in%Comp | convert.code==0)]<-NA
plot(defrast, add=TRUE, col=defco, legend=FALSE)
#composition shift
cshift<-default
cshift[which(convert.code%in%Deforest.2 | convert.code%in%-Deforest.2 | convert.code%in%-Comp | convert.code==0)]<-NA
plot(cshift, add=TRUE, col='green', legend=FALSE)



#source('RecalcAlbedo.R')
alb.chg.compshift<-AlbChange.veg

breaknum=11
col<-colorRampPalette(c('red','light gray', 'blue'))
breaks<-seq(from=-0.1, to=0.1, length.out=breaknum)

albrast<-rasterFromXYZ(cbind(Georef.utm, AlbChange.veg[6]))
plot(trim(albrast), col=bkco, legend='FALSE',main='Albedo Change')
albrast[which(convert.code%in%Deforest.2 | convert.code%in%-Deforest.2 | convert.code%in%-Comp | convert.code==0)]<-NA
plot(trim(albrast), add=TRUE, breaks=breaks, col=col(breaknum))

#source('CompareST_month_v3.R')
st.chg.compshift<-STChg.veg
bkco<-'black'

breaknum=11
col<-(colorRampPalette(c('blue','light gray', 'red')))
breaks<-seq(from=-2, to=2, length.out=breaknum)

strast<-rasterFromXYZ(cbind(Georef.utm, STChg.veg[6]))
plot(trim(strast), col=bkco, legend='FALSE',main='Surface Temperature Change')
strast[which(convert.code%in%Deforest.2 | convert.code%in%-Deforest.2 | convert.code%in%-Comp | convert.code==0)]<-NA
plot(trim(strast), add=TRUE, breaks=breaks, col=col(breaknum))



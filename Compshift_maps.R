#vegetation symbology
source('VegConvert_UTM.R')

par(mar=c(1,1,1,1))
default<-rasterFromXYZ(cbind(Georef.utm, convert.code))
bkco<-'black'
defco<-'gray30'
plot(trim(default), col=bkco, legend=FALSE, box=FALSE, axes=FALSE)
#deforestation
defrast<-default
defrast[which(convert.code%in%-Deforest.2 | convert.code%in%-Comp | convert.code%in%Comp | convert.code==0)]<-NA
plot(defrast, add=TRUE, col=defco, legend=FALSE)
#composition shift
cshift<-default
cshift[which(convert.code%in%Deforest.2 | convert.code%in%-Deforest.2 | convert.code%in%-Comp | convert.code==0)]<-NA
plot(cshift, add=TRUE, col='green', legend=FALSE)

legend(4.5e05,1430000, legend= c('Composition Shift', 'Still Deforested','Other'), cex=1, 
       fill=c('green', 'gray30','black'), text.font=2, bty='n')
dev.copy(png, filename='Figures/Maps/CompshiftMap.png', width=500, height=410); dev.off()


reportnum<-FALSE
source('RecalcAlbedo.R')
par(mar=c(1,1,1,1))
alb.chg.compshift<-AlbChange.veg

breaknum=11
col<-colorRampPalette(c('dark red', 'red','light steel blue', 'blue', 'dark blue'))
breaks<-seq(from=-0.1, to=0.1, length.out=breaknum)

#albrast<-rasterFromXYZ(cbind(Georef.utm, AlbChange.veg[6])) #original, june
albrast<-rasterFromXYZ(cbind(Georef.utm, rowMeans(AlbChange.veg[1:12], na.rm=TRUE)))
plot(trim(albrast), col=bkco, legend='FALSE',main='Albedo Change', box=FALSE, axes=FALSE)
albrast[which(convert.code%in%Deforest.2 | convert.code%in%-Deforest.2 | convert.code%in%-Comp | convert.code==0)]<-NA
plot(trim(albrast), add=TRUE, breaks=breaks, col=col(breaknum))

dev.copy(png, filename='Figures/Maps/AlbChangeMap.png', width=500, height=410); dev.off()



source('CompareST_month_v3.R')
par(mar=c(1,1,1,1))
st.chg.compshift<-STChg.veg


breaknum=11
col<-colorRampPalette(rev(c('dark red', 'red', 'pink2', 'blue', 'dark blue')))
breaks<-seq(from=-2, to=2, length.out=breaknum)

strast<-rasterFromXYZ(cbind(Georef.utm, rowMeans(STChg.veg[1:12])))
plot(trim(strast), col=bkco, legend='FALSE',main='Surface Temperature Change', box=FALSE, axes=FALSE)
strast[which(convert.code%in%Deforest.2 | convert.code%in%-Deforest.2 | convert.code%in%-Comp | convert.code==0)]<-NA
plot(trim(strast), add=TRUE, breaks=breaks, col=col(breaknum))

dev.copy(png, filename='Figures/Maps/STChangeMap.png', width=500, height=410); dev.off()


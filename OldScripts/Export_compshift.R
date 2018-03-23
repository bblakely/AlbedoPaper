source('RecalcAlbedo.R')

rm(list=setdiff(ls(),c('ModernAlb','PaleoAlb','d.alb.month','convert.code', 'Georef')))

#aggregate historic and modern albedo
#Modern
mod.alb.jan<-rowMeans(ModernAlb[1:4],na.rm=TRUE)
mod.alb.feb<-rowMeans(ModernAlb[5:8],na.rm=TRUE)
mod.alb.mar<-rowMeans(ModernAlb[9:12],na.rm=TRUE)
mod.alb.apr<-rowMeans(ModernAlb[13:15],na.rm=TRUE)
mod.alb.may<-rowMeans(ModernAlb[16:19],na.rm=TRUE)
mod.alb.jun<-rowMeans(ModernAlb[20:23],na.rm=TRUE)
mod.alb.jul<-rowMeans(ModernAlb[24:27],na.rm=TRUE)
mod.alb.aug<-rowMeans(ModernAlb[28:31],na.rm=TRUE)
mod.alb.sep<-rowMeans(ModernAlb[32:35],na.rm=TRUE)
mod.alb.oct<-rowMeans(ModernAlb[36:38],na.rm=TRUE)
mod.alb.nov<-rowMeans(ModernAlb[39:42],na.rm=TRUE)
mod.alb.dec<-rowMeans(ModernAlb[43:46],na.rm=TRUE)
mod.alb.month<-data.frame(cbind(mod.alb.jan,mod.alb.feb,mod.alb.mar,mod.alb.apr,mod.alb.may,
                              mod.alb.jun,mod.alb.jul,mod.alb.aug,mod.alb.sep,mod.alb.oct,mod.alb.nov,mod.alb.dec))

albveg.mod<-cbind(mod.alb.month, convert.code, Georef)


#Historic
pal.alb.jan<-rowMeans(PaleoAlb[1:4],na.rm=TRUE)
pal.alb.feb<-rowMeans(PaleoAlb[5:8],na.rm=TRUE)
pal.alb.mar<-rowMeans(PaleoAlb[9:12],na.rm=TRUE)
pal.alb.apr<-rowMeans(PaleoAlb[13:15],na.rm=TRUE)
pal.alb.may<-rowMeans(PaleoAlb[16:19],na.rm=TRUE)
pal.alb.jun<-rowMeans(PaleoAlb[20:23],na.rm=TRUE)
pal.alb.jul<-rowMeans(PaleoAlb[24:27],na.rm=TRUE)
pal.alb.aug<-rowMeans(PaleoAlb[28:31],na.rm=TRUE)
pal.alb.sep<-rowMeans(PaleoAlb[32:35],na.rm=TRUE)
pal.alb.oct<-rowMeans(PaleoAlb[36:38],na.rm=TRUE)
pal.alb.nov<-rowMeans(PaleoAlb[39:42],na.rm=TRUE)
pal.alb.dec<-rowMeans(PaleoAlb[43:46],na.rm=TRUE)
pal.alb.month<-data.frame(cbind(pal.alb.jan,pal.alb.feb,pal.alb.mar,pal.alb.apr,pal.alb.may,
                                pal.alb.jun,pal.alb.jul,pal.alb.aug,pal.alb.sep,pal.alb.oct,pal.alb.nov,pal.alb.dec))

albveg.pal<-cbind(pal.alb.month, convert.code, Georef)
albveg.chg<-cbind(d.alb.month, convert.code, Georef)


albveg.mod.dat<-albveg.mod[!is.na(convert.code),]
albveg.pal.dat<-albveg.pal[!is.na(convert.code),]
albveg.chg.dat<-albveg.chg[!is.na(convert.code),]

albveg.mod.shift<-albveg.mod.dat[albveg.mod.dat$convert.code==3,]
albveg.pal.shift<-albveg.pal.dat[albveg.pal.dat$convert.code==3,]
albveg.chg.shift<-albveg.chg.dat[albveg.chg.dat$convert.code==3,]

write.csv(albveg.chg.shift, 'WriteFile/EGtoDC_Albedo_Change.csv')
write.csv(albveg.mod.shift, 'WriteFile/EGtoDC_Albedo_Modern.csv')
write.csv(albveg.pal.shift, 'WriteFile/EGtoDC_Albedo_Historic.csv')

source('CompareST_month.R')
rm(list=setdiff(ls(),c('ModN_Dat','ModD_Dat','PalN_Dat','PalD_Dat',   #,'n.temp.month','d.temp.month'
                       'Tabweight','convert.code', 'Georef', 'albveg.chg.shift')))

#Apply weights;tabweight indicates portion of day spent in daylight
modern.st<-(ModD_Dat*Tabweight+ModN_Dat*(1-Tabweight))
paleo.st<-(PalD_Dat*Tabweight+PalN_Dat*(1-Tabweight))

#Aggregate to months
mod.st.jan<-rowMeans(modern.st[1:4],na.rm=TRUE)
mod.st.feb<-rowMeans(modern.st[5:8],na.rm=TRUE)
mod.st.mar<-rowMeans(modern.st[9:12],na.rm=TRUE)
mod.st.apr<-rowMeans(modern.st[13:15],na.rm=TRUE)
mod.st.may<-rowMeans(modern.st[16:19],na.rm=TRUE)
mod.st.jun<-rowMeans(modern.st[20:23],na.rm=TRUE)
mod.st.jul<-rowMeans(modern.st[24:27],na.rm=TRUE)
mod.st.aug<-rowMeans(modern.st[28:31],na.rm=TRUE)
mod.st.sep<-rowMeans(modern.st[32:35],na.rm=TRUE)
mod.st.oct<-rowMeans(modern.st[36:38],na.rm=TRUE)
mod.st.nov<-rowMeans(modern.st[39:42],na.rm=TRUE)
mod.st.dec<-rowMeans(modern.st[43:46],na.rm=TRUE)
mod.st.month<-data.frame(cbind(mod.st.jan,mod.st.feb,mod.st.mar,mod.st.apr,mod.st.may,
                                mod.st.jun,mod.st.jul,mod.st.aug,mod.st.sep,mod.st.oct,mod.st.nov,mod.st.dec))

pal.st.jan<-rowMeans(paleo.st[1:4],na.rm=TRUE)
pal.st.feb<-rowMeans(paleo.st[5:8],na.rm=TRUE)
pal.st.mar<-rowMeans(paleo.st[9:12],na.rm=TRUE)
pal.st.apr<-rowMeans(paleo.st[13:15],na.rm=TRUE)
pal.st.may<-rowMeans(paleo.st[16:19],na.rm=TRUE)
pal.st.jun<-rowMeans(paleo.st[20:23],na.rm=TRUE)
pal.st.jul<-rowMeans(paleo.st[24:27],na.rm=TRUE)
pal.st.aug<-rowMeans(paleo.st[28:31],na.rm=TRUE)
pal.st.sep<-rowMeans(paleo.st[32:35],na.rm=TRUE)
pal.st.oct<-rowMeans(paleo.st[36:38],na.rm=TRUE)
pal.st.nov<-rowMeans(paleo.st[39:42],na.rm=TRUE)
pal.st.dec<-rowMeans(paleo.st[43:46],na.rm=TRUE)
pal.st.month<-data.frame(cbind(pal.st.jan,pal.st.feb,pal.st.mar,pal.st.apr,pal.st.may,
                               pal.st.jun,pal.st.jul,pal.st.aug,pal.st.sep,pal.st.oct,pal.st.nov,pal.st.dec))

d.st.month<-mod.st.month-pal.st.month


stveg.pal<-cbind(pal.st.month, convert.code, Georef)
stveg.mod<-cbind(mod.st.month, convert.code, Georef)
stveg.chg<-cbind(d.st.month, convert.code, Georef)


stveg.mod.dat<-stveg.mod[!is.na(convert.code),]
stveg.pal.dat<-stveg.pal[!is.na(convert.code),]
stveg.chg.dat<-stveg.chg[!is.na(convert.code),]

stveg.mod.shift<-stveg.mod.dat[stveg.mod.dat$convert.code==3,]
stveg.pal.shift<-stveg.pal.dat[stveg.pal.dat$convert.code==3,]
stveg.chg.shift<-stveg.chg.dat[stveg.chg.dat$convert.code==3,]

write.csv(stveg.chg.shift, 'WriteFile/EGtoDC_ST_Change.csv')
write.csv(stveg.mod.shift, 'WriteFile/EGtoDC_ST_Modern.csv')
write.csv(stveg.pal.shift, 'WriteFile/EGtoDC_ST_Historic.csv')



dat.raw<-read.csv('MODISalbedo.csv', skip=7)
mod.raw<-read.csv('Samp100_mod.csv',skip=7 )
#('F:/Samp100.csv',skip=7 )
#('Albedo_Modern1.csv',skip=7)
#('ModelAlbedo.csv', skip=7)
palveg.filter<-read.csv("Paleo_vegetation_UTM_v2.csv", skip=7);palveg.filter<-palveg.filter[2:nrow(palveg.filter),]

dat<-dat.raw[2:24535,7:52]
mod<-mod.raw[2:24535,7:52]


dat[dat>1]<-NA
mod[mod>1]<-NA


dat[dat<0.01]<-NA
mod[mod<0.01]<-NA

dat[which(palveg.filter$B1==0),]<-NA; mod[which(palveg.filter$B1==0),]<-NA

vegtype<-read.csv('ModUTM.csv')[,2]
colvec<-rep('black', 24534)
colvec[vegtype==12]<-'yellow'
colvec[vegtype==14]<-'orange'
colvec[vegtype==5]<-'dark blue'
colvec[vegtype==4]<-'forest green'
colvec[vegtype==1]<-'purple4'
colvec[vegtype==26]<-'dark red'

dat[is.na(vegtype),]<-NaN
mod[is.na(vegtype),]<-NaN

#Group by month, data
dat.dat<-dat
dat.jan<-rowMeans(dat.dat[1:4])
dat.feb<-rowMeans(dat.dat[5:8])
dat.mar<-rowMeans(dat.dat[9:12])
dat.apr<-rowMeans(dat.dat[13:15])
dat.may<-rowMeans(dat.dat[16:19])
dat.jun<-rowMeans(dat.dat[20:23])
dat.jul<-rowMeans(dat.dat[24:27])
dat.aug<-rowMeans(dat.dat[28:31])
dat.sep<-rowMeans(dat.dat[32:35])
dat.oct<-rowMeans(dat.dat[36:38])
dat.nov<-rowMeans(dat.dat[39:42])
dat.dec<-rowMeans(dat.dat[43:46])


dat.month<-data.frame(cbind(dat.jan,dat.feb,dat.mar,dat.apr,dat.may,dat.jun,
                            dat.jul,dat.aug,dat.sep,dat.oct,dat.nov,dat.dec))

dat.win<-rowMeans(dat.month[,c(1:2, 12)], na.rm=TRUE)
dat.spr<-rowMeans(dat.month[,3:5], na.rm=TRUE)
dat.sum<-rowMeans(dat.month[,6:8], na.rm=TRUE)
dat.fal<-rowMeans(dat.month[,9:11], na.rm=TRUE)

dat.seas<-cbind(dat.win,dat.spr,dat.sum,dat.fal)


#Group by month, model
mod.dat<-mod
mod.jan<-rowMeans(mod.dat[1:4])
mod.feb<-rowMeans(mod.dat[5:8])
mod.mar<-rowMeans(mod.dat[9:12])
mod.apr<-rowMeans(mod.dat[13:15])
mod.may<-rowMeans(mod.dat[16:19])
mod.jun<-rowMeans(mod.dat[20:23])
mod.jul<-rowMeans(mod.dat[24:27])
mod.aug<-rowMeans(mod.dat[28:31])
mod.sep<-rowMeans(mod.dat[32:35])
mod.oct<-rowMeans(mod.dat[36:38])
mod.nov<-rowMeans(mod.dat[39:42])
mod.dec<-rowMeans(mod.dat[43:46])


mod.month<-data.frame(cbind(mod.jan,mod.feb,mod.mar,mod.apr,mod.may,mod.jun,
                            mod.jul,mod.aug,mod.sep,mod.oct,mod.nov,mod.dec))

mod.win<-rowMeans(mod.month[,c(1:2, 12)], na.rm=TRUE)
mod.spr<-rowMeans(mod.month[,3:5], na.rm=TRUE)
mod.sum<-rowMeans(mod.month[,6:8], na.rm=TRUE)
mod.fal<-rowMeans(mod.month[,9:11], na.rm=TRUE)

mod.seas<-cbind(mod.win,mod.spr,mod.sum,mod.fal)


#Monthly Plots
#library(smoothScatter)
monthlab<-c('jan','feb','mar','april','may','jun','jul','aug','sep','oct','nov','dec')

  #c('jan',0,0,0,'feb',0,0,0,'mar',0,0,0,'april',0,0,0,'may',0,0,0,'jun',0,0,0,'jul',0,0,0,'aug',0,0,0,'sep',0,0,0,'oct',0,0,0,'nov',0,0,0,'dec')
par(mfrow=c(2,2))
par(mar=c(4,4,3,2))
for (i in 1:12){
smoothScatter(y=dat.month[,i], x=mod.month[,i], main=monthlab[i], ylim=c(0.1,0.8), xlim=c(0.1,0.65),
              colramp=colorRampPalette(c('white','forest green', 'black')), ylab='MODIS', xlab='Lowess')
abline(0,1, col='black')
}


for (i in 1:12){
  #resid<-mod[,i]-dat[,i]
  #cutoff<-2.5*sd(resid, na.rm=TRUE)
  #colvec[(mod[,i]-dat[,i]>cutoff | mod[,i]-dat[,i]<(-cutoff))]<-'red'
  dat[is.na(vegtype),i]<-NaN
  mod[is.na(vegtype),i]<-NaN
  plot(y=dat.month[,i], x=mod.month[,i], main=monthlab[i], ylim=c(0.1,0.7), xlim=c(0.1,0.7),
        ylab='MODIS', xlab='Lowess', col=colvec)
  print(summary(lm(dat.month[,i]~mod.month[,i]))$r.squared)
  abline(0,1, col='black')
}

##Seasonal plots
seaslab<-c('Winter', 'Spring', 'Summer','Fall')
par(mfrow=c(2,2))
#par(mar=c(3,3,2,0))
par(mar=c(4,4,3,2))
for (i in 1:4){
  smoothScatter(y=dat.seas[,i], x=mod.seas[,i], main=seaslab[i], ylim=c(0,0.7), xlim=c(0,0.7),
                colramp=colorRampPalette(c('white','forest green', 'black')),
                font=2, font.lab=2,  ylab='MODIS', xlab='Lowess')
  thin<-sample(1:nrow(dat.seas), 8000)
  #smoothScatter(y=dat.seas[,i], x=mod.seas[,i], main=seaslab[i], ylim=c(0,0.7), xlim=c(0,0.7),
  #colramp=colorRampPalette(c('white', 'black')),nrpoints=0,
  #font=2, font.lab=2, ylab='', xlab='')
  colvec.s<-colvec
  colvec.s[colvec=='dark blue']<-'blue'
  colvec.s[colvec=='purple4']<-'purple'
  colvec.s[colvec=='dark red']<-'red'
  #points(y=dat.seas[thin,i], x=mod.seas[thin,i],col=colvec.s[thin], pch='.', cex=1.5)
  # ylab='MODIS', xlab='Lowess',
  abline(0,1, col='black')
  box(lwd=2)
}

for (i in 1:4){

  plot(y=dat.seas[,i], x=mod.seas[,i], main=seaslab[i], ylim=c(0,0.7), xlim=c(0,0.7),
       col=colvec, font=2, font.lab=2, pch='.',ylab='MODIS', xlab='Lowess')
  #ylab='', xlab=''
  abline(0,1, col='black')
  box(lwd=2)
  
  
}


plot(seq(from=0, to=1,by=0.1), col='white', bty='n', xaxt='n',yaxt='n', xlab='',ylab='')
legend(1,1,legend=c("Evergreen","Mixed","Deciduous","Mosaic","Crop","Urban"), fill=c('purple4','dark blue','forest green','orange','yellow','dark red'), text.font = 2)



#Where are residuals?
diffs<-dat-mod
diffs.seas<-dat.seas - mod.seas
geo<-dat.raw[2:24535,5:6]

summary(lm(dat.seas[,1]~mod.seas[,1]))
summary(lm(dat.seas[,2]~mod.seas[,2]))
summary(lm(dat.seas[,3]~mod.seas[,3]))
summary(lm(dat.seas[,4]~mod.seas[,4]))

summary(lm(rowMeans(dat.seas)~rowMeans(mod.seas)))

# feb.diffs<-cbind(geo,diffs[,5])
# feb.diffs.lg<-febdiffs[(feb.diffs[,3]>0.15 | feb.diffs[,3]<(-0.15)),]
# feb.diffs.lg<-feb.diffs.lg[!is.na(feb.diffs.lg[,3]),]
#
# write.csv(feb.diffs.lg,'FebDiverge.csv')


# # ##Original Ends here; this version adds normal noise to bring model variation closer to data variation
# #
# library(MASS)
# 
# new.mod<-mod
# adjstore<-matrix(0,14,46)
# par(mfrow=c(2,2))
# 
# #Loop for adding noise
# for(i in 1:46){ #Big loop for adding noise
#   for(v in (unique(vegtype)[!is.na(unique(vegtype))])){
# 
#     if(v!=26){
# 
#     dat.l<-dat[which(vegtype==v), i]
#     mod.l<-mod[which(vegtype==v),i]
# 
#     lim<-c(min(dat.l, na.rm=TRUE), max(dat.l, na.rm=TRUE))
# 
#     #hist(dat.l, xlim=lim)
#     #hist(mod.l, xlim=lim)
# 
#     modfit<-fitdistr(mod.l[!is.na(mod.l)], 'normal')$estimate
#     datfit<-fitdistr(dat.l[!is.na(dat.l)], 'normal')$estimate
# 
#     adj<-datfit[2]-modfit[2]
#     if(unname(adj)>0){
#     adjstore[v,i]<-adj
# 
#     modc<-mod.l+rnorm(length(mod.l),0, 2*adj)
#   }else(modc<-mod.l)
#     hist(dat.l, xlim=lim)
#     hist(modc, xlim=lim)
# 
#     new.mod[which(vegtype==v), i]<-modc
#     }
# 
#   }
# }
# 
# ##Monthify new modern:####
# 
# new.mod.dat<-new.mod
# 
# new.mod.jan<-rowMeans(new.mod.dat[1:4])
# new.mod.feb<-rowMeans(new.mod.dat[5:8])
# new.mod.mar<-rowMeans(new.mod.dat[9:12])
# new.mod.apr<-rowMeans(new.mod.dat[13:15])
# new.mod.may<-rowMeans(new.mod.dat[16:19])
# new.mod.jun<-rowMeans(new.mod.dat[20:23])
# new.mod.jul<-rowMeans(new.mod.dat[24:27])
# new.mod.aug<-rowMeans(new.mod.dat[28:31])
# new.mod.sep<-rowMeans(new.mod.dat[32:35])
# new.mod.oct<-rowMeans(new.mod.dat[36:38])
# new.mod.nov<-rowMeans(new.mod.dat[39:42])
# new.mod.dec<-rowMeans(new.mod.dat[43:46])
# 
# 
# new.mod.month<-data.frame(cbind(new.mod.jan,new.mod.feb,new.mod.mar,new.mod.apr,new.mod.may,new.mod.jun,
#                             new.mod.jul,new.mod.aug,new.mod.sep,new.mod.oct,new.mod.nov,new.mod.dec))
# 
# new.mod.win<-rowMeans(new.mod.month[,c(1:2, 12)], na.rm=TRUE)
# new.mod.spr<-rowMeans(new.mod.month[,3:5], na.rm=TRUE)
# new.mod.sum<-rowMeans(new.mod.month[,6:8], na.rm=TRUE)
# new.mod.fal<-rowMeans(new.mod.month[,9:11], na.rm=TRUE)
# 
# new.mod.seas<-cbind(new.mod.win,new.mod.spr,new.mod.sum,new.mod.fal)
# #####
# 
# #Modern plots ####
# for (i in 1:12){
#   plot(y=dat.month[,i], x=new.mod.month[,i], main=monthlab[i], ylim=c(0.1,0.7), xlim=c(0.1,0.7),
#        ylab='MODIS', xlab='Lowess', col=colvec)
#   print(summary(lm(dat.month[,i]~new.mod.month[,i]))$r.squared)
#   abline(0,1, col='black')
# }
# 
# for (i in 1:4){
# 
#   plot(y=dat.seas[,i], x=new.mod.seas[,i], main=seaslab[i], ylim=c(0,0.7), xlim=c(0,0.7),
#        ylab='', xlab='',col=colvec, font=2, font.lab=2, pch='.')
#   #ylab='MODIS', xlab='Lowess',
#   abline(0,1, col='black')
#   box(lwd=2)
# }
# 
# #####
# #Fuzzify paleo####
# 
# pal.input<-read.csv('Samp100_pal.csv', skip=7); pal.input<-pal.input[2:nrow(pal.input),]
# pal.dat<-pal.input[,7:ncol(pal.input)]
# pal.dat[pal.dat>1]<-NA;pal.dat[pal.dat<0.01]<-NA
# 
# 
# source('VegConvert_UTM.R')
# pal.vegcov<-paleo.veg
# new.pal<-pal.dat
# 
# for(i in 1:46){
#   for(v in (unique(pal.vegcov)[!is.na(unique(pal.vegcov))])){
# 
#     pal.l<-pal.dat[which(pal.vegcov==v),i]
#     pal.fuzz<-pal.l+rnorm(length(pal.l), 0, 2*adjstore[v,i])
# 
#     new.pal[which(pal.vegcov==v),i]<-pal.fuzz
#   }
#   }
# 

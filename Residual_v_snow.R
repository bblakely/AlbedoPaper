LC.raw<-read.csv('Landcover.csv', skip=7)
Resids.raw<-read.csv('Model_Data_Resids.csv', skip=7)
SNO.raw<-read.csv('Snow.csv', skip=7)
GHCN.raw<-read.csv('GHCN_snow.csv', skip=7)

Diagplot<-FALSE

#QC

Resids<-(Resids.raw[2:24535, 5:52])

rs.dat<-Resids[,3:48]
rs.dat[rs.dat < -10]<-NA


Snow<-(SNO.raw[2:24535, 5:52])
sn.dat<-Snow[,3:48]
sn.dat[sn.dat>1]<-NA

LC<-(LC.raw[2:24535,5:7])
lc.dat<-LC[,3]
lc.dat[lc.dat==0]<-NA

GHCN<-(GHCN.raw[2:24535,5:18])
gh.dat<-GHCN[3:14]


#Stretch GHCN to 46 dates
gh.interp<-matrix(nrow=nrow(gh.dat), ncol=46)

for(i in 1:nrow(gh.interp)){
  interp.row<-approx(as.numeric(gh.dat[i,]),n=46)$y
  gh.interp[i,]<-interp.row
}

gh.interp<-data.frame(gh.interp)
colnames(gh.interp)<-c(1:46)
gh.interp[,16:17]<-0
gh.interp[,38:39]<-0

#Overall
if(Diagplot==TRUE){
for (i in c(1:12,40:46)){
  #plot(rs.dat[,i]~sn.dat[,i], main=paste('Crop',i), 
  #ylim=c(-4,4), xlim=c(0,1), ylab='residuals', xlab='snow probability') 
  #abline(h=0, col='black')
  smoothScatter(x=sn.dat[,i],y=rs.dat[,i],nbin=100,
                nrpoints=Inf, main=paste("ALL", i), cex=1.2,colramp=colorRampPalette(c("white","yellow", "orange","red", "blue"))
                ,xlim=c(0,1), ylim=c(-5,5), ylab="LST residuals", xlab="Snow Probability")
  
  line<-lm(rs.dat[,i]~sn.dat[,i])
  params<-unname(line[[1]])
  abline(params[1],params[2], lwd=2)
  #print(summary(line)$r.squared)
  #print(summary(line))
  print(params[2])
}
}
# #####
# ####By lancover####
# #plot crop redids against snow
# for (i in c(1:12,40:46)){
#   #plot(rs.dat[lc.dat==12,i]~sn.dat[lc.dat==12,i], main=paste('Crop',i), 
#        #ylim=c(-4,4), xlim=c(0,1), ylab='residuals', xlab='snow probability') 
#   #abline(h=0, col='black')
#   smoothScatter(x=sn.dat[lc.dat==12,i],y=rs.dat[lc.dat==12,i],nbin=100,
#                 nrpoints=Inf, main=paste("CROP",i), cex=1.2,colramp=colorRampPalette(c("white","yellow", "orange","red", "blue"))
#                 ,xlim=c(0,1), ylim=c(-5,5))
#   line<-lm(rs.dat[lc.dat==12,i]~sn.dat[lc.dat==12,i])
#   params<-unname(line[[1]])
#   abline(params[1],params[2], lwd=2)
#   #print(summary(line)$r.squared)
#   #print(summary(line))
#   print(params[2])
# }
# 
# #plot forest resids against snow
# for (i in c(1:12,40:46)){
#   #plot(rs.dat[lc.dat==5,i]~sn.dat[lc.dat==5,i], main=paste('Mixed',i), 
#        #ylim=c(-5,5), xlim=c(0,1)) 
#   #abline(h=0, col='red')
#   smoothScatter(x=sn.dat[lc.dat==5,i],y=rs.dat[lc.dat==5,i],nbin=100,
#                 nrpoints=Inf, main=paste('MIXED',i), cex=1.2,
#                 colramp=colorRampPalette(c("white","yellow", "orange","red", "blue")),
#                 xlab="Snow Prob.", ylab="LST residuals",xlim=c(0,1), ylim=c(-5,5))
#   line<-lm(rs.dat[lc.dat==5,i]~sn.dat[lc.dat==5,i])
#   params<-unname(line[[1]])
#   abline(params[1],params[2], lwd=2)
#   #print(summary(line)$r.squared)
#   #print(summary(line
#   print(params[2])
# }
# 
# #plot mosaic resids against snow
# for (i in c(1:12,40:46)){
#   #plot(rs.dat[lc.dat==14,i]~sn.dat[lc.dat==14,i], main=paste('Mixed',i), 
#   #ylim=c(-5,5), xlim=c(0,1)) 
#   #abline(h=0, col='red')
#   smoothScatter(x=sn.dat[lc.dat==14,i],y=rs.dat[lc.dat==14,i],nbin=100,
#                 nrpoints=Inf, main=paste('MOSAIC',i), cex=1.2,
#                 colramp=colorRampPalette(c("white","yellow", "orange","red", "blue")),
#                 xlab="Snow Prob.", ylab="LST residuals",xlim=c(0,1), ylim=c(-5,5))
#   line<-lm(rs.dat[lc.dat==14,i]~sn.dat[lc.dat==14,i])
#   params<-unname(line[[1]])
#   abline(params[1],params[2], lwd=2)
#   print(summary(line)$r.squared)
#   #print(summary(line
# }
# 
# #plot DECIDUOUS resids against snow
# for (i in c(1:12,40:46)){
#   #plot(rs.dat[lc.dat==4,i]~sn.dat[lc.dat==4,i], main=paste('Mixed',i), 
#   #ylim=c(-5,5), xlim=c(0,1)) 
#   #abline(h=0, col='red')
#   smoothScatter(x=sn.dat[lc.dat==4,i],y=rs.dat[lc.dat==4,i],nbin=100,
#                 nrpoints=Inf, main=paste('DECID',i), cex=1.2,
#                 colramp=colorRampPalette(c("white","yellow", "orange","red", "blue")),
#                 xlab="Snow Prob.", ylab="LST residuals",xlim=c(0,1), ylim=c(-5,5))
#   line<-lm(rs.dat[lc.dat==4,i]~sn.dat[lc.dat==4,i])
#   params<-unname(line[[1]])
#   abline(params[1],params[2], lwd=2)
#   #print(summary(line)$r.squared)
#   unname(line[[1]])[2]
#   #print(summary(line)
# }
# 
# 
# #plot evergreen resids against snow
# for (i in c(1:12,40:46)){
#   #plot(rs.dat[lc.dat==1,i]~sn.dat[lc.dat==1,i], main=paste('Mixed',i), 
#   #ylim=c(-5,5), xlim=c(0,1)) 
#   #abline(h=0, col='red')
#   smoothScatter(x=sn.dat[lc.dat==1,i],y=rs.dat[lc.dat==1,i],nbin=100,
#                 nrpoints=Inf, main=paste('EVERGREEN',i), cex=1.2,
#                 colramp=colorRampPalette(c("white","yellow", "orange","red", "blue")),
#                 xlab="Snow Prob.", ylab="LST residuals",xlim=c(0,1), ylim=c(-5,5))
#   line<-lm(rs.dat[lc.dat==1,i]~sn.dat[lc.dat==1,i])
#   params<-unname(line[[1]])
#   abline(params[1],params[2], lwd=2)
#   #print(summary(line)$r.squared)
#   unname(line[[1]])[2]
#   #print(summary(line)
# }
# #####
# 
# ####
####Other analyses####
####
#Plot resids against latitude (is latitude causing a bias?)
if(Diagplot==TRUE){
for (i in c(1:12,40:46)){
  plot(rs.dat[,i]~Resids[,1], main=i, pch='.', xlim=c(40,50))
  #abline(h=0, col='red')
  line<-lm(rs.dat[,i]~Resids[,1])
  params<-unname(line[[1]])
  abline(params[1],params[2], lwd=2)
  print(params[2])
}

#...and longitude
for (i in c(1:12,40:46)){
  plot(rs.dat[,i]~Resids[,2], main=i, pch='.')
  #abline(h=0, col='red')
  line<-lm(rs.dat[,i]~Resids[,2])
  params<-unname(line[[1]])
  abline(params[1],params[2], lwd=2)
  print(params[2])
}
}
####
####Scaling MODIS with GHCN####
####
#LST
rs.jan<-rowMeans(rs.dat[,1:4])
rs.feb<-rowMeans(rs.dat[5:8])
rs.mar<-rowMeans(rs.dat[9:12])
rs.apr<-rowMeans(rs.dat[13:15])
rs.may<-rowMeans(rs.dat[16:19])
rs.nov<-rowMeans(rs.dat[39:42])
rs.dec<-rowMeans(rs.dat[43:46])

rs.month<-data.frame(cbind(rs.jan,rs.feb,rs.mar,rs.apr,rs.may,rs.nov,rs.dec))

#SP
sp.jan<-rowMeans(sn.dat[,1:4])
sp.feb<-rowMeans(sn.dat[5:8])
sp.mar<-rowMeans(sn.dat[9:12])
sp.apr<-rowMeans(sn.dat[13:15])
sp.may<-rowMeans(sn.dat[16:19])
sp.nov<-rowMeans(sn.dat[39:42])
sp.dec<-rowMeans(sn.dat[43:46])

sp.month<-data.frame(cbind(sp.jan,sp.feb,sp.mar,sp.apr,sp.may,sp.nov,sp.dec))


swe.month<-GHCN[,c(3:7,13:14)]
colnames(swe.month)<-c('swe.jan','swe.feb','swe.mar','swe.nov','swe.dec')

monthlab<-c('Jan','Feb','Mar','Apr','May','Nov','Dec')

####
####Plot GHCN against resids####
####
# for (i in c(1:12,40:46)){
#   
#   plot(rs.dat[,i]~gh.interp[,i], main=i, ylim=c(-5,5), xlab="GHCN SWE", ylab="LST residuals", pch='.')
#   line<-lm(rs.dat[,i]~gh.interp[,i])
#   params<-unname(line[[1]])
#   abline(params[1],params[2], lwd=2, col='red')
#   print(params[2])
# }

#...by months, all LC

slopes<-c(1:7)
ints<-c(1:7)
for (i in c(1:7)){
  
  line<-lm(rs.month[,i]~swe.month[,i])
  params<-unname(line[[1]])
  
  #print(params[2])
  slopes[i]<-params[2]
  ints[i]<-params[1]
  
  if(Diagplot==TRUE){
  print(summary(line)$r.squared)
  #legend(150,-1, legend=paste('r2:', line$r.squared))
  
  # smoothScatter(rs.month[,i]~swe.month[,i],ylim=c(-5,5), 
  #               xlab="GHCN SWE", ylab="LST residuals", xlim=c(0,300))
  # abline(params[1],params[2], lwd=2, col='black')
  plot(rs.month[,i]~swe.month[,i], main=monthlab[i], ylim=c(-5,5), 
       xlab="SWE", ylab="LST Residual", pch='.', xlim=c(0,300), 
       font=2,font.lab=2)
  abline(params[1],params[2], lwd=2, col='black')
  box(lwd=2)
  }

}

 


# ####GHCN and prob####
# for (i in c(40:46, 1:12)){
#   
#   #plot(sn.dat[,i]~gh.interp[,i], main=i, xlim=c(0,100), pch='.')
#   
#   snowlevs<-sort(unique(gh.interp[,i]))
#   snowprobs<-c(1:length(snowlevs))
#   for (u in 1:length(snowlevs)){
#     snowprobs[u]<-mean((sn.dat[gh.interp[,i]==snowlevs[u]& lc.dat==12,i]), na.rm=TRUE)
#     if (u%%100 == 0){
#       print(u)}
#     
#   }
#   
#   plot(snowprobs~snowlevs, main=i, xlim=c(0,100), ylim=c(0,1), xlab="GHCN",ylab="MODIS")
# }
# 
# 
# plot(snowprobs~snowlevs, xlim=c(0,300), ylim=c(0,1), col='white',xlab="GHCN",ylab="MODIS")
# colvec<-c("red",'blue','forest green','orange','black')
# 
# for(i in c(4:5,1:3)){
#   #plot(sp.month[,i]~swe.month[,i], main=monthlab[i], pch='.')
#   
#   snowlevs<-sort(unique(swe.month[,i]))
#   snowprobs<-c(1:length(snowlevs))
#   for (u in 1:length(snowlevs)){
#     snowprobs[u]<-mean((sp.month[swe.month[,i]==snowlevs[u],i]), na.rm=TRUE)
#     if (u%%100 == 0){
#       print(u)}
#     
#   }
#   
#   #points(snowprobs~snowlevs, xlim=c(0,300), ylim=c(0,1), xlab="GHCN",ylab="MODIS", main=monthlab[i], pch=16, col='red')
#     points(snowprobs~snowlevs, col=colvec[i])
#     legend(150,0.7,legend=monthlab,col=colvec, pch=1)
# }
# 
# 
# 

source('Residual_v_snow.R')

rm(list=setdiff(ls(), c("gh.interp","Snowlevs", "sn.dat","lc.dat","swe.month","sp.month","monthlab")))


####GHCN and prob####
for (i in c(40:46, 1:12)){
  
  #plot(sn.dat[,i]~gh.interp[,i], main=i, xlim=c(0,100), pch='.')
  
  snowlevs<-sort(unique(gh.interp[,i]))
  snowprobs<-c(1:length(snowlevs))
  for (u in 1:length(snowlevs)){
    snowprobs[u]<-mean((sn.dat[gh.interp[,i]==snowlevs[u]& lc.dat==12,i]), na.rm=TRUE)
    if (u%%100 == 0){
      print(u)}
    
  }
  
  plot(snowprobs~snowlevs, main=i, xlim=c(0,100), ylim=c(0,1), xlab="GHCN",ylab="MODIS")
}


plot(snowprobs~snowlevs, xlim=c(0,300), ylim=c(0,1), col='white',xlab="GHCN",ylab="MODIS")
colvec<-c("red",'blue','forest green','orange','black')

for(i in c(4:5,1:3)){
  #plot(sp.month[,i]~swe.month[,i], main=monthlab[i], pch='.')
  
  snowlevs<-sort(unique(swe.month[,i]))
  snowprobs<-c(1:length(snowlevs))
  for (u in 1:length(snowlevs)){
    snowprobs[u]<-mean((sp.month[swe.month[,i]==snowlevs[u],i]), na.rm=TRUE)
    if (u%%100 == 0){
      print(u)}
    
  }
  
  #points(snowprobs~snowlevs, xlim=c(0,300), ylim=c(0,1), xlab="GHCN",ylab="MODIS", main=monthlab[i], pch=16, col='red')
  points(snowprobs~snowlevs, col=colvec[i])
  legend(150,0.7,legend=monthlab,col=colvec, pch=1)
}



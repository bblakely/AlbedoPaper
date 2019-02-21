library('raster')
library('ncdf4')
library('rgdal')
library('sp')
library('maptools')
library('maps')

pullkern<-function(filename, varname, strict=TRUE, plot=FALSE){
  
  cdf.file.cld<-filename
  cdf.obj<-nc_open(cdf.file.cld)
  alb.cld.br<-brick(cdf.file.cld,varname=varname)
  
  if (plot=='TRUE'){
    #Plot worldwide kernels
    for(j in 1:12){
      plot(alb.cld.br[[j]], useRaster=FALSE, main=j)
      map('world', wrap=c(0,360),add=TRUE)
    }
  }
  
  #Clip to UMW extent
  extent<-extent(250,280,40,50)
  alb.cld<-crop(alb.cld.br, extent)
  
  #Choose states to plot
  states.lib<-c('indiana', 'illinois', 'michigan', 'wisconsin','minnesota')
  states.strict<-c('michigan','wisconsin','minnesota')
  
  if(strict==TRUE){states<-states.strict}else{states<-states.lib}
  
  if(plot==TRUE){
    #Plot all for region
    for(i in 1:12){
      plot(alb.cld[[i]], useRaster=FALSE, main=i)
      map('state', region=states, wrap=c(0,360), add=TRUE)
    }
  }
  
  
  ##Preferred method. Extracts the values of cells overlapped by umw states
  #Make an object that is the states of interest
  states.ch<-map('state', region=states, wrap=c(0,360), add=TRUE, fill=TRUE)
  
  #Internet solution for converting map things to polygons; don't fully understand
  ID <- sapply(strsplit(states.ch$names, ":"), function(x) x[1])
  statedat<- map2SpatialPolygons(states.ch, IDs=ID, proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  #Yields values for each cell covered by each state. Weights are (normalized) portions of cells covered
  state_cells<-extract(alb.cld.br, statedat, method='bilinear', weights=TRUE)
  weighted.states<-matrix(dat=NA,nrow=3,ncol=12)
  #Weighted mean
  for(s in 1:3){
    weighted.states[s,]<-colSums(sweep(state_cells[[s]][,1:12],1,state_cells[[s]][,13],FUN="*"))
  }
  
  #Get the names
  sapply(slot(statedat, "polygons"), function(x) slot(x, "ID")) #also internet solution
  
  #Area weighting to go from state -> region (inefficient, but can't find a better way)
  atot<-sum(area(statedat[1]),area(statedat[2]),area(statedat[3]))
  weights<-c(0,0,0)
  for (w in 1:3){
    weights[w]<-area(statedat[w])/atot
  }
  
  kern<-colSums(sweep(weighted.states,1,weights,FUN='*'))
  
  return(kern)
}

albkern<-pullkern("ECHAM6_CTRL_kernel.nc", 'A_srad0' )#Mauritsen kernels
  #pullkern('CAM3_albedo_sw_kernel.nc', 'monkernel' )#Soden kernels
stkern<-pullkern("ECHAM6_CTRL_kernel.nc", 'Ts_trad0' )#Mauritsen kernels
  #pullkern('CAM3_surft_lw_kernel.nc', 'FLNTA_FLNT' )#Soden kernels

#etkern<-pullkern('CAM3_wv_lw_kernel.nc', 'kernel_p' )

#File options
#'CAM3_surft_lw_kernel.nc'
#'CAM3_wv_lw_kernel.nc'
#'CAM3_albedo_sw_kernel.nc'

#Varname options
#"kernel_p"
#varname="FLNTA_FLNT")
#varname="monkernel")



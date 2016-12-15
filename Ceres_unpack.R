#install.packages('ncdf')
#install.packages('raster')
#install.packages('ncdf4')
#install.packages('rgdal')

#library('ncdf')
library('raster')
library('ncdf4')
library('rgdal')

cdf.file<-'CERES_SYN1deg-Month_Terra-Aqua-MODIS_Ed3A_Subset_200201-201112.nc'
  #'CERES_SYN1deg-Month_Terra-Aqua-MODIS_Ed3A_Subset_200003-201112.nc'

nc<-nc_open(cdf.file)
varnames<-names(nc$var)
varnames
#print(Test)

Sfc<-ncvar_get(nc,"sfc_comp_sw-down_all_mon")
Insol<-ncvar_get(nc,"toa_solar_all_mon")



#This actually works... R is magic.
InsolBR<-brick(cdf.file, varname="toa_solar_all_mon")
#writeRaster(InsolBR, filename="Ceres_insol.tif", options="INTERLEAVE=BAND", overwrite=TRUE)

SWsfcBR<-brick(cdf.file, varname="sfc_comp_sw-down_all_mon")
#writeRaster(SWsfcBR, filename="Ceres_SWsfc.tif", options="INTERLEAVE=BAND", overwrite=TRUE)





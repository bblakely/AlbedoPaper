#Breaking down vegetation conversions

#####Preprocessing#####
modern.raw<-read.csv("Modern_vegetation_UTM.csv", skip=7)
paleo.raw<-read.csv("Paleo_vegetation_UTM_v2.csv", skip=7)

#clip off extra envi line
modern.dat<-modern.raw[2:nrow(modern.raw),] 
paleo.dat<-paleo.raw[2:nrow(paleo.raw),]

#Fill value places in paleo data where there is no modern vegetation; column 7 is the veg code
modern.veg<-modern.dat
modern.veg[paleo.dat[,7]==0,7]<-9999 

#Reassign 0s in paleo dat to fill
paleo.veg<-paleo.dat
paleo.veg[paleo.dat[,7]==0,7]<-9999

#Index of places where there is modern data (0) or modern veg does not fit 6 main categories 
# MODIS codes (EG[1],DC[4],MX[5],CR[12],MO[14], UR[13])
no.mod<-which(modern.veg[,7]==0 | modern.veg[,7]==15| modern.veg[,7]==3 | modern.veg[,7]==11
              |modern.veg[,7]==6 | modern.veg[,7]==8 | modern.veg[,7]==9 |modern.veg[,7]==10)

#Fill value places with no data/wrong categories
modern.veg[no.mod,7]<-9999
paleo.veg[no.mod,7]<-9999

#Change urban to 26 to prevent repeats 
#(i.e. decid (4) to urb (13) = 9, but mixed(5) to mosaic (14) =9 also)
modern.veg[modern.veg==13]<-26

#####Conversion code and list of indices

#Pull just veg codes (separate from georef)
modern.veg<-modern.veg[,7]
paleo.veg<-paleo.veg[,7]

#NAN fill values 
modern.veg[modern.veg==9999]<-NA
paleo.veg[paleo.veg==9999]<-NA

#The important bit. Subtract veg codes from each other
convert.code<-modern.veg-paleo.veg

#visual sanity check -  none should be > |20|
histplot<-FALSE
if(histplot==TRUE){hist(convert.code,breaks=25)}

#List of unique codes (i.e. unique pairwise conversions)
poss<-sort(unique(convert.code))

#make list of indices for each possible conversion
#Yields a 26-member list. Each member is one conversion
#e.g.  historic mosaic (14) to modern evergreen (1) = 1 - 14 = -13. On round i = 1, pulls indices of all palces where convert code is -13 (i.e. where mo -> eg occurred)
list.ind<-list()
for(i in 1:length(poss)){
  list.ind[[i]]<- which(convert.code==poss[i])
}


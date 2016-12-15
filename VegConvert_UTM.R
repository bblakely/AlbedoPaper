#Breaking down vegetation conversions

#####Preprocessing#####
modern.raw<-read.csv("Modern_vegetation_UTM.csv", skip=7)
paleo.raw<-read.csv("Paleo_vegetation_UTM.csv", skip=7)
modern.dat<-modern.raw[2:nrow(modern.raw),]
paleo.dat<-paleo.raw[2:nrow(paleo.raw),]
modern.veg<-modern.dat
modern.veg[paleo.dat[,7]==0,7]<-9999
paleo.veg<-paleo.dat
paleo.veg[paleo.dat[,7]==0,7]<-9999
no.mod<-which(modern.veg[,7]==0 | modern.veg[,7]==15| modern.veg[,7]==3 | modern.veg[,7]==11
              |modern.veg[,7]==6 | modern.veg[,7]==8 | modern.veg[,7]==9 |modern.veg[,7]==10)

modern.veg[no.mod,7]<-9999
paleo.veg[no.mod,7]<-9999

#Change urban to 26 to prevent repeats 
#(i.e. decid (4) to urb (13) = 9, but mixed(5) to mosaic (14) =9 also)
modern.veg[modern.veg==13]<-26

#####Conversion code and list of indices

modern.veg<-modern.veg[,7]
paleo.veg<-paleo.veg[,7]
modern.veg[modern.veg==9999]<-NA
paleo.veg[paleo.veg==9999]<-NA

convert.code<-modern.veg-paleo.veg

convert.code[convert.code>9000]<-NA

hist(convert.code,breaks=25)

poss<-sort(unique(convert.code))

#make list of indices for each possible conversion
list.ind<-list()
for(i in 1:length(poss)){
  list.ind[[i]]<- which(convert.code==poss[i])
  
}

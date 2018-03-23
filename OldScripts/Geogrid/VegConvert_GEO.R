#Breaking down vegetation conversions

#####Preprocessing#####
modern.raw<-read.csv("LC_GEO.csv", skip=7)
paleo.raw<-read.csv("Paleo_GEO.csv", skip=7)
modern.dat<-modern.raw[2:nrow(modern.raw),]
paleo.dat<-paleo.raw[2:nrow(paleo.raw),]
modern.veg<-modern.dat
modern.veg[paleo.dat[,5]==15,5]<-9999
paleo.veg<-paleo.dat
paleo.veg[paleo.dat[,5]==15,5]<-9999
no.mod<-which(modern.veg[,5]==0 | modern.veg[,5]==15| modern.veg[,5]==3 | modern.veg[,5]==11
              |modern.veg[,5]==6 | modern.veg[,5]==8 | modern.veg[,5]==9 |modern.veg[,5]==10)

modern.veg[no.mod,5]<-9999
paleo.veg[no.mod,5]<-9999

#Change urban to 26 to prevent repeats 
#(i.e. decid (4) to urb (13) = 9, but mixed(5) to mosaic (14) =9 also)
modern.veg[modern.veg==13]<-26

#####Conversion code and list of indices

modern.veg<-modern.veg[,5]
paleo.veg<-paleo.veg[,5]
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

Trans<-read.csv('Transmittance.csv', skip=7) #10-year series of monthly transmittances (120 band total) for each pixel. ENVI generated file
Trans<-Trans[2:nrow(Trans),] #Clip off extra ENVI line
Trans_band<-Trans[,5:124] #Clip off georef

monthnames<-c('Jan', 'Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

index.mtr<-matrix(nrow=10,ncol=12) # 10 years, 12 months. Framework to reorganize 120-date series into years/months
for(i in 1:12){
  index.mtr[,i]=seq(from=i, to=120, by=12)
}
colnames(index.mtr)<-monthnames

Months<-data.frame(matrix(nrow=238,ncol=12)) #238 pixels, 12 months
for(j in 1:12){
  Months[,j]<-rowMeans(Trans_band[,index.mtr[,j]]) #averages together all Januaries (month 1, 13, 25 . . . ), all Februaries, etc. for each pixel. 
}

names(Months)<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

 
#Bring in Insol
Insol<-read.csv('Insolation.csv', skip=7)
Insol<-Insol[2:nrow(Insol),]

Insol_band<-Insol[,5:124]


Months.insol<-data.frame(matrix(nrow=238,ncol=12))
for(j in 1:12){
  Months.insol[,j]<-rowMeans(Insol_band[,index.mtr[,j]])
  
}

Insol.avg<-colMeans(Months.insol)




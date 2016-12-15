Trans<-read.csv('Transmittance.csv', skip=7)
Trans<-Trans[2:nrow(Trans),]

Trans_band<-Trans[,5:124]

index.mtr<-matrix(nrow=10,ncol=12)
for(i in 1:12){
  index.mtr[,i]=seq(from=i, to=120, by=12)
}

Months<-data.frame(matrix(nrow=238,ncol=12))
for(j in 1:12){
  Months[,j]<-rowMeans(Trans_band[,index.mtr[,j]])
  
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




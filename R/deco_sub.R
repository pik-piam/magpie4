
deco_sub<-function(Deco,x,Data_x1,Data_x2,names_factor_sub){
  
  Calc_Data<-Data_x1
  Calc_Data[,,]<-NA
  dimnames(Calc_Data)[[3]]<-names_factor_sub
  if(dim(Data_x2)[[3]]==1){
    for(k in 1:dim(Data_x1)[[3]]){
      Calc_Data[,,k]<-Data_x1[,,k]/Data_x2[,,1] 
    }
  }else{
    for(k in 1:dim(Data_x1)[[3]]){
      Calc_Data[,,k]<-Data_x1[,,k]/Data_x2[,,k]
    }
  }
  
  Deltas<-Calc_Data[,1:(dim(Calc_Data)[[2]]-1),]
  Deltas[,,]=0
  
  Deltas_Data_x1<-Data_x1[,1:(dim(Data_x1)[[2]]-1),]
  Deltas_Data_x1[,,]=0
  
  Deltas_Data_x2<-Data_x2[,1:(dim(Data_x2)[[2]]-1),,drop=FALSE]
  Deltas_Data_x2[,,]=0
  
  Deco_ad<-Data_x1[,1:(dim(Data_x1)[[2]]-1),]
  dimnames(Deco_ad)[[3]]<-names_factor_sub
  
  Deco_tot<-Deco_ad
  
  for(i in 1:(dim(Calc_Data)[2]-1)){
      Deltas[,i,]<-Calc_Data[,i+1,]-Calc_Data[,i,]
  }
  
  for(i in 1:(dim(Data_x1)[[2]]-1)){
          Deltas_Data_x1[,i,]<-Data_x1[,i+1,]-Data_x1[,i,]
  }
  for(i in 1:(dim(Data_x2)[[2]]-1)){
    Deltas_Data_x2[,i,]<-Data_x2[,i+1,]-Data_x2[,i,]
  }
  
  R<-as.magpie(Deco[,,1])
  R[,,]<-NA
  R[,,1]<-Deco[,,x]/as.magpie(Deltas[,,1])  
  
  Deco_ad[,,1]<-Deco[,,x]
  Delta_x<-array(0,c(dim(Deco)[[1]]))
  
  for(k in 2:dim(Data_x1)[[3]]){
    for(i in 1:dim(Deco_ad)[[2]]){
      if(dim(Data_x2)[[3]]==1){
        Delta_x<-(Deltas_Data_x1[,i,k]-(Calc_Data[,i,k]*Deltas_Data_x2[,i,1]))/
          Data_x2[,i+1,1]
      }else if(dim(Data_x2)[[3]]==dim(Data_x1)[[3]]){
        Delta_x<-(Calc_Data[,i+1,k]*Data_x2[,i+1,k]-Calc_Data[,i,k]*Data_x2[,i,k]-
          Deltas_Data_x2[,i,k]*Calc_Data[,i,1])/Data_x2[,i+1,1]
      }else{
        stop("The analysis is not possible because the Data has wrong dimensions")
      }
      Deco_tot[,i,k]<-R[,i,1]*Delta_x
      Deco_ad[,i,k]<-R[,i,1]*Delta_x
    }
  }
  
  # Test
  
  for(i in 2:dim(Deco_ad)[[2]]){
    for(j in 1:dim(Deco_ad)[[1]]){
      x=Deco_ad[j,i,1]-sum(Deco_ad[j,i,2:dim(Deco_ad)[[3]]])
      if(x>=1e-5){
        stop("calculation went wrong")
        break}
    }
    break}

  return(as.magpie(Deco_ad))
}

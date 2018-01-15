#' @importFrom magclass setYears
#' @importFrom grDevices gray.colors
# Decomposition-Calculation

deco<-function(data,names_factor=NULL,plots=NULL,path="graphs",color=gray.colors(6)){
  if(is.null(names_factor)){
    names_factor[1]<-dimnames(data)[[3]][1]
    names_factor[2]<-dimnames(data)[[3]][2]
    for(i in 3:(dim(data)[3])){
      names_factor[i]=paste(dimnames(data)[[3]][i],dimnames(data)[[3]][i-1],sep="/")     
    }
    names_factor[dim(data)[3]+1]=paste(dimnames(data)[[3]][1],dimnames(data)[[3]][dim(data)[3]],sep="/")
  }

  Calc_Data<-data[,,c(1:dim(data)[3],1),drop=FALSE]
  Calc_Data[,,]=NA
  dimnames(Calc_Data)[[3]]<-names_factor
  

  Calc_Data[,,1]=data[,,1]  # Decompositions Wert in erste Tabelle einfuegen
  Calc_Data[,,2]=data[,,2]
  if(dim(data)[[3]]>2){
    for(k in 3:(dim(Calc_Data)[3]-1)){
      Calc_Data[,,k]=setNames(data[,,k],NULL)/setNames(data[,,(k-1)],NULL)
    }
  }
  Calc_Data[,,dim(Calc_Data)[3]]=setNames(data[,,1],NULL)/setNames(data[,,dim(data)[3]],NULL)                                


  Deltas<-Calc_Data 
  Deltas[,,]=0
  
  Deco<-Deltas[,2:(dim(Calc_Data)[2]),c(1:(dim(Calc_Data)[3])),drop=FALSE]
  Deco[,,]<-NA
  
  Deco_tot<-Deco
  
  index1=dim(Calc_Data)[1]
  index2=dim(Calc_Data)[2]
  index3=dim(Calc_Data)[3]
  
  # Werte in Matrizen eintragen
  
  for(i in 1:(index2-1)){
    Deltas[,i,]<-setYears(Calc_Data[,i+1,],getYears(Deltas)[i])-Calc_Data[,i,]
  }
  
  # Decomposition
  
  for(i in 1:(index2-1)){
    for(j in 1:index1){
      Deco_tot[j,i,1]=setYears(Deltas[j,i,1],getYears(Deco_tot)[i])
      Deco[j,i,1]=setYears(Deltas[j,i,1]*100/Calc_Data[j,i,1],getYears(Deco_tot)[i])
      for(k in 2:index3){
        A=prod(Calc_Data[j,i,2:index3])*Deltas[j,i,k]/Calc_Data[j,i,k]
        B=0
        C=0
        Y=0
        Z=prod(Deltas[j,i,2:index3])
        for(l in 2:index3){
          if(l!=k){
            if(index3>3){
              B=setNames(B,NULL)+setNames(A,NULL)*setNames(Deltas[j,i,l]/Calc_Data[j,i,l],NULL)
            }
            if(index3>4){
              if(Deltas[j,i,l]==0){
                Deltas[j,i,l]<-1
                Y=Y+prod(Deltas[j,i,2:index3])*Calc_Data[j,i,l]/Deltas[j,i,l]
                Deltas[j,i,l]<-0
              }else{
                Y=Y+prod(Deltas[j,i,2:index3])*Calc_Data[j,i,l]/Deltas[j,i,l]
              }
            }
            for(m in 2:index3){
              if(m!=k && m<l && index3>5){
                if(Deltas[j,i,k]==0){
                  Deltas[j,i,l]<-1
                  C=C+prod(Deltas[j,i,2:index3])*Calc_Data[j,i,l]*Calc_Data[j,i,m]/(Deltas[j,i,l]*Deltas[j,i,m])
                  Deltas[j,i,k]<-0
                }else{
                  C=C+prod(Deltas[j,i,2:index3])*Calc_Data[j,i,l]*Calc_Data[j,i,m]/(Deltas[j,i,l]*Deltas[j,i,m])
                }
              }
            }
          }
        }
        Deco_tot[j,i,k]=setYears(setNames(A,NULL)+setNames(B,NULL)/2+setNames(C,NULL)/3+setNames(Y,NULL)/(index3-2)+setNames(Z,NULL)/(index3-1),getYears(Deco_tot)[i])
        Deco[j,i,k]=Deco_tot[j,i,k]*100/setNames(setYears(Calc_Data[j,i,1],getYears(Deco)[i]),NULL)
      }}}
  
  
  # Test
  
  for(i in 1:(index2-1)){
    for(j in 1:index1){
      x=Deco[j,i,1]-sum(Deco[j,i,2:index3])
      if(x>=1e-2){
        stop("Calculation went wrong")
        break}
    }
    break}
  
  if(is.null(plots)){
    return(as.magpie(Deco))
  }else{
    deco_plot(Deco,plots,path,color)
    return(as.magpie(Deco))
  }
}
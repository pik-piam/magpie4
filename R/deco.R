
#' @title deco
#' @description Function that quantifies the influences of the underlying drivers to a dependent output variable. It attributes the changes of the output variable (A) to changes of several drivers (B, B/C, C/A). The output must be the product of the drivers.
#' @param data Decomposition Data as a magpie object. The first column of the third dimension has to be the output (A), while the subsequent columns are the coefficients of the drivers (B,C,...). Example: Area = Population x Supply/Population x Area/Supply. 3rd-dimension column order then has to be: Area, Population, Supply.
#' @param names_factor Names of the output (A) and the Decomposition-Factors (B,B/C,C/A), if names_factor=NULL the names for the third column will be generated like the factors for decomposition (above example: Area, Population, Supply/Population, Area/Supply)
#' @param plot TRUE or FALSE
#' @details Use function deco_plot in library luplot to make a plot out of this. It is only usable for the decomposition of 5 or less drivers. For documentation, see paper Huber, Veronika, Ina Neher, Benjamin L. Bodirsky, Kathrin Hoefner, and Hans Joachim Schellnhuber. 2014. "Will the World Run out of Land? A Kaya-Type Decomposition to Study Past Trends of Cropland Expansion." Environmental Research Letters 9 (2): 024011. https://doi.org/10.1088/1748-9326/9/2/024011. Or see master Thesis of Ina Neher (2013)
#' @return Decomposes the impact of certain drivers to an output (A) value.
#' @author Ina Neher, Benjamin Leon Bodirsky
#' @importFrom magclass setYears
#' @export
#' @examples
#'  Data<-array(c(1,1.1,1.15,1,1.05,1.1,1,1.05,1.15),c(3,3))
#'  dimnames(Data)<-list(paste("y",2000:2002,sep=""),c("Area","Population","Supply"))
#'  Data <- as.magpie(Data)
#'  deco(Data)
#' 
#' @export

deco<-function(data,names_factor=NULL,plot=FALSE){
  if(is.null(names_factor)){
    names_factor[1]<-dimnames(data)[[3]][1]
    names_factor[2]<-dimnames(data)[[3]][2]
    for(i in 3:(dim(data)[3])){
      names_factor[i]=paste(dimnames(data)[[3]][i],dimnames(data)[[3]][i-1],sep="/")     
    }
    names_factor[dim(data)[3]+1]=paste(dimnames(data)[[3]][1],dimnames(data)[[3]][dim(data)[3]],sep="/")
  }

  # check whether ceomposition is allowed
  tmp<-getYears(data,as.integer = TRUE)
  intervals<-unique(tmp[2:length(tmp)]-tmp[1:(length(tmp)-1)])
  if (length(intervals)>1){warning("Deco only works correctly if timestep length is constant!")}
  
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
      x=abs(Deco[j,i,1]/sum(Deco[j,i,2:index3])-1)
      if(x>=0.01){
        warning(paste0("Outcome and Sum of Decomposition do not exactly add up. Maximum divergence by  ",round(x,2)*100, " percent."))
        break}
    }
    break}
  
  return(as.magpie(Deco))
}






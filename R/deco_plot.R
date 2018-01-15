#' @importFrom grDevices gray.colors pdf dev.off
#' @importFrom magclass is.magpie
#' @importFrom graphics barplot points plot legend par layout
#' @importFrom stats rnorm

# Funktion for  decompositionsplots

deco_plot<-function(Deco,name="",path="graphs",color=gray.colors(6)){
  dir.create(file.path(path), showWarnings = FALSE)
  if(is.magpie(Deco)){
    Deco<-Deco*array(1,c(dim(Deco)))
  }
  Plot_Data<-Deco[,,,drop=FALSE]
  Plot_Data_pos<-Plot_Data
  Plot_Data_pos[Plot_Data<0]<-0
  Plot_Data_pos_sum<-dimSums(as.magpie(Plot_Data_pos[,,2:dim(Plot_Data)[3]]),dim=3.1)
  Plot_Data_neg<-Plot_Data
  Plot_Data_neg[Plot_Data>0]<-0
  Plot_Data_neg_sum<-dimSums(as.magpie(Plot_Data_neg[,,2:dim(Plot_Data)[3]]),dim=3.1)
  
  legend_names<-dimnames(Plot_Data)[[3]]
  legend_names[1]<-dimnames(Deco)[[3]][1]
  color_vec<-legend_names
  color_vec[1]<-"black"
  color_vec[2:(dim(Deco)[[3]]+1)]<-color[1:dim(Deco)[[3]]]
  
  Plot_Data_pos<-aperm(Plot_Data_pos,c(1,3,2))
  Plot_Data_neg<-aperm(Plot_Data_neg,c(1,3,2))
  
  for(land in 1:dim(Plot_Data)[1]){
    pdf(file =  paste(path,"/",dimnames(Plot_Data)[[1]][land],"_",name,".pdf",sep=""),width=7, height=5)
    layout(matrix(c(1,2), nrow = 1), widths = c(0.7, 0.3))
    par(mar = c(5, 4, 4, 1) + 0.1)
    region_graph<-barplot(Plot_Data_pos[land,2:dim(Plot_Data)[3],],
                          ylim=c(min(Plot_Data_neg_sum[land,,1]-3),
                                 max(Plot_Data_pos_sum[land,,1]+3)),
                          main=dimnames(Plot_Data)[[1]][land],
                          ylab=substitute(paste(Delta,nn),list(nn=dimnames(Deco)[[3]][1])),
                          col=color_vec[2:length(color_vec)], space=0.1, las=1)
    region_graph<-barplot(Plot_Data_neg[land,2:dim(Plot_Data)[3],],col=color_vec[2:length(color_vec)], 
                          space=0.1, las=1, add=T)
    points(x=region_graph,Plot_Data[land,,1],pch=16)
    par(mar = c(5, 0, 4, 1) + 0.1)
    plot(1:3, rnorm(3), pch = 1, lty = 1, ylim=c(-2,2), type = "n", axes = FALSE, ann = FALSE)
    legend(1,1,legend=legend_names,
           col=color_vec,
           pch=c(16,rep(15,dim(Deco)[3]))
    )
    dev.off()
  }
}
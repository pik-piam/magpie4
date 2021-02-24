#' @title retrieve_spamfile
#' @description Provides a mapping with regions, cells and grids
#'
#' @param gdx gdx file
#' @param dir directory with spamfiles or clustermap*.rds
#'
#' @return dataframe
#' @author Benjamin Leon Bodirsky, Edna J. Molina Bacca
#' @examples
#' 
#' \dontrun{ spamfile(dir) }
#' @export
#' @importFrom spam triplet
#' @importFrom luscale read.spam

retrieve_spamfile<-function(gdx,dir){
  
  reg_to_cell<-readGDX(gdx=gdx,"cell")
  names(reg_to_cell)<-c("reg","cell")
  reg_to_cell$cell<-gsub(reg_to_cell$cell,pattern = "_",replacement = ".")
  
  spamfile <- Sys.glob(file.path(dir,"*_sum.spam"))
  if(length(spamfile==1)){
    grid_to_cell=triplet(read.spam(spamfile))$indices
    grid_to_cell=grid_to_cell[order(grid_to_cell[,2]),1]
    grid_to_cell<-reg_to_cell[match(x = grid_to_cell, table = as.integer(substring(reg_to_cell[,2],5,7))),]
    grid_to_cell$grid<-paste0(grid_to_cell[,1],".",1:dim(grid_to_cell)[1])
  } else {
    mapfile <- Sys.glob(file.path(dir,"clustermap*.rds"))
    if(length(mapfile==1)) {
      grid_to_cell <- readRDS(mapfile)[c("cell","cluster")]
      names(grid_to_cell) <- c("grid","cell")
    } else {
      grid_to_cell=NULL
    }
  }
  return(grid_to_cell)
}
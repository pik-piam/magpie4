#' @title gdxAggregate
#' @description aggregates and disaggregates on spatial scales using mappings from the gdx files. Very specific to MAgPIE.
#'
#' @param gdx gdx file
#' @param x object to be aggrgeagted or disaggregated
#' @param weight weight can be either an object or a functionname in "", where the function provides the weight
#' @param to options: grid, cell, iso, reg, glo, regglo
#' @param absolute is it a absolute or a relative value (absolute: tons, relative: tons per hectare)
#' @param dir for gridded outputs: magpie output directory which containts the spamfiles or alternatively clusterspam*.rds
#' files for disaggregation. 
#' @param spamfiledirectory outdated name for map directory. Please use \code{dir} instead.
#' @param ... further parameters handed on to weight function.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Edna J. Molina Bacca
#' @examples
#' 
#' \dontrun{ 
#' gdp_pc <- income(gdx,level="reg")
#' is.function(population)
#' gdp_pc_iso <- gdxAggregate(gdx=gdx,x=gdp_pc,weight="population", to="iso", absolute=FALSE)
#' gdp_pc_glo <- gdxAggregate(gdx=gdx,x=gdp_pc,weight="population", to="glo", absolute=FALSE)
#' gdp <- income(gdx,level="reg",per_capita=FALSE)
#' gdp_iso <- gdxAggregate(gdx=gdx,x=gdp,weight="population", to="iso", absolute=TRUE)
#' gdp_glo <- gdxAggregate(gdx=gdx,x=gdp,weight="population", to="glo", absolute=TRUE)
#' 
#' }
#' @export
#' @importFrom magclass getSets
#' @importFrom madrat toolAggregate
#' @importFrom magpiesets Cell2Country

gdxAggregate<-function(gdx, x, weight=NULL, to, absolute=TRUE, dir=".", spamfiledirectory="", ...){
  
  dir <- getDirectory(dir,spamfiledirectory)

  if(is.function(weight)){warning("You provide a function as weight. It is better to use the functionname in '' to avoid overlapping naming in the R environment")}
  if(length(weight==1)){
    if (is.character(weight)){
      weight<-get(weight,mode = "function")
    }
  }
  if(to=="GLO"){to<-"glo"}
  if(to=="REGGLO"){to<-"regglo"}


  reg_to_iso<-readGDX(gdx=gdx,"i_to_iso")
  names(reg_to_iso)<-c("reg","iso")
  reg_to_cell<-readGDX(gdx=gdx,"cell")
  names(reg_to_cell)<-c("reg","cell")
  reg_to_cell$cell<-gsub(reg_to_cell$cell,pattern = "_",replacement = ".")
  
  #0.5 grid mapping
  grid_to_cell = retrieve_spamfile(gdx=gdx,dir=dir)
  
  
  if(all(dimnames(x)[[1]] %in% reg_to_cell$cell)){
    from="cell"
  } else if(all(dimnames(x)[[1]] %in% grid_to_cell$grid)){
    from="grid"
  } else {
    if(all(dimnames(x)[[1]]%in%reg_to_iso$iso)){
      from = "iso"
    } else if (all(dimnames(x)[[1]]%in%reg_to_iso$reg)){
      from = "reg"
    } else if (all(dimnames(x)[[1]]%in%c("GLO","GLO.1"))){
      from="glo"
    } else if (all(dimnames(x)[[1]]%in%c("GLO","glo",reg_to_iso$reg))){
      from="regglo"
    } else if (all(dimnames(x)[[1]]%in%c(grid_to_cell$grid))){
      from="grid"
    } else {stop("unknown regions, wrong or missing dir")}
  }
  
# otherwise it would lead to a second weight for the weight function
  if(from=="cell" & to=="iso" & absolute==FALSE & is.function(weight)){
    stop("Weight for iso aggregation of a relative object must be an object at iso level. Run gdxAggregate to get the weight at iso level")
  }
  

  # get rid of unnecessary data
  if(from%in%c("REGGLO","regglo")){
    x<-x["GLO",,,invert=TRUE]
    if(!is.function(weight)){
      if("GLO"%in%getRegions(weight)){
        if (length(getRegions(weight))>1){
          weight<-weight["GLO",,,invert=TRUE]
        }
      }
    }
    from="reg"
  }
  
  
  if(to%in%c("regglo")){
    to2="regglo"
    to="reg"
  } else {to2=FALSE}

  # no aggregation needed?
  if(from==to){
    out<-x
    #cat(" no aggregation needed")
  } else {
    #cat(paste0("mapping: ",from,"_",to))
    # select mapping
    if(((from=="cell")&(to=="iso"))|(((from=="iso")&(to=="cell")))){
    #mappings for the disaggregation/aggregation process
      mapping<-grid_to_cell
      mapping_iso<-Cell2Country()
      
    } else if(((from=="reg")&(to=="iso"))|(((from=="iso")&(to=="reg")))){
      mapping<-reg_to_iso
    } else if(((from=="cell")&(to=="reg"))|(((from=="reg")&(to=="cell")))){
      mapping<-reg_to_cell
    } else if(to%in%c("glo")) {
      mapping<-data.frame(
        from=dimnames(x)[[1]],
        glo="GLO"
      )
      names(mapping)[1]<-from
    } else if(((from=="grid")&(to=="cell"))|(((from=="cell")&(to=="grid")))|(((from=="reg")&(to=="grid")))|(((from=="grid")&(to=="reg")))){
      mapping<-grid_to_cell
    } else if (from == "glo" & to == "iso"){
      mapping<-reg_to_iso
      mapping$glo<-"GLO"
      mapping<-mapping[,c("glo","iso")]
    } else{stop("unknown mapping")}
  
    if(absolute==TRUE){
      # gewicht nur notwenig bei aggregation
      if(!is.function(weight)){

        if(paste0(from,to)%in%c("gridcell","gridiso","gridreg","gridglo","cellreg","cellglo","isoreg","isoglo","regglo")) {
          # aggregation of absolute values needs no weight
          if(!is.null(weight)){stop("weight provided, but aggregation of absolute values needs no weight")}
        }else if(paste0(from,to)%in%c("celliso")){
        
          if(is.null(weight)){stop("weight to dissagregate cell to grid is needed to be able to aggregate to iso level absolute values")}
        
          } else {
          # disaggregation of absolute values needs weight
          if(is.null(weight)){stop("no weight provided, but disaggregation of absolute values needs weight")}
        }
      }else{

        if(paste0(from,to)%in%c("gridcell","gridiso","gridreg","gridglo","cellreg","cellglo","isoreg","isoglo","regglo")) {
          # aggregation of absolute values needs no weight
          weight=NULL

        } else if(paste0(from,to)%in%c("celliso")) {
          weight<-weight(gdx=gdx, level="grid", dir=dir,...)
        }else {
          # disaggregation of absolute values needs weight
          weight<-weight(gdx=gdx, level=to, dir=dir, ...)
        }
      }
    } else if (absolute==FALSE){
      if(!is.function(weight)){
        if(paste0(from,to)%in%c("gridcell","gridiso","gridreg","gridglo","celliso","cellreg","cellglo","isoreg","isoglo","regglo")) {
          # aggregation of relative values needs weight
          if(is.null(weight)){stop("weight not provided, but aggregation of relative values needs weight")}
        } else {
          # disaggregation of relative values needs no weight
          if(!is.null(weight)){stop("weight provided, but aggregation needs no weight")}
        }
      }else{

        if(paste0(from,to)%in%c("gridcell","gridiso","gridreg","gridglo","cellreg","cellglo","isoreg","isoglo","regglo")) {
          # aggregation of relative values needs weight
          weight<-weight(gdx=gdx, level=from, dir=dir,...)
        } else if(paste0(from,to)%in%c("celliso")){
          stop("Weight for celliso aggregation must be an object at iso level, function weight not supported")
        }else {
          # disaggregation of relative values needs no weight
          weight=NULL
        }
      }
    } else {stop("absolute has to be binary")}
      
    if(!is.null(weight) && !is.null(getYears(weight))) { # problems can occur if function provides different years than object has
      if(!is.null(getYears(x))) {
        weight <- weight[,getYears(x),]
      } else {
        weight <- weight[,1,]
        getYears(weight) <- NULL
      }
    }  
    
    
      
    if(((from=="cell")&(to=="iso"))){
      if(absolute==TRUE){
        
        ind<-toolAggregate(x = x,rel=mapping,weight = weight,from = from,to = "grid",dim = 1)
        getCells(ind)<-mapping_iso$cell
        out<-toolAggregate(x = ind,rel=mapping_iso,weight = NULL,from = "cell",to = "iso",dim = 1)
        
      }else{
        
        ind<-toolAggregate(x = x,rel=mapping,weight = NULL,from = from,to = "grid",dim = 1)
        getCells(ind)<-mapping_iso$cell
        getCells(weight)<-mapping_iso$cell
        out<- toolAggregate(x = ind,rel=mapping_iso,weight = weight,from = "cell",to = "iso",dim = 1)
      }
      
    }else{
      out <- toolAggregate(x = x,rel=mapping,weight = weight,from = from,to = to,dim = 1)
      if(!is.null(weight)){weight <- toolAggregate(x = weight,rel=mapping,from = from,to = to,dim = 1)} # aggregate weight too for the case its needed again in regglo
    }
    
    
    }
  
  
  if (to2=="regglo"){
    if(absolute==TRUE){
      out<- mbind(out,dimSums(out,dim=1))
    } else {
      if(is.function(weight)){
        weight<-weight(gdx=gdx, level="reg", dir=dir,...)
      }
      out<- mbind(out,
                  dimSums(out*collapseNames(weight[getRegions(out),,]),dim=1)/dimSums(collapseNames(weight[getRegions(out),,]),dim=1)
      )
    }
  }
  
  #checks if aggregation to global level  of absolute values is the same for the input x and for the output out
  #commented out until dimSums(x,dim=1) = 0 sorted out
  # if(absolute==TRUE){
  #   if(any(abs(dimSums(x,dim=1)-(dimSums(out,dim=1)))/dimSums(x,dim=1)>1e-2)){
  #     warning("Global summation of input different than output")
  #   }
  # }
  
  return(out)
  
} 
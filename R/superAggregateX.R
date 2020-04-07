#' superAggregateX
#' 
#' drop-in replacement for superAggregate based on toolAggregate
#' 
#' @param data An MAgPIE 
#' @param aggr_type Aggregation Type. Can be any function for one or two
#' objects (data and weight) of the same size. Currently pre-supported
#' functions: "sum","mean","weighted_mean".
#' @param level Allowed level types are global "glo", regional "reg" and 
#' "regglo"
#' @param weight Currently only used for weighted_mean
#' @param crop_aggr determines whether output should be crop-specific (FALSE)
#' or aggregated over all crops (TRUE). The method used for aggregation is set
#' by aggr_type 
#' @return returns a MAgPIE
#' object.
#' @author Jan Philipp Dietrich
#' @importFrom magclass ncells

superAggregateX <- function(data, aggr_type, level="reg", weight=NULL, crop_aggr=FALSE) {
  if(level=="glo") {
    rel <- data.frame(from=getRegions(data),to="GLO")
  } else if(level=="reg") {
    rel <- data.frame(from=getCells(data),to=sub("\\..*$","",getCells(data)))
  } else if(level=="regglo") {
    rel <- data.frame(from=c(getCells(data),getCells(data)),to=c(sub("\\..*$","",getCells(data)),rep("GLO",ncells(data))))
  } else {
    stop("unsupported level", level)
  }
  
  if(aggr_type=="sum") {
    weight <- NULL
  } else if(aggr_type=="mean") {
    weight <- data
    weight[,,] <- 1
  } else if(aggr_type=="weighted_mean") {
    weight <- weight+10^-8
  } else {
    stop("unsupported aggr_type ", aggr_type)
  }
  data <- toolAggregate(data,rel,weight=weight)
  if(crop_aggr) {
    if(!is.null(weight)) weight <- toolAggregate(weight,rel,weight=NULL)
    data <- toolAggregate(data,data.frame(from=getNames(data),to="total"),dim=3,weight=weight)
  }
  return(data)
}
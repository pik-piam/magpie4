#' @title priceIndex
#' @description calcluates price indicies based on a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param index "lasp" (Laspeyres-Index: baseyear weighting), "paas" (Paasche-Index: current weighting), "fish" (Fisher-Index: geometric mean of "lasp" and "paas")
#' @param chain Chain Index: if true, the base period for each time period is the immediately preceding time period. Can be combined with all of the above indices
#' @param baseyear baseyear of the price index
#' @param round shall the results be rounded?
#' @param type For whom are the prices important? "producer" are the prices that farmer face, as they also produce intermediate products (seed, feed). "consumer" are the prices for the end consumer faces (supermarket, bioenergy plant). Currently, the only difference is the basket composition (ideally, also prices should differ between regions)
#' @param product_aggr aggregate over products or not (boolean)
#' @return A MAgPIE object containing price indices for consumers or producers (depending on type)
#' @author Jan Philipp Dietrich, Florian Humpenoeder, Benjamin Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- priceIndex(gdx)
#'   }
#' 
#' @importFrom magclass as.magpie

priceIndex <- function (gdx, file=NULL, level = "reg", products = "kall", index = "lasp", chain=FALSE, baseyear="y2005", round=TRUE, type="consumer",product_aggr=TRUE) {
  if (chain==FALSE) {
    if (index == "lasp" | index == "paas") {
      if(type=="producer") {
        q_t <- readGDX(gdx,"ov_supply",select = list(type="level"),react = "warning")
        p_t <- prices(gdx,type="producer")
      } else if (type =="consumer") {        
        q_t <- dimSums(demand(gdx)[,,c("food","other_util","bioenergy")],dim=3.1)
        p_t <- prices(gdx,type="consumer",products=products)                                
      } else {stop("invalid type")}
      if (!all(products%in%readGDX(gdx,"kall"))) products<-readGDX(gdx, products)
      if (suppressWarnings(is.null(readGDX(gdx,"fcostsALL")))) {
        # products <- products[-which(products=="wood")]
        # products <- products[-which(products=="woodfuel")]
        if ("wood" %in% products)     products <- products[-which(products=="wood")]
        if ("woodfuel" %in% products) products <- products[-which(products=="woodfuel")]
        if ("constr_wood" %in% products) products <- products[-which(products=="constr_wood")]
      } 

      p_t <- p_t[, , products]
      q_t <- q_t[, , products]
      # check if the baseyear is contained in the gdx  
      if(!baseyear %in% getYears(q_t)) {
        miss_year <- baseyear
        q_t <- time_interpolate(q_t, baseyear, integrate_interpolated_years=TRUE)
        p_t <- time_interpolate(p_t, baseyear, integrate_interpolated_years=TRUE)
      }
      q_0 <- q_t
      p_0 <- p_t
      for (year in getYears(q_0)) {
        q_0[, year, ] <- setYears(q_t[, baseyear,],year)
        p_0[, year, ] <- setYears(p_t[, baseyear,],year)
      }        
      
      if (index == "lasp") {
        if (product_aggr) {
          dividend <- dimSums(p_t * q_0,dim=3,na.rm=TRUE)
          divisor <- dimSums(p_0 * q_0,dim=3,na.rm=TRUE)
        } else {
          dividend <- (p_t * q_0)
          divisor <- (p_0 * q_0)
        }
      } else if (index == "paas") {
        if (product_aggr) {
          dividend <- dimSums(p_t * q_t,dim=3,na.rm=TRUE)
          divisor  <- dimSums(p_0 * q_t,dim=3,na.rm=TRUE)
        } else {
          dividend <- (p_t * q_t)
          divisor  <- (p_0 * q_t)
        }
      }
      #aggregate
      dividend <- gdxAggregate(gdx = gdx, x=dividend, absolute = TRUE, to=level)
      divisor <- gdxAggregate(gdx = gdx, x=divisor, absolute = TRUE, to=level)
      px <- dividend/divisor
      
    } else if (index == "fish") {
      lasp <- priceIndex(gdx, level = level, products = products, 
                         chain=chain, baseyear=baseyear,
                         index = "lasp",round=round)/100
      paas <- priceIndex(gdx, level = level, products = products, 
                         chain=chain, baseyear=baseyear,
                         index = "paas",round=round)/100
      px <- sqrt(lasp * paas)
    } else stop(paste("index ", "\"", index, "\"", " does not exist. Please specify a correct index!", sep = ""))
  } else if (chain==TRUE) {
    px <- prices(gdx,level=level,product_aggr = TRUE); px[,,] <- NA; px[,1,] <- 1;
    for (y in 2:dim(px)[2]) {
      px[,y,] <- priceIndex(gdx, level = level, products = products, 
                                 chain=FALSE, baseyear=y-1, index = index,round=FALSE,product_aggr = product_aggr)[,y,]/100
      px[,y,] <- px[,y,]*setYears(px[,y-1,],NULL)
    }
    tmp<-px
    tmp[,,]<-setYears(px[,baseyear,],NULL)
    px<-px/tmp
  }
  px<-px*100
  if (round) {px <- round(px)}
  if(exists("miss_year")) px <- px[,getYears(px)!=miss_year,]
  out(px,file)
}
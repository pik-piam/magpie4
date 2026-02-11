#' @title Seed
#' @description Calculates MAgPIE demand for Seed out of a gdx file
#' @importFrom memoise memoise
#' @importFrom rlang hash
#' @importFrom R.utils lastModified
#' @export
#'
#' @param gdx GDX file
#' @param level Level of regional aggregation ("reg", "glo", "regglo")
#' @param attributes dry matter: Mt ("dm"), gross energy: PJ ("ge"), reactive nitrogen: Mt ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm"). Can also be a vector.
#' @details Demand definitions are equivalent to FAO CBS categories
#' @return demand as MAgPIE object (Unit depends on attributes)
#' @author Benjamin Leon Bodirsky
#' @importFrom magclass getRegions add_dimension
#' @examples
#'
#'   \dontrun{
#'     x <- demand(level="regglo", products="kcr")
#'   }
#'

Seed <- memoise(function(gdx,level = "reg", attributes="dm"){

  products=findset("kcr")
  seed<-readGDX(gdx = gdx, "ov_dem_seed", select = list(type="level"))[,,products]
  products <- getNames(seed)

  if(any(attributes!="dm")){
    att=readGDX(gdx,"fm_attributes")[,,attributes]
    seed<-seed*att[,,products]
  }
  out <- gdxAggregate(gdx = gdx,weight = 'production',x = seed,to = level,absolute = TRUE, products=products, product_aggr=FALSE)

}
# the following line makes sure that a changing timestamp of the gdx file and
# a working directory change leads to new caching, which is important if the
# function is called with relative path args.
,hash = function(x) hash(list(x, getwd(), lastModified(x$gdx))))

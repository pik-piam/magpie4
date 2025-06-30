#' @title reportFoodExpenditure
#' @description reports per-capita calories food supply (including household waste)
#'
#' @importFrom magpiesets reporthelper
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail=F, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @param level spatial aggregation: "reg", "glo", "regglo", "iso"
#' @return per-capita calories as MAgPIE object (kcal/cap/day)
#' @author Benjamin Leon Bodirsky
#' @examples
#'
#'   \dontrun{
#'     x <- reportFoodExpenditure(gdx)
#'   }
#'

reportFoodExpenditure<-function(gdx,detail=FALSE,level="regglo"){
  out<-FoodExpenditure(gdx,level = level,products="kall",product_aggr = FALSE)
  out<-reporthelper(x=out,level_zero_name = "Household Expenditure|Agricultural Primary Products|Expenditure",detail = detail,partly = TRUE)
  getNames(out) <- paste(getNames(out),"(US$2017/capita)",sep=" ")

  out2<-FoodExpenditureShare(gdx,level=level,products = "kfo",product_aggr = FALSE)
  out2 <-  reporthelper(x=out2,level_zero_name = "Household Expenditure|Agricultural Primary Products|Expenditure Share",detail = detail,partly = TRUE)
  getNames(out2) <- paste(getNames(out2),"(US$2017/US$2017)",sep=" ")

  #value added
  out3 <- FoodExpenditure(gdx,level = level,products="kall",product_aggr = FALSE, valueAdded = TRUE)
  out3 <-reporthelper(x=out3,level_zero_name = "Household Expenditure|Food|Expenditure",detail = detail,partly = TRUE)
  getNames(out3) <- paste(getNames(out3),"(US$2017/capita)",sep=" ")

  out4 <- FoodExpenditureShare(gdx,level=level,products = "kfo",product_aggr = FALSE, valueAdded = TRUE)
  out4 <- reporthelper(x=out4,level_zero_name = "Household Expenditure|Food|Expenditure Share",detail = detail,partly = TRUE)
  getNames(out4) <- paste(getNames(out4),"(US$2017/capita)",sep=" ")

  out<-mbind(out,out2)
  out <- mbind(out, out3)
  out <- mbind(out, out4)
  #delete empty categories
  out<-out[,,getNames(out)[which(dimSums(out,dim=c(1,2))!=0)]]
  return(out)
}

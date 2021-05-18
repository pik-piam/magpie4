#' @title CostCapital
#' @description Reads data to calculate capital stocks
#'
#' @export
#'
#' @param gdx GDX file
#' @param type either capital stocks ("stocks") or overall capital investment "investment"
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @return A MAgPIE object containing values related with overall value of production [million US$05]
#' @author Edna Molina Bacca
#' @importFrom gdx readGDX out
#' @importFrom luscale superAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- CostCapital(gdx)
#'   }
#'

CostCapital <- function(gdx,type="stocks",file=NULL,level="cell"){
  
  if(type=="stocks"){
  #Reads existing capital in each time step
  
  capital_im <- if(!is.null(suppressWarnings(readGDX(gdx,"p38_capital_immobile")))) dimSums(readGDX(gdx,"p38_capital_immobile"),dim=3) else NULL
  capital_mo <- if(!is.null(suppressWarnings(readGDX(gdx,"p38_capital_mobile"))))  readGDX(gdx,"p38_capital_mobile") else NULL
  tag <- "Capital Stocks"
  }else if(type == "investment"){
    
  capital_im <- if(!is.null(suppressWarnings(readGDX(gdx,"ov38_investment_immobile")[,,"level"]))) dimSums(collapseNames(readGDX(gdx,"ov38_investment_immobile")[,,"level"]),dim=3) else NULL
  capital_mo <- if(!is.null(suppressWarnings(readGDX(gdx,"ov38_investment_mobile")[,,"level"])))  collapseNames(readGDX(gdx,"ov38_investment_mobile")[,,"level"]) else NULL
  tag <- "Capital Investments"
  }
  
  
  # Mixed and PerTon factor costs realizations don't contain capital info. 
  # Check that stops the function in case capital is not accounted for
  if (any(is.null(capital_im), is.null(capital_mo))) stop("Capital information only available for the sticky factor costs realization")
  
  Sum_cap <- capital_im + capital_mo
  
  getNames(Sum_cap) <- tag
  
  weight<- NULL  
  
  out <- Sum_cap
 

  if (level != "cell") out <- superAggregate(out, aggr_type = "sum", level = level)
  
  
  out(out,file)
}

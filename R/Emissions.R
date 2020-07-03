#' @title Emissions
#' @description reads GHG emissions out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param type emission type(s): "co2_c", "n2o_n" or "ch4"
#' @param unit "element", "gas", "GWP" or "GWP*"; "element": co2_c in Mt C/yr, n2o_n in Mt N/yr, ch4 in Mt CH4/yr; "gas": co2_c Mt CO2/yr, n2o_n in Mt NO2/yr, ch4 in Mt CH4/yr; "GWP": co2_c in Mt CO2/yr, n2o_n in Mt CO2eq/yr, ch4 in Mt CO2eq/yr;
#' @param subcategories FALSE (default) or TRUE
#' @param cumulative Logical; Determines if emissions are reported annually (FALSE) or cumulative (TRUE). The starting point for cumulative emissions is y1995.
#' @param lowpass number of lowpass filter iterations
#' @return emissions as MAgPIE object (unit depends on \code{unit})
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- Emissions(gdx)
#'   }

Emissions <- function(gdx, file=NULL, level="reg", type="co2_c", unit="element", subcategories=FALSE, cumulative=FALSE, lowpass=NULL){
  
  #read in emissions
  a <- readGDX(gdx,"ov_emissions_reg",react="silent",format="first_found",select=list(type="level"))
  a <- add_columns(a,dim=3.2,addnm = "n2o_n")
  a[,,"n2o_n"]<-dimSums(a[,,c("n2o_n_direct","n2o_n_indirect")],dim=3.2)
  
  #set co2_c emissions in 1995 to NA (they are not meaningful)
  a[,1,"co2_c"] <- NA

  #unit conversion
  if (unit == "gas") {
    unit_conversion <- a
    unit_conversion[,,] <- 1
    unit_conversion[,,"n2o_n"] <- 44/28 #from Mt N/yr to Mt N2O/yr
    unit_conversion[,,"ch4"] <- 1 #no conversion needed
    unit_conversion[,,"co2_c"] <- 44/12 #from Mt C/yr to Mt CO2/yr
    unit_conversion[,,"no3_n"] <- 62/14 #from Mt N/yr to Mt NO3/yr
    unit_conversion[,,"nh3_n"] <- 17/14 #from Mt N/yr to Mt NH3/yr
    unit_conversion[,,"no2_n"] <- 46/14 #from Mt N/yr to Mt NO2/yr
    a <- a*unit_conversion
    #Caution. Don't change these reporting names without need. GHG emissions are exchanged with REMIND in the coupling.
    getNames(a)<-sub(getNames(a),pattern = "co2_c",replacement = "co2")
    getNames(a)<-sub(getNames(a),pattern = "n2o_n",replacement = "n2o")
    getNames(a)<-sub(getNames(a),pattern = "no3_n",replacement = "no3")
    getNames(a)<-sub(getNames(a),pattern = "nh3_n",replacement = "nh3")
    getNames(a)<-sub(getNames(a),pattern = "no2_n",replacement = "no2")
    type=substring(type,1,3)
  }  
  
  if (unit == "GWP*") {
    #Lynch et al 2020 ERL equation 3
    years <- getYears(a,as.integer = T)
    b <- a
    for (t in years) {
      t_before <- t-20
      if (!t_before %in% years) t_before <- years[which.min(abs(years - t_before))]
      a[,t,"ch4"] <- 4*b[,t,"ch4"] - 3.75*b[,t_before,"ch4"]
    }
  }
  
  if (unit %in% c("GWP","GWP*")) {
    unit_conversion <- a
    unit_conversion[,,] <- 1
    unit_conversion[,,"n2o_n"] <- 44/28*265 #from Mt N/yr to Mt CO2eq/yr
    unit_conversion[,,"ch4"] <- 1*28 #from Mt CH4 to Mt CO2eq/yr
    unit_conversion[,,"co2_c"] <- 44/12 #from Mt C/yr to Mt CO2/yr
    unit_conversion[,,"no3_n"] <- 0 #from Mt N/yr to Mt CO2eq/yr
    unit_conversion[,,"nh3_n"] <- 0 #from Mt N/yr to Mt CO2eq/yr
    unit_conversion[,,"no2_n"] <- 0 #from Mt N/yr to Mt CO2eq/yr
    a <- a*unit_conversion
    getNames(a)<-sub(getNames(a),pattern = "co2_c",replacement = "co2")
    getNames(a)<-sub(getNames(a),pattern = "n2o_n",replacement = "n2o")
    getNames(a)<-sub(getNames(a),pattern = "no3_n",replacement = "no3")
    getNames(a)<-sub(getNames(a),pattern = "nh3_n",replacement = "nh3")
    getNames(a)<-sub(getNames(a),pattern = "no2_n",replacement = "no2")
    type=substring(type,1,3)
  }
  
  #apply lowpass filter
  if(!is.null(lowpass)) {
    tmp <- a; a <- NULL;
    for (ghg in type) {
      if(ghg == "co2_c") {
        a <- mbind(a,mbind(tmp[,1,ghg],lowpass(tmp[,-1,ghg],i=lowpass)))
      } else {
        a <- mbind(a,lowpass(tmp[,,ghg],i=lowpass))
      }
    }
  }

  #return all ghg emissions if type=NULL; subset otherwise
  if(!is.null(type))  a <- a[,,type]
  if (!subcategories) a <- dimSums(a,dim=3.1)
  
  #cumulative emissions
  if (cumulative) {
    #im_years <- readGDX(gdx,"im_years",format="first_found")
    im_years <- m_yeardiff(gdx)
    a[,"y1995",] <- 0
    a <- a*im_years[,getYears(a),]
    a <- as.magpie(apply(a,c(1,3),cumsum))
  }
  
  #aggregate over regions
  if (level != "reg") a <- superAggregateX(a, aggr_type = "sum", level = level)

  out(a,file)
}

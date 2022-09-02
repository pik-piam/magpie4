#' @title EmissionsBeforeTechnicalMitigation
#' @description reads GHG emissions before technical abatement out of a MAgPIE gdx file. Technical abatement includes all abatement done in the MACC curves, but exclude endogenous mitigation. These emissions are NOT the standard reporting emissions, but used for special purposes like remind-magpie coupling.
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param type emission type(s): "co2_c", "n2o_n" or "ch4" and in the case of unit="gas" "co2" and "n2o"
#' @param unit "element", "gas" or "co2eq"; "element": co2_c in Mt C/yr, n2o_n in Mt N/yr, ch4 in Mt CH4/yr; "gas": co2_c Mt CO2/yr, n2o_n in Mt NO2/yr, ch4 in Mt CH4/yr; "co2eq": co2_c in Mt CO2/yr, n2o_n in Mt CO2eq/yr, ch4 in Mt CO2eq/yr
#' @param subcategories FALSE (default) or TRUE
#' @return emissions as MAgPIE object (unit depends on \code{unit})
#' @author Florian Humpenoeder; Benjamin Leon Bodirsky
#' @examples
#'
#'   \dontrun{
#'     x <- EmissionsBeforeTechnicalMitigation(gdx)
#'   }

EmissionsBeforeTechnicalMitigation <- function(gdx, file=NULL, level="reg", type="co2_c", unit="element", subcategories=FALSE){

  #read in emissions
  a <- readGDX(gdx,"ov_btm_reg",react="silent",format="first_found",select=list(type="level"))
  #in case ov_btm_reg does not exist, calculate emissions before technical mitigation
  if(is.null(a)) {
    emis <- readGDX(gdx,"ov_emissions_reg",react="silent",format="first_found",select=list(type="level"))
    maccs <- readGDX(gdx,"im_maccs_mitigation",react="silent",format="first_found")
    a <- emis/(1-maccs)
  }
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
    getNames(a)<-sub(getNames(a),pattern = "co2_c",replacement = "co2")
    getNames(a)<-sub(getNames(a),pattern = "n2o_n",replacement = "n2o")
    getNames(a)<-sub(getNames(a),pattern = "no3_n",replacement = "no3")
    getNames(a)<-sub(getNames(a),pattern = "nh3_n",replacement = "nh3")
    getNames(a)<-sub(getNames(a),pattern = "no2_n",replacement = "no2")
    type=substring(type,1,3)
  }
  if (unit == "co2eq") {
    unit_conversion <- a
    unit_conversion[,,] <- 1
    unit_conversion[,,"n2o_n"] <- 44/28*298 #from Mt N/yr to Mt CO2eq/yr
    unit_conversion[,,"ch4"] <- 1*34 #from Mt CH4 to Mt CO2eq/yr
    unit_conversion[,,"co2_c"] <- 44/12 #from Mt C/yr to Mt CO2/yr
    unit_conversion[,,"no3_n"] <- 0 #from Mt N/yr to Mt CO2eq/yr
    unit_conversion[,,"nh3_n"] <- 0 #from Mt N/yr to Mt CO2eq/yr
    unit_conversion[,,"no2_n"] <- 0 #from Mt N/yr to Mt CO2eq/yr
    a <- a*unit_conversion
    getNames(a)<-sub(getNames(a),pattern = "co2_c",replacement = "co2")
    getNames(a)<-sub(getNames(a),pattern = "n2o_n",replacement = "co2eq (n2o)")
    getNames(a)<-sub(getNames(a),pattern = "ch4",replacement = "co2eq (ch4)")
    getNames(a)<-sub(getNames(a),pattern = "no3_n",replacement = "co2eq (no3)")
    getNames(a)<-sub(getNames(a),pattern = "nh3_n",replacement = "co2eq (nh3)")
    getNames(a)<-sub(getNames(a),pattern = "no2_n",replacement = "co2eq (no2)")
  }

  #return all ghg emissions if type=NULL; subset otherwise
  if(!is.null(type))  a <- a[,,type]
  if (!subcategories) a <- dimSums(a,dim=3.1)


  #aggregate over regions
  if (level != "reg") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)

  out(a,file)
}

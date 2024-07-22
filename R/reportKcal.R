#' @title reportKcal
#' @description reports per-capita calories food supply (including household waste)

#' @import magpiesets
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail=F, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @param level spatial aggregation: "reg", "glo", "regglo", "iso"
#' @return per-capita calories as MAgPIE object (kcal/cap/day)
#' @author Benjamin Leon Bodirsky, Kristine karstens, Abhijeet Mishra
#' @examples
#'
#'   \dontrun{
#'     x <- reportKcal(gdx)
#'   }
#'

reportKcal<-function(gdx,detail=FALSE,level="regglo"){

  level_zero_name <- "Nutrition|Calorie Supply"

  out<-Kcal(gdx,level = level, products = "kall",product_aggr = FALSE, calibrated=TRUE,magpie_input = FALSE)

  out<-reporthelper(x=out,level_zero_name = level_zero_name,detail = detail)

  if(level_zero_name%in%getNames(out)){
    sumup  <- getNames(out[,,level_zero_name,invert=TRUE])
    getNames(out)  <- c(level_zero_name,getNames(summationhelper(out[,,sumup],sep="+", dim=3.1)))
  } else {getNames(out) <- getNames(summationhelper(out, sep="+", dim=3.1))}

  p15_protein_pc_iso_scp <- readGDX(gdx, "p15_protein_pc_iso_scp", react = "silent")
  if (!is.null(p15_protein_pc_iso_scp)) {
    kfo_rd <- readGDX(gdx,"kfo_rd")
    fm_nutrition_attributes <- readGDX(gdx, "fm_nutrition_attributes")[,getYears(p15_protein_pc_iso_scp),]
    if ("livst_milk" %in% kfo_rd) {
      s15_scp_protein_per_milk <- readGDX(gdx, "s15_scp_protein_per_milk")
      s15_scp_fat_per_milk <- readGDX(gdx, "s15_scp_fat_per_milk")
      s15_scp_sugar_per_milk <- readGDX(gdx, "s15_scp_sugar_per_milk")

      oil_scp_milk <- p15_protein_pc_iso_scp[,,"livst_milk"] / s15_scp_protein_per_milk * s15_scp_fat_per_milk * fm_nutrition_attributes[,,"oils.kcal"]
      oil_scp_milk<-gdxAggregate(gdx = gdx,x = oil_scp_milk,weight = 'population',to = level,absolute = FALSE)
      getNames(oil_scp_milk) <- "Nutrition|Calorie Supply|Secondary products|Oils|+|Ingredient for MP-based milk alternative"
      out <- mbind(out,oil_scp_milk)

      sugar_scp_milk <- p15_protein_pc_iso_scp[,,"livst_milk"] / s15_scp_protein_per_milk * s15_scp_sugar_per_milk * fm_nutrition_attributes[,,"sugar.kcal"]
      sugar_scp_milk<-gdxAggregate(gdx = gdx,x = sugar_scp_milk,weight = 'population',to = level,absolute = FALSE)
      getNames(sugar_scp_milk) <- "Nutrition|Calorie Supply|Secondary products|Sugar|+|Ingredient for MP-based milk alternative"
      out <- mbind(out,sugar_scp_milk)
    }
    if ("livst_rum" %in% kfo_rd) {
      s15_scp_fat_per_meat <- readGDX(gdx, "s15_scp_fat_per_meat")
      s15_scp_fat_protein_ratio_meat <- readGDX(gdx, "s15_scp_fat_protein_ratio_meat")

      oil_scp_meat <- p15_protein_pc_iso_scp[,,"livst_rum"] / fm_nutrition_attributes[,,"scp.protein"] *
        (fm_nutrition_attributes[,,"scp.protein"] * s15_scp_fat_protein_ratio_meat - s15_scp_fat_per_meat) * fm_nutrition_attributes[,,"oils.kcal"]
      oil_scp_meat<-gdxAggregate(gdx = gdx,x = oil_scp_meat,weight = 'population',to = level,absolute = FALSE)
      getNames(oil_scp_meat) <- "Nutrition|Calorie Supply|Secondary products|Oils|+|Ingredient for MP-based meat alternative"
      out <- mbind(out,oil_scp_meat)
    }
  }
  out <- out[,,sort(getNames(out))]

  getNames(out) <- paste(getNames(out),"(kcal/capita/day)",sep=" ")

  #delete empty categories
  out<-out[,,getNames(out)[which(dimSums(out,dim=c(1,2))!=0)]]
  return(out)
}

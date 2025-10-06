#' @title demand
#' @description Calculates MAgPIE demand out of a gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation ("reg", "glo", "regglo")
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr aggregate over products or not (boolean, default FALSE)
#' @param attributes dry matter: Mt ("dm"), gross energy: PJ ("ge"), reactive nitrogen: Mt ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm"). Can also be a vector.
#' @param type Demand type(s): "food", "feed", "processed", "other_util", "bioenergy", "seed", "waste", "dom_balanceflow; NULL returns all types
#' @param type_aggr aggregate over demand types or not (boolean, default FALSE)
#' @details Demand definitions are equivalent to FAO CBS categories
#' @return demand as MAgPIE object (Unit depends on attributes)
#' @author Benjamin Leon Bodirsky, Abhijeet Mishra, Miodrag Stevanovic
#' @importFrom magclass getRegions
#' @importFrom magclass add_dimension
#' @importFrom luscale superAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- demand(level="regglo", products="kcr")
#'   }
#'

demand <-  function(gdx,
                    file = NULL,
                    level = "reg",
                    products = "kall",
                    product_aggr = FALSE,
                    attributes = "dm",
                    type = NULL,
                    type_aggr = FALSE) {

    if (!all(products %in% readGDX(gdx, "kall"))) {
      if (length(products) > 1) {
        stop("unknown product")
      }
      products <- readGDX(gdx, products)
    }

    years <- as.vector(readGDX(gdx, "t"))

    food        <- readGDX(gdx, "ov_dem_food", select = list(type = "level"))
    feed        <- dimSums(readGDX(gdx, "ov_dem_feed", select = list(type = "level")),
                           dim = "kap")
    processing  <- readGDX(gdx, "ov_dem_processing", select = list(type = "level"))
    material    <- readGDX(gdx, "ov_dem_material", select = list(type = "level"))
    bioenergy   <- readGDX(gdx, "ov_dem_bioen", select = list(type = "level"))
    seed        <- readGDX(gdx, "ov_dem_seed", select = list(type = "level"))
    waste       <- readGDX(gdx, "ov16_dem_waste", select = list(type = "level"))
    balanceflow <- readGDX(gdx, "f16_domestic_balanceflow")[, years,]

    forestry_products <- readGDX(gdx, "kforestry")
    forestry    <- readGDX(gdx, "ov_supply",
                           select = list(type = "level"))[, years, forestry_products]
    #forestry_updated <- add_columns(x=forestry, addnm=setdiff(getNames(food),getNames(forestry)), dim=3.1)
    #forestry_updated[,,setdiff(getNames(food),getNames(forestry))] <- 0

    out <- mbind(
      add_dimension(
        x = food,
        dim = 3.1,
        add = "demand",
        nm = "food"
      ),
      add_dimension(
        x = feed,
        dim = 3.1,
        add = "demand",
        nm = "feed"
      ),
      add_dimension(
        x = processing,
        dim = 3.1,
        add = "demand",
        nm = "processed"
      ),
      add_dimension(
        x = material,
        dim = 3.1,
        add = "demand",
        nm = "other_util"
      ),
      add_dimension(
        x = bioenergy,
        dim = 3.1,
        add = "demand",
        nm = "bioenergy"
      ),
      add_dimension(
        x = seed,
        dim = 3.1,
        add = "demand",
        nm = "seed"
      ),
      add_dimension(
        x = waste,
        dim = 3.1,
        add = "demand",
        nm = "waste"
      ),
      add_dimension(
        x = balanceflow,
        dim = 3.1,
        add = "demand",
        nm = "dom_balanceflow"
      )
    )

    # NOTE: Double structure for forestry products. Estimates in material demand and supply chain losses are overwritten
    out[, , forestry_products] <- 0
    out[, , forestry_products][,,"other_util"] <- forestry

    #test for consistency without wood products
    supply <- readGDX(gdx, "ov_supply", select = list(type = "level"))
    if (any(round(dimSums(out, dim = "demand") - supply, 4) != 0)) {
      wrong <- where(round(dimSums(out, dim = "demand") - supply, 4) != 0)$true$data
      warning(paste0("Mismatch of ov_supply and sum of demand types for the product categories ",
                     paste0(wrong, collapse = " ")))
    }

    out <- out[, , products]
    if (any(attributes != "dm")) {
      att <- readGDX(gdx, "fm_attributes")[, , products][, , attributes]
      out <- out * att
    }

    if (product_aggr) {
      out <- dimSums(out, dim = 3.2)
    }

    if (!is.null(type)) out <- out[, , type]
    if (type_aggr) out <- dimSums(out, dim = "demand")
    if (level != "reg") {
      # TODO: Changed to superAggregateX to test aggregation prototype, 
      # results in slight differences in some items unclear why so far
      out <- superAggregateX(out, aggr_type = "sum", level = level)
    }

    out(out, file)
  }

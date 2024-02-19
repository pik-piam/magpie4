#' @title reportFeedConversion
#' @description reportes feed demand by animal type
#'
#' @import magpiesets
#' @export
#'
#' @param gdx GDX file
#' @param livestockSystem if TRUE, ruminant products and poultry products are aggregated
#' @param balanceflow If true, feed includes the calibration balanceflow
#' @return feed demand as MAgPIE object (Mt DM)
#' @author Benjamin Bodirsky
#' @examples
#'
#'   \dontrun{
#'     x <- reportFeed()
#'   }
#'
#'
reportFeedConversion <- function(gdx, livestockSystem = TRUE, balanceflow = FALSE) {

  feed   <-  feed(gdx,level = "regglo", detail = T, nutrient = c("ge","nr"), balanceflow = balanceflow)
  #format the same way as in mrvalidation
  getNames(feed, dim = 1) <- paste0("feed_", getNames(feed, dim = 1))
  getSets(feed) <- c("i", "t", "ElementShort", "ItemCodeItem", "attribtues")

  ap <- production(gdx, products = "kli", attributes = c("ge","nr"), level = "regglo")
  getSets(ap) <- c("i", "t", "ItemCodeItem", "attribtues")

  ### calculate product specific feed conversion efficiency as quotient between
  ### feed and animal products

  x <- NULL
  weight <- NULL

  quotientProductspecfic <- collapseNames(ap)

  # extract feed items from mb

  feedGrouped <- mbind(
    add_dimension(
      dimSums(feed[, , c("tece", "trce", "rice_pro", "maiz", "brans")], dim = c("ItemCodeItem")),
      dim = 3.1, add = "ItemCodeItem", nm = "Cereal Intensity"),
    add_dimension(
      dimSums(feed[, , c("soybean", "groundnut", "puls_pro", "rapeseed", "oils", "oilcakes",
                         "cassav_sp", "oilpalm", "sunflower")], dim = c("ItemCodeItem")),
      dim = 3.1, add = "ItemCodeItem", nm = "Oilcrop intensity"),
    add_dimension(
      dimSums(feed[, , c("potato", "cassav_sp", "others",
                         "sugr_beet", "sugr_cane", "molasses")], dim = c("ItemCodeItem")),
      dim = 3.1, add = "ItemCodeItem", nm = "Roots and other crops"),
    add_dimension(
      dimSums(feed[, , c("pasture")], dim = c("ItemCodeItem")),
      dim = 3.1, add = "ItemCodeItem", nm = "Pasture intensity"),
    add_dimension(
      dimSums(feed[, , c("foddr", "res_cereals", "res_fibrous", "res_nonfibrous")],
              dim = c("ItemCodeItem")),
      dim = 3.1, add = "ItemCodeItem", nm = "Other roughage intensity"),
    add_dimension(
      dimSums(feed[, , c("livst_chick", "livst_egg", "livst_milk", "livst_pig",
                         "livst_rum", "fish")], dim = c("ItemCodeItem")),
      dim = 3.1, add = "ItemCodeItem", nm = "Animal product intensity"),
    add_dimension(
      dimSums(feed[, , c("distillers_grain", "alcohol", "scp")], dim = c("ItemCodeItem")),
      dim = 3.1, add = "ItemCodeItem", nm = "Other processed items intensity")
  )


  feedProductspecific <- collapseNames(feedGrouped[, , c("feed_livst_chick", "feed_livst_egg",
                                                         "feed_livst_milk", "feed_livst_pig",
                                                         "feed_livst_rum")])
  getNames(feedProductspecific, dim = 2) <- substring(getNames(feedProductspecific, dim = 2), 6)


  if (livestockSystem == TRUE) {
    feedProductspecific <- mbind(
      add_dimension(
        dimSums(feedProductspecific[, , c("livst_milk", "livst_rum")], dim = c("ElementShort")),
        dim = 3.2, add = "ElementShort", nm = "Ruminant meat and dairy"),
      add_dimension(
        dimSums(feedProductspecific[, , c("livst_chick", "livst_egg")], dim = c("ElementShort")),
        dim = 3.2, add = "ElementShort", nm = "Poultry meat and eggs"),
      add_dimension(
        dimSums(feedProductspecific[, , c("livst_pig")], dim = c("ElementShort")),
        dim = 3.2, add = "ElementShort", nm = "Monogastric meat")
    )
    quotientProductspecfic <- mbind(
      add_dimension(
        dimSums(quotientProductspecfic[, , c("livst_milk", "livst_rum")], dim = c("ItemCodeItem")),
        dim = 3.1, add = "ItemCodeItem", nm = "Ruminant meat and dairy"),
      add_dimension(
        dimSums(quotientProductspecfic[, , c("livst_chick", "livst_egg")], dim = c("ItemCodeItem")),
        dim = 3.1, add = "ItemCodeItem", nm = "Poultry meat and eggs"),
      add_dimension(
        dimSums(quotientProductspecfic[, , c("livst_pig")], dim = c("ItemCodeItem")),
        dim = 3.1, add = "ItemCodeItem", nm = "Monogastric meat")
    )
  } else {
    getNames(feedProductspecific, dim = 2) <- reportingnames(getNames(feedProductspecific, dim = 2))
    getNames(quotientProductspecfic, dim = 1) <- reportingnames(getNames(quotientProductspecfic, dim = 1))
  }

  # Calculate feed conversion efficiency total
  quotientTmp <- collapseNames(dimSums(quotientProductspecfic, dim = c("ItemCodeItem"))[, , "ge"])
  indicatorTmp <- collapseNames(dimSums(feedProductspecific,
                                        dim = c("ItemCodeItem", "ElementShort"))[, , "ge"]) / quotientTmp
  nameIndicator <- "Productivity|Feed conversion efficiency"
  indicatorTmp <- setNames(collapseNames(indicatorTmp), paste0(nameIndicator, " (", "GE per GE", ")"))
  quotientTmp <- setNames(collapseNames(quotientTmp), paste0(nameIndicator, " (", "GE per GE", ")"))
  x <- mbind(x, indicatorTmp)
  weight <- mbind(weight, quotientTmp)

  # calculate feed conversion efficiency Livestock specific
  quotientTmp <- quotientProductspecfic[, , c("ge", "nr")]
  indicatorTmp <- dimSums(feedProductspecific, dim = c("ItemCodeItem"))[, , c("ge", "nr")] / quotientTmp
  prefix <- "Productivity|Feed conversion efficiency|"
  nameIndicator <- paste0(prefix, getNames(indicatorTmp, dim = 1), " (", "GE per GE", ")")
  x <- mbind(x, setNames(collapseNames(indicatorTmp[, , "ge"]), nameIndicator))
  weight <- mbind(weight, setNames(collapseNames(quotientTmp[, , "ge"]), nameIndicator))
  prefix <- "Productivity|Feed protein conversion efficiency|"
  nameIndicator <- paste0(prefix, getNames(indicatorTmp, dim = 1), " (", "Nr per Nr", ")")
  x <- mbind(x, setNames(collapseNames(indicatorTmp[, , "nr"]), nameIndicator))
  weight <- mbind(weight, setNames(collapseNames(quotientTmp[, , "nr"]), nameIndicator))

  # calculate feed conversion efficiency Livestock and product specific
  quotientTmp <- collapseNames(quotientProductspecfic[, , "ge"])
  indicatorTmp <- collapseNames(feedProductspecific[, , "ge"]) / quotientTmp
  prefix <- "Productivity|Feed conversion efficiency|"
  for (item in getNames(feedProductspecific, dim = 2)) {
    nameIndicator <- paste0(prefix, item, "|+|",
                            getNames(indicatorTmp, dim = 1), " (", "GE per GE", ")")
    x <- mbind(x, setNames(collapseNames(indicatorTmp)[, , item], nameIndicator))
    weightTmp <- collapseNames(indicatorTmp[, , item]) * NA
    weightTmp[, , ] <- collapseNames(quotientTmp[, , item])
    weight <- mbind(weight, setNames(weightTmp, nameIndicator))
  }

  # calculate feed conversion efficiency Product specific
  quotientTmp <- collapseNames(dimSums(quotientProductspecfic, dim = c("ItemCodeItem"))[, , "ge"])
  indicatorTmp <- collapseNames(dimSums(feedProductspecific, dim = c("ElementShort"))[, , "ge"]) / quotientTmp
  prefix <- "Productivity|Feed conversion efficiency|+|"
  nameIndicator <- paste0(prefix, getNames(indicatorTmp, dim = 1), " (", "GE per GE", ")")
  x <- mbind(x, setNames(collapseNames(indicatorTmp), nameIndicator))
  weightTmp <- collapseNames(indicatorTmp) * NA
  weightTmp[, , ] <- collapseNames(quotientTmp)
  weight <- mbind(weight, setNames(weightTmp, nameIndicator))

  # roughage share for ruminants as quotient of roughage and total feed
  quotientTmp <- dimSums(feedProductspecific, dim = "ItemCodeItem")
  indicatorTmp <- dimSums(
    feedProductspecific[, , c("Other roughage intensity", "Pasture intensity")],
    dim = "ItemCodeItem") / quotientTmp
  indicatorTmp2 <- dimSums(
    feedProductspecific[, , c("Pasture intensity")],
    dim = "ItemCodeItem") / quotientTmp
  if (livestockSystem == TRUE) {
    quotientTmp <- quotientTmp[, , "Ruminant meat and dairy"]
    indicatorTmp <- indicatorTmp[, , "Ruminant meat and dairy"]
    indicatorTmp2 <- indicatorTmp2[, , "Ruminant meat and dairy"]
  }
  prefix <- "Productivity|Roughage share|"
  nameIndicator <- paste0(prefix, getNames(indicatorTmp, dim = 1), " (", "GE per GE", ")")
  x <- mbind(x, setNames(collapseNames(indicatorTmp[, , "ge"]), nameIndicator))
  weight <- mbind(weight, setNames(quotientTmp[, , "ge"], nameIndicator))
  prefix <- "Productivity|Pasture share|"
  nameIndicator <- paste0(prefix, getNames(indicatorTmp2, dim = 1), " (", "GE per GE", ")")
  x <- mbind(x, setNames(collapseNames(indicatorTmp2[, , "ge"]), nameIndicator))
  weight <- mbind(weight, setNames(quotientTmp[, , "ge"], nameIndicator))

  # add livestock yields
  sysToKli <- readGDX(gdx, "sys_to_kli", react = "silent", format = "first_found")
  livestockYield <- toolAggregate(readGDX(gdx, "i70_livestock_productivity",
                                          react = "silent",
                                          format = "first_found"),
                                  from = "sys", to = "kli", rel = sysToKli, dim = 3)
  livestockProd <- production(gdx, products = "kli", attributes = "dm", level = "regglo")
  livestockYield <- livestockYield[, getYears(livestockProd), ]
  livestockYield <- mbind(livestockYield,
                          colSums(livestockYield * livestockProd[getRegions(livestockYield), , ]))
  prefix <- "Productivity|Livestock system yield|"
  nameIndicator <- paste0(prefix, getNames(livestockYield, dim = 1), " (", "DM per live animal", ")")
  x <- mbind(x, setNames(livestockYield, nameIndicator))
  weight <- mbind(weight, setNames(livestockProd, nameIndicator))

  getNames(x) <- sub("\\|$", "", getNames(x))
  getNames(weight) <- sub("\\|$", "", getNames(weight))


  return(x)
}

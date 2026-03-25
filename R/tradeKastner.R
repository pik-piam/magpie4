#' @title tradeKastner
#' @description Implements Kastner et al. 2011 (DOI: 10.1016/j.ecolecon.2011.01.012) on the MAgPIE
#' bilateral trade matrix. Adjusts the trade matrix based on a assumption of proportionality such
#' that intermediate trade partners (importing to re-export) are removed from the matrix.
#'
#' @param gdx GDX file to read from (for production data)
#' @param trade Bilateral trade data (ov21_trade or other)
#' @param level Level of regional aggregation ("reg", "glo", "regglo", or custom mapping)
#' @param products Selection of products (e.g. "kall", "kcr", "kli")
#' @param attributes dry matter: Mt ("dm"), gross energy: PJ ("ge"),
#' reactive nitrogen: Mt ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm")

#' @return MAgPIE object with bilateral trade adjusted using Kastner method.
#'   Dimensions: (importer_exporter, year, product)
#'   Values represent apparent consumption of bilateral trade (production + imports - exports)
#' @author David M Chen
#' @importFrom MASS ginv
#' @export

tradeKastner <- function(gdx, trade, level = "reg", products = "kall", attributes = "dm") {

  # Get production data
  prod <- production(gdx, level = level, products = products, 
                     product_aggr = FALSE, attributes = attributes)

  # Align years and items
  citems <- intersect(getItems(prod, dim = 3), getItems(trade, dim = 3))
  cyears <- intersect(getYears(prod), getYears(trade))
  
  prod <- prod[, cyears, citems]
  trade <- trade[, cyears, citems]
  # Get region names
  regions <- getItems(trade, dim = 1.1)
  n_regions <- length(regions)
  
  # Initialize output array: regions x regions (bilateral) x years x products
  # Dimensions: importer.exporter, year, product
  kastnerArray <- array(NA, 
                        dim = c(n_regions^2, length(cyears), length(citems)),
                        dimnames = list(paste(rep(regions, each = n_regions), 
                                             rep(regions, times = n_regions), sep = "."),
                                       cyears,
                                       citems))

  # Apply Kastner method for each product and year
  for (i in citems) {
    for (t in cyears) {

      ### Extract trade matrix for this product-year ###
      # trade has dims: (i_ex.i_im, year, product) in GDX format: exporters.importers
      tradeData <- trade[, t, i]
      
      # Get the bilateral region pairs from dimension names
      regionPairs <- getItems(tradeData, dim = 1)
      
      # Build importers x exporters matrix
      # Rows = importers (i_im), Columns = exporters (i_ex)
      tradeM <- matrix(0, nrow = n_regions, ncol = n_regions, 
                       dimnames = list(regions, regions))
      
      # Fill matrix from bilateral trade data
      # GDX format: exporters.importers, so trade[i_ex.i_im] = T_i_im,i_ex
      for (idx in seq_along(regionPairs)) {
        region_pair <- as.character(regionPairs[idx])
        
        # Split by "." to extract exporter and importer
        # Format in GDX: "exporter.importer"
        pair_split <- unlist(strsplit(region_pair, "\\.", fixed = FALSE))
        if (length(pair_split) == 2) {
          i_ex <- pair_split[1]
          i_im <- pair_split[2]
          # tradeM[importer, exporter] = trade from exporter to importer
          tradeM[i_im, i_ex] <- as.numeric(tradeData[idx])
        }
      }

      ### Extract production vector for this product-year ###
      prodS <- as.numeric(prod[, t, i])
      names(prodS) <- regions

      ### Calculate total imports and exports ###
      # With Z_ij = exports of j to i: rows are importers, columns are exporters.
      # imports_i = sum_j Z_ij (row sum)
      imM <- rowSums(tradeM)
      # exports_i = sum_k Z_k i (column sum)
      exM <- colSums(tradeM)

      ### Calculate DMI (x) and export share matrix (A) ###
      # x = p + Z * 1 (imports by row sum)
      xV <- prodS + imM
      # A+B: cap exports to available supply and clamp consumption share
      exM_raw <- exM
      exM <- pmin(exM, xV)
      xM <- diag(xV)
      # A = x^{-1} * Z, so A_{ij} = Z_{ij} / x_i
      xInv <- diag(1 / pmax(xV, 1e-10))

        matrixA <-  tradeM %*% xInv
              
      ### Calculate R = (I - A)^{-1} * p_hat ###
      I <- diag(n_regions)
      matrixR <- ginv(I - matrixA) %*% diag(prodS)

      ### Calculate consumption shares c and apparent consumption matrix Rbar ###
      # c_i = max(0, (x_i - exports_i) / x_i)
      # first pmax for cases where exports exceed prod + imports
      cV <- pmax(0, (xV - exM) / pmax(xV, 1e-10))
      cV[is.na(cV) | is.infinite(cV)] <- 0
      matrixC <- diag(cV)
      # Rbar = c_hat * R
      kastnerM <- matrixC %*% matrixR

      # Clamp tiny negatives from numerical inversion
      kastnerM[kastnerM < 0] <- 0

      ### Assign to output array ###
      # Convert matrix to vector in correct order (importers.exporters)
      kastnerVec <- as.vector(t(kastnerM))  # Column-major order: column j, row i
      kastnerArray[, t, i] <- kastnerVec

      ### Quality check: row sums should match apparent consumption ###
      appConsumption <- xV - exM
      rowSums_kastner <- rowSums(kastnerM)
    
        pct_diff <- abs(rowSums_kastner - appConsumption) / appConsumption * 100
        if (any(pct_diff >0.1, na.rm = TRUE)) {
          cat("Consumption mismatch for product ", i, " year ", t,
                  ": max diff ", round(max(pct_diff, na.rm = TRUE), 2), "%")
      }
      #note currently for self-self trade of sugr_cane and sugr_beet, this is still ok
      # since we overwrite self-self trade later, as in Kastner it includes production

    }
  }

  # Convert array to magpie object
  kastnerF <- as.magpie(kastnerArray, spatial = 1, temporal = 2)
  #let's reverse the first dimension to exporter.importer by first converting to dataframe
    df_kastner <- as.data.frame(kastnerF, rev = 2)
    # reorder column 1 named "fake" after column 2 named "region"
    df_kastner <- df_kastner[, c(2, 1, 3, 4, 5)]
    kastnerF <- as.magpie(df_kastner, spatial = c(1,2), temporal = 3)

  getSets(kastnerF) <- c("ex", "im", "t", "data")

  # make self-self trade the amount from the trade matrix again, as opposed 
  # to total consumption which is what Kastner gives
  selfselfreg <- paste(regions, regions, sep = ".")
 kastnerF[selfselfreg, , ] <- trade[selfselfreg, , ]
 
  return(kastnerF)
}

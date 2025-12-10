#' @title clearCacheMagpie4
#' @description Clear memoise cache for all memoised functions in magpie4
#'
#' @importFrom memoise is.memoised forget
#' @export
#' @return no return, but cache cleared
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' x <- clearCacheMagpie4()
#' }
#'
#' @importFrom magclass setCells

clearCacheMagpie4 <- function() {
  tryCatch({
    # Get all objects in the magpie4 namespace
    magpie4Env <- asNamespace("magpie4")
    magpie4Objects <- ls(magpie4Env)

    # Clear cache for all memoised functions in magpie4
    cleared <- 0
    for (objName in magpie4Objects) {
      obj <- get(objName, envir = magpie4Env)
      if (is.function(obj) && is.memoised(obj)) {
        forget(obj)
        cleared <- cleared + 1
      }
    }
    cat(">>> Cleared memoise cache for ", cleared, " function(s) \n")
  }, error = function(e) {
    cat(">>> Warning: Could not clear memoise cache: ", e$message, " \n")
  })
}

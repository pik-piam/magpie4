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
    magpie4_env <- asNamespace("magpie4")
    magpie4_objects <- ls(magpie4_env)

    # Clear cache for all memoised functions in magpie4
    cleared <- 0
    for (obj_name in magpie4_objects) {
      obj <- get(obj_name, envir = magpie4_env)
      if (is.function(obj) && is.memoised(obj)) {
        forget(obj)
        cleared <- cleared + 1
      }
    }
    cat(paste0(">>> Cleared memoise cache for ", cleared, " function(s)"))
  }, error = function(e) {
    cat(paste0(">>> Warning: Could not clear memoise cache: ", e$message))
  })
}

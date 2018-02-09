#' @title metadata_comments
#' @description set metadata comments to magpie4 objects
#' 
#' @export
#' 
#' @param x magpie object (magpie4)
#' @param unit provide unit
#' @param description provide short description
#' @param comment optional comment
#' @param note optional note
#' @importFrom magclass getComment getComment<-
#' @importFrom utils packageDescription
#' @return vector of comments following order of input (unit, description, comment, note - further: origin, creation data) 
#' @author Benjamin Bodirsky, Jannes Breier
#' @examples
#' 
#'   \dontrun{
#'     x <- metadata_comments(x,unit,description,comment,note)
#'   }
#' 

metadata_comments<-function(x, unit, description,comment,note){
  .prep_comment <- function(x, name, warning = NULL) {
    if (!is.null(x)) {
      x[1] <- paste0(" ", name, ": ", x[1])
      if (length(x) > 1) {
        x[2:length(x)] <- paste0(paste(rep(" ", 3 +
                                             nchar(name)), collapse = ""), x[2:length(x)])
      }
    }
    else {
      if (!is.null(warning)) {
        warning(warning)
        x <- paste0(" ", name, ": not provided")
      }
    }
    return(x)
  }

  
  unit <- .prep_comment(unit, "unit", paste0("Missing unit information for data set!"))
  description <- .prep_comment(description, "description",
                               paste0("Missing description for data set! Please add a description in the corresponding calc function!"))
  comment <- .prep_comment(comment, "comment")
  note <- .prep_comment(note, "note")
  origin <- .prep_comment(paste0(gsub("\\s{2,}", " ", paste(deparse(match.call()),
                                                            collapse = "")), " (magpie4 ", packageDescription("magpie4")$Version
                                 ,")"), "origin")
  date <- .prep_comment(date(), "creation date")
  
  getComment(x)<-c(unit, description,comment,note, origin,date)
  
  return(x)
}


#' Pull out certain variables from post names that follow the convention of yyyy-mm-dd-title.md
#'
#'
#' @param postNames a vector of post names that follow the convention yyyy-mm-dd-title.md
#' @param date output date? T/F
#' @param name output name? T/F
#'
#' @return a dataframe of variables
#'
#' @export
wranglePostNames <- function(postNames,
                             date = TRUE,
                             name = TRUE) {
  ## helper functions -----------------
  getDate <- function(postNames) {
    substring(postNames, 1, 10)
  }
  getName <- function(postNames) {
    substring(postNames, 12, )
  }

  ## pull out data --------------------
  dateRes <- getDate(postNames)
  nameRes <- getName(postNames)

  toKeep <- c(date, name)

  res <- data.frame(date = as.Date(dateRes),
                    name = nameRes,
                    stringsAsFactors = FALSE)
  # return object
  return(res[ , toKeep])
}

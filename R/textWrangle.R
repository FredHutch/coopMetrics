## main function to pull data from the GitHub Repository Statistics API
#' Pull out certain variables from post names that follow the convention of yyyy-mm-dd-title.md
#'
#'
#' @param postNames a vector of post names that follow the convention yyyy-mm-dd-title.md
#' @param date output date? T/F
#' @param name output name? T/F
#' @param day output day? T/F
#' @param month output month? T/F
#' @param year output year? T/F
#'
#' @return a dataframe of variables
#'
#' @export
wranglePostNames <- function(postNames,
                             date = TRUE,
                             name = TRUE,
                             day = TRUE,
                             month = TRUE,
                             year = TRUE) {
  ## helper functions -----------------
  getDate <- function(postNames) {
    substring(postNames, 1, 10)
  }
  getName <- function(postNames) {
    substring(postNames, 12, )
  }
  getDay <- function(postNames) {
    substring(postNames, 9, 10)
  }
  getMonth <- function(postNames) {
    substring(postNames, 6, 7)
  }
  getYear <- function(postNames) {
    substring(postNames, 1, 4)
  }

  ## pull out data --------------------
  dateRes <- getDate(postNames)
  nameRes <- getName(postNames)
  dayRes <- getDay(postNames)
  monthRes <- getMonth(postNames)
  yearRes <- getYear(postNames)

  toKeep <- c(date, name, day, month, year)

  res <- data.frame(date = dateRes,
                    name = nameRes,
                    day = dayRes,
                    month = monthRes,
                    year = yearRes,
                    stringsAsFactors = FALSE)
  # return object
  return(res[ , toKeep])
}

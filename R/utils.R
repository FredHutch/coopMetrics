#' Fill in missing months in a vector or dataframe column. If a dataframe is supplied the extra columns will be filled in with NA. Dates should be a date object.
#' @param data A vector or dataframe of dates in yyyy-mm-dd format. Filled in dates will be for the first of the month.
#' @param dateCol if a dataframe is supplied, what is the column name of the dates variable. Defaults to "month".

completeMonths <- function(data,
                           dateCol = NULL) {
  # check if data is a dataframe
  isDf <- is.data.frame(data)
  if (isDf) {
    dates <- data[[dateCol]]
  } else {
    dates <- data
  }
  # check that we're working with dates
  if (!is.Date(dates)) {
    stop("Provided vector or column is not in date format")
  }
  # if it's a dataframe return the specified date column
  # if it's a vector return
  completeRange <-  tibble(month = seq.Date(from = min(dates),
                                            to = max(dates),
                                            by = "month"))
  if (isDf) {
    res <- full_join(data, completeRange, by = "month")
    res <- res[order(res$month),]
  } else {
    res <- completeRange[["month"]]
  }
  return(res)
}

## Date handling
is.Ymd <- function(x) {
  !is.na(lubridate::ymd(x, quiet = TRUE))
}

checkDateFormat <- function(dates) {
  isYmd <- sapply(dates, is.Ymd)
  if (!all(isYmd)) {
    stop("ERROR: Dates must be in year-month-date format")
  }
}

dateRangeToStartEnd <- function(dateRange) {
  start <- floor_date(min(dateRange), "month")
  end <- ceiling_date(max(dateRange), "month") - 1

  return(list(start = start, end = end))
}

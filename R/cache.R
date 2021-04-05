#' Create a cache of data. This allows functions to run more quickly. Data is saved internally to the package as `R/sysdata.rda`. Currently, data cache only contains contributor data.
#' @param owner The owner of the repository that the blog lives in.
#' @param repo The repository name.
#' @param dateRange A vector of two dates in yyyy-mm-dd format. Can be strings or date objects. Order doesn't matter.
#' @param overwrite TRUE/FALSE. If TRUE, overwrites current data cache. If FALSE, will not overwrite current cache. If there is no existing cache this argument does nothing.
#'
#' @return A dataframe

createCache <- function(owner,
                        repo,
                        dateRange,
                        overwrite = FALSE) {
  # store cache date
  cacheDate <- Sys.Date()
  # get contributor data for cache
  contributorDataCache <- createContributorCache(owner, repo, dateRange, overwrite)
  # write cache
  message("Writing data cache to R/sysdata.rda")
  usethis::use_data(contributorDataCache, cacheDate, internal = TRUE, overwrite = overwrite)
}

#' Create a cache of contribuor data. This allows other functions to run more quickly. Data is saved internally as `R/sysdata.rda`. This function pulls all data from the last two years.
#' @param owner The owner of the repository that the blog lives in.
#' @param repo The repository name.
#' @param dateRange A vector of two dates in yyyy-mm-dd format. Can be strings or date objects. Order doesn't matter.
#' @param overwrite TRUE/FALSE. If TRUE, overwrites current data cache. If FALSE, will not overwrite current cache. If there is no existing cache this argument does nothing.
#'
#' @return A dataframe

createContributorCache <- function(owner,
                                   repo,
                                   dateRange,
                                   overwrite) {

  # dateRange validation
  # check is ymd
  checkDateFormat(dateRange)
  #check is date
  if (is.Date(dateRange) == FALSE) {
    dateRange <- ymd(dateRange)
  }
  dateRangeObj <- dateRangeToStartEnd(dateRange)
  # check that cache exists
  cacheExist <- checkCacheExists()
  # if cache exits, handle overwrite instructions
  if (cacheExist) {
    if (missing(overwrite)) {
      stop("No option is set for overwrite and a data cache exists. Please indicate TRUE/FALSE for overwrite.")
    } else if (overwrite & cacheExist) {
      message("Overwrite is set to TRUE. Removing stored data cache")
      file.remove(here::here("R/sysdata.rda"))
    } else if (!overwrite & cacheExist) { # overwrite = FALSE, stop function, recommend updateCache
      stop("Overwrite is set to FALSE. To update data cache use `updateCache()`")
    }
  } else {
    message("no cache found. Overwrite argument is overridden.")
  }
  # create contributor cached data
  contributorDataCache <- suppressWarnings(suppressMessages(getContributorDataJekyll(owner,
                                                                                     repo,
                                                                                     onlyUncached = FALSE))) %>%
    filter(commitDate >= dateRangeObj$start, commitDate <= dateRangeObj$end)
  return(contributorDataCache)
}

#' Checks whether or not the internal data cache (`R/sysdata.rda`) exists.
#'
#' @return TRUE/FALSE. TRUE, data cache exists. FALSE, data cache does not exist.
checkCacheExists <- function() {
  sysdata <- here::here("R/sysdata.rda")
  if (file.exists(sysdata)){
    load(sysdata)
    message(paste0("A data cache already exists. It was last updated on ", cacheDate, "."))
  } else {
    message("No data cache found.")
  }
  return(file.exists(sysdata))
}

overwriteCacheInteractive <- function() {
  sysdata <- here::here("R/sysdata.rda")
  overwrite <- readline(prompt = "Overwrite data cache? [Y / n]: ")
  # if overwrite = YES, remove sysdata
  if (toupper(overwrite) == "Y" | overwrite == "") {
    file.remove(sysdata)
  # if overwite = NO, stop function, recommend updateCache()
  } else if (toupper(overwrite) == "N") {
    # a little trick to remove the ERROR message from stop(). Still will act as an error in trycatch I think
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    message("Exiting function. To update data cache use `updateCache()`")
    stop()
  # if anything other than Y or N is provided send error message
  } else {
    stop("Prompt only takes \'Y\' or \'N\' as an input")
  }
}

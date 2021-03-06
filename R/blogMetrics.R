#' Pulls blog statistics for blogs using the GitHub Pages directory structure from both GitHub and Google Analytics.
#'
#' @param webPropertyName A variable from GoogleAnalytics. You can find out the webproperty name using `ga_account_list()` form the `GoogleAnalyticsR` package.
#' @param owner The owner of the relevant GitHub repo.
#' @param repo The repository name of the relevant GitHub repo.
#' @param dateRange A vector of two dates.
#'
#' @return a dataframe of metrics for the month and year specified.
#'
#' @export
getBlogStatistics <- function(webPropertyName,
                              owner,
                              repo,
                              dateRange,
                              useCache) {
  #validate daterange
  checkDateFormat(dateRange)
  isDate <- all(sapply(dateRange, is.Date))
  if (!isDate) {
    dateRange <- ymd(dateRange)
    }
  # pull google analytics data
  gaData <- googleAnalyticsMetrics(webPropertyName = webPropertyName,
                                   dateRange = dateRange)
  # pull github data
  ghData <- githubMetrics(owner = owner,
                          repo = repo,
                          dateRange = dateRange,
                          useCache = useCache)
  #rename cols
  names(ghData)[-1] <- paste0("gh_", names(ghData)[-1])
  names(gaData)[-1] <- paste0("ga_", names(gaData)[-1])
  #merge
  data <- left_join(ghData, gaData, by = "month")
  return(data)
}

# loadReportData <- function(dateRange) {
#   load("R/sysdata.rda")
#   m <- paste0("data last cached on ", as_date(cacheDate))
#   message(m)
#   list(blogMetrics, updated)
#
# }

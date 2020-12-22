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
                              dateRange) {
  #check date
  if (is.Date(dateRange) == FALSE) {
    dateRange <- ymd(dateRange)
  }

  end <- ceiling_date(max(dateRange), unit = "month") - 1
  start <- floor_date(min(dateRange), unit = "month")

  dateRange <- c(start, end)
  # pull google analytics data
  gaData <- pullGoogleAnalytics(webPropertyName = webPropertyName,
                                dateRange = dateRange)
  # pull github data
  ghData <- pullGithub(owner = owner,
                       repo = repo,
                       dateRange = dateRange)
  names(ghData)[-1] <- paste0("gh_", names(ghData)[-1])
  names(gaData)[-1] <- paste0("ga_", names(gaData)[-1])
  data <- left_join(ghData, gaData, by = "month")
  return(data)
}

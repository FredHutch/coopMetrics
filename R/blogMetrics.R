#' Pulls basic blog statistics for blogs using the GitHub Pages directory structure from both GitHub and Google Analytics.
#'
#' @param webPropertyName A variable from GoogleAnalytics. You can find out the webproperty name using `ga_account_list()` form the `GoogleAnalyticsR` package.
#' @param owner The owner of the relevant GitHub repo.
#' @param repo The repository name of the relevant GitHub repo.
#' @param month specify month as `mm` or `m`. Defaults to current month.
#' @param year specify year as `yyyy`. Defaults to current year.
#'
#' @return a dataframe of metrics for the month and year specified.
#'
#' @export
getBlogStatistics <- function(webPropertyName,
                              owner,
                              repo,
                              month = month(Sys.Date()),
                              year = year(Sys.Date())) {
  # pull google analytics data
  gaData <- pullGoogleAnalytics(webPropertyName,
                                month = month,
                                year = year,
                                .metrics = c("users", "newUsers", "sessions", "pageviews"),
                                .topPosts = TRUE)
  # pull github data
  ghData <- pullGithub(owner = owner,
                       repo = repo,
                       month = month,
                       year = year)
  data <- cbind(ghData, gaData)
  names(data) <- c(paste0("gh_", names(ghData)),
                   paste0("ga_", names(gaData)))
  return(data)
}

## main function to pull data from the google analytics API
#' Pull Google Analytics data from an account.
#'
#' Uses the `googleAnalyticsR` package to pull specified data from the Google Analytics API
#'
#' @param accountName specify the account you'd like to get data for, vector length of 1
#' @param dateRange specify the dates you'd like to pull data for, vector length 2. Defaults to the last 30 days.
#' @param metrics specify the metrics you wish to pull. Defaults:
#' @param auth specify if you need to authorize your account.
#'
#' @return a dataframe of variables
#'
#' @export
#' @import googleAnalyticsR
getGoogleAnalytics <- function(accountName,
                               dateRange = c((Sys.Date()- 30), Sys.Date()),
                               metrics = c("users","newUsers",
                                           "sessionsPerUser","pageviews"),
                               dimensions = NULL,
                               auth = FALSE){
  # run auth if true
  if (auth) {
    ga_auth()
  }
  # run checks
  .googleAnalyticsChecks(auth, accountName, dateRange, metrics)
  # get account
  accounts <- ga_account_list()
  accounts_subset <- accounts[grepl(accountName, accounts$accountName),]
  # pull data
  web_data <- google_analytics(accounts_subset$viewId,
                               date_range = dateRange,
                               metrics = metrics,
                               dimensions = dimensions,
                               anti_sample = TRUE)
  return(web_data)
}

## main function to pull data from the GitHub Repository Statistics API
#' Pull GitHub Repository Statistics data from an account.
#'
#' Uses the `gh` package to pull specified data from the GitHubRepository Statistics API
#'
#' @param
#'
#' @return a dataframe of variables
#'
#' @export
#' @import gh
getGithubStatistics <- function{}


## Function that get's the number of new posts
#' Compares posts from this month to last month using the Google Analytics API
#'
#' Uses the `googleAnalyticsR` package to pull specified data from the GitHubRepository Statistics API
#'
#' @param
#'
#' @return a dataframe of variables
#'
#' @export
#' @import googleAnalyticsR




getReportData <- function() {
  accountName <- "Coop"
  # pull in data
  usersRegion <- getGoogleAnalytics(accountName = accountName,
                                    metrics = c("users", "newUsers",
                                                "sessionsPerUser"),
                                    dimensions = "region")
  pageViews <- getGoogleAnalytics(accountName = accountName,
                                  metrics = c("pageviews"),
                                  dimensions = c("pagePath"))
  pageViewsPosts <- .viewOnlyPosts(pageViews,
                                   subPagesToRemove = c("^/coop/$", "/calendar/",
                                                        "/contributors/", "/posts/",
                                                        "/tags/", "/about/",
                                                        "/categories/"))
}

## helper functions -------------
.googleAnalyticsChecks <- function(auth, accountName, dateRange, metrics) {
  if (length(accountName) > 1) {
    stop("only handles one accountName at a time")
  }

  if (length(dateRange) != 2) {
    stop("must specify date range")
  }
}

.viewOnlyPosts <- function(pageViews, subPagesToRemove) {
  pattern <- paste(subPagesToRemove, collapse = "|")
  pageViews[!grepl(pattern, pageViews$pagePath, ),]
}





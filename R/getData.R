## main function to get pull data from the google analytics API
#' Pull Google Analytics data from an account.
#'
#' Uses the `googleAnalyticsR` package to pull specified data from the Google Analytics API
#'
#' @param accountName specify the account you'd like to get data for, vector length of 1
#' @param dateRange specify the dates you'd like to pull data for, vector length 2
#' @param metrics specify the metrics you wish to pull. Defaults:
#' @param auth specify if you need to authorize your account.
#'
#' @return a dataframe of variables
#'
#' @export
#' @import googleAnalyticsR
getGoogleAnalytics <- function(accountName,
                               dateRange,
                               metrics,
                               auth = FALSE){

  .googleAnalyticsChecks(auth, accountName, dateRange, metrics)
  accounts <- ga_account_list()
  accounts_subset <- accounts[grepl(accountName, my_accounts$accountName),]
  # pull data
  web_data <- google_analytics(accounts_subset$viewId,
                               date_range = c("2020-04-01", "2020-04-30"),
                               metrics = c("sessions","pageviews",
                                           "entrances","bounces"),
                               anti_sample = TRUE)
}

## helper functions -------------
.googleAnalyticsChecks <- function(auth, accountName, dateRange, metrics) {
  if (auth) {
    ga_auth()
  }

  if (lenth(accountName) > 1) {
    stop("only handles one accountName at a time")
  }

  if (length(dateRange) != 2) {
    stop("must specify date range")
  }
}

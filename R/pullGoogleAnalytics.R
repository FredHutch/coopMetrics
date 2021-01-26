## MAIN FUNCTION --------------------------------------------------------------

#' API pull from both Google Analytics.
#'
#' @param webPropertyName A variable from GoogleAnalytics. You can find out the webproperty name using `ga_account_list()`
#' @param dateRange A vector of two dates in yyyy-mm-dd format. Can be strings or date objects. Order doesn't matter, the max and min will be used.
#' @param gaAcctEmail Email of the google account associated with Google Analytics.
#'
#' @return a dataframe of monthly metrics from the date range specified.
#'
#' @export
#' @import lubridate
pullGoogleAnalytics <- function(webPropertyName,
                                dateRange,
                                gaAcctEmail) {
  ga_auth(email = gaAcctEmail)
  # Get viewId
  viewId <- webPropertyNameToViewId(webPropertyName = webPropertyName)

  userSessionData <- getUserSessionData(viewId,
                                        dateRange)
  topPages <- getPageViewsByPath(viewId,
                                 dateRange,
                                 onlyPosts = TRUE)
  googleAnalyticsData <- left_join(userSessionData, topPages, by = "month")
  return(googleAnalyticsData)
}


## PARSE ACCOUNT INFO ---------------------------------------------------------

#' Given a Google Analytics `webPropertyName` this function will pull the corresponding GoogleAnalytics account information
#'
#' @param webPropertyName a Google Analytics `webPropertyName`. You can find out the webproperty name using `ga_account_list()`.
#'
#' @return The view ID that corrosponds with the provided webPropertyName
#'
#' @export

webPropertyNameToViewId <- function(webPropertyName) {
  accounts <- ga_account_list()
  accounts_subset <- accounts[grepl(webPropertyName, accounts$webPropertyName),]

  if (length(accounts_subset$accountId) > 1) {
    stop("webPropertyName had more than one hit")
  }
  if (length(accounts_subset$accountId) == 0) {
    stop("webPropertuyName had no hits")
  }
  return(accounts_subset$viewId)
}

## PULL USER / SESSIONS -------------------------------------------------------

#' Given a  view ID and dateRange this function will pull the number of `users`, `newUsers`, `sessions`, and `pageviews` by month.
#'
#' @param viewId a Google Analytics `viewId`.
#' @param dateRange A vector of two dates in yyyy-mm-dd format. Can be strings or date objects. Order doesn't matter, the max and min will be used.

#' @return The view ID that corrosponds with the provided webPropertyName
#'
#' @export
getUserSessionData <- function(viewId,
                               dateRange) {
  if (is.Date(dateRange) == FALSE) {
    dateRange <- ymd(dateRange)
  }
  start <- floor_date(min(dateRange), unit = "month")
  end <- ceiling_date(max(dateRange), unit = "month") -1
  metrics <- c("users", "newUsers", "sessions", "pageviews")
  res <- google_analytics(viewId,
                   date_range = c(start, end),
                   metrics = metrics,
                   dimensions = c("month", "year"),
                   anti_sample = TRUE)
  res %>%
    mutate(month = as_date(paste0(year, "-", month, "-01"))) %>%
    select(-year)
}

## PULL PAGE VIEWS --------------------------------------------------------------

#' Using the Google analytics API pulls page views by page path for the date range specified.
#'
#' @param viewId A Google Analytics `viewId`. You can find out the webproperty name using `getAccountInfo()` or `ga_account_list()`.
#' @param dateRange A vector of two dates in yyyy-mm-dd format. Can be strings or date objects. Order doesn't matter, the max and min will be used.
#' @param onlyPosts A binary parameter. If TRUE only returns page path results for posts.

#' @return a dataframe of metrics for the month and year specified.
#'
#' @export

getPageViewsByPath <- function(viewId,
                               dateRange,
                               onlyPosts = TRUE){
  if (is.Date(dateRange) == FALSE) {
    dateRange <- ymd(dateRange)
  }
  start <- floor_date(min(dateRange), unit = "month")
  end <- ceiling_date(max(dateRange), unit = "month") -1

  pageViews <- google_analytics(viewId = viewId,
                                date_range = c(start, end),
                                metrics = c("pageviews"),
                                dimensions = c("pagePath", "month", "year"))
  # subset
  if (onlyPosts) {
    pageViews <- .keepPostsOnly(pageViews = pageViews,
                               subPagesToRemove = c("^/coop/$", "/calendar/",
                                                      "/contributors/", "/posts/",
                                                      "/tags/", "/about/",
                                                      "/categories/"))
  }
  topPagesByMonth<- pageViews %>%
    mutate(month = as_date(paste0(year, "-", month, "-01")))%>%
    select(-year) %>%
    group_by(month) %>%
    top_n(3) %>%
    summarise(mostViewed = paste0(pagePath, collapse = "; "))

  return(topPagesByMonth)
}

## HELPER FUNCTIONS -----------------------------------------------------------

.keepPostsOnly <- function(pageViews, subPagesToRemove) {
  pattern <- paste(subPagesToRemove, collapse = "|")
  pageViews <- pageViews[!grepl(pattern, pageViews$pagePath, ),]
  # Order by most viewed to least
  pageViews <- pageViews[order(pageViews$pageviews, decreasing = TRUE),]
  return(pageViews)
}


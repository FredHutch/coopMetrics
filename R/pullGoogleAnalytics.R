## MAIN FUNCTION --------------------------------------------------------------

#' API pull from both Google Analytics and GitHub specifically for a GitHub pages website build using Jekyll.
#'
#' @param webPropertyName A variable from GoogleAnalytics. You can find out the webproperty name using `ga_account_list()`
#' @param month specify month as `mm` or `m`. Defaults to current month.
#' @param year specify year as `yyyy`. Defaults to current year.
#'
#' @return a dataframe of metrics for the month and year specified.
#'
#' @export
#' @import lubridate
pullGoogleAnalytics <- function(webPropertyName,
                                dateRange) {
  ga_auth()
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
#' @param year specify year as `yyyy`. Defaults to current year.
#'
#' @return a dataframe of metrics for the month and year specified.
#'
#' @export

webPropertyNameToViewId <- function(webPropertyName) {
  accounts <- ga_account_list()
  accounts_subset <- accounts[grepl(webPropertyName, accounts$webPropertyName),]

  if (length(accounts_subset$accountId) >1) {
    stop("webPropertyName had more than one hit")
  }
  if (length(accounts_subset$accountId) == 0) {
    stop("webPropertuyName had no hits")
  }
  return(accounts_subset$viewId)
}

## PULL USER / SESSIONS -------------------------------------------------------

getUserSessionData <- function(viewId,
                               dateRange) {
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
#' @param dateRange A vector of two date objects.
#' @param onlyPosts A binary parameter. If TRUE only returns page path results for posts.
#' @param topThree A binary parameter. If TRUE only returns the top three most viewed page paths.

#' @return a dataframe of metrics for the month and year specified.
#'
#' @export

getPageViewsByPath <- function(viewId,
                               dateRange,
                               onlyPosts = TRUE){
  start <- floor_date(min(dateRange), unit = "month")
  end <- ceiling_date(max(dateRange), unit = "month") -1

  pageViews <- google_analytics(viewId = viewId,
                                date_range = c(start, end),
                                metrics = c("pageviews"),
                                dimensions = c("pagePath", "month", "year"))
  # subset
  if (onlyPosts) {
    pageViews <- keepPostsOnly(pageViews = pageViews,
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

keepPostsOnly <- function(pageViews, subPagesToRemove) {
  pattern <- paste(subPagesToRemove, collapse = "|")
  pageViews <- pageViews[!grepl(pattern, pageViews$pagePath, ),]
  # Order by most viewed to least
  pageViews <- pageViews[order(pageViews$pageviews, decreasing = TRUE),]
  return(pageViews)
}


###################
## MAIN FUNCTION ##
###################
#' API pull from
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
                                month = month(Sys.Date()),
                                year = year(Sys.Date()),
                                .metrics = c("users", "newUsers", "sessions", "pageviews"),
                                .topPosts = TRUE) {
  ga_auth()
  # Get viewId
  viewId <- getAccountInfo(webPropertyName = webPropertyName, onlyViewId = TRUE)

  # Convert month/year to a date range
  dateRangeList <- monthYear2DateRange(month = month,
                                       year = year)
  # First webdata pull (uses metrics specified in function arguments)
  webData <- pullWebData(viewId = viewId,
                         dateRange = dateRangeList$range,
                         metrics = .metrics,
                         dimensions = NULL)
  # Pull post data by path
  topPages <- getPageViewsByPath(viewId = viewId,
                                 dateRange = dateRangeList$range,
                                 onlyPosts = TRUE,
                                 ordered = TRUE,
                                 topThree = .topPosts)
  # Collapse and merge
  webData$topPages <- paste0(topPages$pagePath, collapse = "; ")
  rownames(webData) <- paste(month, year, sep = "_")
  return(webData)
}

###############

monthYear2DateRange <- function(month,
                                year) {
  # hard code beginning of month as the first
  first <- ymd(paste(year, month, "01", sep = "-"))
  # check if it's current month/year
  thisMonth <- month(Sys.Date())
  thisYear <- year(Sys.Date())
  if (month == thisMonth & year == thisYear) {
    last <- Sys.Date()
  } else {
    # get end of the month
    last <- ceiling_date(first, 'month') - 1
  }

  res <- list(first = first,
              last = last,
              range = c(first, last))

  return(res)

}

getAccountInfo <- function(webPropertyName,
                           onlyViewId = TRUE) {
  accounts <- ga_account_list()
  accounts_subset <- accounts[grepl(webPropertyName, accounts$webPropertyName),]

  if (length(accounts_subset$accountId) >1) {
    stop("webPropertyName had more than one hit")
  }
  if (length(accounts_subset$accountId) == 0) {
    stop("webPropertuyName had no hits")
  }
  ifelse(onlyViewId,
         return(accounts_subset$viewId),
         return(accounts_subset))

}

pullWebData <- function(viewId,
                        dateRange,
                        metrics,
                        dimensions) {
  webData <- google_analytics(viewId,
                              date_range = dateRange,
                              metrics = metrics,
                              dimensions = dimensions,
                              anti_sample = TRUE)
  return(webData)
}

getPageViewsByPath <- function(viewId,
                               dateRange,
                               onlyPosts = TRUE,
                               ordered = TRUE,
                               topThree = TRUE){
  pageViews <- pullWebData(viewId = viewId,
                           dateRange = dateRange,
                           metrics = c("pageviews"),
                           dimensions = c("pagePath"))
  # subset
  if(onlyPosts){
    pageViews <- .gaViewOnlyPosts(pageViews = pageViews,
                                 subPagesToRemove = c("^/coop/$", "/calendar/",
                                                      "/contributors/", "/posts/",
                                                      "/tags/", "/about/",
                                                      "/categories/"),
                                 ordered = ordered)
  }

  if (topThree) {
    pageViews <- pageViews[1:3,]
  }
  return(pageViews)
}

## helper functions -------------
.gaViewOnlyPosts <- function(pageViews, subPagesToRemove, ordered = TRUE) {
  pattern <- paste(subPagesToRemove, collapse = "|")
  pageViews <- pageViews[!grepl(pattern, pageViews$pagePath, ),]
  # Order by most viewed to least
  if (ordered) {
    pageViews <- pageViews[order(pageViews$pageviews, decreasing = TRUE),]
  }

  return(pageViews)
}

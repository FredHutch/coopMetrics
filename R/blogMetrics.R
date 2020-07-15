pullGoogleAnalytics <- function(webPropertyName = "Coop blog",
                                dateRange = c((Sys.Date()- 30), Sys.Date()),
                                auth = FALSE) {
  # go through oauth for the google account that owns the website
  if (auth) {
    ga_auth()
  }

  # get viewID for webpull
  viewId <- getAccountInfo(webPropertyName,
                           onlyViewId = TRUE)

  # general data pull
  metrics <- c("users", "newUsers", "sessions", "pageviews")
  webData <- pullWebData(viewId = viewId,
                         dateRange = dateRange,
                         metrics = metrics,
                         dimensions = dimensions)
  # get pageviews data
  pageViewsbyPath <- getPageViewsByPath(onlyPosts = TRUE,
                                        ordered = TRUE,
                                        topThree = TRUE)
  # pull out top pages
  webData$topPosts <- paste0(pageViewsbyPath$pagePath, collapse = ", ")

  return(webData)
}

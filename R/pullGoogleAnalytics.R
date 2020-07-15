getAccountInfo <- function(webPropertyName,
                           onlyViewId = TRUE) {
  accounts <- ga_account_list()
  accounts_subset <- accounts[grepl(accountName, accounts$webPropertyName),]

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
                        dateRange = c((Sys.Date()- 30), Sys.Date()),
                        metrics,
                        dimensions) {
  webData <- google_analytics(accounts_subset$viewId,
                               date_range = dateRange,
                               metrics = metrics,
                               dimensions = dimensions,
                               anti_sample = TRUE)

  rownames(webData) <- paste0(dateRange, collapse = " - ")
  return(webData)
}

getPageViewsByPath <- function(onlyPosts = TRUE,
                               ordered = TRUE,
                               topThree = TRUE){
  pageViews <- pullWebData(viewId = viewId,
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

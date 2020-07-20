### Brief look at github api with `gh`

## install gh
#install.packages("gh")
library(gh)
# install usethis and edit r environ to include
# gh pat
library(usethis)
#usethis::edit_r_environ()
# restart R and
# make sure it worked
Sys.getenv("GITHUB_PAT")
# pull my repos
my_repos <- gh("GET /users/:username/repos", username = "lakikowolfe")
# unlist
my_repos <- vapply(my_repos, "[[", "", "name")

# try and pull fred hutch repos
fh_repos <- gh("GET /users/:username/repos", username = "FredHutch", page = 2)
fh_repos <- vapply(fh_repos, "[[", "", "name")

# pull stats about FH/coop repo contributors
fh_coop <- gh("GET /repos/:owner/:repo/stats/contributors",
              owner = "FredHutch",
              repo = "coop")
# pull stats about FH/coop commits
fh_coop_commits <- gh("GET /repos/:owner/:repo/stats/commit_activity",
                      owner = "FredHutch",
                      repo = "coop")
posts <- gh("GET /repos/:owner/:repo/contents/:path",
      owner = "FredHutch",
      repo = "coop",
      path = "_posts")

# loop over posts to pull out titles
lapply(seq(1:length(posts)), function(i) {
  postName <- posts[[i]]$name
  return(postName)
})



## Brief look at google analytics with googleAnalyticsR
# install
install.packages("googleAnalyticsR")
library(googleAuthR)
library(googleAnalyticsR)
# set up authorization
ga_auth()
# look at accounts
my_accounts <- ga_account_list()
# pull out view data for the coop blog
coop_acct <- my_accounts[grepl("Coop", my_accounts$accountName),]
coop_viewid <- coop_acct$viewId
# query for some basic data
web_data <- google_analytics(coop_viewid,
                               date_range = c("2020-04-01", "2020-04-30"),
                               metrics = c("sessions","pageviews",
                                           "entrances","bounces"),
                               anti_sample = TRUE)




#### USING COOPMETRICS
coopBlog <- "Coop blog"
month <- 05
year <- 2020

viewId <- getAccountInfo(coopBlog,
                         onlyViewId = TRUE)

googleAnalyticsMetrics <- c("users", "newUsers", "sessions", "pageviews")

dateRangeDf <- monthYear2DateRange(month = month,
                                   year = year)

webData <- pullWebData(viewId = viewId,
                       dateRange = dateRangeDf$range,
                       metrics = googleAnalyticsMetrics,
                       dimensions = NULL)

topPages <- getPageViewsByPath(viewId = viewId,
                               dateRange = dateRangeDf$range,
                               onlyPosts = TRUE,
                               ordered = TRUE,
                               topThree = TRUE)

webData$topPages <- paste0(topPages$pagePath, collapse = "; ")


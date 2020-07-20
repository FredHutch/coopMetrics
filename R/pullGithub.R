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
getGithubStatistics <- function(month = NULL, year = NULL) {
  # get posts
  postNames <- getFileNames()
  posts <- postsThisMonth(postNames, month = month, year = year)
  # calculate number of posts this month
  postsNum <- length(posts$date)
  # get commit info from last year
  commitObj <- getCommits()
  # subset commits
  commitObjThisMonth <- subCommitObjThisMonth(commitObj,
                                                month = month,
                                                year = year)
  # calculate number of commits this month
  commitNum <- calcTotalCommits(commitObjThisMonth)
}

#################################################
## POSTS ----------------------------------------
#################################################

#' Get the names of posts from your github pages _posts repo
#'
#' Uses the `gh` package to pull the names of posts from the `_posts` directory
#'
#' @param owner github pages repo owner. Default = "FredHutch".
#' @param repo github pages repo name. Default = "coop".
#'
#' @return a vector of postNames from the `_posts` directory
#'
#' @export
#' @import gh
getFileNames <- function(owner = "FredHutch",
                         repo = "coop",
                         path = "_posts") {
  fileList <- gh("GET /repos/:owner/:repo/contents/:path",
              owner = owner,
              repo = repo,
              path = path)

  fileNames  <- lapply(seq(1:length(fileList)), function(i) {
    fileName <- fileList[[i]]$name
    return(fileName)})
  return(unlist(fileNames))
}

#' Get the postNames of post from the specified month and year (if left unspecified the current month/year is assumed)
#'
#' Gets postNames from month (`m`) and year (`yyyy`) specified
#'
#' @param postNames vector of postNames in `yyyy-mm-dd-name.md` format
#' @param month month to get postNames for in `m` format
#' @param year year to get postNames for in `yyyy` format
#'
#' @return a df of postNames from this months with other variables
#'
#' @export
#' @import lubridate
postsThisMonth <- function(postNames,
                           month = NULL,
                           year = NULL) {
  ## Pull out month and year if not specified ---
  monthYear <- .checkMonthYear(month, year)

  ## get dates from postNames -------------------
  postNamesWrangled <- wranglePostNames(postNames)
  postNamesWrangled$postNames <- postNames
  postNamesWrangled$monthYear <- paste0(month(postNamesWrangled$date), "_",
                                        year(postNamesWrangled$date))

  ## find match ---------------------------------
  match <- postNamesWrangled[postNamesWrangled$monthYear == monthYear,]
  return(match)
}

#################################################
## COMMITS --------------------------------------
#################################################

getCommits <- function(owner = "FredHutch",
                       repo = "coop") {
  commitObj <- gh("GET /repos/:owner/:repo/stats/commit_activity",
                   owner = "FredHutch",
                   repo = "coop")
}

getCommitWeeks <- function(commitObj) {
  weeksList <- lapply(seq(1:length(commitObj)), function(i) {
    week <- commitObj[[i]]$week
    return(week)
  })
  weeksVec <- unlist(weeksList)
  weeksDate <- as_datetime(weeksVec)
  return(weeksDate)
}

subCommitObjThisMonth <- function(commitObj,
                                   month = NULL,
                                   year = NULL) {
  require(lubridate)
  # check month and year, return month_year
  monthYear <- .checkMonthYear(month, year)
  # get dates from weeksVec
  weeksVec <- getCommitWeeks(commitObj)
  # pull month and year, create monthsYear
  days <- day(weeksVec)
  months <- month(weeksVec)
  years <- year(weeksVec)
  monthsYears <- paste0(months, "_", years)
  # create logical vector where TRUE = commit weeks to keep
  toKeep <- monthsYears == monthYear
  # subset commit object
  resObj <- commitObj[toKeep]

  return(resObj)
}

calcTotalCommits <- function(commitObj) {
  totals <- lapply(seq(1:length(commitObj)), function(i){
    totalNum <- commitObj[[i]]$total
    return(totalNum)
  })
  totals <- unlist(totals)
  return(sum(totals))
}

#################################################
## CONTRIBUTORS ---------------------------------
#################################################

getPaths <- function(owner = "FredHutch",
                     repo = "coop",
                     path = "_contributors"){
  fileNames <- getFileNames(owner = owner,
                            repo = repo,
                            path = path)
  path <- file.path(path, fileNames)
  return(path)
}

path2OldestCommitDate <- function(owner = owner,
                            repo = repo,
                            path = path){
  commitObj <- gh("GET /repos/:owner/:repo/commits",
                  owner = owner,
                  repo = repo,
                  path = path)
  commitNum <- length(commitObj)
  oldestCommitDate <- as_datetime(commitObj[[max(commitNum)]]$commit$author$date)
  return(oldestCommitDate)
}

contributorsAndDates <- function(owner = "FredHutch",
                                 repo = "coop",
                                 path = "_contributors",
                                 ordered = TRUE){
  paths <- getPaths()
  contributorDateList <- lapply(seq(1:length(paths)), function(i){
    oldestCommitDate <- path2OldestCommitDate(owner,
                                            repo,
                                            paths[i])
    resDf <- data.frame(path = paths[i],
                        commitDate = oldestCommitDate,
                        stringsAsFactors = FALSE)
    return(resDf)
  })
  contributorDateDf <- do.call(rbind.data.frame, contributorDateList)
  if (ordered) {
    contributorDateDf <- contributorDateDf[order(contributorDateDf$commitDate),]

  }

  return(contributorDateDf)
}


newContributorPaths <- function(contributorDateDf,
                                first,
                                last) {
  newPathsThisMonth <- contributorDateDf[contributorDateDf$commitDate > first & contributorDateDf$commitDate < last, ]
  return(newPathsThisMonth)
}

path2Contributor <- function(contributorPaths) {
  id <- gsub("_contributors\\/|.md", "", contributorPaths)
  return(id)
}

#################################################
## HELPER FUNCTIONS -----------------------------
#################################################

# Quick search for a PAT that has the term 'github' in it
.checkGithubPat <- function() {
  pats <- names(Sys.getenv())
  patBin <- grepl("GITHUB", pats)
  patExists <- any(patBin)
  ghPat <- pats[patBin]

  messageTrue <- paste0("PAT found: ", ghPat)
  messageFalse <- "No PAT with the term `github` found"
  ifelse(patExists,
         messageTrue,
         messageFalse)
  return(patExists)
}

.ifMonthNull <- function() {
  message("No month specified, using current month")
  month <- month(Sys.Date())
  return(month)
}

.ifYearNull <- function() {
  message("No year specified, using current year")
  year <- year(Sys.Date())
}

.checkMonthYear <- function(month = NULL, year = NULL) {
  if (is.null(month)) {
    month <- .ifMonthNull()
  }
  if (is.null(year)) {
    year <- .ifYearNull()
  }
  return(paste0(month, "_", year))
}

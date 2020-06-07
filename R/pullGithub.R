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
getGithubStatistics <- function() {

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
getPostNames <- function(owner = "FredHutch",
                         repo = "coop") {
  postsList <- gh("GET /repos/:owner/:repo/contents/:path",
              owner = owner,
              repo = repo,
              path = "_posts")

  postNames  <- lapply(seq(1:length(postsList)), function(i) {
    postName <- posts[[i]]$name
    return(postName)})
  return(unlist(postNames))
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
  commitsObj <- gh("GET /repos/:owner/:repo/stats/commit_activity",
                   owner = "FredHutch",
                   repo = "coop")
}

getCommitWeeks <- function(commitsObj) {
  weeksList <- lapply(seq(1:length(commitsObj)), function(i) {
    week <- commitsObj[[i]]$week
    return(week)
  })
  weeksVec <- unlist(weeksList)
  return(weeksVec)
}

subCommitsObjThisMonth <- function(weeksVec,
                                   commitsObj,
                                   month = NULL,
                                   year = NULL) {
  # check month and year, return month_year
  monthYear <- .checkMonthYear(month, year)
  # get dates from weeksVec
  weeksDate <- as_datetime(weeksVec)
  # pull month and year, create monthsYear
  months <- month(weeksDate)
  years <- year(weeksDate)
  monthsYears <- paste0(months, "_", years)
  # create logical vector where TRUE = commit weeks to keep
  toKeep <- monthsYears == monthYear
  # subset commit object
  resObj <- commitsObj[toKeep]

  return(resObj)
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
  message("No month specified, using current year")
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

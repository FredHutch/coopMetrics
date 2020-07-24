###################
## MAIN FUNCTION ##
###################

pullGithub <- function(owner = "FredHutch",
                       repo = "coop",
                       month = month(Sys.Date()),
                       year = year(Sys.Date())) {
  # Get post information
  postNames <- getFileNames(owner = owner,
                            repo = repo,
                            path = "_posts")
  posts <- postsThisMonth(postNames,
                          month = month,
                          year = year)
  githubData <- data.frame(postNum = length(posts$date))
  # get commit info
  commitObj <- getCommitActivity(owner = owner,
                                 repo = repo)
  commitObjOneMonth <- subsetCommitActivityByMonth(commitObj,
                                                    month = month,
                                                    year = year)
  githubData$commitNum <- calcTotalCommits(commitObjOneMonth)
  # get contributor info
  contributorDateDf <- listFilesandEarliestCommitDate(owner = owner,
                                                      repo = repo,
                                                      path = "_contributors",
                                                      ordered = TRUE)
  contributorData <- calcContributorStats(contributorDateDf,
                                          month = month,
                                          year= year)

  githubData <- cbind(githubData, contributorData)
  row.names(githubData) <- paste(month, year, sep = "_")

  return(githubData)
}


#################################################
## POSTS ----------------------------------------
#################################################

#' Get the names of files from your a specified directory in a github pages repo
#'
#' Uses the `gh` package to pull the file names of files from the specified directory.
#'
#' @param owner github pages repo owner. Default = "FredHutch".
#' @param repo github pages repo name. Default = "coop".
#' @param path github pages path (including "_") of directory. Default = "_posts".
#'
#' @return a vector of file names from the specified directory in `path` argument.
#'
#' @export
#' @import gh
getFileNames <- function(owner = "FredHutch",
                         repo = "coop",
                         path = "_posts",
                         month = NULL,
                         year = NULL) {
  fileList <- gh("GET /repos/:owner/:repo/contents/:path",
              owner = owner,
              repo = repo,
              path = path)

  fileNames  <- lapply(seq(1:length(fileList)), function(i) {
    fileName <- fileList[[i]]$name
    return(fileName)})
  fileNames <- unlist(fileNames)
  return(fileNames)
}

#' Get the post names of post from the specified month and year (if left unspecified the current month/year is assumed)
#'
#' Gets postNames from month (`m`) and year (`yyyy`) specified
#'
#' @param postNames vector of postNames in `yyyy-mm-dd-name.md` format
#' @param month month to get postNames for in `m` format
#' @param year year to get postNames for in `yyyy` format
#'
#' @return a df of posts names from this months with other variables
#'
#' @export
#' @import lubridate
postsThisMonth <- function(fileNames,
                           month = NULL,
                           year = NULL) {
  ## Pull out month and year if not specified ---
  monthYear <- paste0(month, "_", year)
  ## get dates from postNames -------------------
  postNamesWrangled <- wranglePostNames(fileNames)
  postNamesWrangled$fileNames <- fileNames
  postNamesWrangled$fileNames <- paste0(month(postNamesWrangled$date), "_",
                                        year(postNamesWrangled$date))

  ## find match ---------------------------------
  match <- postNamesWrangled[postNamesWrangled$monthYear == monthYear,]
  return(match)
}

#################################################
## COMMITS --------------------------------------
#################################################

#' Get an object of commit activity from a specified repository
#'
#' @param owner string of the repository owner. Default "FredHutch".
#' @param repo string of the repository name. Default "coop".
#'
#' @export
#'
getCommitActivity <- function(owner = "FredHutch",
                       repo = "coop") {
  commitObj <- gh("GET /repos/:owner/:repo/stats/commit_activity",
                   owner = "FredHutch",
                   repo = "coop")
  return(commitObj)
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

#' Get an object of commit activity from a specified repository
#'
#' @param commitObj Object returned from an `gh` API pull of commit activity.
#' @param month month to subset object for in `m` format.
#' @param year year to subset object for in `yyyy` format.
#'
#' @return A subsetted commitObj based on the month/year we are gathering information for.
#'
#' @export
#' @import lubridate
subsetCommitActivityByMonth <- function(commitObj,
                                        month = month(Sys.Date()),
                                        year = year(Sys.Date())) {
  monthYear <- paste(month, year, sep = "_")
  # get dates from weeksVec
  weeksVec <- getCommitWeeks(commitObj)
  # use month and year to create monthsYear
  months <- month(weeksVec)
  years <- year(weeksVec)
  monthsYears <- paste0(months, "_", years)
  # create logical vector where TRUE = commit weeks to keep
  toKeep <- monthsYears == monthYear
  # subset commit object
  resObj <- commitObj[toKeep]

  return(resObj)
}

#' Parses an object returned by `getCommitActivity` and calculates the total number of commits that occured during the time frame encapsulated in the object.
#'
#' @param commitObj Object returned from an `gh` API pull of commit activity.
#'
#' @return The total number of commits
#'
#' @export
#'

calcTotalCommits <- function(commitObj) {
  totals <- lapply(seq(1:length(commitObj)), function(i){
    totalNum <- commitObj[[i]]$total
    return(totalNum)
  })
  totals <- unlist(totals)
  return(sum(totals))
}

#################################################
## CONTRIBUTORS
#################################################

getPaths <- function(owner = "FredHutch",
                     repo = "coop",
                     path = "_contributors") {
  fileNames <- getFileNames(owner = owner,
                            repo = repo,
                            path = path)
  path <- file.path(path, fileNames)
  return(path)
}

path2OldestCommitDate <- function(owner = owner,
                            repo = repo,
                            path = path) {
  commitObj <- gh("GET /repos/:owner/:repo/commits",
                  owner = owner,
                  repo = repo,
                  path = path)
  commitNum <- length(commitObj)
  oldestCommitDate <- as_datetime(commitObj[[max(commitNum)]]$commit$author$date)
  return(oldestCommitDate)
}

listFilesandEarliestCommitDate <- function(owner = "FredHutch",
                                           repo = "coop",
                                           path = "_contributors",
                                           ordered = TRUE) {
  paths <- getPaths(owner = owner, repo = repo, path = path)
  fileDateList <- lapply(seq(1:length(paths)), function(i){
    oldestCommitDate <- path2OldestCommitDate(owner,
                                              repo,
                                              paths[i])
    resDf <- data.frame(path = paths[i],
                        commitDate = oldestCommitDate,
                        stringsAsFactors = FALSE)
    return(resDf)
  })
  # bind into a dataframe
  fileDateDf <- do.call(rbind.data.frame, fileDateList)
  if (ordered) {
    fileDateDf <- fileDateDf[order(fileDateDf$commitDate),]

  }

  return(fileDateDf)
}


newFilesWithinDateRange <- function(fileDateDf,
                                    first,
                                    last) {
  newPathsThisMonth <- fileDateDf[fileDateDf$commitDate > first & fileDateDf$commitDate < last, ]
  return(newPathsThisMonth)
}

path2Contributor <- function(contributorPaths) {
  id <- gsub("_contributors\\/|.md", "", contributorPaths)
  return(id)
}

calcContributorStats <- function(contributorDateDf,
                                 month,
                                 year) {
  dateRange <- monthYear2DateRange(month = month, year = year)
  numTotalContributors <- length(contributorDateDf$path)
  newContributorDf <- newFilesWithinDateRange(fileDateDf = contributorDateDf,
                                              first = dateRange$first,
                                              last = dateRange$last)
  numNewContributors <- length(newContributorDf$path)
  newContributorNames <- path2Contributor(newContributorDf$path)

  res <- data.frame(numTotalContributors = numTotalContributors,
                    numNewContributors = numNewContributors,
                    newContributorNames = paste0(newContributorNames, collapse = "; "))
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

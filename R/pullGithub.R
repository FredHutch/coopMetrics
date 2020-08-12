###################
## MAIN FUNCTION ##
###################
#' API pull from
#'
#' @param owner The repository name of the relevant GitHub repo.
#' @param repo The owner of the relevant GitHub repo.
#' @param month specify month as `mm` or `m`. Defaults to current month.
#' @param year specify year as `yyyy`. Defaults to current year.
#'
#' @return a dataframe of metrics for the month and year specified.
#'
#' @export
#' @import lubridate
pullGithub <- function(owner = "FredHutch",
                       repo = "coop",
                       dateRange) {
  # Get contributor information
  calcContributorStats(owner = owner,
                       repo = repo,
                       dateRange = dateRange)
  return(githubData)
}

#################################################
## PULL REPO CONTENTS --------------------------
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
                         path = NULL) {
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

#' Given the repository owner and name and a specific path within that repo this function will return a vector of filepaths within that directory.
#'
#' @param owner The owner of the repository to pull file paths from. Defaults to "FredHutch".
#' @param repo The name of the repository to pull file paths from. Defaults to "coop".
#' @param path The directory name to pull file paths from. Defaults to "_contributors".
#'
#' @return a vector of file paths.
#'
#' @export
#'

getPaths <- function(owner = "FredHutch",
                     repo = "coop",
                     path = NULL) {
  fileNames <- getFileNames(owner = owner,
                            repo = repo,
                            path = path)
  path <- file.path(path, fileNames)
  return(path)
}


#################################################
## PULL REPO COMMITS ----------------------------
#################################################

#' This function pulls the the oldest commit date for the specified path.
#'
#' @param owner The owner of the repository to pull file paths from.
#' @param repo The name of the repository to pull file paths from.
#' @param path The directory name to pull file paths from.
#'
#' @return a date object of the date that the specified file was first commited to the repository.
#'
#' @export
#'

pathToOldestCommitDate <- function(owner = owner,
                                  repo = repo,
                                  path = path) {
  commitObj <- gh("GET /repos/:owner/:repo/commits",
                  owner = owner,
                  repo = repo,
                  path = path)
  commitNum <- length(commitObj)
  oldestCommitDate <- as_date(commitObj[[max(commitNum)]]$commit$author$date)
  return(oldestCommitDate)
}

#' This function pulls the earliest commit date for each file in a specified path.
#'
#' @param owner The owner of the repository to pull file paths from. Defaults to "FredHutch".
#' @param repo The name of the repository to pull file paths from. Defaults to "coop".
#' @param path The directory name to pull file paths from. Defaults to "_contributors".
#' @param ordered A binary parameter. If set to TRUE the dataframe returned will be ordered by newest to oldest file.
#'
#' @return a date object of the date that the specified file was first commited to the repository.
#'
#' @export
#'
listFilesandEarliestCommitDate <- function(owner = "FredHutch",
                                           repo = "coop",
                                           path = "_contributors",
                                           ordered = TRUE) {
  paths <- getPaths(owner = owner, repo = repo, path = path)
  fileDateList <- lapply(seq(1:length(paths)), function(i){
    oldestCommitDate <- pathToOldestCommitDate(owner,
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

pullCommitNum  <- function(owner,
                           repo,
                           dateRange) {
  start <- min(dateRange)
  end <- max(dateRange)

  commitObj <- gh("GET /repos/:owner/:repo/commits",
                  owner = "FredHutch",
                  repo = "coop",
                  since = start,
                  until = end)
  # unnest commitDates from list of lists
  # flatten to a vector
  # coerce to date
  dates <- commitObj %>%
    map("commit") %>%
    map("committer") %>%
    map("date") %>%
    flatten_chr() %>%
    as_date()

  commitsByMonth <- tibble(commitDate = dates) %>%
    mutate(month = floor_date(commitDate, unit = "month")) %>%
    group_by(month) %>%
    summarise(numCommits = n())

  return(commitsByMonth)
}

#################################################
## POSTS ----------------------------------------
#################################################

#' Get the post names of post from the specified month and year (if left unspecified the current month/year is assumed)
#'
#' Gets postNames from month (`m`) and year (`yyyy`) specified
#'
#' @param fileNames vector of file names in `yyyy-mm-dd-name.md` format
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
  postNamesWrangled$monthYear <- paste0(month(postNamesWrangled$date), "_",
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

#' Given paths from the `_contributors` directory this function will return just the contributor ID.
#'
#' @param contributorPaths A vector of paths from the `_contributors` directory.
#' @return A vector of contributor IDs from the `_contributors` directory.
#'
#' @export
#'
pathToContributor <- function(contributorPaths) {
  id <- gsub("_contributors\\/|.md", "", contributorPaths)
  return(id)
}

compareToKnownContributorCache <- function(owner = "FredHutch",
                                           repo = "coop") {
  # load cache
  load("R/sysdata.rda")
  # pull current contributor filepaths
  contributors <- getPaths(owner = owner,
                           repo = repo,
                           path = "_contributors")
  uncachedContributorPath <- setdiff(contributors, knownContributorData$path)
  return(uncachedContributorPath)
}

pullContributorData <- function(contributorPath,
                                owner = "FredHutch",
                                repo = "coop") {
  # pull earlist commit date for unknown paths
  contributorDates <- contributorPath %>%
    map_dbl(~ pathToOldestCommitDate(owner = owner,
                                    repo = repo,
                                    path = .x)) %>%
    as_date()
  contributorData <- tibble(path = contributorPath,
                            commitDate = contributorDates,
                            handle = pathToContributor(contributorPath))
  return(contributorData)
}

calcContributorCommitData <- function(contributorData) {
  contributorDataCount <- contributorData %>%
    group_by(month) %>%
    summarise(numNewContributors = n(),
              handles = paste0(handle, collapse = ", ")) %>%
    mutate(totalContributors = cumsum(numNewContributors)) %>%
    select(month, totalContributors, numNewContributors, handles)

  return(contributorDataCount)
}

completeMonths <- function(df,
                           dateRange) {
  completeRange <- tibble(month = seq.Date(from = min(dateRange),
                                             to = max(dateRange),
                                             by = "month"))
  completeTbl <- full_join(df, completeRange) %>%
    arrange(month) %>%
    mutate_at(c("totalContributors", "numNewContributors"), ~replace(., is.na(.), 0))

  return(completeTbl)
}

#' This function calculates statistics such as the total number of contributors, number of new contributors, and their names for each month in a given date range
#'
#' @param owner The owner of the repository that the blog lives in.
#' @param repo The repository name.
#' @param dateRange A vector of two date objects indicating the date range of interest.
#'
#' @return
#'
#' @export
#'
calcContributorStats <- function(owner,
                                 repo,
                                 dateRange) {
  # load known contributor cache
  load("R/sysdata.rda")
  contributorData <- knownContributorData
  # compare known contributors (generated by data-raw/DATA.R and stored in R/sysdata.rda)
  # to unknown contributors (contributors with a file in _contributors but with no data stored in sysdata.rda)
  # known contributor list will be updated as needed and noted as a new version of the package.
  uncachedContributorPath <- compareToKnownContributorCache(owner = owner, repo = repo)
  # if there are uncached contributors pull contributor data for these paths
  if (length(uncachedContributorPath) != 0) {
    uncachedContributorData <- pullContributorData(uncachedContributorPath, owner = owner, repo = repo)
    contributorData <- bind_rows(contributorData, uncachedContributorData)
  }
  # take contributorData
  # mutate commitDate to month (yyyy-mm-01)
  # calls function calcContributorCommitData
  # filter output by dateRange provided
  # fill in missing months with zeros
  contributorsByMonthYear <-  contributorData %>%
    mutate(month = floor_date(commitDate, unit = "month")) %>%
    calcContributorCommitData() %>%
    # need to pipe an ifelse statement into here
    # if there are no entries within date range need to carry over
    # info about total contributors from the latest month
    completeMonths(dateRange = dateRange) %>%
    filter(month > min(dateRange) & month < max(dateRange))

  return(contributorsByMonthYear)
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

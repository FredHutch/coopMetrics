## MAIN FUNCTION --------------------------------------------------------------

#' Pull repository statistics for specified GitHub repository. Specifically repositories set up to host a Jekyll/GitHub Pages site.
#'
#' @param owner The repository name of the relevant GitHub repo.
#' @param repo The owner of the relevant GitHub repo.
#' @param dateRange A vector of two dates.
#'
#' @return a dataframe of metrics for the date range specified.
#'
#' @export
#' @import lubridate
pullGithub <- function(owner = "FredHutch",
                       repo = "coop",
                       dateRange) {
  # Get contributor information
  contributorInfo <- calcContributorNum(owner = owner,
                                        repo = repo,
                                        dateRange = dateRange)
  # Get the number of commits
  commitInfo <- calcCommitNum(owner = owner,
                              repo = repo,
                              dateRange = dateRange)
  postInfo <- calcPostNum(owner = owner,
                          repo = repo,
                          dateRange = dateRange,
                          by = "postName")
  githubData <- left_join(contributorInfo, commitInfo, by = "month") %>%
    left_join(., postInfo, by = "month") %>%
    select(month, numCommits, numPostTotal, numNewPosts, totalContributors, numNewContributors, handles)
  return(githubData)
}

## PULL REPO CONTENTS ---------------------------------------------------------

#' Get the names of files in a specified directory in a github pages repo
#'
#' Uses the `gh` package to pull the file names of files from the specified directory.
#'
#' @param owner github pages repo owner. Default = "FredHutch".
#' @param repo github pages repo name. Default = "coop".
#' @param path github pages path (including "_") of directory.
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
  fileNames <- fileList %>% map_chr("name")
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

## PULL REPO COMMITS ---------------------------------------------------------------

#' This function pulls the the oldest commit date for the specified filepath. This can be used to determine when a file was first pushed to the repository.
#'
#' @param owner The owner of the repository to pull file paths from.
#' @param repo The name of the repository to pull file paths from.
#' @param filepath The directory name to pull file paths from.
#'
#' @return a date object of the date that the specified file was first commited to the repository.
#'
#' @export
#'

filepathToOldestCommitDate <- function(owner,
                                       repo,
                                       filepath) {
  commitObj <- gh("GET /repos/:owner/:repo/commits",
                  owner = owner,
                  repo = repo,
                  path = filepath)
  commitNum <- length(commitObj)
  oldestCommitDate <- as_date(commitObj[[max(commitNum)]]$commit$author$date)
  return(oldestCommitDate)
}

#' Lists commits for specified repository and dateRange. Set to return the max amount per page (100).
#' @param owner The owner of the repository to pull file paths from.
#' @param repo The name of the repository to pull file paths from.
#' @param dateRange A vector of two dates.
#' @param pageNum Page number of the results to return.
#'
#' @return a date object of the date that the specified file was first commited to the repository.
#'
#' @export
#'
getCommitObj <- function(owner,
                         repo,
                         dateRange,
                         pageNum) {
  # function only works on full months
  # adjust dateRange to the first of the start month until the last day of the end month
  start <- floor_date(min(dateRange), "month")
  end <- ceiling_date(max(dateRange), "month") - 1
  # pull commits for the month range specified
  commitObj <- gh("GET /repos/:owner/:repo/commits",
                  owner = "FredHutch",
                  repo = "coop",
                  since = start,
                  until = end,
                  per_page = 100,
                  page = pageNum)
  return(commitObj)
}

## COMMITS --------------------------------------------------------------------

#' calculates the number of commits to a given repository over a specified date range.
#'
#' @param owner The owner of the repository to pull file paths from.
#' @param repo The name of the repository to pull file paths from.
#' @param dateRange A vector of two dates.
#'
#' @return a date object of the date that the specified file was first commited to the repository.
#'
#' @export

calcCommitNum  <- function(owner,
                           repo,
                           dateRange) {

  dateRange <- as.POSIXct(dateRange + 1)
  ## Pull commit object
  # gh api only returns max of 100 per request
  # iteratively pull until less than the max are returned
  commitObj <- NULL
  commitObjList <- list()
  i <- 1
  while (length(commitObj) == 100 | is.null(commitObj)) {
    commitObj <- getCommitObj(owner = owner,
                              dateRange = dateRange,
                              repo = repo,
                              pageNum = i)
    commitObjList <- append(commitObjList, commitObj)
    i <- i + 1
  }
  # unnest commitDates from list of lists
  # flatten to a vector
  # coerce to date
  dates <- commitObjList %>%
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

## POSTS ----------------------------------------------------------------------

#' Calculate the number of posts per month in a Jekyll/GitHub pages repository.
#'
#' @param owner The owner of the repository to pull file paths from.
#' @param repo The name of the repository to pull file paths from.
#' @param dateRange A vector of two dates.
#' @param by "postNames" is the only option currently. Until I can determine a better way to calculate posts.
#'
#' @return a dataframe of number of posts per month
#'
#' @export
#' @import lubridate
calcPostNum <- function(owner,
                        repo,
                        dateRange,
                        by = "postName") {

  if (by != "postName") {
    message("function does not handle other options for `by` at this time")
  }
  end <- ceiling_date(max(dateRange), unit = "month") - 1
  start <- floor_date(min(dateRange), unit = "month")

  # get filenames from "_posts" directory
  #
  postDf <- tibble(postNames = getFileNames(owner = owner,
                                            repo = repo,
                                            path = "_posts")) %>%
    mutate(date = as_date(str_sub(postNames, start = 1, end = 10))) %>%
    mutate(month = floor_date(date, unit = "month")) %>%
    group_by(month) %>%
    summarise(numNewPosts = n()) %>%
    completeMonths(dateRange = dateRange) %>%
    mutate_at("numNewPosts", ~replace(., is.na(.), 0)) %>%
    mutate(numPostTotal = cumsum(numNewPosts))%>%
    filter(month >= start & month <= end)

  return(postDf)
}

## CONTRIBUTORS ---------------------------------------------------------------

#' Given a filepath from the `_contributors` directory this function will return just the contributor ID.
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

#' Compares current contributor files in repository to known cache of contributors.
#' @param owner The owner of the repository to pull file paths from.
#' @param repo The name of the repository to pull file paths from.
#'
#' @export
#'

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

#' Gets oldest commit date and handle for provided contributor path for a Jekyll/Github Pages repository.
#' @param contributorPath A path to a contributor file.
#' @param owner The owner of the repository to pull file paths from.
#' @param repo The name of the repository to pull file paths from.
#'
#' @export
#'

pullContributorData <- function(contributorPath,
                                owner = "FredHutch",
                                repo = "coop") {
  # pull earlist commit date for unknown paths
  contributorDates <- contributorPath %>%
    map_dbl(~ filepathToOldestCommitDate(owner = owner,
                                         repo = repo,
                                         filepath = .x)) %>%
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
    arrange(month)
  return(completeTbl)
}

#' This function calculates the number of new contributors and total contributors
#'
#' @param owner The owner of the repository that the blog lives in.
#' @param repo The repository name.
#' @param dateRange A vector of two dates.
#'
#' @return
#'
#' @export
#'
calcContributorNum <- function(owner,
                               repo,
                               dateRange) {
  # dateRange to start and end date rounded to the month
  start <- floor_date(min(dateRange), unit = "months")
  end <- ceiling_date(max(dateRange), unit = "months")
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
    completeMonths(dateRange = dateRange) %>%
    mutate_at("numNewContributors", ~replace(., is.na(.), 0)) %>%
    mutate(totalContributors = cumsum(numNewContributors)) %>%
    filter(month >= start & month <= end)

  return(contributorsByMonthYear)
}

## HELPER FUNCTIONS -----------------------------------------------------------

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

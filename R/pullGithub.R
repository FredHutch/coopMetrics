## MAIN FUNCTION --------------------------------------------------------------

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
    left_join(., postInfo, by = "month")

  return(githubData)
}

## PULL REPO CONTENTS ---------------------------------------------------------

#' Get the names of files from your a specified directory in a github pages repo
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
#' @param path The directory name to pull file paths from.
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

getCommitObj <- function(owner,
                         repo,
                         dateRange) {
  # function only works on full months
  # adjust dateRange to the first of the start month until the last day of the end month
  start <- floor_date(min(dateRange), "month")
  end <- ceiling_date(max(dateRange), "month") - 1
  # pull commits for the month range specified
  commitObj <- gh("GET /repos/:owner/:repo/commits",
                  owner = "FredHutch",
                  repo = "coop",
                  since = start,
                  until = end)
  return(commitObj)
}

## COMMITS --------------------------------------------------------------------

calcCommitNum  <- function(owner,
                           repo,
                           dateRange) {

  commitObj <- getCommitObj(owner = owner,
                            dateRange = dateRange,
                            repo = repo)
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

## POSTS ----------------------------------------------------------------------

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
    map_dbl(~ filepathToOldestCommitDate(owner = owner,
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
    arrange(month)
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
    filter(month >= start & month <= end)
  # if no contributors are added during the date range specified contributorsByMonthYear will be a dataframe filled with
  # NAs rather than carrying forth the info from previous months. Need to fill it in with the correct info.
  noTotal <- all(is.na(contributorsByMonthYear$totalContributors))
  if (noTotal) {
    contributorsByMonthYear$totalContributors <- nrow(contributorData)
    contributorsByMonthYear$numNewContributors <- 0
  }

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

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


## COMMITS --------------------------------------------------------------------

#' calculates the number of commits/month to a given repository over a specified date range.
#'
#' @param owner The GitHub repository owner
#' @param repo The GitHub repository name
#' @param dateRange A vector of two dates in yyyy-mm-dd format. Can be strings or date objects. Order doesn't matter, the max and min will be used.
#'
#' @return a date object of the date that the specified file was first commited to the repository.
#'
#' @export

calcCommitNum  <- function(owner,
                           repo,
                           dateRange) {
  # check dateRange format and update to date if needed
  if (is.Date(dateRange) == FALSE) {
    dateRange <- ymd(dateRange)
  }
  ## Pull commit object
  commitObjList <- getCommits(owner, repo, dateRange)
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
#' @param owner The GitHub repository owner
#' @param repo The GitHub repository name
#' @param dateRange A vector of two dates in yyyy-mm-dd format. Can be strings or date objects. Order doesn't matter, the max and min will be used.
#' @param by "postNames" is the only option currently. Until I can determine a better way to calculate posts.
#'
#' @return a dataframe of number of posts per month
#'
#' @export
#' @import lubridate
calcPostNum <- function(owner,
                        repo,
                        dateRange) {
  #check date
  if (is.Date(dateRange) == FALSE) {
    dateRange <- ymd(dateRange)
  }

  end <- ceiling_date(max(dateRange), unit = "month") - 1
  start <- floor_date(min(dateRange), unit = "month")

  # get filenames from "_posts" directory
  #
  postDf <- tibble(postPath = getDirContents(owner = owner,
                                             repo = repo,
                                             dir = "_posts",
                                             fullPath = FALSE)) %>%
    mutate(date = as_date(str_sub(postPath, start = 1, end = 10))) %>%
    mutate(month = floor_date(date, unit = "month")) %>%
    group_by(month) %>%
    summarise(numNewPosts = n()) %>%
    mutate_at("numNewPosts", ~replace(., is.na(.), 0)) %>%
    mutate(numPostTotal = cumsum(numNewPosts))%>%
    filter(month >= start & month <= end)

  return(postDf)
}

## CONTRIBUTORS ---------------------------------------------------------------

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
  contributors <- getDirContents(owner = owner,
                                 repo = repo,
                                 dir = "_contributors",
                                 fullPath = TRUE)
  uncachedContributorPath <- setdiff(contributors, knownContributorData$path)
  return(uncachedContributorPath)
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


#' This function calculates the number of new contributors and total contributors
#'
#' @param owner The owner of the repository that the blog lives in.
#' @param repo The repository name.
#' @param dateRange A vector of two dates in yyyy-mm-dd format. Can be strings or date objects. Order doesn't matter, the max and min will be used.
#'
#' @return
#'
#' @export
#'
calcContributorNum <- function(owner,
                               repo,
                               dateRange) {
  if (is.Date(dateRange) == FALSE) {
    dateRange <- ymd(dateRange)
  }
  # dateRange to start and end date rounded to the month
  start <- floor_date(min(dateRange), unit = "months")
  end <- ceiling_date(max(dateRange), unit = "months")
  # load known contributor cache
  load("R/sysdata.rda")
  contributorData <- knownContributorData
  # compare known contributors (generated by data-raw/DATA.R and stored in R/sysdata.rda)
  # to unknown contributors (contributors with a file in _contributors but with no data stored in sysdata.rda)
  # known contributor list will be updated as needed and noted as a new version of the package.
  uncachedContributorPath <- compareToKnownContributorCache(owner = owner,
                                                            repo = repo)
  # if there are uncached contributors pull contributor data for these paths
  if (length(uncachedContributorPath) != 0) {
    uncachedContributorData <- pullContributorData(uncachedContributorPath,
                                                   owner = owner,
                                                   repo = repo)
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

## HELPER FUNCTIONS FOR COMMIT PULL ------------------------------------------------------
#' Lists commits for specified repository and dateRange. Set to return the max amount per page (100).
#' @param owner The GitHub repository owner
#' @param repo The GitHub repository name
#' @param dateRange A vector of two dates in yyyy-mm-dd format. Can be strings or date objects. Order doesn't matter, the max and min will be used.
#' @param pageNum Page number of the results to return
#'
#' @return
#'

getCommitObj <- function(owner,
                         repo,
                         dateRange,
                         pageNum) {
  # check that dateRange is in correct format
  if (is.Date(dateRange) == FALSE) {
    dateRange <- ymd(dateRange)
  }
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

getCommits <- function(owner,
                       repo,
                       dateRange) {
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
  return(commitObjList)
}


## HELPER FUNCTIONS FOR CONTRIBUTOR PULL -------------------------------------------------

#' Gets oldest commit date and handle for provided contributor path for a Jekyll/Github Pages repository. Used to pull data into the DATA.R script.
#' @param contributorPath A path to a contributor file or directory containing contributor files.
#' @param owner The owner of the repository to pull file paths from.
#' @param repo The name of the repository to pull file paths from.
#'
#' @export
#'

pullContributorData <- function(contributorPath,
                                owner = "FredHutch",
                                repo = "coop") {
  # check if a dir or direct file path
  mdPresent <- any(grepl(".md", contributorPath))
  allMd <- all(mdPresent)
  # if mdpresent is true and all md is true you have direct file paths to contributor mds
  if (mdPresent & allMd) {
    paths <- contributorPath
  } else if (!mdPresent & !allMd) {
    paths <- getDirContents(owner = owner,
                            repo = repo,
                            dir = contributorPath,
                            fullPath = TRUE)
  } else {
    stop("contributor paths provided need to all end in md or be a directory")
  }

  # pull earlist commit date for unknown paths
  contributorDates <- paths %>%
    map_dbl(~ filepathToOldestCommitDate(owner = owner,
                                         repo = repo,
                                         filepath = .x)) %>%
    as_date()
  contributorData <- tibble(path = paths,
                            commitDate = contributorDates,
                            handle = pathToContributor(paths))
  return(contributorData)
}

## GENERIC HELPER FUNCTIONS ------------------------------------------------------

#' Get the names of files in a specified directory in a github repo
#'
#' Uses the `gh` package to pull the file names from a specified directory.
#'
#' @param owner the GitHub repository owner
#' @param repo The GitHub repository name
#' @param dir The directory name or path to directory
#' @param fullPath TRUE returns the full filepath, FALSE returns filename
#'
#' @return a vector of files located in the specified directory.
#'
#' @import gh
getDirContents <- function(owner,
                           repo,
                           dir,
                           fullPath = TRUE) {
  fileList <- gh("GET /repos/:owner/:repo/contents/:path",
                 owner = owner,
                 repo = repo,
                 path = dir)
  fileNames <- fileList %>% map_chr("name")
  #return full path?
  if (fullPath) {
    fileNames <- file.path(dir, fileNames)
  }
  return(fileNames)
}

#' Given a filepath from the `_contributors` directory this function will return just the contributor ID.
#'
#' @param contributorPaths A vector of paths from the `_contributors` directory.
#' @return A vector of contributor IDs from the `_contributors` directory.
#'
pathToContributor <- function(contributorPaths) {
  id <- gsub("_contributors\\/|.md", "", contributorPaths)
  return(id)
}


#' This function pulls the the oldest commit date for the specified filepath. This can be used to determine when a file was first pushed to the repository.
#'
#' @param owner the GitHub repository owner
#' @param repo The GitHub repository name
#' @param filepath Path to the file to return first commit date from
#'
#' @return a date object of the date that the specified file was first commited to the repository.
#'
#' @export
#' @import lubridate

filepathToOldestCommitDate <- function(owner,
                                       repo,
                                       filepath) {
  # check that only 1 filepath is provided
  if (length(filepath) != 1) {
    stop("This function can only take a single filepath")
  }
  commitObj <- gh("GET /repos/:owner/:repo/commits",
                  owner = owner,
                  repo = repo,
                  path = filepath)
  # check that commitObj returned something
  if (length(commitObj) == 0) {
    stop("path returns no commits")
  }
  commitNum <- length(commitObj)
  oldestCommitDate <- as_date(commitObj[[max(commitNum)]]$commit$author$date)
  return(oldestCommitDate)
}

# not exactly sure why I made this fxn but not ready to toss yet
completeMonths <- function(df,
                           dateRange) {
  completeRange <- tibble(month = seq.Date(from = min(dateRange),
                                           to = max(dateRange),
                                           by = "month"))
  completeTbl <- full_join(df, completeRange) %>%
    arrange(month)
  return(completeTbl)
}

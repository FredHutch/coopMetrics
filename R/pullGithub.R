## MAIN FUNCTION -------------------------------------------------------------------------

#' Pull repository statistics for specified GitHub repository. Specifically repositories set up to host a Jekyll/GitHub Pages site.
#'
#' @param owner The repository name of the relevant GitHub repo.
#' @param repo The owner of the relevant GitHub repo.
#' @param dateRange A vector of two dates.
#' @param useCache TRUE/FALSE.
#'
#' @return a dataframe of metrics for the date range specified.
#'
#' @export
#' @import lubridate
githubMetrics <- function(owner,
                          repo,
                          dateRange,
                          useCache) {
  # validate dateRange format is ymd
  checkDateFormat(dateRange)
  # validate that dateRange are date objects, if not make them
  isDate <- all(sapply(dateRange, is.Date))
  if (!isDate) {
    dateRange <- ymd(dateRange)
  }
  # get metrics
  contributor <- contributorMetrics(owner = owner,
                                    repo = repo,
                                    dateRange = dateRange,
                                    useCache = useCache)
  commit <- commitMetrics(owner = owner,
                          repo = repo,
                          dateRange = dateRange)
  post <- postMetrics(owner = owner,
                      repo = repo,
                      dateRange = dateRange)
  # merge
  githubData <- left_join(contributor, commit, by = "month") %>%
    left_join(., post, by = "month") %>%
    select(month, numCommits, numPostTotal, numNewPosts, totalContributors, numNewContributors, handles)
  return(githubData)
}

## COMMITS -------------------------------------------------------------------------------

#' Pull and calculate commit metric (commits per month over a time frame)
#' @param owner The GitHub repository owner
#' @param repo The GitHub repository name
#' @param dateRange A vector of two dates in yyyy-mm-dd format. Can be strings or date objects. Order doesn't matter, the max and min will be used.

commitMetrics <- function(owner,
                          repo,
                          dateRange) {
  # modify dateRange format
  dateRange <- dateRangeToStartEnd(dateRange)
  # pull commits over dateRange
  commitObj <- getCommits(owner,
                          repo,
                          start = dateRange$start,
                          end = dateRange$end)
  # create dataframe aggregating commits by month
  commitTbl <- calcCommitNum(commitObj)

  return(commitTbl)
}

## POSTS ---------------------------------------------------------------------------------

#' Pull and calculate posts metric (posts per month over a time frame)
#' @param owner The GitHub repository owner
#' @param repo The GitHub repository name
#' @param dateRange A vector of two dates in yyyy-mm-dd format. Can be strings or date objects. Order doesn't matter, the max and min will be used.

postMetrics <- function(owner,
                        repo,
                        dateRange) {
  # get _post directory contents
  posts <- getDirContents(owner = owner,
                          repo = repo,
                          dir = "_posts",
                          fullPath = FALSE)
  # calculate posts/month using post file name
  # Jekyll posts always have yyyy-mm-dd in post title
  postTbl <- calcPostNumJekyll(posts,
                               dateRange)

  return(postTbl)
}
## CONTRIBUTORS --------------------------------------------------------------------------

#' This function calculates the number of new contributors and total contributors by month
#'
#' @param owner The owner of the repository that the blog lives in.
#' @param repo The repository name.
#' @param dateRange A vector of two dates in yyyy-mm-dd format. Can be strings or date objects. Order doesn't matter, the max and min will be used.
#' @param useCache TRUE/FALSE. When TRUE, use cached data. When FALSE, pull all data. Using cached data will make the function run faster.
#'
#' @return
#'
#' @export

contributorMetrics <- function(owner,
                               repo,
                               dateRange,
                               useCache = TRUE) {
  dateRange <- dateRangeToStartEnd(dateRange)
  # if useCache is TRUE, only pull new contributors, will merge with cached
  # if useCache is FALSE, pull all contributor data
  contributorData <- getContributorDataJekyll(owner = owner,
                                              repo = repo,
                                              onlyNew = useCache)
  # if useCache is TRUE check that cache exists
  if (!file.exists(here::here("R/sysdata.rda"))) {
    stop("useCache = TRUE but no cache is found. Use createCache() to create store your data locally.")
  }
  # if useCache is TRUE
  # load cache
  # merge with the contributor data of new contributors
  if (useCache) {
    load("R/sysdata.rda")
    contributorData <- suppressMessages(full_join(knownContributorData, contributorData))
  }

  contributorsByMonthYear <-  contributorData %>%
    mutate(month = floor_date(commitDate, unit = "month")) %>%
    aggregateContributorData() %>%
    completeMonths(dateCol = "month") %>%
    mutate_at("numNewContributors", ~replace(., is.na(.), 0)) %>%
    mutate(totalContributors = cumsum(numNewContributors)) %>%
    filter(month >= dateRange$start & month <= dateRange$end)

  return(contributorsByMonthYear)
}

## HELPER FUNCTIONS FOR COMMIT METRICS ---------------------------------------------------
#' Lists commits for a specified repository over a date range.
#' @param owner The GitHub repository owner.
#' @param repo The GitHub repository name.
#' @param start Beginning of date range in yyyy-mm-dd format. Can be strings or date objects.
#' @param end End of date range in yyyy-mm-dd format. Can be strings or date objects.
#'
#' @return A list where each element contains information about a single commit including author, date, and commit message.

getCommits <- function(owner,
                       repo,
                       start,
                       end) {
  commitObj <- NULL
  commitObjList <- list()
  i <- 1
  while (length(commitObj) == 100 | is.null(commitObj)) {
    commitObj <- gh("GET /repos/:owner/:repo/commits",
                    owner = owner,
                    repo = repo,
                    since = start,
                    until = end,
                    per_page = 100,
                    page = i)
    commitObjList <- append(commitObjList, commitObj)
    i <- i + 1
  }
  return(commitObjList)
}

#' Calculates the number of commits/month to a given repository over a specified date range.
#'
#' @param commitObjList The result from `getCommits()` or `gh("GET /repos/:owner/:repo/commits, ... )`. A list where each element contains all info relevant to a single commit.
#'
#' @return a tibble that shows the number of commits to a repository by month. Tibble has two columns: `month`, `numCommits`.

calcCommitNum  <- function(commitObjList) {
  # unnest commitDates from list of lists
  # flatten to a vector
  # coerce to date
  dates <- commitObjList %>%
    purrr::map("commit") %>%
    purrr::map("committer") %>%
    purrr::map("date") %>%
    purrr::flatten_chr() %>%
    lubridate::as_date()
  commitTbl <- tibble(commitDate = dates) %>%
    dplyr::mutate(month = lubridate::floor_date(commitDate, unit = "month")) %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(numCommits = n())

  return(commitTbl)
}

## HELPER FUNCTIONS FOR CONTRIBUTOR PULL -------------------------------------------------

#' Pulls contributor paths from a static Jekyll blog hosted on GitHub pages.
#' @param owner The owner of the repository to pull file paths from.
#' @param repo The name of the repository to pull file paths from.
#' @param onlyNew If TRUE, only returns new, uncached contributors. If FALSE, returns all contributors.
#'
#' @export

getContributorPathsJekyll <- function(owner,
                                      repo,
                                      onlyNew = c(TRUE, FALSE)) {
  # pull current contributor filepaths
  contributorPaths <- getDirContents(owner = owner,
                                     repo = repo,
                                     dir = "_contributors",
                                     fullPath = TRUE)
  # check that cache exists
  cacheExist <- file.exists(here::here("./R/sysdata.rda"))
  # if cache exists, load and setdiff with knownContributors
  # only pull info for new contributors and merge with known.
  if (onlyNew & cacheExist) {
    message("loading cached data")
    load(here::here("./R/sysdata.rda"))
    message(paste0("data last cached on ", cacheDate))
    newContributorPaths <- setdiff(contributorPaths, knownContributorData$path)
    return(newContributorPaths)
  } else if (!onlyNew & cacheExist) {
    return(contributorPaths)
  } else if (!cacheExist) {
    warning("cache does not exist. Loading all contributors.")
    return(contributorPaths)
  }
}

#' Gets oldest commit date and handle for provided contributor paths for a Jekyll/Github Pages repository.
#' @param owner The owner of the repository to pull file paths from.
#' @param repo The name of the repository to pull file paths from.
#' @param onlyNew If TRUE, only returns new, uncached contributors. If FALSE, returns all contributors.
#'
#' @return A tibble with three columns: `path` contains the relative path to the contributor .md file, `commitDate` contains the earliest commit date for each contributor file, and `handle` has the contributors handle (the filename without .md on the end)
#' @export

getContributorDataJekyll <- function(owner,
                                     repo,
                                     onlyNew = TRUE) {
  # pull contributor paths from the repo
  paths <- getContributorPathsJekyll(owner = owner,
                                     repo = repo,
                                     onlyNew = onlyNew)
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

#' Gets oldest commit date and handle for provided contributor paths for a Jekyll/Github Pages repository.
#' @param contributorData takes the output of `getContributorDataJekyll()`. See docs for description of output.

aggregateContributorData <- function(contributorData) {
  contributorDataAgg <- contributorData %>%
    mutate(month = floor_date(commitDate, unit = "month")) %>%
    group_by(month) %>%
    summarise(numNewContributors = n(),
              handles = paste0(handle, collapse = ", ")) %>%
    mutate(totalContributors = cumsum(numNewContributors)) %>%
    select(month, totalContributors, numNewContributors, handles)
  # fill in missing months
  contributorTbl <- completeMonths(contributorDataAgg, "month")
  return(contributorTbl)
}

## HELPER FUNCTIONS FOR POSTS ------------------------------------------------------------
#' Calculate the number of posts per month in a Jekyll GitHub pages repository.
#'
#' @param posts a vector of post file names.
#' @param dateRange A vector of two dates in yyyy-mm-dd format. Can be strings or date objects. Order doesn't matter, the max and min will be used.
#'
#' @return a dataframe of number of posts per month

calcPostNumJekyll <- function(posts,
                              dateRange) {
  dateRange <- dateRangeToStartEnd(dateRange)
  # get filenames from "_posts" directory
  # use dates in filenames to parse by dateRange
  postTbl <- as_date(str_sub(posts, start = 1, end = 10)) %>%
    tibble(post = posts, date = .) %>%
    mutate(month = floor_date(date, unit = "month"))%>%
    group_by(month) %>%
    summarise(numNewPosts = n()) %>%
    mutate_at("numNewPosts", ~replace(., is.na(.), 0)) %>%
    mutate(numPostTotal = cumsum(numNewPosts))%>%
    filter(month >= dateRange$start & month <= dateRange$end)

  return(postTbl)
}

## OTHER HELPER FUNCTIONS ----------------------------------------------------------------

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
  # most recent commit is always the first row. Oldest commit is last.
  oldestCommitDate <- as_date(commitObj[[max(commitNum)]]$commit$author$date)
  return(oldestCommitDate)
}

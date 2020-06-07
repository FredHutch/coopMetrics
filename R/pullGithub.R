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
  if (is.null(month)) {
    message("No month specified, using current month")
    month <- month(Sys.Date())
  }

  if (is.null(year)) {
    message("No month specified, using current year")
    year <- year(Sys.Date())
  }
  monthYear <- paste0(month, "_", year)

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
## HELPER FUNCTIONS -----------------------------

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

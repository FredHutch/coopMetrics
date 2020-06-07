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

getPostNames <- function() {
  postsList <- gh("GET /repos/:owner/:repo/contents/:path",
              owner = "FredHutch",
              repo = "coop",
              path = "_posts")

  postNames  <- lapply(seq(1:length(postsList)), function(i) {
    postName <- posts[[i]]$name
    return(postName)})
  return(unlist(postNames))
}

PostsThisMonth <- function(date = NULL, postNames) {
  if (is.null(month)) {
    message("No date specified, using current month")
    date <- Sys.Date()
  }

}

## helper functions -------------
# Quick search for a PAT that has the term 'github' in it
.checkGithubPat <- function() {
  pats <- names(Sys.getenv())
  patBin <- grepl("GITHUB", pats)
  patExists <- any(patBin)
  ghPat <- pats[patBin]

  messageTrue <- paste0("PAT found: ", ghPat)
  messageFalse <- "No PAT with the term `github` found"
  return(ifelse(patExists,
                messageTrue,
                messageFalse))
}

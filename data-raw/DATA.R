library(coopMetrics)

# Set variables
owner <- "FredHutch"
repo <- "coop"

# Code to prepare known contributor dataset
knownContributorData <- listFilesandEarliestCommitDate(owner = owner,
                                                       repo = repo,
                                                       path = "_contributors",
                                                       ordered = TRUE)
knownContributorData$handle <- pathToContributor(knownContributorData$path)
cacheDate <- Sys.time()

usethis::use_data(knownContributorData, cacheDate, internal = TRUE, overwrite = TRUE)

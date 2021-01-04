## LOAD LIBRARY -------------------------------------------------------------------------
library(lubridate)
library(tidyverse)
library(gh)
library(googleAnalyticsR)

## SOURCE SCRIPTS -----------------------------------------------------------------------
sourceScripts <- TRUE # toggle between using .R scripts to source functions or loading the coopMetrics library

if(sourceScripts) {
  path <- file.path(here::here(), "R")
  scripts <- list.files(path, pattern = "R", full.names = TRUE)
  lapply(scripts, source)
} else {
  library(coopMetrics)
}

## SET VARIABLES ------------------------------------------------------------------------
webPropertyName <- "Coop blog"
owner <- "FredHutch"
repo <- "coop"
monthsAgo <- 12 # values is the number of months back we want to collect data from
begin <- ymd(Sys.Date()) - months(monthsAgo)
end <- ymd(Sys.Date())
dateRange <- c(begin, end)

## PREPARE KNOWN CONTRIBUTOR DATA -------------------------------------------------------
load("R/sysdata.rda") # loads known contributorData
uncachedContributor <- getUncachedContributorPath(owner = owner,
                                                  repo = repo,
                                                  knownContributors = knownContributorData)
uncachedContributorData <- tryCatch(
  {
    getContributorData(uncachedContributor,
                       owner = owner,
                       repo = repo)
  },
  error=function(cond) {
    message(cond)
    return(NULL)
  }
)

if(is.null(uncachedContributorData)) {
  message("no new contributor data to be added")
  updatedContributorData <- knownContributorData
} else {
  message("updating contributor data")
  updatedContributorData <- bind_rows(knownContributorData, uncachedContributorData)
}

## PREPARE BLOG DATA --------------------------------------------------------------------
blogMetrics <- getBlogStatistics(webPropertyName = webPropertyName,
                                 owner = owner,
                                 repo = repo,
                                 dateRange = dateRange)

## SAVE DATE CACHED ---------------------------------------------------------------------
cacheDate <- Sys.Date()

## RENAME UPDATED CONTRIUBTOR DATA
knownContributorData <- updatedContributorData

## SAVE DATA ----------------------------------------------------------------------------
usethis::use_data(knownContributorData, cacheDate, blogMetrics, internal = TRUE, overwrite = TRUE)

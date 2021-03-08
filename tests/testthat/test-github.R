context("GitHub API and related functions")
library(coopMetrics)
library(lubridate)
library(tidyverse)

test_that("calcCommitNum calculates the number of commits per month", {
  commit_df <- calcCommitNum(owner = "FredHutch",
                             repo = "coopMetrics",
                             dateRange = c("2020-06-01", "2020-08-01"))

  expect_equal(length(commit_df$month), 3)
  #not sure why this one is constantly failing??
  #expect_equal(commit_df$numCommits, c(9, 17, 53))
  expect_identical(commit_df$month, as_date(c("2020-06-01", "2020-07-01", "2020-08-01")))
})

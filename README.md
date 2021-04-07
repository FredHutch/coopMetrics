# coopMetrics

The `coopMetrics` package was developed as a thin wrapper around the [`gh`](https://github.com/r-lib/gh) and [`googleAnalyticsR`](https://code.markedmondson.me/googleAnalyticsR/) packages to pull specific metrics. It's meant to work specifically with GitHub Pages blogs set up using [Jekyll](https://jekyllrb.com/). 

## Installation

```
devtools::install_github(repo = "FredHutch/coopMetrics",
                         ref = "main")
```

## Usage

This package works by pulling data from GitHub and Google Analytics. This package is a work in progress! The goal is to build out functions that make it easier to pull data on blogs hosted with GitHub and tracked with Google Analytics.

### Pulling data

Use the function `getBlogStatistics()` to pull data from both Google Analytics and GitHub. User must specify the owner and repo for GitHub, web property name for GoogleAnalytics, the desired dateRange, and whether or not they would like to use cached data to speed up the function.

Here's an example where I pull data from the [FredHutch/coop](https://github.com/FredHutch/coop) repository.

```
getBlogStatistics(owner = "FredHutch",
                  repo = "coop",
                  webPropertyName = "Coop",
                  dateRange = c("2020-01-01", "2020-06-30"),
                  useCache = FALSE)
```

### Metrics

The following metrics are captured:

From Github:
- `gh_numCommits`: Number of commits
- `gh_numPostTotal`: Running total of number of posts
- `gh_numNewPosts`: Number of new posts that month
- `gh_totalContributor`: Running total number of contributors
- `gh_numNewContributor`: Number of new contributors that month
- `gh_handles`: If a new contributor was added shows their handle. If no new contributors shows NA.

From Google Analytics:
- `ga_users`: Number of users
- `ga_newUsers`: Number of new users
- `ga_sessions`: Number of sessions
- `ga_pageviews`: Number of pageviews
- `ga_mostViewed`: The top three posts (by views)

Columns with the prefix `gh` are pulled from GitHub and columns with the prefix `ga` are pulled from Google Analytics.

### Caching

The user can create a data cache to speed up the data pull using the function `createCache()`. This function saves the data internally to the package as `R/sysdata.rda`. Currently the only data that is cached is the contributor data.

Example usage:

```
createCache(owner = "FredHutch",
            repo = "coop",
            dateRange = c("2019-01-01", "2021-03-31"),
            overwrite = TRUE)
```

### A note on `dateRange`

- DateRange _must_ be a vector of dates in year-month-day format.
- Order does not matter
- Dates can be a date object or character

## More info:

### Google Analytics API
- Google Analytics requires an account. 
- The [`googleAnalyticsR::ga_auth`](https://www.rdocumentation.org/packages/googleAnalyticsR/versions/0.8.0/topics/ga_auth) function is utilized to authenticate. It will prompt you to supply your credentials through an interactive pop out.

* See documentation about `GoogleAnalyticsR` [here](https://code.markedmondson.me/googleAnalyticsR/)
* See documentation about the Google Analytics API [here](https://developers.google.com/analytics/devguides/reporting/core/v4)

### GitHub API
- No account information is required to get data from public repositories. Simply provide the repo owner and repo name.

* See documentation about the `gh` package [here](https://github.com/r-lib/gh)
* See documentation about the GitHub REST API [here](https://docs.github.com/en/rest)

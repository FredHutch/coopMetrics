# coopMetrics

The `coopMetrics` package was developed as a thin wrapper around the [`gh`](https://github.com/r-lib/gh) and [`googleAnalyticsR`](https://code.markedmondson.me/googleAnalyticsR/) packages to pull specific metrics from either API. It's meant to work specifically with GitHub Pages blog set up using [Jekyll](https://jekyllrb.com/). 

## Installation

```
devtools::install_github(repo = "FredHutch/coopMetrics",
                         ref = "main")
```

## Usage

This package works in two parts. First, pulling the data from GitHub and Google Analytics and saving it. Second, building the report.

### To refresh the available dataset:
- update variables in `data-raw/DATA.R` if needed
  - you can change what repo is being used, timeframe pulled, etc
- run script

Data is pulled from GitHub and GoogleAnalytics using the `gh` and `googleAnalyticsR` packages. The data object is saved to `R/sysdata.Rda`. This internal data object is then used to generate the figures in `monthly_report.md`. 

Two data objects are saved as `1sysdata.Rda1`: `blogMetrics` and `knownContributorData`.

**`blogMetrics`**

This object is a dataframe capturing the following metrics from GitHub and Google Analytics.

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

**`knownContributorData`**

This object is a dataframe capturing the following metrics from `_contributor` files.

- Contributor filename
- Contributor handle (this is the filename with the path and `.md` removed)
- Date of first commit

### To generate report
- open `monthly_report.Rmd` and knit

## More info:

### Google Analytics
- Google Analytics requires an account. 
- The [`googleAnalyticsR::ga_auth`](https://www.rdocumentation.org/packages/googleAnalyticsR/versions/0.8.0/topics/ga_auth) function is utilized to authenticate. It will prompt you to supply your credentials through an interactive pop out.

* See documentation about `GoogleAnalyticsR` [here](https://code.markedmondson.me/googleAnalyticsR/)
* See documentation about the Google Analytics API [here](https://developers.google.com/analytics/devguides/reporting/core/v4)

### GitHub
- No account information is required to get data from public repositories. Simply provide the repo owner and repo name.

* See documentation about the `gh` package [here](https://github.com/r-lib/gh)
* See documentation about the GitHub REST API [here](https://docs.github.com/en/rest)

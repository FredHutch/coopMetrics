# coopMetrics

The `coopMetrics` package was developed as a thin wrapper around the [`gh`](https://github.com/r-lib/gh) and [`googleAnalyticsR`](https://code.markedmondson.me/googleAnalyticsR/) packages developed to work specifically with GitHub Pages blog set up using [Jekyll](https://jekyllrb.com/). Providing it with a GoogleAnalytics `webPropertyName`, a GitHub repository name and owner, and the desired date range and it will return various monthly metrics related to your blog!

## Installation
```
devtools::install_github(repo = "FredHutch/coopMetrics",
                         ref = "main")
```

## Usage
```
library(coopMetrics)
## Not sure if the package will auto load all the required dependency packages just yet
library(gh)
library(googleAnalyticsR)
library(googleAuthR)
library(tidyverse)
```
Currently there is one main function that pulls pre-set metrics from both GitHub and Google Analytics. Below is an code chunk showing how to use this function using [the Coop Blog's repository](https://github.com/FredHutch/coop) as an example. 
```
getBlogData(webPropertyName = "Coop blog",
            owner = "FredHutch",
            repo = "coop",
            dateRange = c(ymd("2020-01-01"), ymd("2020-03-30")))
                              
```
And output is the table below. Returning a dataframe of the specified months metrics.


| month | gh_numCommits | gh_numPostTotal | gh_numNewPosts | gh_totalContributor | gh_numNewContributor | gh_handles | ga_users | ga_newUsers | ga_sessions | ga_pageviews | ga_mostViewed |
|-|-|-|-|-|-|-|-|-|-|-|-|
| 2020-01-01  | 96 | 8 | 6 | 4 | 1 | carly | 92 | 83 | 163 | 970 | /coop/community/another-transition/; /coop/community/science/uw-capstone-collaboration/; /coop/community/technical/nextflow/ |
| 2020-02-01  | 50 | 12 | 4 | 4 | 0 | NA | 115 | 97 | 166 | 491 | "/coop/community/technical/nextflow/; /coop/community/online-training/; /coop/community/technical/rstudio-conf2020/" |
| 2020-03-01  | 42 | 17 | 5 | 5 | 1 | lwolfe | 184 | 160 | 301 | 675 | "/coop/community/wfh-tips/; /coop/community/remote-teamwork/; /coop/community/ms-teams/" |

Right now the metrics include:

From Github:
- Number of posts
- Number of commits
- Total number of contributors
- New contributors
- The names of new contributors (based on their contributor file)

From Google Analytics:
- Number of users
- Number of new users
- Number of sessions
- Number of pageviews
- The top three posts (by views)

Columns with the prefix `gh` are pulled from GitHub and columns with the prefix `ga` are pulled from Google Analytics.

## Whats going on under the hood

The main function `getBlogData()` works by calling `pullGoogleAnalytics()` and `pullGithub()`. 

The Google Analytics side of things is fairly straight forward. It uses the function `ga_auth()` to get the users Google login info. Then it converts the provided `webPropertyId` to a `viewId` and pulls the relevant data using `google_analytics()`. Since we want to pull monthly statistics we are able to use the dimensions parameter set to `month` and `year`.

See documentation about `GoogleAnalyticsR` [here](https://code.markedmondson.me/googleAnalyticsR/)
See documentation about the Google Analytics API [here](https://developers.google.com/analytics/devguides/reporting/core/v4)

The GitHub data is a little less straightforward. We specifically make use of the [`repos endpoint`](https://docs.github.com/en/rest/overview/endpoints-available-for-github-apps#repos) to list information about repository contents and commits. The package makes use of the `usethis` package to cache data about known collaborators. You can check out the data generation script [here](../data-raw/DATA.R).

See documentation about the `gh` package [here](https://github.com/r-lib/gh)
See documentation about the GitHub REST API [here](https://docs.github.com/en/rest)


## Notes

* Please note that this package is in very early developement and there are currently no tests. USE WITH CAUTION LOL.

# coopMetrics

The `coopMetrics` package was developed as a thin wrapper around the [`gh`](https://github.com/r-lib/gh) and [`googleAnalyticsR`](https://code.markedmondson.me/googleAnalyticsR/) packages developed to work specifically with GitHub Pages blog set up using [Jekyll](https://jekyllrb.com/). Providing it with a GoogleAnalytics `webPropertyName`, a GitHub repository name and owner, and the desired month and year will return various statistics related to your blog!

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
            dateRange = c(ymd("2020-01-01"), ymd("2020-05-30")))
                              
```
And output is the table below. Returning a dataframe of the specified months metrics.


| month | gh_numCommits | gh_numPostTotal | gh_numNewPosts | gh_totalContributor | gh_numNewContributor | gh_handles | ga_users | ga_newUsers | ga_sessions | ga_pageviews | ga_mostViewed |
|-|-|-|-|-|-|-|-|-|-|-|-|
| 2020-01-01  | 96 | 8 | 6 | 4 | 1 | carly | 92 | 163 | 970 | /coop/community/another-transition/; /coop/community/science/uw-capstone-collaboration/; /coop/community/technical/nextflow/ |

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





## Notes

* Please note that this package is in very early developement and there are currently no tests.

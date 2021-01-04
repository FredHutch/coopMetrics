Monthly report
================

## Set variables

For this report we are looking at the first 6 months of the
year

``` r
# cache is set to store the last year of data - must update cache if you need to go further back than a year.
begin <- as_date("2020-01-01")
end <- as_date(Sys.Date())
dateRange <- c(begin, end)
```

## Load data

``` r
load("R/sysdata.rda")
message(paste0("Data loaded was last updated on ", cacheDate))
```

    ## Data loaded was last updated on 2021-01-04

``` r
message(paste0("Today is ", Sys.Date()))
```

    ## Today is 2021-01-04

# Plot posts over time

``` r
ggplot(blogMetrics, aes(x = month, y = gh_numPostTotal)) +
  geom_line() +
  labs(title = "total posts by month", x = "month", y = "total number of posts") +
  theme_minimal()
```

    ## Warning: Removed 1 row(s) containing missing values (geom_path).

![](monthly_report_files/figure-gfm/posts-total-plot-1.png)<!-- -->

``` r
ggplot(blogMetrics, aes(x = month, y = gh_numNewPosts)) +
  geom_bar(stat = "identity") +
  labs(title = "number of posts per month", x = "month", y = "number of posts") +
  theme_minimal()
```

    ## Warning: Removed 1 rows containing missing values (position_stack).

![](monthly_report_files/figure-gfm/posts-monthly-plot-1.png)<!-- -->

``` r
ggplot(blogMetrics, aes(x = month)) +
  geom_bar(aes(y = gh_numNewPosts), stat = "identity") +
  geom_line(aes(y = gh_numPostTotal)) +
  labs(title = "number of posts per month with running total", x = "month", y = "number of posts") +
  theme_minimal()
```

    ## Warning: Removed 1 rows containing missing values (position_stack).

    ## Warning: Removed 1 row(s) containing missing values (geom_path).

![](monthly_report_files/figure-gfm/posts-overlay-1.png)<!-- -->

# Plot commits over time

``` r
ggplot(blogMetrics, aes(x = month, y = gh_numCommits)) +
  geom_bar(stat = "identity") +
  labs(title = "number of commits per month", x = "month", y = "number of commits") +
  theme_minimal()
```

    ## Warning: Removed 1 rows containing missing values (position_stack).

![](monthly_report_files/figure-gfm/commits-monthly-plot-1.png)<!-- -->

# Plot google analytics metrics

``` r
blogMetrics %>%
  select(month, ga_users, ga_newUsers) %>%
  ggplot(aes(x = month)) +
  geom_line(aes(y = ga_users, color = "users")) +
  geom_line(aes(y = ga_newUsers, color = "newUsers")) +
  labs(x = "month", y = "number of users") +
  theme_minimal()
```

![](monthly_report_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
blogMetrics %>%
  ggplot(aes(x = month)) +
  geom_line(aes(y = ga_pageviews, color = "page views")) +
  geom_line(aes(y = ga_sessions, color = "sessions")) +
  labs(x = "month", y = "count") +
  theme_minimal()
```

![](monthly_report_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

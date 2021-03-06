% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pullGoogleAnalytics.R
\name{googleAnalyticsMetrics}
\alias{googleAnalyticsMetrics}
\title{API pull from both Google Analytics.}
\usage{
googleAnalyticsMetrics(webPropertyName, dateRange)
}
\arguments{
\item{webPropertyName}{A variable from GoogleAnalytics. You can find out the webproperty name using `ga_account_list()`}

\item{dateRange}{A vector of two dates in yyyy-mm-dd format. Can be strings or date objects. Order doesn't matter, the max and min will be used.}

\item{gaAcctEmail}{Email of the google account associated with Google Analytics.}
}
\value{
a dataframe of monthly metrics from the date range specified.
}
\description{
API pull from both Google Analytics.
}

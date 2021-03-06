% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pullGithub.R
\name{commitMetrics}
\alias{commitMetrics}
\title{Pull and calculate commit metric (commits per month over a time frame)}
\usage{
commitMetrics(owner, repo, dateRange)
}
\arguments{
\item{owner}{The GitHub repository owner}

\item{repo}{The GitHub repository name}

\item{dateRange}{A vector of two dates in yyyy-mm-dd format. Can be strings or date objects. Order doesn't matter, the max and min will be used.}
}
\description{
Pull and calculate commit metric (commits per month over a time frame)
}

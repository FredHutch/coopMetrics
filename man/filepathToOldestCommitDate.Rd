% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pullGithub.R
\name{filepathToOldestCommitDate}
\alias{filepathToOldestCommitDate}
\title{This function pulls the the oldest commit date for the specified filepath. This can be used to determine when a file was first pushed to the repository.}
\usage{
filepathToOldestCommitDate(owner, repo, filepath)
}
\arguments{
\item{owner}{the GitHub repository owner}

\item{repo}{The GitHub repository name}

\item{filepath}{Path to the file to return first commit date from}
}
\value{
a date object of the date that the specified file was first commited to the repository.
}
\description{
This function pulls the the oldest commit date for the specified filepath. This can be used to determine when a file was first pushed to the repository.
}

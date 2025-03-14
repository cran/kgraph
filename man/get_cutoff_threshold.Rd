% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_embeds.R
\name{get_cutoff_threshold}
\alias{get_cutoff_threshold}
\title{Get cut-off threshold}
\usage{
get_cutoff_threshold(roc_obj, specificity_lvl = 0.95)
}
\arguments{
\item{roc_obj}{Object returned by pROC::roc}

\item{specificity_lvl}{Specificity threshold
(default 0.95 is equivalent to 5 percent false positives,
and 0.9 to 10 percent false positives)}
}
\value{
Similarity value threshold
}
\description{
Get cut-off threshold
}

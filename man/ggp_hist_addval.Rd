% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggp_hist_addval.R
\name{ggp_hist_addval}
\alias{ggp_hist_addval}
\title{Histogram for added value analysis}
\usage{
ggp_hist_addval(
  data,
  x_var,
  group_var,
  binwidth = 0.25,
  accuracy = 1,
  pal,
  fun,
  titles
)
}
\arguments{
\item{data}{Data.frame.}

\item{x_var}{String to identify the x variable.}

\item{group_var}{String to identify the group variable.}

\item{binwidth}{Number for the bin width.}

\item{accuracy}{Accuracy for the x axis.}

\item{pal}{Palette of colors used by group.}

\item{fun}{Function used to transform x-axis labels.}

\item{titles}{List of plot titles.}
}
\value{
Object of class \code{gg}.
}
\description{
Histogram for added value analysis.
}
\details{
Histogram for added value analysis with a custom format.
}
\examples{
\dontrun{
TODO
}
}

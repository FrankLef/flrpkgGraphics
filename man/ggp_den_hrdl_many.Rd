% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggp_dens_hrdl_many.R
\name{ggp_den_hrdl_many}
\alias{ggp_den_hrdl_many}
\title{Many density plots created with \code{ggp_den_hrdl} for hurdle analysis}
\usage{
ggp_den_hrdl_many(data, cols, vals, fun = identity, thm, titles)
}
\arguments{
\item{data}{Data.fame.}

\item{cols}{Named character vector where the names are the value variables
and the string are the name of the falg variables.}

\item{vals}{Named character() with colors.}

\item{fun}{Function used to transform the data. Default is \code{identity}.
Very common to use \code{ceiling(flrpkgTools::expm1s())}.}

\item{thm}{Theme used by \code{ggp_den_hrdl}.}

\item{titles}{List of titles used by \code{labs}.}
}
\value{
Object of class \code{gg}.
}
\description{
Many density plots created with \code{ggp_den_hrdl} for hurdle analysis.
}
\details{
Many density plots for hurdle analysis all joined together using
\pkg{patchwork}.
}
\examples{
\dontrun{
TODO
}
}
\seealso{
\link{ggp_den_hrdl}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ply_scatter3D_valid.R
\name{ply_scatter3D_valid_axis}
\alias{ply_scatter3D_valid_axis}
\title{Create the axis used by \code{ply_scatter3D_valid}}
\usage{
ply_scatter3D_valid_axis(data, var, title, colrs, fun = identity)
}
\arguments{
\item{data}{Data frame.}

\item{var}{String with name of the variable used for the axis.}

\item{title}{String with the axis' label.}

\item{colrs}{List of colors with the following items:
\describe{
\item{title}{Color of axes label.}
\item{grid}{Color of grid.}
\item{zeroline}{Color of zero line.}
}}

\item{fun}{Function used to transform values on the scale. Default is
\code{identity()}. Often used is \code{ceiling(flrpkgTools::expm1s())}.}
}
\value{
List of arguments used by \code{plotly::layout}.
}
\description{
Create the axis used by \code{ply_scatter3D_valid}.
}
\details{
Create the axis used by \code{ply_scatter3D_valid} with labels formatted
using \code{fun}.
}
\examples{
\dontrun{
TODO
}

}
\seealso{
\link{ply_scatter3D_valid}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ply_scatter3D_group.R
\name{ply_scatter3D_group}
\alias{ply_scatter3D_group}
\title{Create a 3D plot with \pkg{plotly} showing data colored by group}
\source{
\url{https://plotly.com/r/3d-axes/}
}
\usage{
ply_scatter3D_group(
  data,
  x_var,
  y_var,
  z_var,
  color_var,
  colrs,
  size,
  fun = identity,
  titles,
  show_legend = TRUE
)
}
\arguments{
\item{data}{Data frame.}

\item{x_var}{String with name of x variable.}

\item{y_var}{String with name of y variable.}

\item{z_var}{String with name of y variable.}

\item{color_var}{String with name of color variable.}

\item{colrs}{List with colors:
\describe{
\item{groups}{Palette of colors for the groups.}
\item{title}{Color of title.}
\item{background}{Color of background.}
}}

\item{size}{Size of markers.}

\item{fun}{Function used to transform values on the scale. Default is
\code{identity()}. Often used is \code{ceiling(flrpkgTools::expm1s())}.}

\item{titles}{List of titles:
\describe{
\item{title}{Main title.}
\item{x, y, z}{Labels for x, y and z axis.}
}}

\item{show_legend}{Falg to indocate if the legend should appear.
Default is \code{TRUE}}
}
\value{
Object of class \code{"plotly", "htmlwidget"}.
}
\description{
Create a 3D plot with \pkg{plotly} showing data colored by group.
}
\details{
The 3D graph will be colored based on the \code{color_var} to differentiate
the groups.
}
\examples{
\dontrun{
TODO
}
}

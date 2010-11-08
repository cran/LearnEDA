\name{slider.compare}
\alias{slider.compare}
\title{Interactive comparison of groups by a power transformation}
\description{
Constructs a boxplot of power-transformed data, where the power of
the transformation is controlled by a slider.
}
\usage{
slider.compare(x,group,...)
}
\arguments{
  \item{x}{numeric vector of values}
  \item{group}{grouping variable}
  \item{...}{additional graphics parameters passed to the boxplot function}
}

\author{Jim Albert}

\examples{
## Not run: 
## This example cannot be run by examples() but should be work in an interactive R session

slider.compare(homeruns.2000$HOMERUNS, homeruns.2000$YEARS)

## End(Not run)}

\keyword{manip}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/svMisc-package.R
\docType{package}
\name{svMisc-package}
\alias{svMisc}
\alias{svMisc-package}
\title{svMisc: 'SciViews' - Miscellaneous Functions}
\description{
\if{html}{\figure{logo.png}{options: style='float: right' alt='logo' width='120'}}

Miscellaneous functions for 'SciViews::R' or general use: manage a temporary environment attached to the search path for temporary variables you do not want to save() or load(), test if 'Aqua', 'Mac', 'Win', ... Show progress bar, etc.
}
\details{
The 'svMisc' package is of general use among \code{SciViews::R}, a layer on top of
R, and the tidyverse. This package collects together a series of general
functions to manage a centralized environment for temporary variables, a
progress bar and batch analysis mode, etc.
}
\section{Important functions}{

\itemize{
\item \code{\link[=temp_env]{temp_env()}} for using a temporary environment attached to the search path,
\item \code{\link[=temp_var]{temp_var()}} create the name of temporary variables,
\item \code{\link[=capture_all]{capture_all()}} to capture R output, errors, warnings and messages,
\item \code{\link[=parse_text]{parse_text()}} to parse any R expression, including partial or incorrect
ones (fails gracefully).
}
}

\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/SciViews/svMisc}
  \item \url{https://www.sciviews.org/svMisc/}
  \item Report bugs at \url{https://github.com/SciViews/svMisc/issues}
}

}
\author{
\strong{Maintainer}: Philippe Grosjean \email{phgrosjean@sciviews.org} (\href{https://orcid.org/0000-0002-2694-9471}{ORCID})

Other contributors:
\itemize{
  \item Romain Francois \email{romain@r-enthusiasts.com} [contributor]
  \item Kamil Barton \email{kamil.barton@uni-wuerzburg.de} [contributor]
}

}
\keyword{internal}

\name{captureAll}
\alias{capture_all}
\alias{captureAll}

\title{ Run an R expression and capture output and messages in a similar way
  as it would be done at the command line }

\description{
  This function captures results of evaluating one or several R expressions the
  same way as it would be issued at the prompt in a R console. The result is
  returned in a character string. Errors, warnings and other conditions are
  treated as usual, including the delayed display of the warnings if
  \code{options(warn = 0)}.
}

\usage{
capture_all(expr, split = TRUE, echo = TRUE, file = NULL, markStdErr = FALSE)
captureAll(expr, split = TRUE, echo = TRUE, file = NULL, markStdErr = FALSE)
}

\arguments{
  \item{expr}{ a valid R expression to evaluate (names and calls are also
    accepted). }
  \item{split}{ do we split output, that is, do we also issue it at the R console
    too, or do we only capture it silently? }
  \item{echo}{ do we echo each expression in front of the results (like in the
    console)? In case the expression spans on more than 7 lines, only first and
	last three lines are echoed, separated by [...]. }
  \item{file}{ a file, or a valid opened connection where output is sinked. It
    is closed at the end, and the function returns \code{NULL} in this case. If
	\code{file = NULL} (by default), a textConnection() captures the output and
	it is returned is a character string by the function. }
  \item{markStdErr}{ if \code{TRUE}, stderr is separated from sddout by STX/ETX
    character }
}

\value{
  Returns a string with the result of the evaluation done in the user workspace.
}

\note{
  If the expression is provided as a character string that should be evaluated,
  and you need a similar behaviour as at the prompt for incomplete lines of code
  (that is, to prompt for more), you should not parse the expression with
  \code{parse(text = "<mycode>")} because it returns an error instead of an
  indication of an incomplete code line. Use
  \code{parseText("<mycode>")} instead, like in the examples bellow.
  Of course, you have to deal with incomplete line management in your GUI/CLI
  application... the function only returns \code{NA} instead of a character
  string.
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{parseText}}, \code{\link[base]{parse}},
  \code{\link[base]{expression}}, \code{\link[utils]{capture.output}},
  \code{\link{sourceClipboard}} }

\examples{
writeLines(capture_all(expression(1+1), split = FALSE))
writeLines(capture_all(expression(1+1), split = TRUE))
writeLines(capture_all(parse_text("search()"), split = FALSE))

\dontrun{
writeLines(capture_all(parse_text('1:2 + 1:3'), split = FALSE))
writeLines(capture_all(parse_text("badname"), split = FALSE))
}

# Management of incomplete lines
capt_res <- capture_all(parse_text("1 +")) # Clearly an incomplete command
if (is.na(capt_res)) cat("Incomplete line!\n") else writeLines(capt_res)
rm(capt_res)
}

\keyword{ IO }

\concept{ capturing output for GUI clients }

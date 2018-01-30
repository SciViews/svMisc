\name{parseText}
\alias{parse_text}
\alias{parseText}

\title{ Parse a character string as if it was a command entered at the command
  line }

\description{
  Parse R instructions provided as a string and return the expression if it
  is correct, or an object of class 'try-error' if it is an incorrect code, or
  \code{NA} if the (last) instruction is incomplete.
}

\usage{
parse_text(text, firstline = 1, srcfilename = NULL, encoding = "unknown")
parseText(text, firstline = 1, srcfilename = NULL, encoding = "unknown")
}

\arguments{
  \item{text}{ the character string vector to parse into an R expression. }
  \item{firstline}{ the index of first line being parsed in the file. If this
    is larger than \code{1}, empty lines are added in front of \code{text} in
	order to match the correct position in the file. }
  \item{srcfilename}{ a character string with the name of the source file. }
  \item{encoding}{ encoding of \code{text}, as in \code{\link[base]{parse}}. }
}

\value{
  Returns an expression with the parsed code or \code{NA} if the last
  instruction is correct but incomplete, or an object of class'try-error' with
  the error message if the code is incorrect.
}

\note{
  On the contrary to \code{parse()}, \code{parseText()} recovers from incorrect
  code and also detects incomplete code. It is also easier to use in case you
  pass a character string to it, because you don't have to name the argument
  explicitly (\code{text = ....}).
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{captureAll}}, \code{\link{sourceClipboard}},
  \code{\link[base]{parse}} }

\examples{
parseText('1+1')
parseText('1+1; log(10)')
parseText(c('1+1', 'log(10)'))

## Incomplete instruction
parseText('log(')

## Incomplete strings
parseText('text <- "some string')
parseText("text <- 'some string")

## Incomplete names (don't write backtick quoted names on several lines!)
## ...but just in case
parseText('`myvar')

## Wrong instruction
parseText('log)')
}

\keyword{ IO }

\concept{ parsing expression, incomplete code }

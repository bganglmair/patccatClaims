\name{patccat_defaults}
\alias{patccat_defaults}
\title{The validated internal parameters and word lists}
\description{
Returns the classifier's default parameters and word lists as a named list --
what \code{\link{fn.patccat}} uses unless overridden, and a convenient starting
point for the override arguments.
}
\usage{
patccat_defaults()
}
\value{
A named list with \code{params}, \code{claim.rules}, \code{process.words},
\code{product.words}, and \code{processwords.simple}.
}
\seealso{\code{\link{fn.patccat}}}
\examples{
d <- patccat_defaults()
\dontrun{
## override: extend the product word list, keep everything else validated
out <- fn.patccat(claim_lines, product.words = c(d$product.words, "widget"))
}
}

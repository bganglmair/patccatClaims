\name{fn.patccat}
\alias{fn.patccat}
\title{Classify patent claims as product or process}
\description{
Classifies U.S. patent claims as product or process and attaches
claim-structure flags, using udpipe part-of-speech tags.
}
\usage{
fn.patccat(data, varPatentClaim = "PatentClaim", varText = "text",
  varLevel = "level", varSequence = "sequence", varID = "id",
  extractPreambleTerms = TRUE, extractPreambleText = TRUE,
  timeOutput = TRUE, integrityCheck = TRUE,
  params = NULL, claim.rules = NULL, process.words = NULL,
  product.words = NULL, processwords.simple = NULL)
}
\arguments{
\item{data}{A data frame of claim lines: one row per claim line.}
\item{varPatentClaim}{Column naming the claim key (default "PatentClaim").}
\item{varText}{Column with the claim text (default "text").}
\item{varLevel}{Column with the line level (default "level").}
\item{varSequence}{Column with the line sequence (default "sequence").}
\item{varID}{Column with a unique row id (default "id").}
\item{extractPreambleTerms}{Extract preamble terms (default TRUE).}
\item{extractPreambleText}{Extract preamble text; required for Beauregard detection (default TRUE).}
\item{timeOutput}{Print timing (default TRUE).}
\item{integrityCheck}{Run the input integrity check (default TRUE).}
\item{params, claim.rules, process.words, product.words, processwords.simple}{
Optional overrides for the classifier's internal parameters and word lists.
Each defaults to \code{NULL}, meaning the validated internal value is used, so
the default call reproduces the classifier exactly. Supplying your own values
changes the classification and leaves the validated AMT baseline behind; intended
for research and experimentation. See \code{\link{patccat_defaults}} to inspect
the internal values (if available), or the package data.}
}
\value{
A data frame with one row per independent claim, keyed by \code{PatentClaim}. The
label is \code{claimType} (0 = uncategorized, 1 = process/method, 2 = product);
product-by-process is folded into product and flagged in \code{prodByProcess}.
Alongside it are the claim-structure flags, including \code{independent},
\code{singleLine}, \code{Jepson}, \code{isMeans} (means-plus-function),
\code{markush}, \code{beauregard}, \code{nonTransitory}, \code{computerImplemented},
and \code{stepPlusFunction}, plus structural fields such as \code{preambleType},
\code{bodyType}, \code{transitionType}, and the word counts \code{wordsPreamble}
and \code{wordsBody}.
}
\details{
Requires the udpipe POS model; it is loaded automatically on first use via
\code{\link{patccat_load_model}}. Overrides passed here are resolved against the
validated internal defaults and used for that call only.
}
\seealso{\code{\link{patccat_classify}}, \code{\link{patccat_load_model}}, \code{\link{fn.benchmarking}}}
\examples{
\dontrun{
patccat_load_model()
out <- fn.patccat(claim_lines)

## research: try a custom product-word list
out2 <- fn.patccat(claim_lines, product.words = c("apparatus", "device", "widget"))
}
}

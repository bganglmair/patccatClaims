\name{fn.benchmarking}
\alias{fn.benchmarking}
\title{Score classifier output against a benchmark}
\description{
Compares patccat classifications to a benchmark (gold) set and reports accuracy
and coverage -- overall and broken down by claim format, Jepson status, approach,
and category.
}
\usage{
fn.benchmarking(dataPatccat, dataBenchmark)
}
\arguments{
\item{dataPatccat}{Output of \code{\link{fn.patccat}} (must contain
\code{PatentClaim} and \code{claimType}).}
\item{dataBenchmark}{A gold set keyed by \code{PatentClaim}, with the benchmark
category in a \code{finalcategory} column (1 = process, 2 = product;
product-by-process is folded into product).}
}
\value{
A data frame with columns \code{type}, \code{accuracy}, and \code{coverage}. The
row where \code{type == "Overall results"} carries the headline figures; further
rows break the results down by claim format (regular vs. single-line), Jepson vs.
non-Jepson, the simple preamble/body approach, and benchmark vs. automated
category, and end with claim counts. Blank separator rows carry \code{NA} in
\code{type}, so select the overall figures positionally, e.g.
\code{b[which(b$type == "Overall results"), ]}.
}
\details{
\code{accuracy} is the share of \emph{classified} claims (\code{claimType} 1 or 2)
whose label matches the benchmark; \code{coverage} is the share of claims the
classifier assigned a non-zero \code{claimType}. Product-by-process rows report
\code{NaN}, since that category is retired (folded into product).
}
\seealso{\code{\link{fn.patccat}}, \code{\link{patccat_defaults}}}
\examples{
\dontrun{
patccat_load_model()
out  <- fn.patccat(claim_lines)
gold <- data.frame(PatentClaim = out$PatentClaim, finalcategory = my_gold_labels)
b    <- fn.benchmarking(out, gold)
b[which(b$type == "Overall results"), ]   # headline accuracy + coverage
}
}

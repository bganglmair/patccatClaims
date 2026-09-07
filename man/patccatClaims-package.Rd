\name{patccatClaims-package}
\alias{patccatClaims-package}
\alias{patccatClaims}
\docType{package}
\title{patccatClaims: Product vs Process U.S. Patent Claim Classifier}
\description{
A rule-based classifier that labels U.S. patent claims as product or process and
extracts claim-structure flags (means-plus-function, Jepson, Beauregard, Markush,
single-line, and more) from udpipe part-of-speech tags. It is the core classifier
of the patccat pipeline, packaged for standalone use.
}
\section{Getting started}{
\enumerate{
  \item Load the POS model once per session with \code{\link{patccat_load_model}()};
    it downloads and caches the pinned udpipe English-EWT model on first use.
  \item Classify a data frame of claim lines (one row per line) with
    \code{\link{fn.patccat}()}, or its idiomatic alias \code{\link{patccat_classify}()}.
}
}
\section{Key functions}{
\describe{
  \item{\code{\link{fn.patccat}}, \code{\link{patccat_classify}}}{Classify claims;
    return \code{claimType} and the claim-structure flags.}
  \item{\code{\link{patccat_load_model}}}{Download and load the pinned udpipe POS model.}
  \item{\code{\link{patccat_defaults}}}{Inspect the validated internal parameters and
    word lists (and use them as a starting point for the override arguments).}
  \item{\code{\link{fn.benchmarking}}}{Score classifier output against a gold benchmark.}
}
}
\section{Claim types}{
\code{claimType} is \code{0} (uncategorized), \code{1} (process / method), or
\code{2} (product). Product-by-process claims are folded into product (\code{2})
and flagged separately in the \code{prodByProcess} column.
}
\section{Reproducibility}{
On its default (no-override) path the classifier reproduces the committed AMT
validation baseline -- accuracy 0.9972248 and coverage 0.9897253 on the
9,830-claim multi-line sample. Supplying the override arguments of
\code{\link{fn.patccat}} changes the classification and leaves that baseline behind.
}
\section{Part-of-speech model}{
Claim text is tagged with the udpipe English-EWT model, pinned to build
\code{ud-2.5-191206} so classifications stay reproducible. The model file is a
third-party artifact and is \emph{not} shipped with the package; it is downloaded
once into a per-user cache by \code{\link{patccat_load_model}}. Credit: the
\pkg{udpipe} R package (Jan Wijffels) and the Universal Dependencies English-EWT
treebank.
}
\author{
Bernhard Ganglmair (maintainer, \email{b.ganglmair@gmail.com}) and W. Keith Robinson.
}
\seealso{
\code{\link{fn.patccat}}, \code{\link{patccat_classify}},
\code{\link{patccat_load_model}}, \code{\link{patccat_defaults}},
\code{\link{fn.benchmarking}}
}

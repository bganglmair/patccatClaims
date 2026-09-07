\name{patccat_load_model}
\alias{patccat_load_model}
\title{Download and load the udpipe POS model}
\description{
Downloads (once, into a per-user cache) and loads the pinned udpipe English-EWT
model the classifier requires, storing it in the package for internal use.
}
\usage{
patccat_load_model(dir = tools::R_user_dir("patccatClaims", "data"))
}
\arguments{
\item{dir}{Cache directory for the model file.}
}
\value{The udpipe model object, invisibly.}
\details{
Called automatically on the first \code{\link{fn.patccat}} call; call it
explicitly to pre-fetch the model. The exact build (ud-2.5-191206) is pinned so
classifications stay reproducible.
}
\seealso{\code{\link{fn.patccat}}}
\examples{
\dontrun{
patccat_load_model()
}
}

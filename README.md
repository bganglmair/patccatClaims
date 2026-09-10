# patccatClaims

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22697822.svg)](https://doi.org/10.5281/zenodo.22697822)

Rule-based classifier that labels U.S. patent claims as **product** or **process**
and extracts claim-structure flags (means-plus-function, Jepson, Beauregard,
single-line, and others), using udpipe part-of-speech tags. It is the core
classifier of the patccat pipeline, packaged for standalone use.

## Install

```r
# install.packages("remotes")
remotes::install_github("bganglmair/patccatClaims")
```

From a local copy of the source, `remotes::install_local("patccatClaims")` also
works.

## Quick start

```r
library(patccatClaims)

## one-time: download + cache the pinned udpipe POS model
patccat_load_model()

## `data` is one row per claim line, with columns for the claim key, text,
## structural level, sequence, and a row id.
out <- patccat_classify(
  data, varPatentClaim = "PatentClaim", varText = "text",
  varLevel = "level", varSequence = "sequence", varID = "id")

## `out` has one row per independent claim, with claimType
## (0 = none, 1 = process, 2 = product) and the claim-structure flags.
```

`patccat_classify()` is an alias for `fn.patccat()`; use either.

## The POS model

The classifier tags claims with the udpipe **English-EWT (ud-2.5-191206)** model.
That third-party file is not shipped with the package; `patccat_load_model()`
downloads it once into a per-user cache (`tools::R_user_dir`) and pins the exact
build the classifier was validated against.

## Validation

The bundled `testthat` check runs the classifier on a frozen AMT fixture and
asserts it reproduces the expected labels and clears an accuracy floor against
the human gold. The full 9,830-claim AMT gate lives in the research repository.

## Attribution

POS tagging by [udpipe](https://cran.r-project.org/package=udpipe) (Jan Wijffels),
using the Universal Dependencies English-EWT treebank.

## Acknowledgements

The R-package scaffolding, documentation, and repository and release setup were
done with assistance from Anthropic's Claude (Cowork). The classifier and its
validation are the authors' own work.

## Citing

If you use this package, cite the archived release. The concept DOI below always
resolves to the latest version:

> Ganglmair, B., and W. K. Robinson. patccatClaims: Product vs Process U.S. Patent
> Claim Classifier. https://doi.org/10.5281/zenodo.22697822

`CITATION.cff` in this repository carries the machine-readable version, and GitHub's
"Cite this repository" link renders it.

## License

MIT (c) 2026 Bernhard Ganglmair, W. Keith Robinson.

# patccatClaims: Product vs Process U.S. Patent Claim Classifier

A rule-based classifier that labels U.S. patent claims as product or
process and extracts claim-structure flags (means-plus-function, Jepson,
Beauregard, Markush, single-line, and more) from udpipe part-of-speech
tags. It is the core classifier of the patccat pipeline, packaged for
standalone use.

## Getting started

1.  Load the POS model once per session with
    [`patccat_load_model()`](https://bganglmair.github.io/patccatClaims/reference/patccat_load_model.md);
    it downloads and caches the pinned udpipe English-EWT model on first
    use.

2.  Classify a data frame of claim lines (one row per line) with
    [`fn.patccat()`](https://bganglmair.github.io/patccatClaims/reference/fn.patccat.md),
    or its idiomatic alias
    [`patccat_classify()`](https://bganglmair.github.io/patccatClaims/reference/patccat_classify.md).

## Key functions

- [`fn.patccat`](https://bganglmair.github.io/patccatClaims/reference/fn.patccat.md),
  [`patccat_classify`](https://bganglmair.github.io/patccatClaims/reference/patccat_classify.md):

  Classify claims; return `claimType` and the claim-structure flags.

- [`patccat_load_model`](https://bganglmair.github.io/patccatClaims/reference/patccat_load_model.md):

  Download and load the pinned udpipe POS model.

- [`patccat_defaults`](https://bganglmair.github.io/patccatClaims/reference/patccat_defaults.md):

  Inspect the validated internal parameters and word lists (and use them
  as a starting point for the override arguments).

- [`fn.benchmarking`](https://bganglmair.github.io/patccatClaims/reference/fn.benchmarking.md):

  Score classifier output against a gold benchmark.

## Claim types

`claimType` is `0` (uncategorized), `1` (process / method), or `2`
(product). Product-by-process claims are folded into product (`2`) and
flagged separately in the `prodByProcess` column.

## Reproducibility

On its default (no-override) path the classifier reproduces the
committed AMT validation baseline – accuracy 0.9972248 and coverage
0.9897253 on the 9,830-claim multi-line sample. Supplying the override
arguments of
[`fn.patccat`](https://bganglmair.github.io/patccatClaims/reference/fn.patccat.md)
changes the classification and leaves that baseline behind.

## Part-of-speech model

Claim text is tagged with the udpipe English-EWT model, pinned to build
`ud-2.5-191206` so classifications stay reproducible. The model file is
a third-party artifact and is *not* shipped with the package; it is
downloaded once into a per-user cache by
[`patccat_load_model`](https://bganglmair.github.io/patccatClaims/reference/patccat_load_model.md).
Credit: the udpipe R package (Jan Wijffels) and the Universal
Dependencies English-EWT treebank.

## Acknowledgements

The R-package scaffolding, documentation, and repository setup were done
with assistance from Anthropic's Claude (Cowork). The classifier and its
validation are the authors' own work.

## Author

Bernhard Ganglmair (maintainer, <b.ganglmair@gmail.com>) and W. Keith
Robinson.

## See also

[`fn.patccat`](https://bganglmair.github.io/patccatClaims/reference/fn.patccat.md),
[`patccat_classify`](https://bganglmair.github.io/patccatClaims/reference/patccat_classify.md),
[`patccat_load_model`](https://bganglmair.github.io/patccatClaims/reference/patccat_load_model.md),
[`patccat_defaults`](https://bganglmair.github.io/patccatClaims/reference/patccat_defaults.md),
[`fn.benchmarking`](https://bganglmair.github.io/patccatClaims/reference/fn.benchmarking.md)

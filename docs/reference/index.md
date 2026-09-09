# Package index

## Classify claims

Label U.S. patent claims product vs process and extract the
claim-structure flags.

- [`fn.patccat()`](https://bganglmair.github.io/patccatClaims/reference/fn.patccat.md)
  : Classify patent claims as product or process
- [`patccat_classify()`](https://bganglmair.github.io/patccatClaims/reference/patccat_classify.md)
  : Classify patent claims (alias for fn.patccat)

## Model and defaults

The pinned udpipe part-of-speech model, and the validated internal
parameters and word lists behind the default classification path.

- [`patccat_load_model()`](https://bganglmair.github.io/patccatClaims/reference/patccat_load_model.md)
  : Download and load the udpipe POS model
- [`patccat_defaults()`](https://bganglmair.github.io/patccatClaims/reference/patccat_defaults.md)
  : The validated internal parameters and word lists

## Validation

Score classifier output against a gold benchmark.

- [`fn.benchmarking()`](https://bganglmair.github.io/patccatClaims/reference/fn.benchmarking.md)
  : Score classifier output against a benchmark

## Package overview

- [`patccatClaims-package`](https://bganglmair.github.io/patccatClaims/reference/patccatClaims-package.md)
  [`patccatClaims`](https://bganglmair.github.io/patccatClaims/reference/patccatClaims-package.md)
  : patccatClaims: Product vs Process U.S. Patent Claim Classifier

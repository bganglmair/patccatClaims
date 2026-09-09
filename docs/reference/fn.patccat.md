# Classify patent claims as product or process

Classifies U.S. patent claims as product or process and attaches
claim-structure flags, using udpipe part-of-speech tags.

## Usage

``` r
fn.patccat(data, varPatentClaim = "PatentClaim", varText = "text",
  varLevel = "level", varSequence = "sequence", varID = "id",
  extractPreambleTerms = TRUE, extractPreambleText = TRUE,
  timeOutput = TRUE, integrityCheck = TRUE,
  params = NULL, claim.rules = NULL, process.words = NULL,
  product.words = NULL, processwords.simple = NULL)
```

## Arguments

- data:

  A data frame of claim lines: one row per claim line.

- varPatentClaim:

  Column naming the claim key (default "PatentClaim").

- varText:

  Column with the claim text (default "text").

- varLevel:

  Column with the line level (default "level").

- varSequence:

  Column with the line sequence (default "sequence").

- varID:

  Column with a unique row id (default "id").

- extractPreambleTerms:

  Extract preamble terms (default TRUE).

- extractPreambleText:

  Extract preamble text; required for Beauregard detection (default
  TRUE).

- timeOutput:

  Print timing (default TRUE).

- integrityCheck:

  Run the input integrity check (default TRUE).

- params, claim.rules, process.words, product.words,
  processwords.simple:

  Optional overrides for the classifier's internal parameters and word
  lists. Each defaults to `NULL`, meaning the validated internal value
  is used, so the default call reproduces the classifier exactly.
  Supplying your own values changes the classification and leaves the
  validated AMT baseline behind; intended for research and
  experimentation. See
  [`patccat_defaults`](https://bganglmair.github.io/patccatClaims/reference/patccat_defaults.md)
  to inspect the internal values (if available), or the package data.

## Value

A data frame with one row per independent claim, keyed by `PatentClaim`.
The label is `claimType` (0 = uncategorized, 1 = process/method, 2 =
product); product-by-process is folded into product and flagged in
`prodByProcess`. Alongside it are the claim-structure flags, including
`independent`, `singleLine`, `Jepson`, `isMeans` (means-plus-function),
`markush`, `beauregard`, `nonTransitory`, `computerImplemented`, and
`stepPlusFunction`, plus structural fields such as `preambleType`,
`bodyType`, `transitionType`, and the word counts `wordsPreamble` and
`wordsBody`.

## Details

Requires the udpipe POS model; it is loaded automatically on first use
via
[`patccat_load_model`](https://bganglmair.github.io/patccatClaims/reference/patccat_load_model.md).
Overrides passed here are resolved against the validated internal
defaults and used for that call only.

## See also

[`patccat_classify`](https://bganglmair.github.io/patccatClaims/reference/patccat_classify.md),
[`patccat_load_model`](https://bganglmair.github.io/patccatClaims/reference/patccat_load_model.md),
[`fn.benchmarking`](https://bganglmair.github.io/patccatClaims/reference/fn.benchmarking.md)

## Examples

``` r
if (FALSE) { # \dontrun{
patccat_load_model()
out <- fn.patccat(claim_lines)

## research: try a custom product-word list
out2 <- fn.patccat(claim_lines, product.words = c("apparatus", "device", "widget"))
} # }
```

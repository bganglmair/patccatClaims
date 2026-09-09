# Score classifier output against a benchmark

Compares patccat classifications to a benchmark (gold) set and reports
accuracy and coverage – overall and broken down by claim format, Jepson
status, approach, and category.

## Usage

``` r
fn.benchmarking(dataPatccat, dataBenchmark)
```

## Arguments

- dataPatccat:

  Output of
  [`fn.patccat`](https://bganglmair.github.io/patccatClaims/reference/fn.patccat.md)
  (must contain `PatentClaim` and `claimType`).

- dataBenchmark:

  A gold set keyed by `PatentClaim`, with the benchmark category in a
  `finalcategory` column (1 = process, 2 = product; product-by-process
  is folded into product).

## Value

A data frame with columns `type`, `accuracy`, and `coverage`. The row
where `type == "Overall results"` carries the headline figures; further
rows break the results down by claim format (regular vs. single-line),
Jepson vs. non-Jepson, the simple preamble/body approach, and benchmark
vs. automated category, and end with claim counts. Blank separator rows
carry `NA` in `type`, so select the overall figures positionally, e.g.
`b[which(b$type == "Overall results"), ]`.

## Details

`accuracy` is the share of *classified* claims (`claimType` 1 or 2)
whose label matches the benchmark; `coverage` is the share of claims the
classifier assigned a non-zero `claimType`. Product-by-process rows
report `NaN`, since that category is retired (folded into product).

## See also

[`fn.patccat`](https://bganglmair.github.io/patccatClaims/reference/fn.patccat.md),
[`patccat_defaults`](https://bganglmair.github.io/patccatClaims/reference/patccat_defaults.md)

## Examples

``` r
if (FALSE) { # \dontrun{
patccat_load_model()
out  <- fn.patccat(claim_lines)
gold <- data.frame(PatentClaim = out$PatentClaim, finalcategory = my_gold_labels)
b    <- fn.benchmarking(out, gold)
b[which(b$type == "Overall results"), ]   # headline accuracy + coverage
} # }
```

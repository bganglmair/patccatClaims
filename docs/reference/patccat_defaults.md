# The validated internal parameters and word lists

Returns the classifier's default parameters and word lists as a named
list – what
[`fn.patccat`](https://bganglmair.github.io/patccatClaims/reference/fn.patccat.md)
uses unless overridden, and a convenient starting point for the override
arguments.

## Usage

``` r
patccat_defaults()
```

## Value

A named list with `params`, `claim.rules`, `process.words`,
`product.words`, and `processwords.simple`.

## See also

[`fn.patccat`](https://bganglmair.github.io/patccatClaims/reference/fn.patccat.md)

## Examples

``` r
d <- patccat_defaults()
if (FALSE) { # \dontrun{
## override: extend the product word list, keep everything else validated
out <- fn.patccat(claim_lines, product.words = c(d$product.words, "widget"))
} # }
```

# Download and load the udpipe POS model

Downloads (once, into a per-user cache) and loads the pinned udpipe
English-EWT model the classifier requires, storing it in the package for
internal use.

## Usage

``` r
patccat_load_model(dir = tools::R_user_dir("patccatClaims", "data"))
```

## Arguments

- dir:

  Cache directory for the model file.

## Value

The udpipe model object, invisibly.

## Details

Called automatically on the first
[`fn.patccat`](https://bganglmair.github.io/patccatClaims/reference/fn.patccat.md)
call; call it explicitly to pre-fetch the model. The exact build
(ud-2.5-191206) is pinned so classifications stay reproducible.

## See also

[`fn.patccat`](https://bganglmair.github.io/patccatClaims/reference/fn.patccat.md)

## Examples

``` r
if (FALSE) { # \dontrun{
patccat_load_model()
} # }
```

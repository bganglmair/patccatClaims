## patccatClaims -- package-internal state.
## `.patccat` holds the udpipe POS model (see patccat_load_model) and the operative
## parameter / word-list values. fn.patccat() resets the parameter slots on every
## call -- to the user's overrides if supplied, else the validated internal defaults;
## .onLoad seeds those defaults so the internal helpers always find a value.
.patccat <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .patccat$params              <- my.params
  .patccat$claim.rules         <- my.claim.rules
  .patccat$process.words       <- my.process.words
  .patccat$product.words       <- my.product.words
  .patccat$processwords.simple <- my.processwords.simple
}

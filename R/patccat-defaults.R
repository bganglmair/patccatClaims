## patccat_defaults(): the validated internal parameters and word lists, as a
## named list. Use it to see what fn.patccat() applies by default, and as a
## starting point for the override arguments (copy, modify, pass back).
patccat_defaults <- function() {
  list(
    params              = my.params,
    claim.rules         = my.claim.rules,
    process.words       = my.process.words,
    product.words       = my.product.words,
    processwords.simple = my.processwords.simple
  )
}

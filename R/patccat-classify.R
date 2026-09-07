## patccat_classify(): idiomatic public alias for fn.patccat().
## Same arguments and return value; provided so the public API reads
## patccat_classify() / patccat_load_model() in the patccat* family style.
patccat_classify <- function(data, ...) {
  fn.patccat(data = data, ...)
}

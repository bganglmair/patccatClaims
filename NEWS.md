# patccatClaims 0.1.0

Initial release -- the core patccat product/process claim classifier as a
standalone, installable package.

* `fn.patccat()` (and its alias `patccat_classify()`) classify U.S. patent claims
  as product or process and return the claim-structure flags (means-plus-function,
  Jepson, Beauregard, Markush, single-line, and others).
* `patccat_load_model()` downloads and caches the pinned udpipe English-EWT model
  (`ud-2.5-191206`) on first use; the model is not shipped with the package.
* `patccat_defaults()` exposes the validated internal parameters and word lists,
  and seeds the opt-in override arguments of `fn.patccat()`.
* `fn.benchmarking()` scores classifier output against a gold benchmark.
* Reproduces the committed AMT validation baseline exactly: accuracy 0.9972248,
  coverage 0.9897253 on the 9,830-claim multi-line sample. `R CMD check` clean.

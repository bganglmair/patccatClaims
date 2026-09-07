test_that("fn.patccat reproduces the frozen AMT fixture labels and clears the accuracy floor", {
  skip_on_cran()
  fx_path <- system.file("extdata", "amt-fixture.csv", package = "patccatClaims")
  skip_if(!nzchar(fx_path), "fixture not installed")
  fx <- read.csv(fx_path, stringsAsFactors = FALSE, colClasses = c(PatentClaim = "character"))

  ok <- tryCatch({ patccat_load_model(); TRUE }, error = function(e) FALSE)
  skip_if_not(ok, "udpipe model unavailable (offline?)")

  input <- fx[, setdiff(names(fx), c("gold", "expected_claimType"))]
  out <- fn.patccat(input, varID = "id", varPatentClaim = "PatentClaim",
                    varText = "text", varLevel = "level", varSequence = "sequence",
                    timeOutput = FALSE)

  key <- unique(fx[, c("PatentClaim", "gold", "expected_claimType")])
  m <- merge(out[, c("PatentClaim", "claimType")], key, by = "PatentClaim")
  m <- m[order(m$PatentClaim), ]

  ## (1) regression: the classifier still reproduces its frozen labels exactly
  expect_equal(m$claimType, m$expected_claimType)

  ## (2) accuracy the patccat way -- among COVERED claims (claimType 1/2);
  ## uncategorized (0) is a coverage figure, not an accuracy miss.
  covered <- m$claimType %in% c(1L, 2L)
  expect_gte(mean(m$claimType[covered] == m$gold[covered]), 0.98)  # accuracy | covered
  expect_gte(mean(covered), 0.90)                                  # coverage
})

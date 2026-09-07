## patccat_load_model(): fetch (once) and load the pinned udpipe POS model.
## The model is a third-party file (UD English-EWT via udpipe) with its own
## licence, so it is NOT shipped with the package: it is downloaded on first use
## into a per-user cache, pinned to the exact build the classifier was validated
## against, and stored in the package-internal environment for the classifier.
patccat_load_model <- function(dir = tools::R_user_dir("patccatClaims", "data")) {
  f <- file.path(dir, "english-ewt-ud-2.5-191206.udpipe")
  if (!file.exists(f)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    message("Downloading udpipe model 'english-ewt' (ud-2.5-191206) to ", dir, " ...")
    dl <- udpipe::udpipe_download_model("english-ewt", model_dir = dir)
    if (!file.exists(f) && !is.null(dl$file_model) && file.exists(dl$file_model)) f <- dl$file_model
  }
  m <- udpipe::udpipe_load_model(f)
  .patccat$udmodel <- m
  invisible(m)
}

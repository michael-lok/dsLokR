#' Format String Values with Substitution.
#' @description Substitution keys are set with double curly brackets.
#' Note that all keys detected in `txt` must appear in optional arguments
#' @param txt str - text string with substitution keys.
#' @param ... key/value pairs that should be substituted into `txt`
#' @return `txt` with correctly substituted values
#' @export
str_form <- function(txt, ...) {

  txt.keys = gsub("[\\{\\}]", "",
                  unlist(regmatches(txt, gregexpr("\\{\\{.*?\\}\\}", txt))))

  opts = list(...)
  opt.keys = names(opts)

  if (!all(txt.keys %in% opt.keys)) {

    missing.keys <- txt.keys[!txt.keys %in% opt.keys]
    stop(sprintf("missing keys from optional args! (%s)", paste(missing.keys, collapse = ", ")))

  }
  if (any(opt.keys == "", (length(txt.keys) > 0 & is.null(opt.keys)))) {

    stop("optional arguments must be key/value pairs!")

  }
  if (!all(opt.keys %in% txt.keys)) {

    extra.keys <- opt.keys[!opt.keys %in% txt.keys]
    stop(sprintf("extra keys in optional args! (%s)", paste(extra.keys, collapse = ", ")))

  }
  if (any(is.na(opts))) {

    na.values <- opt.keys[is.na(opts)]
    stop(sprintf("NA values found! (%s)", paste(na.values, collapse = ", ")))

  }
  if (any(unlist(lapply(opts, is.null)))) {

    null.values <- names(Filter(is.null, opts))
    stop(sprintf("NULL values found! (%s)", paste(null.values, collapse = ", ")))

  }
  if (any(duplicated(opt.keys))) {

    dupe.keys <- opt.keys[duplicated(opt.keys)]
    stop(sprintf("duplicated keys! (%s)", paste(dupe.keys, collapse = ", ")))

  }

  for (key in opt.keys) {
    txt <- gsub(paste0("\\{\\{", key, "\\}\\}"), opts[[key]], txt)
  }

  return(txt)

}

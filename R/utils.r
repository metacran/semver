
check_string <- function(x, should_stop = TRUE) {
  res <- is.character(x) && length(x) == 1
  if (!res && should_stop) {
    stop("Need character scalar", call. = FALSE)
  }
  res
}

`%+%` <- function(lhs, rhs) {
  check_string(lhs)
  check_string(rhs)
  paste0(lhs, rhs)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

trim_leading <- function (x)  sub("^\\s+", "", x)

trim_trailing <- function (x) sub("\\s+$", "", x)

re_match <- function(pattern, text, global = FALSE) {

  check_string(pattern)
  check_string(text)

  fun <- if (global) gregexpr else regexpr

  match <- fun(pattern, text, perl = TRUE)

  if (match == -1) { return(list()) }

  g_start <- attr(match, "capture.start")
  g_length <- attr(match, "capture.length")

  res <- substring(text, g_start, g_start + g_length - 1)
  res[ res == "" ] <- NA_character_
  res <- as.list(res)
  res$input <- text
  res$match <- substring(text, as.vector(match),
                         as.vector(match) + attr(match, "match.length") - 1)

  res
}

`%||%` <- function(lhs, rhs) {
  lres <- withVisible(eval(lhs, envir = parent.frame()))
  if (! lres$value) {
    eval(rhs, envir = parent.frame())
  } else {
    if (lres$visible) { lres$value } else { invisible(lres$value) }
  }
}

## callback will be called with
## - 'match', the matching part
## - all groups, with names if they are named
## - 'input', the input string

re_place <- function(pattern, text, replacement, callback, global = FALSE) {

  if (missing(replacement) + missing(callback) != 1) {
    stop("Give exactly one of 'replacement' and 'callback'")
  }

  if (!missing(callback)) {
    match <- re_match(pattern, text, global = global)
    if (length(match)) {
      args <- c(match["match"], match[ names(match) == "" ], match["input"])
      do.call(callback, args)
    } else {
      text
    }
  } else {
    fun <- if (global) gsub else sub
    fun(pattern, replacement, perl = TRUE)
  }
}

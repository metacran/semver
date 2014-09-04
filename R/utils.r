
check_string <- function(x) {
  if (!is.character(x) || length(x) != 1) {
    stop("Need character scalar", call. = FALSE)
  }
}

`%+%` <- function(lhs, rhs) {
  check_string(lhs)
  check_string(rhs)
  paste0(lhs, rhs)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

trim_leading <- function (x)  sub("^\\s+", "", x)

trim_trailing <- function (x) sub("\\s+$", "", x)

rematch <- function(pattern, text, global = FALSE) {

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

  res
}

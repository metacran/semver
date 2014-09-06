
is_string <- function(x, should_stop = TRUE) {
  res <- is.character(x) && length(x) == 1
  if (!res && should_stop) {
    stop("Need character scalar", call. = FALSE)
  }
  res
}

`%+%` <- function(lhs, rhs) {
  is_string(lhs)
  is_string(rhs)
  paste0(lhs, rhs)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

trim_leading <- function (x)  sub("^\\s+", "", x)

trim_trailing <- function (x) sub("\\s+$", "", x)

re_match <- function(pattern, text, global = FALSE) {

  is_string(pattern)
  is_string(text)

  fun <- if (global) gregexpr else regexpr

  match <- fun(pattern, text, perl = TRUE)

  if (match == -1) { return(list()) }

  g_start <- attr(match, "capture.start")
  g_length <- attr(match, "capture.length")

  res <- list()
  res$match <- substring(text, as.vector(match),
                         as.vector(match) + attr(match, "match.length") - 1)
  res2 <- ifelse(g_start > 0,
                 substring(text, g_start, g_start + g_length - 1),
                 NA_character_)
  res2 <- as.list(res2)
  res <- c(res, res2)
  res$input <- text

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
      do.call(callback, match)
    } else {
      text
    }
  } else {
    fun <- if (global) gsub else sub
    fun(pattern, replacement, text, perl = TRUE)
  }
}

re_split <- function(text, split) {
  is_string(text)
  is_string(split)
  str_split(text, perl(split))[[1]]
}

add_class <- function(x, class) {
  if (!is(x, class)) attr(x, "class") <- c(class, attr(x, "class"))
  x
}

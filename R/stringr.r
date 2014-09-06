str_c <- str_join <- function(..., sep = "", collapse = NULL) {
  strings <- Filter(function(x) length(x) > 0, list(...))
  atomic <- vapply(strings, is.atomic, logical(1))
  if (!all(atomic)) {
    stop("Input to str_c should be atomic vectors", call. = FALSE)
  }

  do.call("paste", c(strings, list(sep = sep, collapse = collapse)))
}
check_string <- function(string) {
  if (!is.atomic(string))
    stop("String must be an atomic vector", call. = FALSE)

  if (!is.character(string))
    string <- as.character(string)

  string
}

check_pattern <- function(pattern, string, replacement = NULL) {
  if (!is.character(pattern))
    stop("Pattern must be a character vector", call. = FALSE)

  if (!recyclable(string, pattern, replacement)) {
    stop("Lengths of string and pattern not compatible")
  }

  pattern
}
str_count <- function(string, pattern) {
  if (length(string) == 0) return(character())
  string <- check_string(string)
  pattern <- check_pattern(pattern, string)

  if (length(pattern) == 1) {
    matches <- re_call("gregexpr", string, pattern)
  } else {
    matches <- unlist(re_mapply("gregexpr", string, pattern),
      recursive = FALSE)
  }

  match_length <- function(x) {
    len <- length(x)
    if (len > 1) return(len)
    if (identical(c(x), -1L)) 0L else 1L
  }
  vapply(matches, match_length, integer(1))
}
str_detect <- function(string, pattern) {
  string <- check_string(string)
  pattern <- check_pattern(pattern, string)

  if (length(pattern) == 1) {
    results <- re_call("grepl", string, pattern)
  } else {
    results <- unlist(re_mapply("grepl", string, pattern))
  }
  is.na(results) <- is.na(string)

  results
}
str_dup <- function(string, times) {
  string <- check_string(string)

  data <- data.frame(string, times)
  n <- nrow(data)
  string <- data$string
  times <- data$times

  output <- vapply(seq_len(n), function(i) {
    paste(rep.int(string[i], times[i]), collapse = "")
  }, character(1))

  names(output) <- names(string)
  output
}
str_extract <- function(string, pattern) {
  string <- check_string(string)
  pattern <- check_pattern(pattern, string)

  positions <- str_locate(string, pattern)
  str_sub(string, positions[, "start"], positions[, "end"])
}

str_extract_all <- function(string, pattern) {
  string <- check_string(string)
  pattern <- check_pattern(pattern, string)

  positions <- str_locate_all(string, pattern)
  lapply(seq_along(string), function(i) {
    position <- positions[[i]]
    str_sub(string[i], position[, "start"], position[, "end"])
  })
}
str_length <- function(string) {
  string <- check_string(string)

  nc <- nchar(string, allowNA = TRUE)
  is.na(nc) <- is.na(string)
  nc
}
str_locate <- function(string, pattern) {
  string <- check_string(string)
  pattern <- check_pattern(pattern, string)

  if (length(pattern) == 1) {
    results <- re_call("regexpr", string, pattern)
    match_to_matrix(results)
  } else {
    results <- re_mapply("regexpr", string, pattern)
    out <- t(vapply(results, match_to_matrix, integer(2)))
    colnames(out) <- c("start", "end")
    out
  }
}

str_locate_all <- function(string, pattern) {
  string <- check_string(string)
  pattern <- check_pattern(pattern, string)

  if (length(pattern) == 1) {
    matches <- re_call("gregexpr", string, pattern)
  } else {
    matches <- unlist(re_mapply("gregexpr", string, pattern),
      recursive = FALSE)
  }
  lapply(matches, match_to_matrix, global = TRUE)
}

match_to_matrix <- function(match, global = FALSE) {
  if (global && length(match) == 1 && (is.na(match) || match == -1)) {
    null <- matrix(0, nrow = 0, ncol = 2)
    colnames(null) <- c("start", "end")

    return(null)
  }

  start <- as.vector(match)
  start[start == -1] <- NA
  end <- start + attr(match, "match.length") - 1L

  cbind(start = start, end = end)
}


invert_match <- function(loc) {
  cbind(
    start = c(0L, loc[, "end"] + 1L),
    end = c(loc[, "start"] - 1L, -1L)
  )
}
str_match <- function(string, pattern) {
  string <- check_string(string)
  pattern <- check_pattern(pattern, string)

  if (length(string) == 0) return(character())

  matcher <- re_call("regexec", string, pattern)
  matches <- regmatches(string, matcher)

  tmp <- str_replace_all(pattern, "\\\\\\(", "")
  n <- str_length(str_replace_all(tmp, "[^(]", "")) + 1

  len <- vapply(matches, length, integer(1))
  matches[len == 0] <- rep(list(rep(NA_character_, n)), sum(len == 0))

  do.call("rbind", matches)
}

str_match_all <- function(string, pattern) {
  matches <- str_extract_all(string, pattern)

  lapply(matches, function(match) {
    str_match(match, pattern)
  })
}
fixed <- function(string) {
  if (is.perl(string)) message("Overriding Perl regexp matching")
  structure(string, fixed = TRUE)
}

is.fixed <- function(string) {
  fixed <- attr(string, "fixed")
  if (is.null(fixed)) FALSE else fixed
}

ignore.case <- function(string) {
  structure(string, ignore.case = TRUE)
}

case.ignored <- function(string) {
  ignore.case <- attr(string, "ignore.case")
  if (is.null(ignore.case)) FALSE else ignore.case
}


perl <- function(string) {
  if (is.fixed(string)) message("Overriding fixed matching")
  structure(string, perl = TRUE)
}

is.perl <- function(string) {
  perl <- attr(string, "perl")
  if (is.null(perl)) FALSE else perl
}
str_pad <- function(string, width, side = "left", pad = " ") {
  string <- check_string(string)
  stopifnot(length(width) == 1)
  stopifnot(length(side) == 1)
  stopifnot(length(pad) == 1)
  if (str_length(pad) != 1) {
    stop("pad must be single character single")
  }

  side <- match.arg(side, c("left", "right", "both"))
  needed <- pmax(0, width - str_length(string))

  left <- switch(side,
    left = needed, right = 0, both = floor(needed / 2))
  right <- switch(side,
    left = 0, right = needed, both = ceiling(needed / 2))

  lengths <- unique(c(left, right))
  padding <- str_dup(pad, lengths)

  str_c(padding[match(left, lengths)], string, padding[match(right, lengths)])
}

str_trim <- function(string, side = "both") {
  string <- check_string(string)
  stopifnot(length(side) == 1)

  side <- match.arg(side, c("left", "right", "both"))
  pattern <- switch(side, left = "^\\s+", right = "\\s+$",
    both = "^\\s+|\\s+$")

  str_replace_all(string, pattern, "")
}
str_replace <- function(string, pattern, replacement) {
  string <- check_string(string)
  pattern <- check_pattern(pattern, string, replacement)

  if (length(pattern) == 1 && length(replacement) == 1) {
    re_call("sub", string, pattern, replacement)
  } else {
    unlist(re_mapply("sub", string, pattern, replacement))
  }
}

str_replace_all <- function(string, pattern, replacement) {
  string <- check_string(string)
  pattern <- check_pattern(pattern, string, replacement)

  if (length(pattern) == 1 && length(replacement) == 1) {
    re_call("gsub", string, pattern, replacement)
  } else {
    unlist(re_mapply("gsub", string, pattern, replacement))
  }
}
str_split_fixed <- function(string, pattern, n) {
  if (length(string) == 0) {
    return(matrix(character(), nrow = 0, ncol = n))
  }
  string <- check_string(string)
  pattern <- check_pattern(pattern, string)

  if (!is.numeric(n) || length(n) != 1) {
    stop("n should be a numeric vector of length 1")
  }

  if (n == Inf) {
    stop("n must be finite", call. = FALSE)
  } else if (n == 1) {
    matrix(string, ncol = 1)
  } else {
    locations <- str_locate_all(string, pattern)
    do.call("rbind", lapply(seq_along(locations), function(i) {
      location <- locations[[i]]
      string <- string[i]

      pieces <- min(n - 1, nrow(location))
      cut <- location[seq_len(pieces), , drop = FALSE]
      keep <- invert_match(cut)

      padding <- rep("", n - pieces - 1)
      c(str_sub(string, keep[, 1], keep[, 2]), padding)
    }))
  }
}

str_split <- function(string, pattern, n = Inf) {
  if (length(string) == 0) return(list())
  string <- check_string(string)
  pattern <- check_pattern(pattern, string)

  if (!is.numeric(n) || length(n) != 1) {
    stop("n should be a numeric vector of length 1")
  }

  if (n == 1) {
    as.list(string)
  } else {
    locations <- str_locate_all(string, pattern)
    pieces <- function(mat, string) {
      cut <- mat[seq_len(min(n - 1, nrow(mat))), , drop = FALSE]
      keep <- invert_match(cut)

      str_sub(string, keep[, 1], keep[, 2])
    }
    mapply(pieces, locations, string,
      SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }
}
str_sub <- function(string, start = 1L, end = -1L) {
  if (length(string) == 0L || length(start) == 0L || length(end) == 0L) {
    return(vector("character", 0L))
  }

  string <- check_string(string)

  n <- max(length(string), length(start), length(end))
  string <- rep(string, length = n)
  start <- rep(start, length = n)
  end <- rep(end, length = n)

  len <- str_length(string)

  neg_start <- !is.na(start) & start < 0L
  start[neg_start] <- start[neg_start] + len[neg_start] + 1L

  neg_end <- !is.na(end) & end < 0L
  end[neg_end] <- end[neg_end] + len[neg_end] + 1L

  substring(string, start, end)
}

"str_sub<-" <- function(string, start = 1L, end = -1L, value) {

  str_c(
    str_sub(string, end = start - 1L),
    value,
    ifelse(end == -1L, "", str_sub(string, start = end + 1L)))
}
compact <- function(l) Filter(Negate(is.null), l)

re_call <- function(f, string, pattern, replacement = NULL) {
  args <- list(pattern, replacement, string,
    fixed = is.fixed(pattern), ignore.case = case.ignored(pattern),
    perl = is.perl(pattern))

  if (!("perl" %in% names(formals(f)))) {
    if (args$perl) message("Perl regexps not supported by ", f)
    args$perl <- NULL
  }

  do.call(f, compact(args))
}

re_mapply <- function(f, string, pattern, replacement = NULL) {
  args <- list(
    FUN = f, SIMPLIFY = FALSE, USE.NAMES = FALSE,
    pattern, replacement, string,
    MoreArgs = list(
      fixed = is.fixed(pattern),
      ignore.case = case.ignored(pattern))
    )
  do.call("mapply", compact(args))
}

recyclable <- function(...) {
  lengths <- vapply(list(...), length, integer(1))

  lengths <- lengths[lengths != 0]
  if (length(lengths) == 0) return(TRUE)

  all(max(lengths) %% lengths == 0)
}
word <- function(string, start = 1L, end = start, sep = fixed(" ")) {
  n <- max(length(string), length(start), length(end))
  string <- rep(string, length = n)
  start <- rep(start, length = n)
  end <- rep(end, length = n)

  breaks <- str_locate_all(string, sep)
  words <- lapply(breaks, invert_match)

  len <- vapply(words, nrow, integer(1))

  neg_start <- !is.na(start) & start < 0L
  start[neg_start] <- start[neg_start] + len[neg_start] + 1L

  neg_end <- !is.na(end) & end < 0L
  end[neg_end] <- end[neg_end] + len[neg_end] + 1L

  starts <- mapply(function(word, loc) word[loc, "start"], words, start)
  ends <-   mapply(function(word, loc) word[loc, "end"], words, end)

  str_sub(string, starts, ends)
}

str_wrap <- function(string, width = 80, indent = 0, exdent = 0) {
  string <- check_string(string)

  pieces <- strwrap(string, width, indent, exdent, simplify = FALSE)
  unlist(lapply(pieces, str_c, collapse = "\n"))
}

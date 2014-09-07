
#' @export
#' @importFrom R6 R6Class

range <- R6Class("range",
  public = list(
    loose = NA,
    raw = NA_character_,
    set = list(list()),
    range = NA_character_,

    initialize = function(range, loose = FALSE) {
      ra_new(self, private, range, loose)
    },

    print = function() { ra_print(self, private) },
    test = function(version) { ra_test(self, private, version) }
  ),
  private = list(
    format = function() { ra_format(self, private) },
    parse = function(range) { ra_parse(self, private, range) }
  )
)

ra_new <- function(self, private, range, loose = FALSE) {

  if (is(range, "range") && range$loose == loose) {
    self$loose <- range$loose
    self$raw <- range$raw
    self$set <- range$set
    return()
  }

  self$loose <- loose

  ## First, split based on boolean or ||
  self$raw <- range
  tmp <- re_split(range, "\\s*\\|\\|\\s*")
  tmp <- lapply(tmp, function(range) {
    private$parse(trim(range))
  })
  ## throw out any that are not relevant for whatever reason
  self$set <- Filter(function(x) length(x), tmp)

  if (!length(self$set)) {
    stop('Invalid SemVer Range: ', range)
  }

  private$format()
}

ra_print <- function(self, private) {
  private$format()
  cat('<SemVer Range "' %+% self$range %+% '">\n')
  invisible(self)
}

ra_test <- function(self, private, version) {
  ## if ANY of the sets match ALL of its comparators, then pass
  for (set in self$set) { if (test_set(set, version)) return(TRUE) }
  FALSE
}

test_set <- function(set, version) {
  for (range in set) { if (! range$test(version)) { return(FALSE) } }
  TRUE
}

ra_format <- function(self, private) {
  tmp <- lapply(self$set, function(comps) {
    comps <- sapply(comps, "[[", "value")
    trim(paste(comps, collapse = " "))
  })
  tmp <- paste(tmp, collapse = "||")
  self$range <- trim(tmp)

  invisible(self)
}

ra_parse <- function(self, private, range) {

  loose <- self$loose
  range <- trim(range)

  ## `1.2.3 - 1.2.4` => `>=1.2.3 <=1.2.4`
  hr <- if (loose) src$HYPHEN_RANGE_LOOSE else src$HYPHEN_RANGE
  range <- re_place(hr, range, callback = hyphen_replace)

  ## `> 1.2.3 < 1.2.5` => `>1.2.3 <1.2.5`
  range <- re_place(src$COMPARATOR_TRIM, range,
                    replacement = comparator_trim_replace, global = TRUE)

  ## `~ 1.2.3` => `~1.2.3`
  range <- re_place(src$TILDE_TRIM, range, replacement = tilde_trim_replace,
                    global = TRUE)

  ## `^ 1.2.3` => `^1.2.3`
  range <- re_place(src$CARET_TRIM, range, replacement = caret_trim_replace,
                    global = TRUE)

  ## normalize spaces
  range <- paste(re_split(range, "\\s+"), collapse = " ")

  ## At this point, the range is completely trimmed and
  ## ready to be split into comparators.

  comp_re <- if (loose) src$COMPARATOR_LOOSE else src$COMPARATOR
  set <- re_split(range, " ")
  set <- lapply(set, function(comp) {
    parse_comparator(comp, loose = loose)
  })
  set <- paste(set, collapse = " ")
  set <- re_split(set, "\\s+")

  if (self$loose) {
    ## in loose mode, throw out any that are not valid comparators
    set <- Filter(function(comp) grepl(comp_re, comp), set)
  }

  lapply(set, function(comp) comparator$new(comp, loose))
}

hyphen_replace <- function(match,
                           from, fM, fm, fp, fpr, fb,
                           to, tM, tm, tp, tpr, tb,
                           input) {
  if (is_x(fM)) {
    from <- ''
  } else if (is_x(fm)) {
    from <- '>=' %+% fM %+% '.0.0-0'
  } else if (is_x(fp)) {
    from <- '>=' %+% fM %+% '.' %+% fm %+% '.0-0'
  } else {
    from <- '>=' %+% from
  }

  if (is_x(tM)) {
    to <- ''
  } else if (is_x(tm)) {
    to <- '<' %+% as.character(as.numeric(tM) + 1) %+% '.0.0-0'
  } else if (is_x(tp)) {
    to <- '<' %+% tM %+% '.' %+% as.character(as.numeric(tm) + 1) %+% '.0-0'
  } else if (!is.na(tpr)) {
    to <- '<=' %+% tM %+% '.' %+% tm %+% '.' %+% tp %+% '-' %+% tpr
  } else {
    to <- '<=' %+% to
  }

  trim(from %+% ' ' %+% to)
}

parse_comparator <- function(comp, loose = FALSE) {
  comp <- replace_carets(comp, loose = loose)
  comp <- replace_tildes(comp, loose = loose)
  comp <- replace_xranges(comp, loose = loose)
  comp <- replace_stars(comp, loose = loose)
  comp
}

is_x <- function(id) {
  is.null(id) || identical(id, "") || is.na(id) ||
    identical(tolower(id), "x") || identical(id, "*")
}

## ~, ~> --> * (any, kinda silly)
## ~2, ~2.x, ~2.x.x, ~>2, ~>2.x ~>2.x.x --> >=2.0.0 <3.0.0
## ~2.0, ~2.0.x, ~>2.0, ~>2.0.x --> >=2.0.0 <2.1.0
## ~1.2, ~1.2.x, ~>1.2, ~>1.2.x --> >=1.2.0 <1.3.0
## ~1.2.3, ~>1.2.3 --> >=1.2.3 <1.3.0
## ~1.2.0, ~>1.2.0 --> >=1.2.0 <1.3.0
replace_tildes <- function(comp, loose = FALSE) {
  comp <- trim(comp)
  comp <- re_split(comp, "\\s+")
  comp <- lapply(comp, replace_tilde, loose = loose)
  paste(comp, collapse = " ")
}

replace_tilde <- function(comp, loose) {
  r <- if (loose) src$TILDE_LOOSE else src$TILDE
  re_place(r, comp, callback = function(match, M, m, p, pr, bu, input) {

    if (is_x(M)) {
      ''
    } else if (is_x(m)) {
      '>=' %+% M %+% '.0.0-0 <' %+%
        as.character(as.numeric(M) + 1) %+% '.0.0-0'
    } else if (is_x(p)) {
      ## ~1.2 == >=1.2.0- <1.3.0-
      '>=' %+% M %+% '.' %+% m %+% '.0-0 <' %+% M %+% '.' %+%
        as.character(as.numeric(m) + 1) %+% '.0-0'
    } else if (!is.na(pr)) {
      if (substring(pr, 1, 1) != '-') pr <- '-' %+% pr
      '>=' %+% M %+% '.' %+% m %+% '.' %+% p %+% pr %+%
        ' <' %+% M %+% '.' %+% as.character(as.numeric(m) + 1) %+% '.0-0'
    } else {
      ## ~1.2.3 == >=1.2.3-0 <1.3.0-0
      '>=' %+% M %+% '.' %+% m %+% '.' %+% p %+% '-0' %+%
        ' <' %+% M %+% '.' %+% as.character(as.numeric(m) + 1) %+% '.0-0'
    }
  })
}

## ^ --> * (any, kinda silly)
## ^2, ^2.x, ^2.x.x --> >=2.0.0 <3.0.0
## ^2.0, ^2.0.x --> >=2.0.0 <3.0.0
## ^1.2, ^1.2.x --> >=1.2.0 <2.0.0
## ^1.2.3 --> >=1.2.3 <2.0.0
## ^1.2.0 --> >=1.2.0 <2.0.0
replace_carets <- function(comp, loose = FALSE) {
  comp <- trim(comp)
  comp <- re_split(comp, "\\s+")
  comp <- lapply(comp, replace_caret, loose = loose)
  paste(comp, collapse = " ")
}

replace_caret <- function(comp, loose = FALSE) {
  r <- if (loose) src$CARET_LOOSE else src$CARET
  re_place(r, comp, callback = function(match, M, m, p, pr, bu, input) {

    if (!is.na(pr)) {
      if (substring(pr, 1, 1) != '-') { pr <- '-' %+% pr }
    } else {
      pr <- ''
    }

    if (is_x(M)) {
      ''
    } else if (is_x(m)) {
      '>=' %+% M %+% '.0.0-0 <' %+% as.character(as.numeric(M) + 1) %+%
        '.0.0-0'
    } else if (is_x(p)) {
      if (identical(M, '0')) {
        '>=' %+% M %+% '.' %+% m %+% '.0-0 <' %+% M %+% '.' %+%
          as.character(as.numeric(m) + 1) %+% '.0-0'
      } else {
        '>=' %+% M %+% '.' %+% m %+% '.0-0 <' %+%
          as.character(as.numeric(M) + 1) %+% '.0.0-0'
      }
    } else if (identical(M, '0')) {
      '=' %+% M %+% '.' %+% m %+% '.' %+% p %+% pr
    } else if (pr != "") {
      '>=' %+% M %+% '.' %+% m %+% '.' %+% p %+% pr %+%
            ' <' %+% as.character(as.numeric(M) + 1) %+% '.0.0-0'
    } else {
      '>=' %+% M %+% '.' %+% m %+% '.' %+% p %+% '-0' %+%
        ' <' %+% as.character(as.numeric(M) + 1) %+% '.0.0-0'
    }
  })
}

replace_xranges <- function(comp, loose = FALSE) {
  comp <- re_split(comp, "\\s+")
  comp <- lapply(comp, replace_xrange, loose = loose)
  paste(comp, collapse = " ")
}

replace_xrange <- function(comp, loose = FALSE) {
  comp <- trim(comp)
  r <- if (loose) src$XRANGE_LOOSE else src$XRANGE
  re_place(r, comp, callback = function(match, gtlt, M, m, p, pr, ...) {

    ret <- match

    xM <- is_x(M)
    xm <- xM || is_x(m)
    xp <- xm || is_x(p)
    anyX <- xp

    if (identical(gtlt, '=') && anyX) gtlt <- ''

    if (gtlt != "" && anyX) {
      ## replace X with 0, and then append the -0 min-prerelease
      if (xM) M <- "0"
      if (xm) m <- "0"
      if (xp) p <- "0"

      if (identical(gtlt, '>')) {
        ## >1 => >=2.0.0-0
        ## >1.2 => >=1.3.0-0
        ## >1.2.3 => >= 1.2.4-0
        gtlt <- '>='
        if (xM) {
          ## no change
        } else if (xm) {
          M <- as.character(as.numeric(M) + 1)
          m <- "0"
          p <- "0"
        } else if (xp) {
          m <- as.character(as.numeric(m) + 1)
          p <- "0"
        }
      }

      ret <- gtlt %+% M %+% '.' %+% m %+% '.' %+% p %+% '-0'
    } else if (xM) {
      ## allow any
      ret <- '*'
    } else if (xm) {
      ## append '-0' onto the version, otherwise
      ## '1.x.x' matches '2.0.0-beta', since the tag
      ## *lowers* the version value
      ret <- '>=' %+% M %+% '.0.0-0 <' %+%
        as.character((as.numeric(M) + 1)) %+% '.0.0-0'
    } else if (xp) {
      ret <- '>=' %+% M %+% '.' %+% m %+% '.0-0 <' %+% M %+% '.' %+%
        as.character(as.numeric(m) + 1) %+% '.0-0'
    }

    ret
  })
}

## Because * is AND-ed with everything else in the comparator,
## and '' means "any version", just remove the *s entirely.
replace_stars <- function(comp, loose = FALSE) {
  ## Looseness is ignored here.  star is always as loose as it gets!
  re_place(src$STAR, trim(comp), replacement = "")
}

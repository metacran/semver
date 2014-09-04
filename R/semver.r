
## -----------------------------------------------------------------------

#' @importFrom R6 R6Class
#' @export

semver <- R6Class("semver",
  public = list(
    version = NA_character_,
    loose = NA,
    raw = NA_character_,
    build = NA_character_,
    major = NA_integer_,
    minor = NA_integer_,
    patch = NA_integer_,
    prerelease = NA_character_,

    initialize = function(version, loose = FALSE) {
      sv_create(self, private, version, loose)
    },

    compare = function(other) { sv_compare(self, private, other) },
    format = function() { sv_format(self, private) },
    inc = function(release = c("premajor", "preminor", "prepatch",
                   "prerelease", "major", "minor", "patch", "pre")) {
      sv_inc(self, private, release) },
    print = function(...) { sv_print(self, private, ...) }
  ),
  private = list(
    compare_main = function(other) { sv_compare_main(self, private, other) },
    compare_pre = function(other) { sv_compare_pre(self, private, other) },
    compare_identifiers = function(a, b) { sv_compare_identifiers(a, b) }
  )
)

sv_create <- function(self, private, version, loose) {

  if (is(version, "semver")) {
    if (version$loose == loose ) {
      self$version    <- version$version
      self$loose      <- version$loose
      self$raw        <- version$raw
      self$build      <- version$build
      self$major      <- version$major
      self$minor      <- version$minor
      self$patch      <- version$patch
      self$prerelease <- version$prerelease
      return()
    } else {
      version <- version$version
    }
  } else {
    check_string(version)
  }

  self$loose <- loose

  m <- rematch(if (loose) src$LOOSE else src$FULL, trim(version))

  if (! length(m)) { stop("Invalid version ", version) }

  self$raw <- version

  self$major <- as.numeric(m[[1]])
  self$minor <- as.numeric(m[[2]])
  self$patch <- as.numeric(m[[3]])

  if (is.na(m[[4]])) {
    self$prerelease <- list()
  } else {
    ids <- strsplit(m[[4]], "\\.")[[1]]
    self$prerelease <- lapply(ids, function(id) {
      if (grepl("^[0-9]+$", id)) as.numeric(id) else  id
    })
  }

  self$build <- if (is.na(m[[5]])) {
    list()
  } else {
    as.list(strsplit(m[[5]], "\\.")[[1]])
  }

  self$format()
}

sv_format <- function(self, private) {
  self$version <- sprintf("%d.%d.%d", self$major, self$minor, self$patch)
  if (length(self$prerelease)) {
    self$version <- self$version %+% "-" %+%
      paste(self$prerelease, collapse = ".")
  }
  self$version
}

sv_inc <- function(self, private, release = c("premajor", "preminor",
  "prepatch", "prerelease", "major", "minor", "patch", "pre")) {
  release <- match.arg(release)
  TODO
}

sv_print <- function(self, private, ...) {
  cat('<SemVer "' %+% self$version %+% '">\n')
  invisible(self)
}

sv_compare <- function(self, private, other) {
  if (! is(other, "semver")) { other = semver$new(other, self$loose) }
  private$compare_main(other) || private$compare_pre(other)
}

sv_compare_main <- function(self, private, other) {
  if (!is(other, "semver")) { other = semver$new(other, self$loose) }
  (private$compare_identifiers(self$major, other$major) ||
   private$compare_identifiers(self$minor, other$minor) ||
   private$compare_identifiers(self$patch, other$patch))
}

sv_compare_pre <- function(self, private, other) {
  if (!is(other, "semver")) { other = semver$new(other, self$loose) }

  ## Not having a prerelease is > having one
  if (length(self$prerelease) && !length(other$prerelease)) {
    return(-1L)
  } else if (!length(self$prerelease) && length(other$prerelease)) {
    return(1L)
  } else if (!length(self$prerelease) && !length(other$prerelease)) {
    return(0)
  }

  i <- 1
  repeat {
    if (i == length(self$prerelease) && i == length(other$prerelease)) {
      return(0L)
    } else if (i == length(other$prerelease)) {
      return(1L)
    } else if (i == length(self$prerelease)) {
      return(-1L)
    }
    a <- self$prerelease[[i]]
    b <- other$prerelease[[i]]
    if (! identical(a, b)) {
      return(self$compare_identifiers(a, b))
    }
    i <- i + 1
  }
}

sv_compare_identifiers <- function(a, b) {
  numeric <- "^[0-9]+$"
  a_num <- grepl(numeric, a)
  b_num <- grepl(numeric, b)

  if (a_num && b_num) {
    a <- as.numeric(a)
    b <- as.numeric(b)
  }

  if (a_num && !b_num) {
    -1L
  } else if (b_num && !a_num) {
    1L
  } else if (a < b) {
    1L
  } else if (a > b) {
    1L
  } else {
    0
  }
}


## -----------------------------------------------------------------------

#' Semantic version number
#'
#' An R6 class for semantic version numbers. See \url{http://semver.org}
#' for details about semantic versioning.
#'
#' @section Fields:
#' \describe{
#'   \item{\code{raw}:}{The raw version number that was used as the input
#'     to create a \code{semver} object.}
#'   \item{\code{version}:}{The parsed and canonized version number string.}
#'   \item{\code{major}:}{Major version, a number.}
#'   \item{\code{minor}:}{Minor version, a number.}
#'   \item{\code{patch}:}{Patch version, a number.}
#'   \item{\code{prerelease}:}{Pre-release version strings, in a list.}
#'   \item{\code{build}:}{Build strings, in a list.}
#'   \item{\code{loose}:}{Logical scalar, whether to allow (slightly)
#'     invalid version strings.}
#' }
#'
#' @section Method \code{new}:
#'
#' \preformatted{semver$new(version, loose = FALSE)}
#'
#' \subsection{Arguments}{
#'   \describe{
#'     \item{\code{version}:}{Version string.}
#'     \item{\code{loose}:}{Whether loose strings are allowed.}
#'   }
#' }
#'
#' \subsection{Description:}{
#'   Create a new \code{semver} object.
#' }
#' 
#' @section Method \code{compare}:
#'
#' TODO
#' 
#' @section Method \code{format}:
#'
#' TODO
#' 
#' @section Method \code{inc}:
#'
#' TODO
#' 
#' @section Method \code{print}:
#'
#' \preformatted{object$print(\dots)}
#'
#' \subsection{Arguments}{
#'   \describe{
#'     \item{\dots}{Currently ignored}
#'   }
#' }
#'
#' \subsection{Description:}{
#'   This method is called automatically when a \code{semver} object
#'   is printed to the R console.
#' }
#'
#' @docType class
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
    if (!is_string(version, should_stop = FALSE)) {
      stop("Invalid version ", version)
    }
  }

  self$loose <- loose

  m <- re_match(if (loose) src$LOOSE else src$FULL, trim(version))

  if (! length(m)) { stop("Invalid version ", version) }

  self$raw <- version

  self$major <- as.numeric(m[[2]])
  self$minor <- as.numeric(m[[3]])
  self$patch <- as.numeric(m[[4]])

  if (is.na(m[[5]])) {
    self$prerelease <- list()
  } else {
    ids <- strsplit(m[[5]], "\\.")[[1]]
    self$prerelease <- lapply(ids, function(id) {
      if (grepl("^[0-9]+$", id)) as.numeric(id) else  id
    })
  }

  self$build <- if (is.na(m[[6]])) {
    list()
  } else {
    as.list(strsplit(m[[6]], "\\.")[[1]])
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

  if (release == "premajor") {
    self$prerelease <- list()
    self$patch <- 0
    self$minor <- 0
    self$major <- self$major + 1
    self$inc("pre")

  } else if (release == "preminor") {
    self$prerelease <- list()
    self$patch <- 0
    self$minor <- self$minor + 1
    self$inc("pre")

  } else if (release == "prepatch") {
    ## If this is already a prerelease, it will bump to the next version
    ## drop any prereleases that might already exist, since they are not
    ## relevant at this point.
    self$prerelease <- list()
    self$inc('patch')
    self$inc('pre')

  } else if (release == "prerelease") {
    ## If the input is a non-prerelease version, this acts the same as
    ## prepatch.
    if (length(self$prerelease) == 0) { self$inc('patch') }
    self$inc('pre')

  } else if (release == "major") {
    ## If this is a pre-major version, bump up to the same major version.
    ## Otherwise increment major.
    ## 1.0.0-5 bumps to 1.0.0
    ## 1.1.0 bumps to 2.0.0
    if (self$minor != 0 || self$patch != 0 || length(self$prerelease) == 0 ) {
      self$major <- self$major + 1
    }
    self$minor <- 0
    self$patch <- 0
    self$prerelease <- list()

  } else if (release == "minor") {
    ## If this is a pre-minor version, bump up to the same minor version.
    ## Otherwise increment minor.
    ## 1.2.0-5 bumps to 1.2.0
    ## 1.2.1 bumps to 1.3.0
    if (self$patch != 0 || length(self$prerelease) == 0 ) {
      self$minor <- self$minor + 1
    }
    self$patch <- 0
    self$prerelease <- list()

  } else if (release == "patch") {
    ## If this is not a pre-release version, it will increment the patch.
    ## If it is a pre-release it will bump up to the same patch version.
    ## 1.2.0-5 patches to 1.2.0
    ## 1.2.0 patches to 1.2.1
    if (length(self$prerelease) == 0) {
      self$patch <- self$patch + 1
    }
    self$prerelease = list();

  } else if (release == "pre") {
    ## This probably shouldn't be used publically.
    ## 1.0.0 "pre" would become 1.0.0-0 which is the wrong direction.
    if (length(self$prerelease) == 0) {
      self$prerelease <- list(0)
    } else {
      i <- length(self$prerelease)
      updated <- FALSE
      while (i > 0) {
        if (is.numeric(self$prerelease[[i]])) {
          self$prerelease[[i]] <- self$prerelease[[i]] + 1
          updated <- TRUE
          break
        }
        i <- i - 1
      }
      if (! updated) {
        ## didn't increment anything
        self$prerelease <- c(self$prerelease, list(0))
      }
    }
  }
  self$format()
  self
}

sv_print <- function(self, private, ...) {
  cat('<SemVer "' %+% self$version %+% '">\n')
  invisible(self)
}

sv_compare <- function(self, private, other) {
  if (! is(other, "semver")) { other = semver$new(other, self$loose) }
  private$compare_main(other) %||% private$compare_pre(other)
}

sv_compare_main <- function(self, private, other) {
  if (!is(other, "semver")) { other = semver$new(other, self$loose) }
  (private$compare_identifiers(self$major, other$major) %||%
   private$compare_identifiers(self$minor, other$minor) %||%
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
    if (i > length(self$prerelease) && i > length(other$prerelease)) {
      return(0L)
    } else if (i > length(other$prerelease)) {
      return(1L)
    } else if (i > length(self$prerelease)) {
      return(-1L)
    }
    a <- self$prerelease[[i]]
    b <- other$prerelease[[i]]
    if (! identical(a, b)){
      return(private$compare_identifiers(a, b))
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

  locale <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", locale), add = TRUE)
  Sys.setlocale("LC_COLLATE", "C")

  if (a_num && !b_num) {
    -1L
  } else if (b_num && !a_num) {
    1L
  } else if (a < b) {
    -1L
  } else if (a > b) {
    1L
  } else {
    0L
  }
}

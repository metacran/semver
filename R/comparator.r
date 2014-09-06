
ANY <- NULL
is_any <- is.null

#' @export
#' @importFrom R6 R6Class

comparator <- R6Class("comparator",
  public = list(
    value = NA_character_,
    loose = NA,
    operator = NA_character_,
    semver = ANY,

    initialize = function(comp, loose = FALSE) {
      comp_new(self, private, comp, loose)
    },

    print = function() { comp_print(self, private) },
    test = function(version) { comp_test(self, private, version) }
  ),
  private = list(
    parse = function(comp, loose = FALSE) {
      comp_parse(self, private, comp, loose)
    }
  )
)

comp_new <- function(self, private, comp, loose) {
  if (is(comp, "comparator")) {
    if (comp$loose == loose) {
      self$value <- comp$value
      self$loos <- comp$loose
      self$operator <- comp$operator
      self$semver <- semver$new(comp$semver)
      return()
    } else {
      comp <- comp$value
    }
  }

  self$loose <- loose
  private$parse(comp)

  if (is_any(self$semver)) {
    self$value <- ""
  } else {
    self$value <- self$operator %+% self$semver$version
  }
}

comp_print <- function(self, private) {
  cat('<SemVer Comparator "' %+% self$value %+% '">\n')
}

comp_test <- function(self, private, version) {
  if (is_any(self$semver)) {
    TRUE
  } else {
    cmp(version, self$operator, self$semver, self$loose)
  }
}

comp_parse <- function(self, private, comp, loose) {
  r <- if (self$loose) src$COMPARATOR_LOOSE else src$COMPARATOR
  m <- re_match(r, comp)

  if (! length(m)) { stop("Invalid comparator ", comp) }

  self$operator <- m[[2]]
  if (is.na(self$operator)) { self$operator = "" }
  if (self$operator == "=") { self$operator = "" }

  ## if it literally is just '>' or '' then allow anything.
  if (is.na(m[[3]])) {
    self$semver = ANY;
  } else {
    self$semver = semver$new(m[[3]], self$loose)

    ## <1.2.3-rc DOES allow 1.2.3-beta (has prerelease)
    ## >=1.2.3 DOES NOT allow 1.2.3-beta
    ## <=1.2.3 DOES allow 1.2.3-beta
    ## However, <1.2.3 does NOT allow 1.2.3-beta,
    ## even though `1.2.3-beta < 1.2.3`
    ## The assumption is that the 1.2.3 version has something you
    ## *don't* want, so we push the prerelease down to the minimum.
    if (self$operator == '<' && length(self$semver$prerelease) == 0) {
      self$semver$prerelease = list(0)
      self$semver$format()
    }
  }
}

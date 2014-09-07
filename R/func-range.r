
#' @export

satisfies <- function(version, range, loose = FALSE) {

  range <- try(semver::range$new(range, loose), silent = TRUE)

  if (is(range, "try-error")) { return(FALSE) }

  range$test(version)
}

#' @export

max_satisfying <- function(versions, range, loose = FALSE) {
  versions <- Filter(function(version) satisfies(version, range, loose),
                     versions)
  if (length(versions) > 0) {
    versions <- semver_sort(versions, loose = loose)
    versions[[length(versions)]]
  } else {
    NULL
  }
}

#' @export

valid_range <- function(range, loose = FALSE) {

  range <- try(semver::range$new(range, loose)$range, silent = TRUE)

  if (is(range, "try-error")) {
    NULL
  } else {
    if (range == "") "*" else range
  }
}

#' @export

ltr <- function(version, range, loose = FALSE) {
  outside(version, range, "<", loose)
}

#' @export

gtr <- function(version, range, loose = FALSE) {
  outside(version, range, ">", loose)
}

outside <- function(version, range, hilo, loose) {

  version <- semver$new(version, loose)
  range <- semver::range$new(range, loose)

  if (hilo == ">") {
    gt_fn <- gt
    lte_fn <- lte
    lt_fn <- lt
    comp <- ">"
    ecomp <- ">="

  } else if (hilo == "<") {
    gt_fn <- lt
    lte_fn <- gte
    lt_fn <- gt
    comp <- "<"
    ecomp <- "<="

  } else {
    stop("Internal error, this should not happen")
  }

  ## If it satisifes the range it is not outside
  if (satisfies(version, range, loose)) {
    return(FALSE)
  }

  ## From now on, variable terms are as if we're in "gtr" mode.
  ## but note that everything is flipped for the "ltr" function.

  for (comparators in range$set) {

    high <- NULL
    low <- NULL

    for (comparator in comparators) {
      high <- high %||% comparator
      low <- low %||% comparator
      if (gt_fn(comparator$semver, high$semver, loose)) {
        high <- comparator
      } else if (lt_fn(comparator$semver, low$semver, loose)) {
        low <- comparator
      }
    }

    ## If the edge version comparator has a operator then our version
    ## isn't outside it
    if ((high$operator %===% comp) %||% (high$operator %===% ecomp)) {
      return(FALSE)
    }

    ## If the lowest version comparator has an operator and our version
    ## is less than it then it isn't higher than the range
    if ((nay(low$operator) %||% (low$operator %===% comp)) &&
        lte_fn(version, low$semver)) {
      return(FALSE)
    } else if ((low$operator %===% ecomp) &&
               (lt_fn(version, low$semver))) {
      return(FALSE)
    }
  }

  TRUE
}

to_comparators <- function(range, loose = FALSE) {
  res <- semver::range$new(range, loose)$set
  lapply(res, function(comp) {
    r <- lapply(comp, function(c) { c$value })
    r <- paste(r, collapse = " ")
    r <- trim(r)
    r <- re_split(r, " ")
    as.list(r)
  })
}

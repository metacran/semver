
#' @export

satisfies <- function(version, range, loose = FALSE) {

  range <- try(semver::range$new(range, loose), silent = TRUE)

  if (is(range, "try-error")) { return(FALSE) }

  range$test(version)
}

#' @export

max_satisfying <- function(versions, range, loose = FALSE) {
  TODO ## (need sorting, need <, >, etc first)
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
  TODO
}

#' @export

gtr <- function(version, range, loose = FALSE) {
  TODO
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

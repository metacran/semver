
#' Compare semantic version numbers
#'
#' TODO
#'
#' @param a Version string or \code{semver} object.
#' @param b Version string or \code{semver} object.
#' @param loose Whether loose ranges are allowed.
#' @return \code{-1} if \code{a} is older, \code{1} if \code{a} is
#'   newer, \code{0} if \code{a} and \code{b} are equal.
#' 
#' @export
#' @rdname compare

compare <- function(a, b, loose = FALSE) {
  semver$new(a, loose)$compare(b)
}

#' @export
#' @rdname compare

compare_loose <- function(a, b) {
  compare(a, b, loose = TRUE)
}

#' @export
#' @rdname compare

rev_compare <- function(a, b, loose = FALSE) {
  compare(b, a, loose = loose)
}

#' @export
#' @rdname compare

gt <- function(a, b, loose = FALSE) {
  compare(a, b, loose = loose) > 0
}

#' @export
#' @rdname compare

lt <- function(a, b, loose = FALSE) {
  compare(a, b, loose = loose) < 0
}

#' @export
#' @rdname compare

eq <- function(a, b, loose = FALSE) {
  compare(a, b, loose = loose) == 0
}

#' @export
#' @rdname compare

neq <- function(a, b, loose = FALSE) {
  compare(a, b, loose = loose) != 0
}

#' @export
#' @rdname compare

gte <- function(a, b, loose = FALSE) {
  compare(a, b, loose = loose) >= 0
}

#' @export
#' @rdname compare

lte <- function(a, b, loose = FALSE) {
  compare(a, b, loose = loose) <= 0
}

#' @param op Operator to test for. Possible values:
#'   \code{=} and \code{==} are equivalent, the empty string also means
#'   \code{==}; \code{!=}, \code{>}, \code{>=}, \code{<}, \code{<=}.
#' @return Logical scalar.
#' 
#' @export
#' @rdname compare

cmp <- function(a, op = "==", b, loose = FALSE) {
  if (op %in% c("", "=", "==")) { eq(a, b, loose) }
  else if (op == "!=") { neq(a, b, loose) }
  else if (op == ">") { gt(a, b, loose) }
  else if (op == ">=") { gte(a,b, loose) }
  else if (op == "<") { lt(a, b, loose) }
  else if (op == "<=") { lte(a, b, loose) }
  else { stop("Invalid operator: ", as.character(op)) }
}

#' Increase a semantic version number
#'
#' TODO
#'
#' @param version Version number string or \code{semver} object. The
#'   version number to increase.
#' @param release Which component to increase. See \code{\link{semver}} and
#'   the \code{inc} method for possible values.
#' @param loose  Whether loose ranges are allowed.
#' 
#' @export

inc <- function(version, release, loose = FALSE) {
  tryCatch(
    semver$new(version, loose)$inc(release)$version,
    error = function(e) NULL
  )
}

#' @method "[" semver_list
#' @export
#' @rdname semver_sort

`[.semver_list` <- function(x, i) {
  add_class(unclass(x)[i], "semver_list")
}

#' @method ">" semver_list
#' @export
#' @rdname semver_sort

`>.semver_list` <- function(a, b) {
  a[[1]] > b[[1]]
}

#' @method "==" semver_list
#' @export
#' @rdname semver_sort

`==.semver_list` <- function(a, b) {
  a[[1]] == b[[1]]
}

#' Sort semantic version numbers
#'
#' TODO
#'
#' @param list List or vector of version number strings or \code{semver}
#'   objects. You can also mix them.
#' @param loose Whether loose ranges are allowed.
#' @return Sorted list or vector of versions numbers or \code{semver}
#'   objects.
#' 
#' @export

semver_sort <- function(list, loose = FALSE) {
  list2 <- lapply(list, function(x) semver$new(x, loose = loose))
  class(list2) <- "semver_list"
  list[order(list2)]
}

#' Parse semantic version numbers
#'
#' TODO
#'
#' @param version Version string.
#' @param loose Whether loose ranges are allowed.
#' 
#' @export

parse_ver <- function(version, loose = FALSE) {
  r <- if (loose) src$LOOSE else src$FULL
  if (grepl(r, version, perl = TRUE)) semver$new(version, loose) else NULL
}

#' Validate semantic version numbers
#'
#' TODO
#'
#' @param version Version string.
#' @param loose Whether loose ranges are allowed.
#' 
#' @export

valid <- function(version, loose = FALSE) {
  v <- parse_ver(version, loose)
  if (!is.null(v)) v$version else NULL
}

#' Clean semantic version numbers
#'
#' TODO
#'
#' @param version Version number string.
#' @param loose Whether loose ranges are allowed.
#' 
#' @export

clean <- function(version, loose = FALSE) {
  version <- trim(version)
  version <- re_place("^[=v]+", version, replacement = "")
  s <- parse_ver(version, loose)
  if (!is.null(s)) s$version else NULL
}

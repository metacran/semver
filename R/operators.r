
#' @method "<" semver
#' @rdname compare

`<.semver` <- function(lhs, rhs) {
  lhs$compare(rhs) < 0
}

#' @method ">" semver
#' @rdname compare

`>.semver` <- function(lhs, rhs) {
  lhs$compare(rhs) > 0
}

#' @method "<=" semver
#' @rdname compare

`<=.semver` <- function(lhs, rhs) {
  lhs$compare(rhs) <= 0
}

#' @method ">=" semver
#' @rdname compare

`>=.semver` <- function(lhs, rhs) {
  lhs$compare(rhs) >= 0
}

#' @method "==" semver
#' @rdname compare

`==.semver` <- function(lhs, rhs) {
  lhs$compare(rhs) == 0
}

#' @method "!=" semver
#' @rdname compare

`!=.semver` <- function(lhs, rhs) {
  lhs$compare(rhs) != 0
}


#' @method "<" semver

`<.semver` <- function(lhs, rhs) {
  lhs$compare(rhs) < 0
}

#' @method ">" semver

`>.semver` <- function(lhs, rhs) {
  lhs$compare(rhs) > 0
}

#' @method "<=" semver

`<=.semver` <- function(lhs, rhs) {
  lhs$compare(rhs) <= 0
}

#' @method ">=" semver

`>=.semver` <- function(lhs, rhs) {
  lhs$compare(rhs) >= 0
}

#' @method "==" semver

`==.semver` <- function(lhs, rhs) {
  lhs$compare(rhs) == 0
}

#' @method "!=" semver

`!=.semver` <- function(lhs, rhs) {
  lhs$compare(rhs) != 0
}

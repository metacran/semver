
#' @export

compare <- function(a, b, loose = FALSE) {
  semver$new(a, loose)$compare(b)
}

#' @export

compare_loose <- function(a, b) {
  compare(a, b, loose = TRUE)
}

#' @export

rev_compare <- function(a, b, loose = FALSE) {
  compare(b, a, loose = loose)
}

#' @export

gt <- function(a, b, loose = FALSE) {
  compare(a, b, loose = loose) > 0
}

#' @export

lt <- function(a, b, loose = FALSE) {
  compare(a, b, loose = loose) < 0
}

#' @export

eq <- function(a, b, loose = FALSE) {
  compare(a, b, loose = loose) == 0
}

#' @export

neq <- function(a, b, loose = FALSE) {
  compare(a, b, loose = loose) != 0
}

#' @export

gte <- function(a, b, loose = FALSE) {
  compare(a, b, loose = loose) >= 0
}

#' @export

lte <- function(a, b, loose = FALSE) {
  compare(a, b, loose = loose) <= 0
}

#' @export

cmp <- function(a, op = "==", b, loose = FALSE) {
  if (op == "==") { eq(a, b) }
  else if (op == "!=") { neq(a, b, loose) }
  else if (op == ">") { gt(a, b, loose) }
  else if (op == ">=") { gte(a,b, loose) }
  else if (op == "<") { lt(a, b, loose) }
  else if (op == "<=") { lte(a, b, loose) }
  else { stop("Invalid operator: %s", as.character(op)) }
}

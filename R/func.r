
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
  if (op %in% c("", "=", "==")) { eq(a, b, loose) }
  else if (op == "!=") { neq(a, b, loose) }
  else if (op == ">") { gt(a, b, loose) }
  else if (op == ">=") { gte(a,b, loose) }
  else if (op == "<") { lt(a, b, loose) }
  else if (op == "<=") { lte(a, b, loose) }
  else { stop("Invalid operator: ", as.character(op)) }
}

#' @export

inc <- function(version, release, loose = FALSE) {
  tryCatch(
    semver$new(version, loose)$inc(release)$version,
    error = function(e) NULL
  )
}

`[.semver_list` <- function(x, i) {
  add_class(unclass(x)[i], "semver_list")
}
`>.semver_list` <- function(a, b) {
  a[[1]] > b[[1]]
}
`==.semver_list` <- function(a, b) {
  a[[1]] == b[[1]]
}

#' @export

semver_sort <- function(list, loose = FALSE) {
  list2 <- lapply(list, function(x) semver$new(x, loose = loose))
  class(list2) <- "semver_list"
  list[order(list2)]
}

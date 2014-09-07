
#' Semantic versioning for R packages
#'
#' Semantic versioning is a method for consistently assigning
#' version numbers to various builds of your software. See
#' \url{http://semver.org} for details.
#'
#' This package includes utilities to deal with semantic versioning
#' tasks: \itemize{
#'   \item Validate version numbers, \code{\link{valid}}.
#'   \item Decide if a version is newer than another one,
#'     \code{\link{gt}}, \code{\link{lt}}.
#'   \item Increasing a version number, \code{\link{inc}}.
#'   \item Sorting a list of version numbers, \code{\link{semver_sort}}.
#'   \item Tools to specifies ranges of version numbers,
#'     \code{\link{range}}.
#'   \item Tools to check if a version number satisfied a range
#'     requirement, \code{\link{satisfies}}.
#' }
#'
#' The API and the implementation closely follows the
#' Javascript semantic versioning implementation for the npm
#' package manager, see \url{https://github.com/npm/node-semver}.
#' 
#' @docType package
#' @rdname semver-package
#' @name semver-package
NULL

#' The version of the semantic versioning standard this package implements
#'
#' See \url{http://semver.org} for details.
#' 
#' @export

SEMVER_SPEC_VERSION <-  "2.0.0"

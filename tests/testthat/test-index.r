
tests <- list(
  c('0.0.0', '0.0.0-foo'),
  c('0.0.1', '0.0.0'),
  c('1.0.0', '0.9.9'),
  c('0.10.0', '0.9.0'),
  c('0.99.0', '0.10.0'),
  c('2.0.0', '1.2.3'),
  c('v0.0.0', '0.0.0-foo', TRUE),
  c('v0.0.1', '0.0.0', TRUE),
  c('v1.0.0', '0.9.9', TRUE),
  c('v0.10.0', '0.9.0', TRUE),
  c('v0.99.0', '0.10.0', TRUE),
  c('v2.0.0', '1.2.3', TRUE),
  c('0.0.0', 'v0.0.0-foo', TRUE),
  c('0.0.1', 'v0.0.0', TRUE),
  c('1.0.0', 'v0.9.9', TRUE),
  c('0.10.0', 'v0.9.0', TRUE),
  c('0.99.0', 'v0.10.0', TRUE),
  c('2.0.0', 'v1.2.3', TRUE),
  c('1.2.3', '1.2.3-asdf'),
  c('1.2.3', '1.2.3-4'),
  c('1.2.3', '1.2.3-4-foo'),
  c('1.2.3-5-foo', '1.2.3-5'),
  c('1.2.3-5', '1.2.3-4'),
  c('1.2.3-5-foo', '1.2.3-5-Foo'),
  c('3.0.0', '2.7.2+asdf'),
  c('1.2.3-a.10', '1.2.3-a.5'),
  c('1.2.3-a.b', '1.2.3-a.5'),
  c('1.2.3-a.b', '1.2.3-a'),
  c('1.2.3-a.b.c.10.d.5', '1.2.3-a.b.c.5.d.100')
)

context("Simple comparisons, gt")

test_that("gt", {
  sapply(tests, function(args)
    expect_true(do.call(gt, as.list(args)),
                info = paste(args, collapse = ",")))
})

context("Simple comparisons, lt")

test_that("lt", {
  sapply(tests, function(args) {
    args[1:2] <- args[2:1]
    expect_true(do.call(lt, as.list(args)),
                info = paste(args, collapse = ","))
  })
})

context("Simple comparisons, eq")

test_that("eq", {
  sapply(tests, function(args) {
    args[1:2] <- args[c(1,1)]
    expect_true(do.call(eq, as.list(args)),
                info = paste(args, collapse = ","))
  })
  sapply(tests, function(args) {
    args[1:2] <- args[c(2,2)]
    expect_true(do.call(eq, as.list(args)),
                info = paste(args, collapse = ","))
  })
})

context("Simple comparisons, neq")

test_that("neq", {
  sapply(tests, function(args) {
    expect_true(do.call(neq, as.list(args)),
                info = paste(args, collapse = ","))
  })
})

context("Simple comparisons, cmp")

test_that("cmp", {
  sapply(tests, function(args) {
    args[2] <- args[1]
    args <- c(args[1], "==", args[2:length(args)])
    expect_true(do.call(cmp, as.list(args)),
                info = paste(args, collapse = ","))
  })
  sapply(tests, function(args) {
    args[1] <- args[2]
    args <- c(args[1], "==", args[2:length(args)])
    expect_true(do.call(cmp, as.list(args)),
                info = paste(args, collapse = ","))
  })
  sapply(tests, function(args) {
    args <- c(args[1], ">=", args[2:length(args)])
    expect_true(do.call(cmp, as.list(args)),
                info = paste(args, collapse = ","))
  })
  sapply(tests, function(args) {
    args[1:2] <- args[2:1]
    args <- c(args[1], "<=", args[2:length(args)])
    expect_true(do.call(cmp, as.list(args)),
                info = paste(args, collapse = ","))
  })
  sapply(tests, function(args) {
    args[1:2] <- args[2:1]
    args <- c(args[1], "!=", args[2:length(args)])
    expect_true(do.call(cmp, as.list(args)),
                info = paste(args, collapse = ","))
  })
})

context("Operators, <")

test_that("<", {
  sapply(tests, function(args) {
    lhs <- args[[1]]
    rhs <- args[[2]]
    loose <- if (length(args) == 3) args[[3]] else FALSE
    expect_true(semver$new(rhs, loose) < lhs,
                info = paste(args, collapse = ","))
  })
})

context("Operators, >")

test_that(">", {
  sapply(tests, function(args) {
    lhs <- args[[1]]
    rhs <- args[[2]]
    loose <- if (length(args) == 3) args[[3]] else FALSE
    expect_true(semver$new(lhs, loose) > rhs,
                info = paste(args, collapse = ","))
  })
})

context("Operators, ==")

test_that("==", {
  sapply(tests, function(args) {
    lhs <- args[[1]]
    rhs <- args[[2]]
    loose <- if (length(args) == 3) args[[3]] else FALSE
    expect_true(semver$new(lhs, loose) == lhs,
                info = paste(args, collapse = ","))
  })

  sapply(tests, function(args) {
    lhs <- args[[1]]
    rhs <- args[[2]]
    loose <- if (length(args) == 3) args[[3]] else FALSE
    expect_true(semver$new(rhs, loose) == rhs,
                info = paste(args, collapse = ","))
  })
})

context("Operators, !=")

test_that("!=", {
  sapply(tests, function(args) {
    lhs <- args[[1]]
    rhs <- args[[2]]
    loose <- if (length(args) == 3) args[[3]] else FALSE
    expect_true(semver$new(lhs, loose) != rhs,
                info = paste(args, collapse = ","))
  })
})

tests2 <- list(
  c('1.2.3', 'v1.2.3', TRUE),
  c('1.2.3', '=1.2.3', TRUE),
  c('1.2.3', 'v 1.2.3', TRUE),
  c('1.2.3', '= 1.2.3', TRUE),
  c('1.2.3', ' v1.2.3', TRUE),
  c('1.2.3', ' =1.2.3', TRUE),
  c('1.2.3', ' v 1.2.3', TRUE),
  c('1.2.3', ' = 1.2.3', TRUE),
  c('1.2.3-0', 'v1.2.3-0', TRUE),
  c('1.2.3-0', '=1.2.3-0', TRUE),
  c('1.2.3-0', 'v 1.2.3-0', TRUE),
  c('1.2.3-0', '= 1.2.3-0', TRUE),
  c('1.2.3-0', ' v1.2.3-0', TRUE),
  c('1.2.3-0', ' =1.2.3-0', TRUE),
  c('1.2.3-0', ' v 1.2.3-0', TRUE),
  c('1.2.3-0', ' = 1.2.3-0', TRUE),
  c('1.2.3-1', 'v1.2.3-1', TRUE),
  c('1.2.3-1', '=1.2.3-1', TRUE),
  c('1.2.3-1', 'v 1.2.3-1', TRUE),
  c('1.2.3-1', '= 1.2.3-1', TRUE),
  c('1.2.3-1', ' v1.2.3-1', TRUE),
  c('1.2.3-1', ' =1.2.3-1', TRUE),
  c('1.2.3-1', ' v 1.2.3-1', TRUE),
  c('1.2.3-1', ' = 1.2.3-1', TRUE),
  c('1.2.3-beta', 'v1.2.3-beta', TRUE),
  c('1.2.3-beta', '=1.2.3-beta', TRUE),
  c('1.2.3-beta', 'v 1.2.3-beta', TRUE),
  c('1.2.3-beta', '= 1.2.3-beta', TRUE),
  c('1.2.3-beta', ' v1.2.3-beta', TRUE),
  c('1.2.3-beta', ' =1.2.3-beta', TRUE),
  c('1.2.3-beta', ' v 1.2.3-beta', TRUE),
  c('1.2.3-beta', ' = 1.2.3-beta', TRUE),
  c('1.2.3-beta+build', ' = 1.2.3-beta+otherbuild', TRUE),
  c('1.2.3+build', ' = 1.2.3+otherbuild', TRUE),
  c('1.2.3-beta+build', '1.2.3-beta+otherbuild'),
  c('1.2.3+build', '1.2.3+otherbuild'),
  c('  v1.2.3+build', '1.2.3+otherbuild')
)

context("Equality tests, eq")

test_that("eq", {
  sapply(tests2, function(args) {
    expect_true(do.call(eq, as.list(args)),
                info = paste(args, collapse = ","))
  })
})

context("Equality tests, neq")

test_that("neq", {
  sapply(tests2, function(args) {
    expect_false(do.call(neq, as.list(args)),
                 info = paste(args, collapse = ","))
  })
})

context("Equality tests, gt")

test_that("gt", {
  sapply(tests2, function(args) {
    expect_false(do.call(gt, as.list(args)),
                 info = paste(args, collapse = ","))
  })
})

context("Equality tests, gte")

test_that("gte", {
  sapply(tests2, function(args) {
    expect_true(do.call(gte, as.list(args)),
                 info = paste(args, collapse = ","))
  })
})

context("Equality tests, lt")

test_that("lt", {
  sapply(tests2, function(args) {
    expect_false(do.call(lt, as.list(args)),
                 info = paste(args, collapse = ","))
  })
})

context("Equality tests, lte")

test_that("lte", {
  sapply(tests2, function(args) {
    expect_true(do.call(lte, as.list(args)),
                 info = paste(args, collapse = ","))
  })
})

context("Equality tests, cmp")

test_that("cmp", {
  sapply(tests2, function(args) {
    args <- c(args[1], "==", args[2:length(args)])
    expect_true(do.call(cmp, as.list(args)),
                info = paste(args, collapse = ","))
  })
  sapply(tests2, function(args) {
    args <- c(args[1], "!=", args[2:length(args)])
    expect_false(do.call(cmp, as.list(args)),
                 info = paste(args, collapse = ","))
  })
})

context("Equality tests, ==")

test_that("eq ==", {
  sapply(tests2, function(args) {
    lhs <- args[[1]]
    rhs <- args[[2]]
    loose <- if (length(args) == 3) args[[3]] else FALSE
    expect_true(semver$new(lhs, loose) == rhs,
                info = paste(args, collapse = ","))
  })
})

context("Equality tests, !=")

test_that("eq !=", {
  sapply(tests2, function(args) {
    lhs <- args[[1]]
    rhs <- args[[2]]
    loose <- if (length(args) == 3) args[[3]] else FALSE
    expect_false(semver$new(lhs, loose) != rhs,
                 info = paste(args, collapse = ","))
  })
})

context("Equality tests, >")

test_that("eq >", {
  sapply(tests2, function(args) {
    lhs <- args[[1]]
    rhs <- args[[2]]
    loose <- if (length(args) == 3) args[[3]] else FALSE
    expect_false(semver$new(lhs, loose) > rhs,
                 info = paste(args, collapse = ","))
  })
})

context("Equality tests, >=")

test_that("eq >=", {
  sapply(tests2, function(args) {
    lhs <- args[[1]]
    rhs <- args[[2]]
    loose <- if (length(args) == 3) args[[3]] else FALSE
    expect_true(semver$new(lhs, loose) >= rhs,
                info = paste(args, collapse = ","))
  })
})

context("Equality tests, <")

test_that("eq <", {
  sapply(tests2, function(args) {
    lhs <- args[[1]]
    rhs <- args[[2]]
    loose <- if (length(args) == 3) args[[3]] else FALSE
    expect_false(semver$new(lhs, loose) < rhs,
                 info = paste(args, collapse = ","))
  })
})

context("Equality tests, <=")

test_that("eq <=", {
  sapply(tests2, function(args) {
    lhs <- args[[1]]
    rhs <- args[[2]]
    loose <- if (length(args) == 3) args[[3]] else FALSE
    expect_true(semver$new(lhs, loose) <= rhs,
                info = paste(args, collapse = ","))
  })
})

context("Invalid version numbers")

test_that("Error for invalid version strings", {
  inv <- list(
    '1.2.3.4',
    'NOT VALID',
    1.2,
    NULL,
    'Infinity.NaN.Infinity'
  )

  sapply(inv, function(arg) {
    expect_error(semver$new(arg), "Invalid version")
  })
})

context("Strict and loose version numbers")

test_that("Strict and loose version numbers", {

  loose <- list(
    c('=1.2.3', '1.2.3'),
    c('01.02.03', '1.2.3'),
    c('1.2.3-beta.01', '1.2.3-beta.1'),
    c('   =1.2.3', '1.2.3'),
    c('1.2.3foo', '1.2.3-foo')
  )

  sapply(loose, function(args) {
    expect_error(semver$new(args[1]), "Invalid version")
    lv <- semver$new(args[2], TRUE)
    expect_equal(lv$version, args[2])
    expect_true(eq(args[1], args[2], TRUE))
    expect_error(eq(args[1], args[2]), "Invalid version")
    expect_error(semver$new(args[2])$compare(args[1]), "Invalid version")
  })

})

context("Sorting")

test_that("sorting", {

  sapply(tests, function(args) {
    v1 <- args[1:2]
    v2 <- args[2:1]
    wanted <- args[2:1]
    loose <- if (length(args) == 3) args[[3]] else FALSE

    found1 <- semver_sort(v1, loose = loose)
    expect_equal(found1, wanted, info = paste(args, collapse = ","))

    found2 <- semver_sort(v2, loose = loose)
    expect_equal(found2, wanted, info = paste(args, collapse = ","))

    v3 <- lapply(args[1:2], semver$new)
    v4 <- lapply(args[2:1], semver$new)
    wanted2 <- v4

    found3 <- semver_sort(v3, loose = loose)
    expect_equal(found3, wanted2, info = paste(args, collapse = ","))

    found4 <- semver_sort(v4, loose = loose)
    expect_equal(found4, wanted2, info = paste(args, collapse = ","))
  })
})


context("Increment version")

tests <- list(
  list('1.2.3', 'major', '2.0.0'),
  list('1.2.3', 'minor', '1.3.0'),
  list('1.2.3', 'patch', '1.2.4'),
  list('1.2.3tag', 'major', '2.0.0', TRUE),
  list('1.2.3-tag', 'major', '2.0.0'),
  list('1.2.3', 'fake', NULL),
  list('1.2.0-0', 'patch', '1.2.0'),
  list('fake', 'major', NULL),
  list('1.2.3-4', 'major', '2.0.0'),
  list('1.2.3-4', 'minor', '1.3.0'),
  list('1.2.3-4', 'patch', '1.2.3'),
  list('1.2.3-alpha.0.beta', 'major', '2.0.0'),
  list('1.2.3-alpha.0.beta', 'minor', '1.3.0'),
  list('1.2.3-alpha.0.beta', 'patch', '1.2.3'),
  list('1.2.4', 'prerelease', '1.2.5-0'),
  list('1.2.3-0', 'prerelease', '1.2.3-1'),
  list('1.2.3-alpha.0', 'prerelease', '1.2.3-alpha.1'),
  list('1.2.3-alpha.1', 'prerelease', '1.2.3-alpha.2'),
  list('1.2.3-alpha.2', 'prerelease', '1.2.3-alpha.3'),
  list('1.2.3-alpha.0.beta', 'prerelease', '1.2.3-alpha.1.beta'),
  list('1.2.3-alpha.1.beta', 'prerelease', '1.2.3-alpha.2.beta'),
  list('1.2.3-alpha.2.beta', 'prerelease', '1.2.3-alpha.3.beta'),
  list('1.2.3-alpha.10.0.beta', 'prerelease', '1.2.3-alpha.10.1.beta'),
  list('1.2.3-alpha.10.1.beta', 'prerelease', '1.2.3-alpha.10.2.beta'),
  list('1.2.3-alpha.10.2.beta', 'prerelease', '1.2.3-alpha.10.3.beta'),
  list('1.2.3-alpha.10.beta.0', 'prerelease', '1.2.3-alpha.10.beta.1'),
  list('1.2.3-alpha.10.beta.1', 'prerelease', '1.2.3-alpha.10.beta.2'),
  list('1.2.3-alpha.10.beta.2', 'prerelease', '1.2.3-alpha.10.beta.3'),
  list('1.2.3-alpha.9.beta', 'prerelease', '1.2.3-alpha.10.beta'),
  list('1.2.3-alpha.10.beta', 'prerelease', '1.2.3-alpha.11.beta'),
  list('1.2.3-alpha.11.beta', 'prerelease', '1.2.3-alpha.12.beta'),
  list('1.2.0', 'prepatch', '1.2.1-0'),
  list('1.2.0-1', 'prepatch', '1.2.1-0'),
  list('1.2.0', 'preminor', '1.3.0-0'),
  list('1.2.3-1', 'preminor', '1.3.0-0'),
  list('1.2.0', 'premajor', '2.0.0-0'),
  list('1.2.3-1', 'premajor', '2.0.0-0'),
  list('1.2.0-1', 'minor', '1.2.0'),
  list('1.0.0-1', 'major', '1.0.0')
)

test_that("inc", {

  sapply(tests, function(args) {
    pre <- args[[1]]
    what <- args[[2]]
    wanted <- args[[3]]
    loose <- if (length(args) >= 4) args[[4]] else FALSE
    found <- inc(pre, what, loose)
    expect_equal(found, wanted, info = paste(args, collapse = ", "))
  })

})

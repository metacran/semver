
context("Clean")

clean_tests <- list(
  list('1.2.3', '1.2.3'),
  list(' 1.2.3 ', '1.2.3'),
  list(' 1.2.3-4 ', '1.2.3-4'),
  list(' 1.2.3-pre ', '1.2.3-pre'),
  list('  =v1.2.3   ', '1.2.3'),
  list('v1.2.3', '1.2.3'),
  list(' v1.2.3 ', '1.2.3'),
  list('\t1.2.3', '1.2.3')
)

test_that("clean", {

  sapply(clean_tests, function(args) {
    range <- args[[1]]
    version <- args[[2]]
    expect_equal(clean(range), version,
                 info = paste(args, collapse = ","))
  })
  
})

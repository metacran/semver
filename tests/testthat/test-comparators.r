
context("Comparators")

tests <-  list(
  list('1.0.0 - 2.0.0', list(list('>=1.0.0', '<=2.0.0'))),
  list('1.0.0', list(list('1.0.0'))),
  list('>=*', list(list('>=0.0.0-0'))),
  list('', list(list(''))),
  list('*', list(list(''))),
  list('*', list(list(''))),
  list('>=1.0.0', list(list('>=1.0.0'))),
  list('>=1.0.0', list(list('>=1.0.0'))),
  list('>=1.0.0', list(list('>=1.0.0'))),
  list('>1.0.0', list(list('>1.0.0'))),
  list('>1.0.0', list(list('>1.0.0'))),
  list('<=2.0.0', list(list('<=2.0.0'))),
  list('1', list(list('>=1.0.0-0', '<2.0.0-0'))),
  list('<=2.0.0', list(list('<=2.0.0'))),
  list('<=2.0.0', list(list('<=2.0.0'))),
  list('<2.0.0', list(list('<2.0.0-0'))),
  list('<2.0.0', list(list('<2.0.0-0'))),
  list('>= 1.0.0', list(list('>=1.0.0'))),
  list('>=  1.0.0', list(list('>=1.0.0'))),
  list('>=   1.0.0', list(list('>=1.0.0'))),
  list('> 1.0.0', list(list('>1.0.0'))),
  list('>  1.0.0', list(list('>1.0.0'))),
  list('<=   2.0.0', list(list('<=2.0.0'))),
  list('<= 2.0.0', list(list('<=2.0.0'))),
  list('<=  2.0.0', list(list('<=2.0.0'))),
  list('<    2.0.0', list(list('<2.0.0-0'))),
  list('<\t2.0.0', list(list('<2.0.0-0'))),
  list('>=0.1.97', list(list('>=0.1.97'))),
  list('>=0.1.97', list(list('>=0.1.97'))),
  list('0.1.20 || 1.2.4', list(list('0.1.20'), list('1.2.4'))),
  list('>=0.2.3 || <0.0.1', list(list('>=0.2.3'), list('<0.0.1-0'))),
  list('>=0.2.3 || <0.0.1', list(list('>=0.2.3'), list('<0.0.1-0'))),
  list('>=0.2.3 || <0.0.1', list(list('>=0.2.3'), list('<0.0.1-0'))),
  list('||', list(list(''), list(''))),
  list('2.x.x', list(list('>=2.0.0-0', '<3.0.0-0'))),
  list('1.2.x', list(list('>=1.2.0-0', '<1.3.0-0'))),
  list('1.2.x || 2.x', list(list('>=1.2.0-0', '<1.3.0-0'),
                            list('>=2.0.0-0', '<3.0.0-0'))),
  list('1.2.x || 2.x', list(list('>=1.2.0-0', '<1.3.0-0'),
                            list('>=2.0.0-0', '<3.0.0-0'))),
  list('x', list(list(''))),
  list('2.*.*', list(list('>=2.0.0-0', '<3.0.0-0'))),
  list('1.2.*', list(list('>=1.2.0-0', '<1.3.0-0'))),
  list('1.2.* || 2.*', list(list('>=1.2.0-0', '<1.3.0-0'),
                            list('>=2.0.0-0', '<3.0.0-0'))),
  list('1.2.* || 2.*', list(list('>=1.2.0-0', '<1.3.0-0'),
                            list('>=2.0.0-0', '<3.0.0-0'))),
  list('*', list(list(''))),
  list('2', list(list('>=2.0.0-0', '<3.0.0-0'))),
  list('2.3', list(list('>=2.3.0-0', '<2.4.0-0'))),
  list('~2.4', list(list('>=2.4.0-0', '<2.5.0-0'))),
  list('~2.4', list(list('>=2.4.0-0', '<2.5.0-0'))),
  list('~>3.2.1', list(list('>=3.2.1-0', '<3.3.0-0'))),
  list('~1', list(list('>=1.0.0-0', '<2.0.0-0'))),
  list('~>1', list(list('>=1.0.0-0', '<2.0.0-0'))),
  list('~> 1', list(list('>=1.0.0-0', '<2.0.0-0'))),
  list('~1.0', list(list('>=1.0.0-0', '<1.1.0-0'))),
  list('~ 1.0', list(list('>=1.0.0-0', '<1.1.0-0'))),
  list('~ 1.0.3', list(list('>=1.0.3-0', '<1.1.0-0'))),
  list('~> 1.0.3', list(list('>=1.0.3-0', '<1.1.0-0'))),
  list('<1', list(list('<1.0.0-0'))),
  list('< 1', list(list('<1.0.0-0'))),
  list('>=1', list(list('>=1.0.0-0'))),
  list('>= 1', list(list('>=1.0.0-0'))),
  list('<1.2', list(list('<1.2.0-0'))),
  list('< 1.2', list(list('<1.2.0-0'))),
  list('1', list(list('>=1.0.0-0', '<2.0.0-0'))),
  list('1 2', list(list('>=1.0.0-0', '<2.0.0-0', '>=2.0.0-0', '<3.0.0-0'))),
  list('1.2 - 3.4.5', list(list('>=1.2.0-0', '<=3.4.5'))),
  list('1.2.3 - 3.4', list(list('>=1.2.3', '<3.5.0-0')))
)

test_that("comparators", {

  sapply(tests, function(args) {
    pre <- args[[1]]
    wanted <- args[[2]]
    found <- to_comparators(pre)
    expect_equal(wanted, found, info = paste(args, collapse = " "))
  })
  
})

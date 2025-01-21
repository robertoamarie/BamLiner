test_that("parseCIGAR correctly parses a valid CIGAR string", {
  result <- .parseCIGAR("5M1I3M")
  expect_equal(nrow(result), 2)
  expect_equal(result$count[1], 5)
  expect_equal(result$operation[1], "M")
  expect_equal(result$count[2], 1)
  expect_equal(result$operation[2], "I")
  expect_equal(result$count[3], 3)
  expect_equal(result$operation[3], "M")
})


test_that("parseCIGAR returns an error for invalid CIGAR string", {
  expect_error(.parseCIGAR("5M1Z3M"), "Invalid CIGAR string")
})

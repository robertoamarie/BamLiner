## Testing Invalid inputs
test_that("parseCIGAR returns an error for an input other than a string", {
  expect_error(.parseCIGAR(42), "the input must be string")
})


test_that("parseCIGAR returns an error for an empty CIGAR string", {
  expect_error(.parseCIGAR(""), "the CIGAR string cannot be empty")
})


test_that("parseCIGAR returns an error for an invalid CIGAR with unsupported operation", {
  expect_error(.parseCIGAR("5M1Z3M"), "Invalid CIGAR, problematic operation")
})


test_that("parseCIGAR returns an error for an invalid CIGAR with missing number", {
  expect_error(.parseCIGAR("5M1NM3M"), "Invalid CIGAR, problematic operation")
})


## Testing expected outcomes

test_that("parseCIGAR correctly parses a valid CIGAR string", {
  result <- .parseCIGAR("5M1I3M")
  expect_equal(nrow(result), 3)
  expect_equal(result$count[1], 5)
  expect_equal(result$operation[1], "M")
  expect_equal(result$count[2], 1)
  expect_equal(result$operation[2], "I")
  expect_equal(result$count[3], 3)
  expect_equal(result$operation[3], "M")
})


test_that("show_alignment_to_reference shows correct alignment without soft clipping", {
  ref_seq <- "AGCTAGCT"
  read_seq <- "AGCTAGCT"
  cigar_ops <- data.frame(count = c(8), operation = c("M"))

  result <- .show_alignment_to_reference(ref_seq, read_seq, cigar_ops)
  expect_equal(result$ref_seq, ref_seq)
  expect_equal(result$read_seq, read_seq)
})

test_that("show_alignment_to_reference highlights soft clipping in red", {
  ref_seq <- "AGCTAGCT"
  read_seq <- "A-GCTAG"
  cigar_ops <- data.frame(count = c(1, 5), operation = c("S", "M"))

  result <- .show_alignment_to_reference(ref_seq, read_seq, cigar_ops, colored_softclips = TRUE)
  expect_true(grepl("\033[31m", result$read_seq))  # Red color escape code
})

## Testing Invalid inputs

test_that("Invalid ref_seq raises an error", {
  read_seq <- "AGCTTGCTA"
  cigar_ops <- data.frame(count = c(5, 1, 3), operation = c("M", "I", "M"), stringsAsFactors = FALSE)
  # Empty ref_seq or non-character
  expect_error(.show_alignment_to_reference("", read_seq, cigar_ops, colored_softclips = TRUE), "ref_seq must be a non-empty character string.")
  expect_error(.show_alignment_to_reference(123, read_seq, cigar_ops, colored_softclips = TRUE), "ref_seq must be a non-empty character string.")
})


test_that("Invalid read_seq raises an error", {
  ref_seq <- "AGCTAGCTA"
  cigar_ops <- data.frame(count = c(5, 1, 3), operation = c("M", "I", "M"), stringsAsFactors = FALSE)
  # Empty read_seq or non-character
  expect_error(.show_alignment_to_reference(ref_seq, "", cigar_ops, colored_softclips = TRUE), "read_seq must be a non-empty character string.")
  expect_error(.show_alignment_to_reference(ref_seq, 123, cigar_ops, colored_softclips = TRUE), "read_seq must be a non-empty character string.")
})


test_that("Invalid cigar_ops raises an error", {
  ref_seq <- "AGCTAGCTA"
  read_seq <- "AGCTTGCTA"
  # cigar_ops not a data frame or empty
  expect_error(.show_alignment_to_reference(ref_seq, read_seq, list(count = c(5, 1, 3), operation = c("M", "I", "M"))), "cigar_ops must be a data frame.")
  expect_error(.show_alignment_to_reference(ref_seq, read_seq, data.frame()), "cigar_ops must not be empty.")
})



test_that("Invalid colored_softclips raises an error", {
  ref_seq <- "AGCTAGCTA"
  read_seq <- "AGCTTGCTA"
  cigar_ops <- data.frame(count = c(5, 1, 3), operation = c("M", "I", "M"), stringsAsFactors = FALSE)
  # Non-logical colored_softclips
  expect_error(.show_alignment_to_reference(ref_seq, read_seq, cigar_ops, colored_softclips = "TRUE"), "colored_softclips must be a single boolean value.")
})





## Testing expected outcomes

test_that("Valid input does not raise errors", {
  ref_seq <- "AGCTAGCTA"
  read_seq <- "AGCTTGCTA"
  cigar_ops <- data.frame(count = c(5, 1, 3), operation = c("M", "I", "M"), stringsAsFactors = FALSE)

  expect_silent(.show_alignment_to_reference(ref_seq, read_seq, cigar_ops, colored_softclips = TRUE))
})




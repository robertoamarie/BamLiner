## Testing Invalid inputs

test_that("fetch_ref_seq returns an error for a read_granges input other than a GRanges object", {
  ref_genome <- DNAStringSet(c(chr1 = "AGCTAGCTAGCT"))
  cigar_ops <- data.frame(count = c(5, 1, 3), operation = c("M", "I", "M"))

  expect_error(.fetch_ref_seq(ref_genome, "chr1:1-9", cigar_ops), "The input must be a GRanges object.")
})


test_that("fetch_ref_seq returns an error for an empty GRanges object", {
  read_granges <- GRanges()
  ref_genome <- DNAStringSet(c(chr1 = "AGCTAGCTAGCT"))
  cigar_ops <- data.frame(count = c(5, 1, 3), operation = c("M", "I", "M"))

  expect_error(.fetch_ref_seq(ref_genome, read_granges, cigar_ops), "The GRanges object must not be empty")
})


test_that("fetch_ref_seq returns an error for a ref_genome input other than DNAStringSet or BSgenome ", {
  read_granges <- GRanges("chr1:1-9", strand = "+")
  ref_genome <- list(chr1 = "AGCTAGCTAGCT")
  cigar_ops <- data.frame(count = c(5, 1, 3), operation = c("M", "I", "M"))

  expect_error(.fetch_ref_seq(ref_genome, read_granges, cigar_ops), "The ref_genome must be a DNAStringSet or a BSgenome object.")
})


test_that("fetch_ref_seq returns an error for a cigar_ops input other than data.frame", {
  read_granges <- GRanges("chr1:1-9", strand = "+")
  ref_genome <- DNAStringSet(c(chr1 = "AGCTAGCTAGCT"))
  cigar_ops <- list(count = c(5, 1, 3), operation = c("M", "I", "M"))

  expect_error(.fetch_ref_seq(ref_genome, read_granges, cigar_ops), "The cigar_ops must be a data frame.")
})


test_that("fetch_ref_seq returns an error for an empty cigar_ops input", {
  read_granges <- GRanges("chr1:1-9", strand = "+")
  ref_genome <- DNAStringSet(c(chr1 = "AGCTAGCTAGCT"))
  cigar_ops <- data.frame(count = numeric(), operation = character())

  expect_error(.fetch_ref_seq(ref_genome, read_granges, cigar_ops), "The cigar_ops data frame must not be empty.")
})



## Testing expected outcomes


test_that("Valid GRanges input passes validation", {
  read_granges <- GRanges("chr1:1-9", strand = "+")
  ref_genome <- DNAStringSet(c(chr1 = "AGCTAGCTAGCT"))
  cigar_ops <- data.frame(count = c(5, 1, 3), operation = c("M", "I", "M"))

  expect_silent(.fetch_ref_seq(ref_genome, read_granges, cigar_ops))
})


test_that("Valid ref_genome input passes validation", {
  read_granges <- GRanges("chr1:1-9", strand = "+")
  ref_genome <- DNAStringSet(c(chr1 = "AGCTAGCTAGCT"))
  cigar_ops <- data.frame(count = c(5, 1, 3), operation = c("M", "I", "M"))

  expect_silent(.fetch_ref_seq(ref_genome, read_granges, cigar_ops))
})



test_that("Valid cigar_ops input passes validation", {
  read_granges <- GRanges("chr1:1-9", strand = "+")
  ref_genome <- DNAStringSet(c(chr1 = "AGCTAGCTAGCT"))
  cigar_ops <- data.frame(count = c(5, 1, 3), operation = c("M", "I", "M"))

  expect_silent(.fetch_ref_seq(ref_genome, read_granges, cigar_ops))
})




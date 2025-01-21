test_that("fetch_ref_seq fetches the correct reference sequence without soft clipping", {
  ref_genome <- DNAStringSet(c("AGCTAGCTAGCT"))
  read_granges <- GRanges(seqnames = "chr1", ranges = IRanges(start = 1, end = 5))
  cigar_ops <- data.frame(count = c(5), operation = c("M"))

  result <- .fetch_ref_seq(ref_genome, read_granges, cigar_ops)
  expect_equal(result, "AGCTA")
})

test_that("fetch_ref_seq adjusts for soft clipping", {
  ref_genome <- DNAStringSet(c("AGCTAGCTAGCT"))
  read_granges <- GRanges(seqnames = "chr1", ranges = IRanges(start = 1, end = 8))
  cigar_ops <- data.frame(count = c(3, 5), operation = c("S", "M"))

  result <- .fetch_ref_seq(ref_genome, read_granges, cigar_ops)
  expect_equal(result, "AGCTA")
})

test_that("fetch_ref_seq returns an error for empty GRanges", {
  expect_error(.fetch_ref_seq(NULL, NULL, NULL), "Invalid GRanges object provided")
})

test_that("extract_read_by_qname_and_show_alignment extracts correct alignment", {
  # Dummy BAM object setup
  bam <- GRanges(seqnames = "chr1", ranges = IRanges(start = 1, end = 10),
                 qname = "read1", cigar = "5M")
  ref_genome <- DNAStringSet(c("AGCTAGCTAGCT"))
  result <- extract_read_by_qname_and_show_alignment(bam, "read1", ref_genome)

  expect_equal(result$cigar, "5M")
  expect_equal(result$strand, "+")
  expect_equal(result$ref_seq, "AGCTA")
  expect_equal(result$aligned_read_seq, "AGCTA")
})



test_that("extract_read_by_qname_and_show_alignment handles query name not found", {
  bam <- GRanges(seqnames = "chr1", ranges = IRanges(start = 1, end = 10),
                 qname = "read1", cigar = "5M")
  ref_genome <- DNAStringSet(c("AGCTAGCTAGCT"))

  expect_error(extract_read_by_qname_and_show_alignment(bam, "read2", ref_genome),
               "No read found with the specified qname")
})



test_that("extract_read_by_qname_and_show_alignment handles missing primary alignment", {
  bam <- GRanges(seqnames = "chr1", ranges = IRanges(start = 1, end = 10),
                 qname = "read1", cigar = "5M")
  ref_genome <- DNAStringSet(c("AGCTAGCTAGCT"))

  # Simulate secondary alignment by setting flag
  mcols(bam)$flag <- c(0x100)  # Secondary alignment flag
  expect_error(extract_read_by_qname_and_show_alignment(bam, "read1", ref_genome),
               "The specified qname exists but has no primary alignment")
})

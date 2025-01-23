## Testing Invalid inputs

test_that("Invalid bam raises an error", {
  ref_genome <- DNAStringSet("AGCTAGCTAGCT")
  names(ref_genome) <- "chr1"
  qname <- "example_read_001"

  # bam is not a GAlignments object
  expect_error(extract_read_by_qname_and_show_alignment(list(), qname, ref_genome, colored_softclips = TRUE), "bam must be a GAlignments object.")
})





test_that("Invalid qname raises an error", {
  ref_genome <- DNAStringSet("AGCTAGCTAGCT")
  names(ref_genome) <- "chr1"
  bam <- GAlignments(
    seqnames = Rle("chr1"),
    pos = 1,
    cigar = "5M1I3M",
    strand = Rle(strand("+")),
    seq = DNAStringSet("AGCTTGCTA")
  )
  mcols(bam)$flag <- 0
  mcols(bam)$qname <- "example_read_001"

  # Empty qname
  expect_error(extract_read_by_qname_and_show_alignment(bam, "", ref_genome, colored_softclips = TRUE), "qname must be a non-empty character string.")

  # Non-character qname
  expect_error(extract_read_by_qname_and_show_alignment(bam, 123, ref_genome, colored_softclips = TRUE), "qname must be a non-empty character string.")

  # Qname not found
  expect_error(extract_read_by_qname_and_show_alignment(bam, "read2", ref_genome), "No read found with the specified qname")
})



test_that("Invalid ref_genome raises an error", {
  ref_genome <- "AGCTAGCTAGCT"  # Not a DNAStringSet
  names(ref_genome) <- "chr1"
  bam <- GAlignments(
    seqnames = Rle("chr1"),
    pos = 1,
    cigar = "5M1I3M",
    strand = Rle(strand("+")),
    seq = DNAStringSet("AGCTTGCTA")
  )
  mcols(bam)$flag <- 0
  mcols(bam)$qname <- "example_read_001"
  qname <- mcols(bam)$qname[1]

  # Invalid ref_genome type
  expect_error(extract_read_by_qname_and_show_alignment(bam, qname, ref_genome, colored_softclips = TRUE), "ref_genome must be a DNAStringSet or similar object.")
})



test_that("Invalid colored_softclips raises an error", {
  ref_genome <- DNAStringSet("AGCTAGCTAGCT")
  names(ref_genome) <- "chr1"
  bam <- GAlignments(
    seqnames = Rle("chr1"),
    pos = 1,
    cigar = "5M1I3M",
    strand = Rle(strand("+")),
    seq = DNAStringSet("AGCTTGCTA")
  )
  mcols(bam)$flag <- 0
  mcols(bam)$qname <- "example_read_001"
  qname <- mcols(bam)$qname[1]

  # Non-logical colored_softclips
  expect_error(extract_read_by_qname_and_show_alignment(bam, qname, ref_genome, colored_softclips = "TRUE"), "colored_softclips must be a single boolean value.")
  # Non-logical print_output
  expect_error(extract_read_by_qname_and_show_alignment(bam, qname, ref_genome, print_output = "TRUE"), "print_output must be a single boolean value.")
})





test_that("missing primary alignment raises an error", {
  ref_genome <- DNAStringSet("AGCTAGCTAGCT")
  names(ref_genome) <- "chr1"
  bam <- GAlignments(
    seqnames = Rle("chr1"),
    pos = 1,
    cigar = "5M1I3M",
    strand = Rle(strand("+")),
    seq = DNAStringSet("AGCTTGCTA")
  )

  # secondary alignment
  mcols(bam)$flag <- c(0x100)
  mcols(bam)$qname <- "example_read_001"
  expect_error(extract_read_by_qname_and_show_alignment(bam, "example_read_001", ref_genome), "The specified qname exists but has no primary alignment.")
})




## Testing expected outcomes

test_that("Valid input does not raise errors", {
  ref_genome <- DNAStringSet("AGCTAGCTAGCT")
  names(ref_genome) <- "chr1"

  bam <- GAlignments(
    seqnames = Rle("chr1"),
    pos = 1,
    cigar = "5M1I3M",
    strand = Rle(strand("+")),
    seq = DNAStringSet("AGCTTGCTA")
  )
  mcols(bam)$flag <- 0
  mcols(bam)$qname <- "example_read_001"

  qname <- mcols(bam)$qname[1]

  expect_output(extract_read_by_qname_and_show_alignment(bam, qname, ref_genome, colored_softclips = TRUE), "CIGAR:\\t 5M1I3M \\nstrand:\\t \\+ \\nRef:\\t AGCTA-GCT \\nRead:\\t AGCTAGGCT \\n")
})

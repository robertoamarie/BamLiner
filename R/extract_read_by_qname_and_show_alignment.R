#' Extract a read by its query name and display its alignment
#'
#' `extract_read_by_qname_and_show_alignment()` extracts the primary alignment of a read from a GAlignment object based on a specified query name (qname),
#'  then computes the read's alignment to the reference genome as determined by the CIGAR operations
#'  (such as soft-clipping, insertions, and deletions), as to then finally enabling a visual contrast of the read's sequence to the reference sequence.
#'  The function outputs a detailed string representation of the alignment and allows for optional highlighting of soft-clipped regions in red.
#'  This can be useful for inspecting the alignment quality and understanding how specific reads align to the reference.
#'
#' @usage extract_read_by_qname_and_show_alignment(bam, qname, ref_genome, colored_softclips = TRUE, print_output = TRUE)
#' @param bam A BAM object containing the alignment data.
#' @param qname A character string representing the query name of the read to extract.
#' @param ref_genome A DNAStringSet or similar object representing the reference genome.
#' @param colored_softclips A boolean (default TRUE) indicating whether to highlight soft-clipped regions in the read.
#' @param print_output A boolean (default TRUE) specifying whether to print the results to the console.
#' @return A list containing the CIGAR string, the strand of the read, the reference sequence, and the aligned read sequence.
#' @examples
#' library(BSgenome.Hsapiens.UCSC.hg38)
#' ref_genome <- BSgenome.Hsapiens.UCSC.hg38::BSgenome.Hsapiens.UCSC.hg38
#'
#' bam <- readGAlignments("R_PROJCECT-BamLiner/data/10perc_subset.bam",
#'                         param = ScanBamParam(what = c("seq","flag","mapq","qname")))
#'
#' extract_read_by_qname_and_show_alignment(bam, "A00750:404:HMTM3DMXX:1:1102:19759:11913", ref_genome)
#' @export extract_read_by_qname_and_show_alignment
#' @importFrom Biostrings reverseComplement
#' @importFrom S4Vectors mcols
#' @importFrom GenomicRanges granges


extract_read_by_qname_and_show_alignment <- function(bam, qname, ref_genome, colored_softclips = TRUE, print_output = TRUE) {
  # Check if the qname exists in the BAM file
  qname_indices <- which(mcols(bam)$qname == qname)

  if (length(qname_indices) == 0) {
    stop("No read found with the specified qname.")
  }

  # Check for primary alignment among the indices
  primary_indices <- qname_indices[!bitwAnd(mcols(bam)$flag[qname_indices], 0x100)]

  if (length(primary_indices) == 0) {
    stop("The specified qname exists but has no primary alignment.")
  }

  # Use the primary alignment index
  read_index <- primary_indices

  # Extract the corresponding GRanges object for the read
  read_granges <- granges(bam[read_index])
  strand <- as.character(strand(read_granges))

  # Extract the CIGAR string and sequence for the specified read index
  cigar <- bam@cigar[read_index]
  cigar_ops <- .parseCIGAR(cigar)

  # Fetch the reference sequence by passing the GRanges object
  ref_seq <- .fetch_ref_seq(ref_genome, read_granges, cigar_ops)

  # Extract the full read sequence while accounting for the strand positioning
  seq <- mcols(bam)$seq[read_index]
  seq <- as.character(ifelse(strand == "-",
                             reverseComplement(DNAStringSet(seq)), seq)
  )

  # Apply alignment function
  alignment <- .show_alignment_to_reference(ref_seq, seq, cigar_ops, colored_softclips)

  # Prepare the result as a list
  result <- list(
    cigar = cigar,
    strand = strand,
    ref_seq = alignment$ref_seq,
    aligned_read_seq = alignment$read_seq
  )

  # Print the output if the user prefers (print_output = TRUE)
  if (print_output) {
    cat("CIGAR:\t", result$cigar, "\n")
    cat("strand:\t", result$strand, "\n")
    cat("Ref:\t", result$ref_seq, "\n")
    cat("Read:\t", result$aligned_read_seq, "\n\n")
  } else {
    # If print_output is FALSE, return the result as a list
    return(result)
  }
}

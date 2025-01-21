#' Fetch the reference sequence for a given read
#'
#' `.fetch_ref_seq()` extracts the reference sequence corresponding to the genomic location of a read. The genomic range is adjusted to account
#' for any soft-clipping present in the CIGAR string, which could affect the alignment at both the start and end of the read.
#' The function ensures that the adjusted range of the reference sequence aligns correctly with the read sequence.
#'
#' @usage .fetch_ref_seq(ref_genome, read_granges, cigar_ops)
#' @param ref_genome A DNAStringSet or similar object representing the reference genome.
#' @param read_granges A GRanges object representing the read's genomic alignment.
#' @param cigar_ops A data frame representing the parsed CIGAR string with operations and counts.
#' @return A character string representing the extracted reference sequence for the given read.
#' @examples
#' ref_genome <- "AGCTAGCTAGCT"
#' read_granges <- GRanges("chr1:1-9", strand="+")
#' cigar <- "5M1I3M"
#' cigar_ops <- data.frame(count = c(5, 1, 3),
#'                         operation = c("M", "I", "M"),
#'                         stringsAsFactors = FALSE
#'                         )
#'
#' BamLiner:::.fetch_ref_seq(ref_genome, read_granges, cigar_ops)
#'
#' @importFrom GenomicRanges GRanges seqnames start end strand
#' @importFrom IRanges IRanges
#' @importFrom Biostrings getSeq reverseComplement




.fetch_ref_seq <- function(ref_genome, read_granges, cigar_ops) {
  # Validate that the read_granges is not null
  if (is.null(read_granges) || length(read_granges) == 0) {
    stop("Invalid GRanges object provided.")
  }

  # Adjust start and end positions based on soft clipping
  soft_clip_start <- ifelse(cigar_ops$operation[1] == "S", cigar_ops$count[1], 0)
  soft_clip_end <- ifelse(cigar_ops$operation[nrow(cigar_ops)] == "S", cigar_ops$count[nrow(cigar_ops)], 0)

  adjusted_start <- max(1, start(read_granges) - soft_clip_start)  # Ensure start does not go below 1
  adjusted_end <- end(read_granges) + soft_clip_end

  # Update the GRanges object with the adjusted start and end
  adjusted_granges <- GRanges(
    seqnames = as.character(seqnames(read_granges)),
    ranges = IRanges(start = adjusted_start, end = adjusted_end),
    strand = strand(read_granges)
  )

  # Extract the reference sequence from the given genomic range
  ref_seq <- as.character(getSeq(ref_genome, adjusted_granges))

  # Return the validated reference sequence
  return(ref_seq)
}

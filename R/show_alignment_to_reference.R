#' Display the alignment of a read to the reference genome
#'
#' `.show_alignment_to_reference()` generates a clear visualization of a read aligning to its reference sequence, based on its CIGAR string.
#' Soft-clipped regions can be optionally highlighted in red for better visualization.
#'
#' @usage .show_alignment_to_reference(ref_seq, read_seq, cigar_ops, colored_softclips = FALSE)
#' @param ref_seq A character string representing the reference sequence.
#' @param read_seq A character string representing the read sequence.
#' @param cigar_ops A data frame representing the parsed CIGAR string with operations and counts.
#' @param colored_softclips A boolean (default FALSE) indicating whether soft-clipped regions should be highlighted in red (if TRUE).
#' @return A character string representing the aligned read sequence, with soft-clipped regions highlighted if requested.
#' @examples
#' ref_seq <- "AGCTAGCTA"
#' read_seq <- "AGCTTGCTA"
#' cigar <- "5M1I3M"
#' cigar_ops <- data.frame(count = c(5, 1, 3),
#'                         operation = c("M", "I", "M"),
#'                         stringsAsFactors = FALSE
#'                         )
#'
#' BamLiner:::.show_alignment_to_reference(ref_seq, read_seq, cigar_ops, colored_softclips = TRUE)




cigar_ops <- data.frame(
  count = c(4, 1, 6),
  operation = c("M", "I", "M"),
  stringsAsFactors = FALSE
)

.show_alignment_to_reference <- function(ref_seq, read_seq, cigar_ops, colored_softclips = FALSE) {
  consumed_ref_seq <- ""
  consumed_read_seq <- ""

  ref_pos <- 1
  read_pos <- 1

  for (i in seq_len(nrow(cigar_ops))) {
    op <- cigar_ops$operation[i]
    n <- cigar_ops$count[i]

    if (op %in% c("S", "M", "=", "X")) {
      # Soft clipping: show both in read in reference, unlike traditional soft-clipping which only shows in read; optionally highlights the read_seq in red
      # Match/Mismatch: Aligned sequences in both reference and read
      read_subseq <- substr(read_seq, read_pos, read_pos + n - 1)
      ref_subseq <- substr(ref_seq, ref_pos, ref_pos + n - 1)
      consumed_ref_seq <- paste0(consumed_ref_seq, ref_subseq)
      if (op == "S" & colored_softclips) {
        consumed_read_seq <- paste0(consumed_read_seq, "\033[31m", read_subseq, "\033[0m")
      } else {
        consumed_read_seq <- paste0(consumed_read_seq, ref_subseq)
      }

      ref_pos <- ref_pos + n
      read_pos <- read_pos + n

    } else if (op == "I") {
      # Insertion in the read: Add gaps to the reference, show inserted bases in the read
      read_subseq <- substr(read_seq, read_pos, read_pos + n - 1)
      consumed_ref_seq <- paste0(consumed_ref_seq, strrep("-", n))
      consumed_read_seq <- paste0(consumed_read_seq, read_subseq)
      read_pos <- read_pos + n

    } else if (op %in% c("D", "N")) {
      # Deletion in the reference: Add gaps to the read
      # Skipped region in reference (e.g., intron, gapped read): Add gaps to the read
      ref_subseq <- substr(ref_seq, ref_pos, ref_pos + n - 1)
      consumed_ref_seq <- paste0(consumed_ref_seq, ref_subseq)
      consumed_read_seq <- paste0(consumed_read_seq, strrep("-", n))
      ref_pos <- ref_pos + n

    } else if (op == "H") {
      # Hard clipping: Ignore entirely
      next
    }
  }

  # Remove colored codes if not using colored soft clips
  if (!colored_softclips) {
    consumed_read_seq <- gsub("\033\\[[0-9;]*[mK]", "", consumed_read_seq)
  }

  return(list(ref_seq = consumed_ref_seq, read_seq = consumed_read_seq))
}

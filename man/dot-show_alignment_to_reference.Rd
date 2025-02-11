% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/show_alignment_to_reference.R
\name{.show_alignment_to_reference}
\alias{.show_alignment_to_reference}
\title{Display the alignment of a read to the reference genome}
\usage{
.show_alignment_to_reference(ref_seq, read_seq, cigar_ops, colored_softclips = FALSE)
}
\arguments{
\item{ref_seq}{A character string representing the reference sequence.}

\item{read_seq}{A character string representing the read sequence.}

\item{cigar_ops}{A data frame representing the parsed CIGAR string with operations and counts.}

\item{colored_softclips}{A boolean (default FALSE) indicating whether soft-clipped regions should be highlighted in red (if TRUE).}
}
\value{
A character string representing the aligned read sequence, with soft-clipped regions highlighted if requested.
}
\description{
\code{.show_alignment_to_reference()} generates a clear visualization of a read aligning to its reference sequence, based on its CIGAR string.
Soft-clipped regions can be optionally highlighted in red for better visualization.
}
\examples{

ref_seq <- "AGCTAGCTA"
read_seq <- "AGCTTGCTA"
cigar <- "5M1I3M"
cigar_ops <- data.frame(count = c(5, 1, 3),
                        operation = c("M", "I", "M"),
                        stringsAsFactors = FALSE
                        )
BamLiner:::.show_alignment_to_reference(ref_seq, read_seq, cigar_ops, colored_softclips = TRUE)

}

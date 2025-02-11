% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_read_by_qname_and_show_alignment.R
\name{extract_read_by_qname_and_show_alignment}
\alias{extract_read_by_qname_and_show_alignment}
\title{Extract a read by its query name and display its alignment}
\usage{
extract_read_by_qname_and_show_alignment(bam, qname, ref_genome, colored_softclips = TRUE, print_output = TRUE)
}
\arguments{
\item{bam}{A BAM object containing the alignment data.}

\item{qname}{A character string representing the query name of the read to extract.}

\item{ref_genome}{A DNAStringSet or similar object representing the reference genome.}

\item{colored_softclips}{A boolean (default TRUE) indicating whether to highlight soft-clipped regions in the read.}

\item{print_output}{A boolean (default TRUE) specifying whether to print the results to the console.}
}
\value{
A list containing the CIGAR string, the strand of the read, the reference sequence, and the aligned read sequence.
}
\description{
\code{extract_read_by_qname_and_show_alignment()} extracts the primary alignment of a read from a GAlignment object based on a specified query name (qname),
then computes the read's alignment to the reference genome as determined by the CIGAR operations
(such as soft-clipping, insertions, and deletions), as to then finally enabling a visual contrast of the read's sequence to the reference sequence.
The function outputs a detailed string representation of the alignment and allows for optional highlighting of soft-clipped regions in red.
This can be useful for inspecting the alignment quality and understanding how specific reads align to the reference.
}
\examples{
# employing some Dummy BSGenome object and GAlignments object, for examples with real data look into the vignette of the package
library(Biostrings)
library(GenomicAlignments)

ref_genome <- DNAStringSet("AGCTAGCTAGCT")
names(ref_genome) <- "chr1"

bam <- GAlignments(
            seqnames = Rle("chr1"),                 # Chromosome name
            pos = 1,                                # Start position of alignment
            cigar = "5M1I3M",                       # CIGAR string (10 matched bases)
            strand = Rle(strand("+")),              # Strand of the alignment
            seq = DNAStringSet("AGCTTGCTA"),        # The sequence
            )
mcols(bam)$flag <- 0                          # SAM flag
mcols(bam)$qname <- "example_read_001"        # qname


qname <- mcols(bam)$qname[1]

extract_read_by_qname_and_show_alignment(bam, qname, ref_genome)
}

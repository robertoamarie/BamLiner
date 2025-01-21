#' BamLiner - Visualization of the alignment of a mapped read
#'
#'
#' The R package **BamLiner** provides a function that extracts the primary alignment of a desired read and enables the visualization of its
#' alignment along its reference sequence, as an useful useful quality control inspection tool. As to achieve this, the package also includes
#' some internal functions for managing CIGAR operations, determining the reference sequence of the full read, as well as the recomputing the
#' consumed read and reference sequences upon accounting for the CIGAR operations.
#'
#'
#' \tabular{ll}{
#' Package: \tab BamLiner\cr
#' Type: \tab Package\cr
#' Version: \tab 0.0.0.9000\cr
#' License: \tab GPL (>=2)\cr
#' }
#'
#' The package provides the following functions:
#'
#' \tabular{ll}{
#'
#' extract_read_by_qname_and_show_alignment():\tab Main function\cr
#'                          \tab extracts the primary alignment of a read from a GAlignment object based on a specified query name \cr
#'                          \tab and outputs a visual contrast of the read's sequence to the reference sequence\cr
#'
#' .show_alignment_to_reference():\tab Internal function\cr
#'                          \tab generates a clear visualization of a read aligning to its reference sequence, based on its CIGAR string.\cr
#'
#' .fetch_ref_seq():\tab Internal function\cr
#'                          \tab extracts the exact reference sequence corresponding to the whole genomic location of a read, softclip included.\cr
#'
#' .parseCIGAR():\tab Internal function\cr
#'                          \tab takes a CIGAR string and splits it into individual operations (such as match, insertions, deletions, etc.) along with their corresponding counts.\cr
#'
#' }
#'
#' @name BamLiner-package
#' @aliases BamLiner-package
#' @author Roberto Amarie, Politecnico di Milano & Universita degli Studi di Milano Statale [aut, cre]\cr
#' Maintainer: Roberto Amarie\cr
#' E-Mail: <roberto.amarie@@mail.polimi.it> or <roberto.amarie@@studenti.unimi.it> or <roberto.amarie@@external.fht.org>
#' @seealso \link{author}, \link{creator}
NULL

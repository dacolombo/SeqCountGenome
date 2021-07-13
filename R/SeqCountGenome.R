#' Count sequences occurrences in subsets of the genome
#'
#' This function find exact matches of search sequences in subsets of the
#' genome. The reference genome, the regions of interest and a character vector
#' containing the search sequences must be passed. Optionally, regions for
#' which no match is found can be dropped and the total count of matches can be
#' computed for each search sequence. A count matrix is returned,
#' with search sequences as columns and genomic regions as rows.
#' 
#' @param genome A BSgenome object corresponding to the reference genome
#' @param regions A GRanges object containing the genomic regions of interest
#' @param patterns A character vector containing the search sequences
#' @param overall.count A logical, if TRUE the sum of matches for each search
#' sequence will be computed
#' @param remove.0.matches A logical, if TRUE the regions with no matches will
#' be removed from the output matrix
#' @return A matrix containing the exact matches counts, with search sequences
#' as columns and genomic regions as rows
#' @author Daniele Colombo\cr
#' Politecnico di Milano\cr
#' Maintainer: Daniele Colombo\cr
#' E-Mail: <daniele20.colombo@@mail.polimi.it>
#' 
#' @usage 
#' SeqCountGenome(genome,
#' regions,
#' patterns,
#' overall.count = FALSE,
#' remove.0.matches = FALSE)
#' 
#' @examples
#' # Reference genome: hg38
#' Hsapiens <- BSgenome.Hsapiens.UCSC.hg38::BSgenome.Hsapiens.UCSC.hg38
#' # Genomic regions: promoters
#' txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene::TxDb.Hsapiens.UCSC.hg38.knownGene
#' promoters <- promoters(txdb, upstream=2000, downstream=200)
#' promoters <- promoters[grep("_",seqnames(promoters),invert=TRUE)]
#' # Search patterns: transcription factor consensus
#' transcription_consensus <- c("GTCAAGGTCA","CACGTG","GGGCGG")
#' 
#' SeqCountGenome(Hsapiens,promoters,transcription_consensus)
#' 
#' 
#' @importFrom methods is
#' @importFrom GenomicRanges GRanges
#' @importFrom BSgenome getSeq
#' @importFrom GenomeInfoDb seqinfo
#' @importFrom Biostrings vcountPattern
#'
#' @export

SeqCountGenome <- function(genome, regions, patterns,
                            overall.count=FALSE, remove.0.matches=FALSE) {

    # Check that all the required arguments are passed
    if (missing(genome)) { stop("Argument 'genome' required.") }
    if (missing(regions)) { stop("Argument 'regions' required.") }
    if (missing(patterns)) { stop("Argument 'patterns' required.") }

    # Check that the argument passed are of the correct class
    if (!is(genome, "BSgenome")) { 
        stop("'genome' must be of class 'BSgenome'") }
    if (!is(regions, "GRanges")) { 
        stop("'regions' must be of class 'GRanges'") }
    if (!is(patterns,"character")) { 
        stop("'patterns' must be a vector of strings") }
    if (!is(overall.count, "logical")) { 
        stop("'total.count' must be of class 'logical'") }
    if (!is(remove.0.matches,"logical")) { 
        stop("'remove.0.matches' must be of class 'logical'") }

    # Check if patterns contain any character which is not in DNA_ALPHABET 
    patterns_char <- unlist(strsplit(patterns, split=""))
    if (any(!(patterns_char %in% Biostrings::DNA_ALPHABET))) {
        stop("patterns must contain only characters in the DNA alphabet") }

    # Specify the reference genome for the regions
    regions <- GRanges(regions, seqinfo=seqinfo(genome))

    # Get the sequences of the genome regions
    genomic_seqs <- getSeq(genome,regions)

    # Compute the number of matches of each pattern for each region
    nregions <- length(genomic_seqs)
    count_matrix <- vapply(patterns, vcountPattern,
                            c(rep(0,nregions)),
                            subject=genomic_seqs)
    if (nregions>1) { rownames(count_matrix) <- names(regions) }

    # Remove region with 0 matches if specified
    if (remove.0.matches) {
        count_matrix <- count_matrix[rowSums(count_matrix)>0,] }

    # Sum all the matches for each pattern if specified
    if (overall.count) { count_matrix <- colSums(count_matrix) }

    return(count_matrix)
}

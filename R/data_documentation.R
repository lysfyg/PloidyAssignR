# data_K562_ROC

#' Example Data: Regions of Confidence for K562
#'
#' A dataset containing the start and end point of possible regions of confidence in
#' several chromosomes as determined by the authors of this package.
#' @format A data frame with 6 rows and 3 variables
#' \itemize{
#'   \item {chrom} {Chromosome (chr1...chrX)}
#'   \item {start} {Start position of the ROC}
#'   \item {end} {End position of the ROC}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name data_K562_ROC
#' @usage data(data_K562_ROC)
#'
NULL

# data_K562_ploidy

#' Example Data: Results of ploidy analysis for K562 using PloidyAssignR
#'
#' A dataset containing the complete output after ploidy analysis using PloidyAssignR.
#' @format A data frame with 473863 rows and 11 variables
#' \itemize{
#'   \item {cell} {Cell}
#'   \item {chrom} {Chromosome (chr1...chrX)}
#'   \item {start} {Start position of the sliding window}
#'   \item {end} {End position of the sliding window}
#'   \item {sample} {Sample name: K562}
#'   \item {total_count} {Total count number within the window}
#'   \item {fraction_w} {Proportion of W-oriented reads within the window}
#'   \item {sil_k} {optimized number of clusters k by maximizing a silhouette score}
#'   \item {cons_ploidy} {Ploidy state as determined from counting clusters n = k-1}
#'   \item {baseline_coverage} {Mean count number within the ROC divided by the specific consensus.}
#'   \item {norm_count} {Normalized count number for the window: total_count divided by the baseline_coverage }
#' }
#'
#' @docType data
#' @keywords datasets
#' @name data_K562_ploidy
#' @usage data(data_K562_ploidy)
#'
NULL

# data_K562_strand_seq_count

#' Example Data: Strand-seq count data created by MosaiCatcher pipeline.
#'
#' A dataset containing the binned Strand-seq count data.
#' @format A data frame with 2502414 rows and 8 variables
#' \itemize{

#'   \item {chrom} {Chromosome (chr1...chrX)}
#'   \item {start} {Start position of the bin}
#'   \item {end} {End position of the bin}
#'   \item {sample} {Sample name: K562}
#'   \item {cell} {Cell}
#'   \item {c} {number of Crick-oriented reads in this bin}
#'   \item {w} {number of Watson-oriented reads in this bin}
#'   \item {class} {Class attribute as determined by MosaiCatcher pipeline}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name data_K562_strand_seq_count
#' @usage data(data_K562_strand_seq_count)
#'
NULL

# template_input_ploidy_assignment

#' Example Structure of Input Data.
#'
#'
#' @docType data
#' @keywords datasets
#' @name template_input_ploidy_assignment
#'
NULL

# test_data_ploidy_consensus
#' Dataset for testing
#'
#'
#' @docType data
#' @keywords datasets
#' @name test_data_ploidy_consensus
#'
NULL

# test_data_ROC
#' Dataset for testing
#'
#'
#' @docType data
#' @keywords datasets
#' @name test_data_ROC
#'
NULL

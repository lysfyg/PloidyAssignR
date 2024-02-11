# data_K562_ROC

#' Example Data: Regions of Confidence for K562
#'
#' A data set containing the start and end point of possible regions of confidence in
#' several chromosomes as determined by the authors of this package.
#' @format A data frame with 6 rows and 3 variables
#' \itemize{
#'   \item {chrom} {Chromosome (chr1...chrX)}
#'   \item {start} {Start position of the ROC}
#'   \item {end} {End position of the ROC}
#' }
#'
#'
#'
#' @docType data
#' @keywords datasets
#' @name data_K562_ROC
#' @usage data(data_K562_ROC)
#'
NULL

# data_K562_ploidy

#' Example Data: Results of ploidy analysis for K562 using PloidyAssignR.
#'
#' Used for demonstrating PloidyAssignR's capabilities in analyzing and visualizing ploidy in single cells.
#' Serves as a benchmark or reference data set for comparative studies and method validation. The cell line K562 was extensively characterized by Zhou et al. (2019).
#'
#'
#' A data set containing the complete output after ploidy analysis using PloidyAssignR.
#' @format A data frame with 473863 rows and 11 variables
#' \itemize{
#'  \item {cell} {Identifier for each cell analyzed.}
#'  \item {chrom} {Chromosome specification: chr1...chrX.}
#'  \item {start} {Start position of the genomic segment, e.g. the sliding window.}
#'  \item {end} {End position of the genomic segment, e.g. the sliding window.}
#'  \item {sample} {Sample identifier, here specifically for K562 cells.}
#'  \item {total_count} {Total count of reads in the segment.}
#'  \item {fraction_w} {Relative W-strand state frequency - proportion of W-oriented reads in the segment.}
#'  \item {sil_k} { Optimal number of clusters determined by a silhouette score.}
#'  \item {cons_ploidy} {Consensus ploidy state of the genomic segment, determined from counting clusters n = k-1}
#'  \item {baseline_coverage} {Baseline coverage for each cell: Mean count number within the ROC divided by the specific consensus ploidy.}
#'  \item {norm_count} {Normalized count representing single-cell copy number of the genomic segment: total_count divided by the baseline_coverage.}
#' }
#' @examples
#' # Visualize the consensus ploidy states using the karyogram style plot
#' karyogram_plot <- fct_plot_karyogram(data_K562_ploidy)
#' # Visualize the distribution patterns of fraction_w
#' watson_plot <- fct_plot_distribution_patterns(data_K562_ploidy, input_chrom = "chr20",
#' input_cell = "01|370", cell_color = "magenta", cell_size = 1)
#' # Visualize the single cell copy number states using the heatmap plotting function
#' heatmap_chr20 <-fct_plot_sc_heatmap(data_ploidy = data_K562_ploidy, input_chrom = "chr20")
#' @docType data
#' @keywords datasets
#' @name data_K562_ploidy
#' @usage data(data_K562_ploidy)
#'
NULL

# data_K562_strand_seq_count

#' Example Data: Strand-seq count data created by MosaiCatcher pipeline.
#'
#' A data set containing the binned Strand-seq count data of the cell line K562.
#'
#' @format A data frame with 2502414 rows and 8 variables
#' \itemize{
#'  \item {cell} {Identifier for each cell analyzed.}
#'  \item {chrom} {Chromosome specification: chr1...chrX.}
#'  \item {start} {Start position of the genomic segment, e.g. the sliding window.}
#'  \item {end} {End position of the genomic segment, e.g. the sliding window.}
#'  \item {sample} {Sample identifier, here specifically for K562 cells.}
#'   \item {c} {Number of Crick-oriented reads in this bin.}
#'   \item {w} {Number of Watson-oriented reads in this bin.}
#'   \item {class} {Class attribute as determined by MosaiCatcher pipeline.}
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
#' Datas et for testing
#'
#'
#' @docType data
#' @keywords datasets
#' @name test_data_ploidy_consensus
#'
NULL

# test_data_ROC
#' Data set for testing
#'
#'
#' @docType data
#' @keywords datasets
#' @name test_data_ROC
#'
NULL

#' Single Cell Coverage Normalization
#'
#' 'scCoverageNorm' normalizes the coverage of single-cell sequencing data.
#' The function uses regions of confidence (ROC) to establish a baseline coverage for each cell library.
#' This baseline is then utilized to determine the copy number of each genomic segment within the cells.
#' By leveraging user-defined regions of confidence, the function ensures that variations in library size
#' and sequencing depth across different cells are accounted for, enabling more reliable copy number estimations at a single-cell level.
#'
#' @import data.table
#'
#' @param dt_input_fractions A data.table containing the relative Watson-strand frequencies fraction_w and the respective consensus ploidy states.
#' @param dt_input_ROC A data.table of regions of confidence as specified by the user. This table should have columns chrom, start, and end to define genomic regions used for normalization.
#'
#' @return This function adds the normalized read count that represents the single cell copy number as a column to the input data.table.
#' @examples
#' # Normalizing coverage for single-cell data
#' analysis_results <- scCoverageNorm(data_K562_ploidy[,1:9], data_K562_ROC)
#'
#' # Visualizing the normalized data
#' fct_plot_sc_heatmap(analysis_results, input_chrom = "chr1")
#' @export
#'

scCoverageNorm <- function(dt_input_fractions,
                           dt_input_ROC) {
    # local binding of variable to function
    i.start <- i.end <- total_count <- cons_ploidy <- baseline_coverage_seg <- norm_count <- baseline_coverage <- NULL


    assertthat::assert_that("chrom" %in% colnames(dt_input_fractions),
        "start" %in% colnames(dt_input_fractions) && is.numeric(dt_input_fractions$start),
        "end" %in% colnames(dt_input_fractions) && is.numeric(dt_input_fractions$end),
        "sample" %in% colnames(dt_input_fractions),
        "cell" %in% colnames(dt_input_fractions),
        "cons_ploidy" %in% colnames(dt_input_fractions) && is.numeric(dt_input_fractions$fraction_w),
        "total_count" %in% colnames(dt_input_fractions) && is.numeric(dt_input_fractions$total_count),
        msg = "One or more required columns in your input file are missing for analysis.
        Please make sure to use the correct formatting to ensure correct ploidy detection."
    )

    assertthat::assert_that("chrom" %in% colnames(dt_input_ROC),
        "start" %in% colnames(dt_input_ROC) && is.numeric(dt_input_ROC$start),
        "end" %in% colnames(dt_input_ROC) && is.numeric(dt_input_ROC$end),
        msg = "One or more required columns in your ROC file are missing for analysis.
        Please make sure to use the correct formatting to ensure correct ploidy detection."
    )


    # input_chrom <- extract_chromosomes(dt_input_fractions)
    message("You have supplied ", nrow(dt_input_ROC), " regions of confidence. Starting normalization process...")

    # Filter Data by ROC
    data.table::setkey(dt_input_ROC)
    data_ROC <- data.table::foverlaps(dt_input_fractions, dt_input_ROC, type = "within",
                                      mult = "all", nomatch = 0L, by.x = c("chrom", "start", "end"))
    # Remove unnecessary columns
    data_ROC[, `:=`(start = i.start, end = i.end)][, `:=`(i.start = NULL, i.end = NULL)]
    # calculate basline coverage: first mean of each chrom ROC segment by each cell, then mean of all segments by cell
    baseline_cov_data <- data_ROC[, list(baseline_coverage_seg = mean(total_count / cons_ploidy)),
                                  by = c("cell", "chrom")][, list(baseline_coverage = mean(baseline_coverage_seg)),
                                                           by = "cell"]

    # add norm_count
    dt_sc_norm <- merge(dt_input_fractions, baseline_cov_data, by = c("cell"))


    dt_sc_norm[, norm_count := total_count / baseline_coverage]


    return(dt_sc_norm)
}

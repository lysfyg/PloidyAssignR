#' Assign Consensus Ploidy
#'
#' assignConsensusPloidy is a critical function in the PloidyAssignR algorithm, responsible for determining cell ploidy using k-means clustering.
#' It operates on Watson-reads fraction distributions from calWatsonFractions().
#' The function's key feature is the use of the silhouette score to determine the optimal number of clusters (k).
#' By determining the number of clusters, the tool detects the underlying consensus ploidy state.
#'
#' @import data.table
#' @import assertthat
#'
#' @param dt_input_fractions Data.table from calWatsonFractions(), containing relative Watson.strand state frequencies fraction_w.
#' @param input_chrom (Optional) Vector that specifies chromosomes for analysis ("chr1, chr2..."). Defaults to all chromosomes if not set.
#' @param max_ploidy_force (Optional) Sets the maximum ploidy level, determined automatically from the number of cells if not specified.
#'
#' @return Returns the original data.able with added columns: "sil_k" is the optimal k determined by maximizing a silhouette score and "cons_ploidy" is the assigned consensus ploidy value for each window.
#' @examples
#' # Ploidy assignment for chromosome 1 in K562 cells
#' K562_chr1_ploidy <- assignConsensusPloidy(data_K562_ploidy[,1:7], input_chrom = "chr1")
#'
#' # Karyogram visualization of the results
#' fct_plot_karyogram(K562_chr1_ploidy)
#' @export
#'

assignConsensusPloidy <- function(
    dt_input_fractions,
    input_chrom = NULL,
    max_ploidy_force = NULL) {
    # to address "no visible binding for global variable"
    cell <- sil_k <- fraction_w <- cons_ploidy <- NULL
    # v dt_input_fractions is a data.table
    assertthat::assert_that(data.table::is.data.table(dt_input_fractions),
                            msg = "assignConsensusPloidy requires a data.table as input.")
    # v dt_input_fractions has correct format
    assertthat::assert_that("chrom" %in% colnames(dt_input_fractions),
        "start" %in% colnames(dt_input_fractions) && is.numeric(dt_input_fractions$start),
        "end" %in% colnames(dt_input_fractions) && is.numeric(dt_input_fractions$end),
        "sample" %in% colnames(dt_input_fractions),
        "cell" %in% colnames(dt_input_fractions),
        "fraction_w" %in% colnames(dt_input_fractions) && is.numeric(dt_input_fractions$fraction_w),
        "total_count" %in% colnames(dt_input_fractions) && is.numeric(dt_input_fractions$total_count),
        msg = "One or more required columns in your input file are missing for analysis. "
    )





    # v max_ploidy_force = Null, then extract max_ploidy automatically from data set
    if (is.null(max_ploidy_force)) {
        # min required cells for analysis for max_ploidy is 2^ploidy -> therefore using log2 to calculate max_ploidy
        max_ploidy <- trunc(log(length(dt_input_fractions[, unique(cell)]), base = 2))

        message("Your data set contains ", length(dt_input_fractions[, unique(cell)]),
                " cells. Consensus ploidy analysis is performed with max_ploidy = ", max_ploidy, ".\n")
    } else {
        max_ploidy <- max_ploidy_force
        message("You have forced max_ploidy to ", max_ploidy, ". Your data set contains ",
                length(dt_input_fractions[, unique(cell)]), " cells and therefore allows max_ploidy = ",
                trunc(log(length(dt_input_fractions[, unique(cell)]), base = 2)), ".\n")
    }
    # v If no input_chrom is given extract from input data set
    if (is.null(input_chrom)) {
        input_chrom <- extract_chromosomes(dt_input_fractions) # sort chromosomes
    } else {
        input_chrom <- extract_chromosomes(input_chrom) # sort chromosomes
    }
    message("Running analysis on ", paste(input_chrom, collapse = " "), ".\n")
    # initialize tmp data.table because otherwise changes would be made outside of function to original data.table
    dt_ploidy <- dt_input_fractions[input_chrom, on = "chrom"]
    # Silhouette Score analysis
    dt_ploidy[, sil_k := find_optimal_k(fraction_w, max_ploidy = max_ploidy), by = c("chrom", "start")]
    dt_ploidy[, cons_ploidy := (sil_k - 1)]

    return(dt_ploidy)
}

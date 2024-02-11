#' Calculate Watson Fractions using a Sliding Window
#'
#' calWatsonFractions is a core function in PloidyAssignR that calculates the relative Watson-strand state frequency "fraction_w" of single-cell Strand-seq count data using a sliding window approach. This calculation is essential for the assessment of strand-specific chromatid segregation patterns, a fundamental step in the systematic inference of ploidy states.
#'
#' @import data.table
#' @import assertthat
#'
#' @param dt_input_count The strand-seq count data prepared by input_prep(). This data.table should contain strand-specific read counts for each cell and chromosome.
#' @param input_chrom (Default: NULL) Specifies the chromosomes to analyze. If not provided, all chromosomes in the data set will be analyzed.
#' @param input_window (Default: 10000000) The size of the sliding window used for calculating the relative Watson-strand state frequency.
#' @param input_step (Default: 5000000) The step size for the sliding window.
#' @param na_rm (Default: TRUE) Determines whether NA values should be removed from the data set. NA values may cause errors in calculations.
#'
#' @return Returns a data.table with the columns "chrom", "start", "end", "sample", "cell", "total_count", "fraction_w". This table provides detailed information on the Watson strand fraction for each window, essential for downstream analyses like chromatid segregation pattern recognition.
#' @examples
#' # Calculate relative W-strand state frequencies for chromosome 1 in K562 cells
#' K562_chr1_windows <- calWatsonFractions(data_K562_strand_seq_count, input_chrom = "chr1")
#'
#' # Visualize distribution patterns using internal plotting function
#' fct_plot_distribution_patterns(data_K562_ploidy, input_chrom = "chr1")
#'
#' # Continue with ploidy assignment using assignConsensusPloidy()
#' assignConsensusPloidy(K562_chr1_windows)
#' @export
#'

#'
#'
calWatsonFractions <- function(dt_input_count,
                               input_chrom = NULL,
                               input_window = 10000000,
                               input_step = 5000000,
                               na_rm = TRUE) {
    # to address "no visible binding for global variable"
    chrom <- w <- cell <- NULL
    ### IO dt_input_count, input_chrom, input_window, input_step
    # v Check that dt_input_count is a data.table.
    assertthat::assert_that(data.table::is.data.table(dt_input_count),
                            msg = c("calWatsonFraction requires a data.table as input. You have provided class(): ",
                                    class(dt_input_count), "."))
    # v Check that dt_input has correct format
    assertthat::assert_that("chrom" %in% colnames(dt_input_count),
        "start" %in% colnames(dt_input_count) && is.numeric(dt_input_count$start),
        "end" %in% colnames(dt_input_count) && is.numeric(dt_input_count$end),
        "sample" %in% colnames(dt_input_count),
        "cell" %in% colnames(dt_input_count),
        "w" %in% colnames(dt_input_count) && is.numeric(dt_input_count$w),
        "c" %in% colnames(dt_input_count) && is.numeric(dt_input_count$c),
        msg = "One or more required columns in your input file are missing for analysis.
        Please make sure to use the correct formatting to ensure correct ploidy detection."
    )
    # v If it is a path pass to input_prep to create the data.table

    # v If no input_chrom is given extract from input data set
    if (is.null(input_chrom)) {
        input_chrom <- extract_chromosomes(dt_input_count) # sort chromosomes
    } else {
        input_chrom <- extract_chromosomes(input_chrom) # sort chromosomes
    }
    # v Check input_window/step have correct format and size.
    if (!is.numeric(input_window) | !is.numeric(input_step) |
        dt_input_count[, round(mean(end - start))] > input_step | input_step > input_window) {
        warning("Please check the sliding window parameters. Window should be larger than step.
                These parameters should also be larger than the data bin-size. Your data has the mean bin-size: ",
                dt_input_count[, round(mean(end - start))], ". Continuing analysis with default window size 10000000
                and step size 5000000.\n")
        input_window <- 10000000
        input_step <- 5000000
    }


    ### Calculations: create a sliding window with the input parameters and calculate watson fraction for each window.
    # initalize data.tables required for analysis
    dt_tmp <- dt_input_count
    chr_bin <- data.table::data.table(
        chrom = character(),
        start = numeric(),
        end = numeric(),
        # bin = numeric(),
        sample = character(),
        cell = character(),
        fraction_w = numeric(),
        total_count = numeric()
    )

    sliding_window_all <- data.table::data.table(
        chrom = character(),
        start = numeric(),
        end = numeric()
    )

    # for loop : each chrom
    message("Creating sliding windows (", input_window, "|", input_step, ") and calculating Watson fractions.")
    for (cur_chrom in input_chrom) {
        # subset dt_tmp by cur_chrom
        dt_subset_chrom <- dt_tmp[chrom == cur_chrom]
        # sliding window start positions
        start_positions <- seq(from = dt_subset_chrom[, min(start)], to = dt_subset_chrom[, max(end)], by = input_step)
        start_positions <- start_positions[-length(start_positions)] # removes last element
        # data.table with window start positions and end positions = start + input_step
        dt_sliding_window <- data.table::data.table(
            start = start_positions,
            end = start_positions + input_window,
            chrom = cur_chrom
        )
        dt_sliding_window[.N, end := dt_subset_chrom[, max(end)]] # replace last row step_end by max_chrom_end

        message(cur_chrom, " has ", nrow(dt_sliding_window), " windows.\n")

        # add dt_sliding_window to master window data.table
        sliding_window_all <- data.table::rbindlist(list(sliding_window_all, dt_sliding_window),
                                                    use.names = TRUE, fill = TRUE) # introduces NA


        # calculate fraction_w and total_count in input data.table using the sliding window coordinates
        for (window in seq_len(nrow(dt_sliding_window))) { # extracts coordinates from dt_sliding_window
            dt_tmp[
                chrom == cur_chrom &
                    start >= dt_sliding_window[window, start] # start of window
                & end <= dt_sliding_window[window, end], # end of window
                # calc total_count and fraction_w
                `:=`(fraction_w = sum(w) / (sum(c) + sum(w)), total_count = (sum(c) + sum(w))),
                by = cell
            ] # for each cell
        }
    }
    message("Writing Watson Fractions table!")
    dt_watson_fractions <- merge(sliding_window_all, dt_tmp, by = c("start", "chrom"),
                                 suffixes = c("_SW", "_old_bins"), all.x = TRUE)


    colnames(dt_watson_fractions)[colnames(dt_watson_fractions) == "end_SW"] <- "end"
    dt_watson_fractions <- dt_watson_fractions[, c("chrom", "start", "end", "sample", "cell", "total_count", "fraction_w")]
    data.table::setkey(dt_watson_fractions)
    if (na_rm) {
        dt_watson_fractions <- na.omit(dt_watson_fractions)
    }
    dt_tmp <- NULL
    return(dt_watson_fractions)
}

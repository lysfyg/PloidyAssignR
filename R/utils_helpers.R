#' Chromosomes
#'
#' Returns a vector of ordered chromosomes.
#'
#' @import data.table
#' @import assertthat
#' @import attempt
#'
#' @param input_chrom a vector of chromosomes or a data.table that contains a column "chrom" with chromosomes.
#'
#' @return Vector of chromosomes ordered by ascending number 1 to X/Y
#' @export
#'
#' @examples
#' extract_chromosomes(c("chr1", "chr2"))
#' extract_chromosomes(data_K562_strand_seq_count[c("chr1", "chr2"), on = "chrom"])
#'
extract_chromosomes <- function(input_chrom) {
    # to address "no visible binding for global variable"
    chrom <- NULL

    chrom_order <- c(
        "chr1", "chr2", "chr3", "chr4", "chr5",
        "chr6", "chr7", "chr8", "chr9", "chr10",
        "chr11", "chr12", "chr13", "chr14", "chr15",
        "chr16", "chr17", "chr18", "chr19", "chr20",
        "chr21", "chr22", "chrX", "chrY"
    )
    # v check if data.table
    if (data.table::is.data.table(input_chrom)) {
        assertthat::assert_that("chrom" %in% colnames(input_chrom),
                                msg = "The data.table you have supplied does not contain a column specified as >>chrom<<.\n")
        input_chrom <- input_chrom[, unique(chrom)]
    }

    # v report any elements that were not in interal chrom_order and report in warning. Continue
    attempt::message_if(length(dplyr::setdiff(input_chrom, chrom_order)) != 0,
        msg = "The input dataset contains elements that are not considered chromosomes. Removing these elements and continuing analysis."
    )
    # a find unique chromosome elements that are in internal chrom_order
    chrom_unique <- dplyr::intersect(input_chrom, chrom_order)


    # a sort input by internal chrom_order
    chrom_sorted <- chrom_unique[order(match(chrom_unique, chrom_order))]

    # v if no chromosomes can be extracted throw error
    assertthat::assert_that(length(chrom_unique) != 0,
                            msg = "Input data format not correct. Should be a data.table or vector and contain only strings such as chr1.\n")


    # r returns a vector of sorted chrom
    return(chrom_sorted)
}


#' Find optimal K
#'
#' Uses a silhouette score to determine optimal k in binomially distributed Watson-reads fraction data.
#'
#' @import data.table
#' @import stats
#'
#' @param fraction_w_vector Vector of Watson-reads fractions from one segments from all cells in sample set.
#' @param max_ploidy supplied by user and defaults to tetraploid.
#'
#' @return optimal k
#' @export
#' @examples
#' find_optimal_k(c(0,0.01,0.001,0.334,0.32,0.33,0.65,0.67,0.66,1.0,0.99,0.998), max_ploidy=5)
#'

find_optimal_k <- function(fraction_w_vector, max_ploidy = 4) {
    if (any(is.na(fraction_w_vector))) {
        return(NA)
    } else {
        k_val <- 2:max_ploidy

        avg_sil <- sapply(k_val, cal_silhouette_score, fraction_w_vector)
        if (any(is.na(avg_sil))) {
            return(NA)
        } else {
            optimal_k <- which.max(avg_sil) + 1


            ploidy_kmeans <- stats::kmeans(fraction_w_vector, centers = optimal_k, nstart = 25)
            # make sure that frac=0 or frac=1 are present. If not add the missing clusters to k

            check_one <- stats::dist(c(1, max(ploidy_kmeans$centers[, 1])))
            check_zero <- stats::dist(c(0, min(ploidy_kmeans$centers[, 1])))

            if (optimal_k < 10) {
                k_cutoff <- 1 / 10
            } else if (optimal_k >= 10) {
                message("more than 10 clusters found. optimal_k =  ", optimal_k)
            }
            if (check_zero >= k_cutoff) {
                optimal_k <- optimal_k + 1
            }

            if (check_one >= k_cutoff) {
                optimal_k <- optimal_k + 1
            }

            return(optimal_k)
        }
    }
}

#' Calculate Silhouette Scores
#'
#' The silhouette score for a given k of kmeans is calculated and returned.
#'
#' @import data.table
#' @import stats
#' @import cluster
#'
#' @param cur_k Integer of k to be scored.
#' @param fraction_w_vector Vector of Watson-read fractions of a given segment to be clustered using kmeans.
#'
#' @return Silhouette score for the kmeans of a fraction_w vector using cur_k. If any NA is present in the input vector function returns NA.
#' @export
#'
#' @examples
#' cal_silhouette_score(cur_k = 3, c(0,0.01,0.001,0.334,0.32,0.33,0.65,0.67,0.66,1.0,0.99,0.998))
#' cal_silhouette_score(cur_k = 4, c(0,0.01,0.001,0.334,0.32,0.33,0.65,0.67,0.66,1.0,0.99,0.998))

cal_silhouette_score <- function(cur_k,
                                 fraction_w_vector) {
    if (any(is.na(fraction_w_vector))) {
        return(NA)
    } else {
        tryCatch(
            {
                km <- stats::kmeans(fraction_w_vector, centers = cur_k, nstart = 25)
                ss <- cluster::silhouette(km$cluster, dist(fraction_w_vector))
                return(mean(ss[, 3]))
            },
            error = function(err) {
                message(">>", conditionMessage(err),
                        "<< Could not determine silhouette score. Entering NA and skipping to next loop.")
                NA
            }
        )
    }
}

#' Input Preparation
#' Reads in Strand-seq library count file, checks formatting and prepares for analysis.
#'
#' @import data.table
#' @import assertthat
#'
#' @param input_data Path to or data.table of the Strand-seq counts file of one sample

#'
#' @return Data.table with necessary columns and sorted by setkey() to be more memory efficient.
#' @export
#'
#' @examples input_prep(data_K562_strand_seq_count)
input_prep <- function(input_data) {
    if (data.table::is.data.table(input_data)) {
        dt_input_data <- input_data
    }
    if (assertthat::is.string(input_data)) {
        # v Make sure file extension is correct
        assertthat::assert_that(tools::file_ext(input_data) %in% c("csv", "txt", "gz"),
                                msg = "Wrong file extension. Please make sure you have supplied the correct file.")
        # IO Load dataset
        dt_input_data <- data.table::fread(input_data)
    }
    # v Make sure dataset has most basic correct format
    assertthat::assert_that("chrom" %in% colnames(dt_input_data),
        "start" %in% colnames(dt_input_data) && is.numeric(dt_input_data$start),
        "end" %in% colnames(dt_input_data) && is.numeric(dt_input_data$end),
        msg = "One or more required columns in your input file are missing for analysis.
        Please make sure to use the correct formatting to ensure correct ploidy detection."
    )
    # v check whether parameter files (exclusion or segmentation) are given
    # a remove any data not necessary for analysis and filter by parameter files

    # IO return(data)
    data.table::setkey(dt_input_data)
    return(dt_input_data)
}

#' Ploidy Summary
#'
#' This function returns a data.table with the proportion of ploidy states.
#'
#' @param data_ploidy PloidyAssignR output containing consensus ploidy states.
#'
#' @return the mean ploidy state and the individual proportion of each ploidy state.
#' @export
#'
#' @examples
#' data_summary <- ploidy_summary(data_K562_ploidy)
#'
#'

ploidy_summary <- function(data_ploidy){
    cell <- cons_ploidy <- NULL
    # v input orrect format
    assertthat::assert_that(data.table::is.data.table(data_ploidy),
                            msg = "ploidy_summary requires a data.table as input.")

    assertthat::assert_that("sample" %in% colnames(data_ploidy),
                            "cell" %in% colnames(data_ploidy),
                            "cons_ploidy" %in% colnames(data_ploidy) && is.numeric(data_ploidy$cons_ploidy),
                            msg = "One or more required columns in your input file are missing for analysis."
    )

    # select a random cell
    random_cell <- data_ploidy[,unique(cell)][1]
    # subset data set
    data_ploidy <- na.omit(data_ploidy[cell == random_cell])
    # calculate number of window
    window_count <- nrow(na.omit(data_ploidy))
    # mean ploidy
    mean_ploidy <- round(data_ploidy[,mean(cons_ploidy)],2)
    # sample name
    sample_name <- data_ploidy[,unique(sample)]

    # calculate percentage
    data_summary <- data_ploidy[, list(window = .N), by = cons_ploidy]
    data_summary[,`:=`(percent = round(window/window_count*100,1), mean_ploidy = mean_ploidy, sample = sample_name)]
    setcolorder(data_summary, c("sample","mean_ploidy", "cons_ploidy","window","percent"))
    setorder(data_summary, cons_ploidy)
    data.table::setkey(data_summary)
    return(data_summary)

}

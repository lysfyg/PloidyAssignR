testthat::test_that("calWatsonFractions only takes data.table in correct format as input", {
    testthat::expect_error(calWatsonFractions("fufu"))
    testthat::expect_error(calWatsonFractions(data.table::data.table(start = c(0, 1, 2, 3))))
})

testthat::test_that("calWatsonFraction returns a data.table for the chromosomes specified", {
    suppressMessages(res_allchrom <- calWatsonFractions(data_K562_strand_seq_count[c("chr1", "chr2"), on = "chrom"]))
    testthat::expect_equal(res_allchrom[, unique(chrom)], c("chr1", "chr2")) # did not give input chrom, therefore resulting table contains the same chrom as in original data set
    suppressMessages(res_selchrom <- calWatsonFractions(data_K562_strand_seq_count[c("chr1", "chr2", "chr3"), on = "chrom"], input_chrom = c("chr2", "chr3")))
    testthat::expect_equal(res_selchrom[, unique(chrom)], c("chr2", "chr3")) # user supplied input chrom, therefore resulting table contains the same chrom as input_chrom
    testthat::expect_true(data.table::is.data.table(res_allchrom))
})
testthat::test_that("calWatsonFraction catches wrong Sliding Window input and continues analysis with default setting", {
    testthat::expect_warning(calWatsonFractions(data_K562_strand_seq_count[c("chr1", "chr2"), on = "chrom"], input_step = "fufu", input_window = "fufu")) # step size or window size are not numeric
    testthat::expect_warning(calWatsonFractions(data_K562_strand_seq_count[c("chr1"), on = "chrom"], input_window = 500000, input_step = 1000000)) # wrong step and window size & step smaller than bin
})

testthat::test_that("calWatsonFraction returns a data table that contains the right columns", {
    testthat::expect_equal(colnames(calWatsonFractions(data_K562_strand_seq_count[c("chr1"), on = "chrom"])), c("chrom", "start", "end", "sample", "cell", "total_count", "fraction_w"))
})

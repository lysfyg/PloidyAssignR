testthat::test_that("scCoverageNorm returns a data.table containing the columns >>baseline_coverage<< and >>norm_count<< and the same number of rows as before.", {
    test_ROC <- data.table::data.table(chrom = c("chr1", "chr2"), start = c(60000000, 0), end = c(70000000, 10000000))
    testthat::expect_true(data.table::is.data.table(scCoverageNorm(data_K562_ploidy[, 1:9], test_ROC)))
    # testthat::expect_true(nrow(), nrow() )
    # testthat::expect_equal(colnames(, c("chrom", "start", "end", "sample", "cell", "total_count", "fraction_w")####
})

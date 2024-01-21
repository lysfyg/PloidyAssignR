# Testing
testthat::test_that("assignConsensusPloidy returns a data.table with correct columns", {
    K562_fractions <- data_K562_ploidy[, 1:7]
    res <- assignConsensusPloidy(K562_fractions[c("chr1", "chr2"), on = "chrom"])
    testthat::expect_true(data.table::is.data.table(res))
    testthat::expect_equal(colnames(res), c("cell", "chrom", "start", "end", "sample", "total_count", "fraction_w", "sil_k", "cons_ploidy"))
})
testthat::test_that("assignConsensusPloidy only takes data.table in correct format as input", {
    testthat::expect_error(assignConsensusPloidy("fufu"))
    testthat::expect_error(assignConsensusPloidy(dt_input_fractions = data_K562_strand_seq_count))
})

testthat::test_that("assignConsensusPloidy handles max-ploidy input correctly", {
    testthat::expect_message(assignConsensusPloidy(data_K562_ploidy[chrom == "chr1" & start < 40000000, ]), paste0("Your data set contains ", length(data_K562_ploidy[, unique(cell)]), " cells. Consensus ploidy analysis is performed with max_ploidy = ", trunc(log(length(data_K562_ploidy[, unique(cell)]), base = 2)), ".\n"))
    testthat::expect_message(assignConsensusPloidy(data_K562_ploidy[chrom == "chr1" & start < 40000000, ], max_ploidy_force = 3), paste0("* Your data set contains ", length(data_K562_ploidy[, unique(cell)]), " cells and therefore allows max_ploidy = ", trunc(log(length(data_K562_ploidy[, unique(cell)]), base = 2)), ".\n"))
})
testthat::test_that("assignConsensusPloidy runs analysis only on specified chromosomes", {
    K562_fractions <- data_K562_ploidy[start <= 40000000, 1:7]
    res <- assignConsensusPloidy(K562_fractions, input_chrom = "chr1")
    testthat::expect_true(res[, unique(chrom)] == "chr1")
})

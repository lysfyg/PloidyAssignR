# extract_chromosomes()
testthat::test_that("extract_chromosomes returns a vector containing a string of chromosomes", {
    testthat::expect_setequal(extract_chromosomes(c("chr1", "chr2")), c("chr1", "chr2")) # right order and correct format
    testthat::expect_setequal(extract_chromosomes(c("chr2", "chr1")), c("chr1", "chr2")) # wrong order
    testthat::expect_setequal(extract_chromosomes(c("chr2", "chr1", "chr2", "chr1")), c("chr1", "chr2")) # wrong order and repeated elements
    testthat::expect_setequal(suppressWarnings(extract_chromosomes(c("chr1", "chr2", "fufu"))), c("chr1", "chr2")) # right order and correct format with one wrong element
    testthat::expect_setequal(extract_chromosomes(data_K562_strand_seq_count[c("chr1", "chr2"), on = "chrom"]), c("chr1", "chr2")) # data table as input
})

testthat::test_that("fct_chromsomes catches wrong formatting", {
    testthat::expect_message(extract_chromosomes(c("chr1", "chr2", "fufu")))
    testthat::expect_error(extract_chromosomes(100))
    testthat::expect_error(extract_chromosomes(data.table(fufu = c(0, 0, 0, 1, 1, 2))))
})


# find_optimal_k()
testthat::test_that("find_optimal_k returns a numeric", {
    test_fraction <- data_K562_ploidy[chrom == "chr1" & start == 10000000, fraction_w]
    testthat::expect_true(is.numeric(find_optimal_k(test_fraction)))
    testthat::expect_true(5 == (find_optimal_k(test_fraction)))
})
testthat::test_that("find_optimal_k returns an NA if any NA is present in input vector", {
    testthat::expect_true(is.na(find_optimal_k(NA)))
})

# cal_silhouette_score()
testthat::test_that("cal_silhouette_score returns a silhouette score", {
    testthat::expect_equal(cal_silhouette_score(3, c(1, 1, 1, 1, 4, 4, 4, 4, 4, 9, 9, 9)), 1)
})
testthat::test_that("cal_silhouette_score returns NA if any NA is present in input vector", {
    testthat::expect_true(is.na(cal_silhouette_score(3, fraction_w_vector = c(2, 3, 4, 5, NA, 5))))
})
testthat::test_that("cal_silhouette_score throws a warning if cannot calculate silhouette score and returns NA", {
    testthat::expect_condition(cal_silhouette_score(5, c(1, 1, 1, 1, 4, 4, 4, 4, 4, 9, 9, 9)))
    testthat::expect_true(is.na(cal_silhouette_score(5, c(1, 1, 1, 1, 4, 4, 4, 4, 4, 9, 9, 9))))
})

# Input_prep()

testthat::test_that("input_prep takes data.table and returns a sorted data.table", {
    testthat::expect_true(data.table::is.data.table(input_prep(data_K562_strand_seq_count[c("chr1", "chr2"), on = "chrom"])) &
        "sorted" %in% names(attributes(input_prep(data_K562_strand_seq_count[c("chr1", "chr2"), on = "chrom"]))))
})
testthat::test_that("input_prep throws error if wrong file extension", {
    testthat::expect_error(input_prep("wrong.extension"))
})
testthat::test_that("input_prep stops if necessary columns are missing", {
    testthat::expect_error(input_prep(data_K562_ploidy[, 1:2]))
})


# ploidy_summary()
testthat::test_that("ploidy_summary recognizes wrong input", {
    testthat::expect_true(data.table::is.data.table(input_prep(data_K562_strand_seq_count[c("chr1", "chr2"), on = "chrom"])) &
                              "sorted" %in% names(attributes(input_prep(data_K562_strand_seq_count[c("chr1", "chr2"), on = "chrom"]))))
})


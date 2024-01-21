testthat::test_that("Mb is correctly formatted", {
    testthat::expect_equal(format_Mb(1000000), 1)
})

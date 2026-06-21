context("Server requests")

test_that("request for variables in area(s) works as expected", {
    skip_on_cran()
    skip_if_not(identical(Sys.getenv("NOT_CRAN"), ""))
    df <- get_variables_areas("FR")
    expect_gt(nrow(df), 0)
    expect_equal(ncol(df), 5)
    expect_named(df, c("percentile", "age", "pop", "variable", "country"))
    expect_true(all(df$country == "FR"))

    df <- get_variables_areas("XX")
    expect_equal(nrow(df), 0)
})

test_that("request for data works as expected", {
    skip_on_cran()
    skip_if_not(identical(Sys.getenv("NOT_CRAN"), "true"))
    df <- get_data_variables("FR", "sptinc_p99p100_992_t")
    expect_gt(nrow(df), 0)
    expect_equal(ncol(df), 4)
    expect_named(df, c("indicator", "country", "year", "value"))
    expect_true(all(df$country == "FR"))
    expect_true(all(df$indicator == "sptinc_p99p100_992_t"))

    df <- get_data_variables("FR", "xxxxxx_p99p100_992_t")
    expect_equal(nrow(df), 0)
})

test_that("request for metadata works as expected", {
    skip_on_cran()
    skip_if_not(identical(Sys.getenv("NOT_CRAN"), "true"))
    res <- get_metadata_variables("FR", "sptinc_p99p100_992_t")
    df <- res$response_table
    expect_gt(nrow(df), 0)
    expect_gte(ncol(df), 15)
    expect_true(all(c("variable", "country", "countryname", "quality", "imputation") %in% names(df)))
    expect_true(all(df$country == "FR"))

    df <- get_metadata_variables("FR", "xxxxxx_p99p100_992_t")$response_table
    expect_equal(nrow(df), 0)
})

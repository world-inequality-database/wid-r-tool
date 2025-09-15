context("Server requests")

as_df <- function(x) if (is.null(x) || !is.data.frame(x)) data.frame() else x

test_that("request for variables in area(s) works as expected", {
    skip_on_cran()

    df <- as_df(get_variables_areas("FR"))
    expect_true(nrow(df) > 0)
    expect_equal(ncol(df), 5)

    df <- as_df(get_variables_areas("XX"))
    expect_equal(nrow(df), 0)
})

test_that("request for data works as expected", {
    skip_on_cran()

    df <- as_df(get_data_variables("FR", "sptinc_p99p100_992_t"))
    expect_true(nrow(df) > 0)
    expect_equal(ncol(df), 4)

    df <- as_df(get_data_variables("FR", "xxxxxx_p99p100_992_t"))
    expect_equal(nrow(df), 0)
})

test_that("request for metadata works as expected", {
    skip_on_cran()

    res <- get_metadata_variables("FR", "sptinc_p99p100_992_t")
    df <- as_df(res$response_table)
    if (nrow(df) == 0) skip("No metadata returned from API for given areas/variables")
    df <- df[, intersect(names(df), c(
        "variable","unit","unitname","shortname","shortdes","technicaldes",
        "shorttype","longtype","shortpop","pop","shortage","age",
        "country","countryname","method","source","quality","imputation"
    ))]
    df <- df[, 1:15]
    expect_true(nrow(df) > 0)
    expect_equal(ncol(df), 15)

    res <- get_metadata_variables("FR", "xxxxxx_p99p100_992_t")
    df <- as_df(res$response_table)
    expect_equal(nrow(df), 0)
})

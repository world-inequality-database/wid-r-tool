context("Test overall downloads")

test_that("we can download multiple indicators for a single country", {
    skip_on_cran()

    data <- download_wid(
        areas = "FR",
        indicators = c("sfiinc", "aptinc"),
        perc = c("p90p100", "p20p30"),
        years = 1990:2000
    )

    expect_true(all(data$year >= 1990 & data$year <= 2000))
    expect_true(all(data$country == "FR"))
    expect_true(all(c("p20p30", "p90p100") %in% unique(data$percentile)))
    expect_true(all(c("aptinc", "sfiinc") %in% unique(substr(data$variable, 1, 6))))
})

test_that("we can download a single indicator for multiple countries", {
    skip_on_cran()

    data <- download_wid(
        areas = c("FR", "US"),
        indicators = "sptinc",
        perc = "p90p100",
        ages = "992",
        pop = "j",
        years = 1990:2000
    )

    expect_true(all(data$percentile == "p90p100"))
    expect_true(all(c("FR", "US") %in% unique(data$country)))
    expect_true(all(data$year >= 1990 & data$year <= 2000))
    expect_true(all(substr(data$variable, 10, 11) == "j"))
    expect_true(all(substr(data$variable, 7, 9) == "992"))
    expect_true(all(substr(data$variable, 1, 6) == "sptinc"))
})

test_that("we can download population data", {
    skip_on_cran()

    data <- download_wid(
        areas = "DE",
        indicators = "npopul"
    )

    expect_true(all(data$country == "DE"))
    expect_true(all(data$percentile == "p0p100"))
    expect_true(all(substr(data$variable, 1, 6) == "npopul"))
    expect_true(all(c("i", "f", "m") %in% unique(substr(data$variable, 10, 10))))
})

test_that("we can download metadata", {
    skip_on_cran()

    data <- download_wid(
        areas = "FR",
        indicators = "sptinc",
        perc = "p99p100",
        ages = "992",
        pop = "j",
        metadata = TRUE
    )

    expect_true(all(data$country == "FR"))
    expect_true(all(data$countryname == "France"))
    expect_true(all(data$variable == "sptinc992j"))
    expect_true(all(data$percentile == "p99p100"))
})

test_that("we can exclude extrapolations/interpolations", {
    skip_on_cran()

    data <- download_wid(
        areas = "MZ",
        indicators = "sptinc",
        perc = "p99p100",
        ages = "992",
        pop = "j",
        include_extrapolations = TRUE
    )

    data_noextra <- download_wid(
        areas = "MZ",
        indicators = "sptinc",
        perc = "p99p100",
        ages = "992",
        pop = "j",
        include_extrapolations = FALSE
    )

    expect_true(all(data$country == "MZ"))
    expect_true(all(data$variable == "sptinc992j"))
    expect_true(all(data$percentile == "p99p100"))

    expect_true(all(data_noextra$variable == "sptinc992j"))
    expect_true(all(data_noextra$percentile == "p99p100"))

    merged <- merge(data, data_noextra,
                    by = c("country", "variable", "percentile", "year"), all = TRUE
    )

    expect_true(all(is.na(merged$value.y) | merged$value.x == merged$value.y))
})

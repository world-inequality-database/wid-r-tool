library(testthat)

context("Comprehensive age validation tests")

# --- Single valid 3-digit strings
test_that("validate_single_age handles 3-digit strings", {
    expect_equal(validate_single_age("014"), "014")
    expect_equal(validate_single_age("992"), "992")
    expect_equal(validate_single_age("999"), "999")
})

# --- Numeric input conversion
test_that("validate_single_age converts numeric input", {
    expect_equal(validate_single_age(14), "014")
    expect_equal(validate_single_age(5), "005")
    expect_equal(validate_single_age(999), "999")
})

# --- Vectorized validation
test_that("validate_age_codes handles vectors", {
    expect_equal(validate_age_codes(c("014", 92, "999")), c("014", "092", "999"))
    expect_equal(validate_age_codes(c(1, 2, 3)), c("001", "002", "003"))
})

# --- Special case 'all'
test_that("validate_single_age handles 'all'", {
    expect_equal(validate_single_age("all"), "all")
    expect_equal(validate_age_codes("all"), "all")
})

# --- NULL and empty vectors
test_that("validate_age_codes handles NULL and empty", {
    expect_equal(validate_age_codes(NULL), character(0))
    expect_equal(validate_age_codes(character(0)), character(0))
})

# --- NA and empty string
test_that("validate_single_age rejects NA and empty", {
    expect_error(validate_single_age(NA), "Invalid age code")
    expect_error(validate_single_age(""), "Invalid age code")
})

# --- Invalid inputs
test_that("validate_single_age rejects invalid formats", {
    expect_error(validate_single_age("abc"), "Invalid age code")
    expect_error(validate_single_age("1a3"), "Invalid age code")
    expect_error(validate_single_age(1000), "must be between 0 and 999")
    expect_error(validate_single_age(-1), "must be between 0 and 999")
})

# --- Mixed vector edge cases
test_that("validate_age_codes rejects mixed invalid input", {
    expect_error(validate_age_codes(c("014", "XXX", 92)), "Invalid age code")
    expect_error(validate_age_codes(c(NA, 5)), "Invalid age code")
})

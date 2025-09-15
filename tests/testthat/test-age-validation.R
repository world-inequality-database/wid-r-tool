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

# --- Empty/Boundary Inputs
test_that("validate_age_codes handles edge case inputs", {
    # Empty vector
    expect_equal(validate_age_codes(c()), character(0))

    # NULL input
    expect_equal(validate_age_codes(NULL), character(0))

    # Boundary values
    expect_equal(validate_age_codes(0), "000")
    expect_equal(validate_age_codes(999), "999")
})

# --- Whitespace Edge Cases
test_that("validate_age_codes handles whitespace correctly", {
    # Leading/trailing spaces
    expect_equal(validate_age_codes(" 5 "), "005")
    expect_equal(validate_age_codes("  992  "), "992")

    # Multiple spaces
    expect_equal(validate_age_codes("   65   "), "065")

    # Tab characters
    expect_equal(validate_age_codes("\t5\t"), "005")
})

# --- Numeric Type Variations
test_that("validate_age_codes handles different numeric types", {
    # Integers
    expect_equal(validate_age_codes(5L), "005")

    # Doubles that are whole numbers
    expect_equal(validate_age_codes(5.0), "005")

    # Very small decimals (should truncate)
    expect_equal(validate_age_codes(5.1), "005")
    expect_equal(validate_age_codes(5.9), "005")
})

# --- More Comprehensive Error Testing
test_that("validate_age_codes error messages are specific", {
    # Test exact error messages
    expect_error(validate_age_codes(NA), "Invalid age code", fixed = TRUE)
    expect_error(validate_age_codes(""), "Invalid age code: empty string", fixed = TRUE)
    expect_error(validate_age_codes("abc"), 'Invalid age code: "abc"', fixed = TRUE)
    expect_error(validate_age_codes(-1), "must be between 0 and 999. Got: -1", fixed = TRUE)
    expect_error(validate_age_codes(1000), "must be between 0 and 999. Got: 1000", fixed = TRUE)
})

# --- Special Character Inputs
test_that("validate_age_codes rejects special characters", {
    expect_error(validate_age_codes("5.5"), 'Invalid age code: "5.5"')
    expect_error(validate_age_codes("5a"), 'Invalid age code: "5a"')
    expect_error(validate_age_codes("05a"), 'Invalid age code: "05a"')
    expect_error(validate_age_codes("1000a"), 'Invalid age code: "1000a"')
})

# --- Large Vector Performance Test
test_that("validate_age_codes works with large vectors", {
    large_vector <- rep(c(5, 65, 992), 100)
    result <- validate_age_codes(large_vector)
    expect_equal(length(result), 300)
    expect_true(all(result %in% c("005", "065", "992")))
})

#' Validate age codes for WID database
#'
#' @param ages Character or numeric vector of age codes
#' @return Character vector of validated 3-digit age codes
#' @export
validate_age_codes <- function(ages) {
    # Handle NULL or length-0 input
    if (is.null(ages) || length(ages) == 0) return(character(0))

    # Handle 'all' special case
    if (length(ages) == 1 && identical(ages, "all")) return("all")

    # Apply single-age validation vectorized
    sapply(ages, validate_single_age, USE.NAMES = FALSE)
}

# Internal helper, not exported
validate_single_age <- function(age) {
    # Handle 'all'
    if (identical(age, "all")) return("all")

    # NA handling first
    if (is.null(age) || is.na(age)) {
        stop("Invalid age code", call. = FALSE)
    }

    # Convert numeric to integer
    if (is.numeric(age)) {
        if (age < 0 || age > 999) {
            stop("must be between 0 and 999. Got: ", age, call. = FALSE)
        }
        return(sprintf("%03d", as.integer(age)))
    }

    # Convert character input
    age <- trimws(as.character(age))

    # Empty string
    if (nchar(age) == 0) {
        stop("Invalid age code: empty string", call. = FALSE)
    }

    # Check numeric string
    if (grepl("^[0-9]{1,3}$", age)) {
        return(sprintf("%03d", as.integer(age)))
    }

    # Invalid format
    stop("Invalid age code: \"", age, "\"", call. = FALSE)
}

#' @title Get variables associated to a list of area codes
#'
#' @author Thomas Blanchet
#'
#' @description Perform the GET request to the server to retrieve all
#' variables for a list of area codes.
#'
#' @param areas List of area codes.
#' @param sixlet Six-letter code for which to fetch variables.
#'
#' @importFrom httr GET add_headers content
#' @importFrom base64enc base64encode
#' @importFrom jsonlite fromJSON

get_variables_areas <- function(areas, sixlet = "all") {
    # Concatenate area codes
    query_areas <- paste(areas, collapse = ",")

    # Perform request
    url <- paste0(
        "https://rfap9nitz6.execute-api.eu-west-1.amazonaws.com/prod/",
        "countries-available-variables?countries=", query_areas, "&variables=", sixlet
    )
    response_request <- GET(url, add_headers("x-api-key" = base64encode(api_key)))
    response_content <- content(response_request, as = "text", encoding = "UTF-8")
    response_json <- fromJSON(response_content, simplifyVector = FALSE)
    if (length(response_json) == 1) {
        response_json <- response_json[[1]]
    }

    response_table <- data.frame()
    for (variable in names(response_json)) {
        json_variable <- response_json[[variable]]
        for (country in names(json_variable)) {
            json_country <- json_variable[[country]]
            df_country <- data.frame()
            for (i in json_country) {
                df_country <- rbind(df_country, data.frame(
                    percentile = i[[1]],
                    age = i[[2]],
                    pop = i[[3]],
                    stringsAsFactors = FALSE
                ))
            }
            df_country$variable <- variable
            df_country$country <- country

            response_table <- rbind(response_table, df_country)
        }
    }

    return(response_table)
}

#' @title Get data associated to a list of variables
#'
#' @author Thomas Blanchet
#'
#' @description Perform GET request to the server to retrieve data associated
#' to a list of variables.
#'
#' @param areas List of area codes.
#' @param variables List of variables, of the form: \code{"xxxxxx_pXXpYY_999_i"}
#' @param no_extrapolation Logical: should interpolated/extrapolated years be
#' included or not?
#'
#' @importFrom httr GET add_headers content
#' @importFrom base64enc base64encode
#' @importFrom jsonlite fromJSON

get_data_variables <- function(areas, variables, no_extrapolation = FALSE) {
    # Concatenate area codes, variables
    query_areas <- paste(areas, collapse = ",")
    query_variables <- paste(variables, collapse = ",")

    # Perform request
    url <- paste0(
        "https://rfap9nitz6.execute-api.eu-west-1.amazonaws.com/prod/",
        "countries-variables?countries=", query_areas,
        "&variables=", query_variables, "&years=all"
    )
    response_request <- GET(url, add_headers("x-api-key" = base64encode(api_key)))
    response_content <- content(response_request, as = "text", encoding = "UTF-8")
    response_json <- fromJSON(response_content, simplifyVector = FALSE)

    response_table <- data.frame()
    for (variable in names(response_json)) {
        json_variable <- response_json[[variable]]
        for (json_country in json_variable) {
            # Extract country
            country <- names(json_country)
            # Extract data
            df_data <- data.frame()
            for (i in json_country[[1]]$values) {
                df_data <- rbind(df_data, data.frame(
                    indicator = variable,
                    country = country,
                    year = i[[1]],
                    value = i[[2]],
                    stringsAsFactors = FALSE
                ))
            }
            # Extract metadata
            json_meta <- json_country[[1]]$meta

            if (no_extrapolation) {
                # Periods of extrapolated data
                extrapol_brackets <- json_meta$extrapolation
                if (!is.null(extrapol_brackets)) {
                    if (!is.na(extrapol_brackets) & extrapol_brackets != "") {
                        extrapol_brackets <- fromJSON(extrapol_brackets)

                        # Data points to be included
                        data_points <- json_meta$data_points
                        if (!is.null(data_points)) {
                            if (!is.na(data_points) & data_points != "") {
                                data_points <- fromJSON(data_points)
                            } else {
                                data_points <- NULL
                            }
                        }

                        # List of year to exclude because they are extrapolations
                        to_exclude <- NULL
                        for (i in 1:nrow(extrapol_brackets)) {
                            exclude_range <- seq(
                                from = as.integer(extrapol_brackets[i, 1]) + 1,
                                to = as.integer(extrapol_brackets[i, 2])
                            )
                            to_exclude <- c(to_exclude, exclude_range)
                        }
                        to_exclude <- as.character(to_exclude)
                        to_exclude <- to_exclude[!(to_exclude %in% data_points)]

                        # Remove extrapolations from the data
                        df_data <- df_data[!(df_data$year %in% to_exclude), ]
                    }
                }
            }

            response_table <- rbind(response_table, df_data)
        }
    }

    return(as.data.frame(response_table))
}

#' @title Get metadata associated to a list of variables
#'
#' @author Thomas Blanchet
#'
#' @description Perform GET request to the server to retrieve metadata
#' associated to a list of variables.
#'
#' @param areas List of area codes.
#' @param variables List of variables, of the form: \code{"xxxxxx_pXXpYY_999_i"}
#' @param report_missing Logical, whether to report missing metadata (default TRUE)
#' @param collected_metadata List of metadata already collected (default NULL)
#'
#' @importFrom httr GET add_headers content
#' @importFrom base64enc base64encode
#' @importFrom jsonlite fromJSON

get_metadata_variables <- function(areas, variables, report_missing = TRUE, collected_metadata = NULL) {
    query_areas <- paste(areas, collapse = ",")
    query_variables <- paste(variables, collapse = ",")

    url <- paste0(
        "https://rfap9nitz6.execute-api.eu-west-1.amazonaws.com/prod/",
        "countries-variables-metadata?countries=", query_areas,
        "&variables=", query_variables
    )
    response_request <- GET(url, add_headers("x-api-key" = base64encode(api_key)))
    response_content <- content(response_request, as = "text", encoding = "UTF-8")
    response_json <- fromJSON(response_content, simplifyVector = FALSE)

    # Fail-safe for empty metadata_func
    if (is.null(response_json[[1]]$metadata_func) || length(response_json[[1]]$metadata_func) == 0) {
        warning("No metadata returned from API for given areas/variables")
        return(list(response_table = data.frame(), collected_metadata = collected_metadata))
    }

    safe_scalar <- function(x) if (length(x) == 0) NA else x

    response_table <- data.frame()
    response_json <- response_json[[1]]$metadata_func
    all_returned_areas <- c()

    for (json_variable in response_json) {
        variable <- names(json_variable)
        json_name  <- json_variable[[variable]][[1]][[1]]
        json_type  <- json_variable[[variable]][[2]][[1]]
        json_pop   <- json_variable[[variable]][[3]][[1]]
        json_age   <- json_variable[[variable]][[4]][[1]]
        json_units <- json_variable[[variable]][[5]][[1]]
        json_notes <- json_variable[[variable]][[6]][[1]]

        for (meta_country in json_units) {
            all_returned_areas <- c(all_returned_areas, meta_country$country)

            meta_note <- NULL
            for (note in json_notes) {
                if (!is.null(note) && length(note) > 0 && !is.null(note$alpha2) && note$alpha2 == meta_country$country) {
                    meta_note <- note
                    break
                }
            }
            if (is.null(meta_note)) meta_note <- list(
                method = NA, source = NA, data_quality = NA, imputation = NA
            )

            meta <- data.frame(
                variable       = safe_scalar(variable),
                unit           = safe_scalar(meta_country$metadata$unit),
                unitname       = safe_scalar(meta_country$metadata$unit_name),
                shortname      = safe_scalar(json_name$shortname),
                shortdes       = safe_scalar(json_name$simpledes),
                technicaldes   = safe_scalar(json_name$technicaldes),
                shorttype      = safe_scalar(json_type$shortdes),
                longtype       = safe_scalar(json_type$longtype),
                shortpop       = safe_scalar(json_pop$shortdes),
                pop            = safe_scalar(json_pop$longdes),
                shortage       = safe_scalar(json_age$shortname),
                age            = safe_scalar(json_age$fullname),
                country        = safe_scalar(meta_country$country),
                countryname    = safe_scalar(meta_country$country_name),
                method         = safe_scalar(meta_note$method),
                source         = safe_scalar(meta_note$source),
                quality        = safe_scalar(meta_note$data_quality),
                imputation     = safe_scalar(meta_note$imputation),
                stringsAsFactors = FALSE
            )

            response_table <- rbind(response_table, meta)
        }

        missing_countries <- setdiff(areas, all_returned_areas)
        if (length(missing_countries) > 0 && !is.null(collected_metadata)) {
            if (!(variable %in% names(collected_metadata))) collected_metadata[[variable]] <- list()
            if (!("Completely missing" %in% names(collected_metadata[[variable]]))) collected_metadata[[variable]][["Completely missing"]] <- c()
            collected_metadata[[variable]][["Completely missing"]] <- unique(c(
                collected_metadata[[variable]][["Completely missing"]],
                missing_countries
            ))
        }
    }

    response_table$imputation[response_table$imputation == "region"]    <- "regional imputation"
    response_table$imputation[response_table$imputation == "survey"]    <- "adjusted surveys"
    response_table$imputation[response_table$imputation == "tax"]       <- "surveys and tax data"
    response_table$imputation[response_table$imputation == "full"]      <- "surveys and tax microdata"
    response_table$imputation[response_table$imputation == "rescaling"] <- "rescaled fiscal income"

    return(list(response_table = response_table, collected_metadata = collected_metadata))
}


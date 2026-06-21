#' @title Get variables associated to a list of area codes
#'
#' @author Thomas Blanchet
#'
#' @description Perform the GET request to the server to retrieve all
#' variables for a list of area codes.
#'
#' @param areas List of area codes.
#' @param sixlet Six-letter code for which to fetch variables.
#' @param environment = either "prod" or "dev"; default = "prod".
#'
#' @importFrom httr GET add_headers content
#' @importFrom base64enc base64encode
#' @importFrom jsonlite fromJSON
#' @importFrom utils capture.output str
#' @export
get_variables_areas <- function(areas, sixlet = "all",
    environment=c("prod", "dev") ) {
    # Check environment
    environment <- match.arg(environment)
    # Concatenate area codes
    query_areas <- paste(areas, collapse = ",")

    # Perform request
    url <- paste0(
        base_api_url(environment),
        "countries-available-variables?countries=", query_areas, "&variables=",
        sixlet
    )
    response_request <- GET(url, add_headers("x-api-key" =
                                                 base64encode(api_key)))
    response_content <- content(response_request, as = "text", encoding =
                                    "UTF-8")
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
#' @param environment = either "prod" or "dev"; default = "prod".
#'
#' @importFrom httr GET add_headers content
#' @importFrom base64enc base64encode
#' @importFrom jsonlite fromJSON
#' @export
get_data_variables <- function(areas, variables, no_extrapolation = FALSE,
                               environment=c("prod", "dev") ) {
    # Check environment
    environment <- match.arg(environment)
    # Concatenate area codes, variables
    query_areas <- paste(areas, collapse = ",")
    query_variables <- paste(variables, collapse = ",")
    # Perform request
    url <- paste0(
        base_api_url(environment),
        "countries-variables?countries=", query_areas,
        "&variables=", query_variables, "&years=all"
    )
    response_request <- GET(url, add_headers("x-api-key" = base64encode(api_key)))
    response_content <- content(response_request, as = "text", encoding = "UTF-8")
    response_json <- fromJSON(response_content, simplifyVector = FALSE)

    #handle large payload
    if (is.list(response_json) && !is.null(response_json$status)) {
        if (response_json$status == "payload_too_large") {
            message("Downloading large data from alternative route (please wait)")
            if (isTRUE(getOption("wid.debug"))) {
                payload_summary <- capture.output(str(response_json))
                message("Large payload response summary:\n", paste(payload_summary, collapse = "\n"))
            }
            download_url <- response_json$download_url
            if (is.null(download_url) || is.na(download_url) || download_url == "") {
                stop("could not download large result: missing download URL.")
            }
            response_request <- GET(download_url)
            response_content <- content(response_request, as = "text", encoding = "UTF-8")
            response_json <- fromJSON(response_content, simplifyVector = FALSE)
        } else {
            server_message <- response_json$message
            if (is.null(server_message) || is.na(server_message) || server_message == "") {
                server_message <- "server response invalid"
            }
            stop(server_message)
        }
    }
    if (length(response_json) == 1 && is.null(names(response_json))) {
        response_json <- response_json[[1]]
    }
    if (!is.list(response_json)) {
        stop("server response invalid")
    }


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
#' @param report_missing Logical: report any missing metadata when set to TRUE.
#' @param collected_metadata List used to accumulate missing metadata across calls.
#' @param environment = either "prod" or "dev"; default = "prod".
#'
#' @importFrom httr GET add_headers content
#' @importFrom base64enc base64encode
#' @importFrom jsonlite fromJSON
#' @export
get_metadata_variables <- function(areas, variables, report_missing = TRUE,
    collected_metadata = NULL, environment=c("prod", "dev") ) {
    # Check environment
    environment <- match.arg(environment)
    # Concatenate area codes
    query_areas <- paste(areas, collapse = ",")
    query_variables <- paste(variables, collapse = ",")

    # Perform request
    url <- paste0(
        base_api_url(environment),
        "countries-variables-metadata?countries=", query_areas,
        "&variables=", query_variables)
    response_request <- GET(url, add_headers("x-api-key" =
                                                 base64encode(api_key)))
    response_content <- content(response_request, as = "text",
                                encoding = "UTF-8")
    response_json <- fromJSON(response_content, simplifyVector = FALSE)

    response_table <- data.frame()
    response_json <- response_json[[1]]$metadata_func

    missing_metadata <- list()
    all_returned_areas <- c()

    for (json_variable in response_json) {
        # Extract variable name
        variable <- names(json_variable)
        # Extract the various metadata
        json_name  <- json_variable[[variable]][[1]][[1]]
        json_type  <- json_variable[[variable]][[2]][[1]]
        json_pop   <- json_variable[[variable]][[3]][[1]]
        json_age   <- json_variable[[variable]][[4]][[1]]
        json_units <- json_variable[[variable]][[5]][[1]]
        json_notes <- json_variable[[variable]][[6]][[1]]
        # The item "unit" (5th position) is always filled, so we use it to
        # loop over the different countries
        for (meta_country in json_units) {

            all_returned_areas <- c(all_returned_areas, meta_country$country)
            meta_note <- NULL
            for (note in json_notes[[1]][[1]]) {
                if (note$alpha2 == meta_country$country) {
                    meta_note <- note
                }
            }
            meta <- data.frame(variable = variable, stringsAsFactors = FALSE)

            meta$unit     <- meta_country$metadata$unit
            meta$unitname <- meta_country$metadata$unit_name

            meta$shortname    <- json_name$shortname
            meta$shortdes     <- json_name$simpledes
            meta$technicaldes <- json_name$technicaldes

            meta$shorttype <- json_type$shortdes
            meta$longtype  <- json_type$longtype

            meta$shortpop <- json_pop$shortdes
            meta$pop      <- json_pop$longdes

            meta$shortage <- json_age$shortname
            meta$age      <- json_age$fullname

            meta$country     <- meta_country$country
            meta$countryname <- meta_country$country_name

            # Handle potential missing metadata by setting them to NA
            meta$method     <- if (!is.null(meta_note$method)) meta_note$method else NA
            meta$source     <- if (!is.null(meta_note$source)) meta_note$source else NA
            meta$quality    <- if (!is.null(meta_note$data_quality)) meta_note$data_quality else NA
            meta$imputation <- if (!is.null(meta_note$imputation)) meta_note$imputation else NA


            # Identify missing fields
            missing_fields <- names(meta)[sapply(meta, function(x) all(is.na(x)))]
            if (length(missing_fields) > 0) {
              if (!(variable %in% names(collected_metadata))) {
                collected_metadata[[variable]] <- list()
              }
              key <- paste(sort(missing_fields), collapse = ", ")
              if (!(key %in% names(collected_metadata[[variable]]))) {
                collected_metadata[[variable]][[key]] <- c()
              }
              #collected_metadata[[variable]][[key]] <- c(collected_metadata[[variable]][[key]], meta$country)
              collected_metadata[[variable]][[key]] <- unique(c(collected_metadata[[variable]][[key]], meta$country))

            }

            response_table <- rbind(response_table, meta)
        }

        # **After processing all countries, check for completely missing ones**
        missing_countries <- setdiff(areas, all_returned_areas)
        if (length(missing_countries) > 0) {
          if (!(variable %in% names(collected_metadata))) {
            collected_metadata[[variable]] <- list()
          }
          if (!("Completely missing" %in% names(collected_metadata[[variable]]))) {
            collected_metadata[[variable]][["Completely missing"]] <- c()
          }
          collected_metadata[[variable]][["Completely missing"]] <- unique(c(
            collected_metadata[[variable]][["Completely missing"]],
            missing_countries
          ))
        }
    }

    # Clarify meaning of 'imputation'
    response_table$imputation[response_table$imputation == "region"]    <- "regional imputation"
    response_table$imputation[response_table$imputation == "survey"]    <- "adjusted surveys"
    response_table$imputation[response_table$imputation == "tax"]       <- "surveys and tax data"
    response_table$imputation[response_table$imputation == "full"]      <- "surveys and tax microdata"
    response_table$imputation[response_table$imputation == "rescaling"] <- "rescaled fiscal income"

    return(list(response_table = response_table, collected_metadata = collected_metadata))
    #return(response_table)
}


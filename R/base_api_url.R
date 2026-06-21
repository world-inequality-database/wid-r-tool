#' @title base_api_url
#'
#' @author Spencer Graves (replacing a variable by Thomas Blanchet)
#'
#' @description Create the URL of the API for either the "prod" or "dev"
#' environment in the World Inequality Database.
#'
#' @param environment = either "prod" or "dev"; default = "prod".
#'
#' @return a [`character`] string.
#'
#' @examples
#' bau1 <- base_api_url()
#' bau0 <- base_api_url('dev')
#' @export
base_api_url <- function(environment=c("prod", "dev")){
    Env <- match.arg(environment)
    bau <- paste0("https://rfap9nitz6.execute-api.eu-west-1.amazonaws.com/",
                  Env, "/")
    bau
}

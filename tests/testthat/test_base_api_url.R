test_that("base_api_url", {
    bau1 <- base_api_url()
    bau0 <- base_api_url('dev')
    bau1. <- paste0("https://rfap9nitz6.execute-api.eu-west-1.amazonaws.com/",
                    "prod/")
    bau0. <- paste0("https://rfap9nitz6.execute-api.eu-west-1.amazonaws.com/",
                    "dev/")
    expect_equal(bau1, bau1.)
    expect_equal(bau0, bau0.)
    expect_error(base_api_url('0'))
})

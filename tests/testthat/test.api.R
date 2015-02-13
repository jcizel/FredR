library(FredR)

context("API Tests")

test_that(
    "Call to FredR returns the expected object",
    expect_is(FredR(),"FredR")
)

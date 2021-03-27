test_that("query params is a list of 7", {
  query_params <- nyt_set_query_params()
  expect_is(query_params, "list")
  expect_equal(length(query_params), 7)
})

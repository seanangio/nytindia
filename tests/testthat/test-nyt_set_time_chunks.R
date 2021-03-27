test_that("time chunks are a Date vector", {
  time_chunks <- nyt_set_time_chunks()
  expect_is(time_chunks, "Date")
})

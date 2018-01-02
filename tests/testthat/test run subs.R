

# tests for date funs

context("Check run subs")

# test_that("pull_bucket_from_cloud returns dates correctly", {
#   expect_equal(pull_bucket_from_cloud("html-speeches", "test"), "2014-03-13")
#
# })

test_that("calibrator fun: senario 1", {
  expect_equal(
    calibrator(read_rds("/Users/rosseji/dev/packages/submarines/extdata/default_subs_df.rds"),
                        system = "jet",
                        method = "hotel match",
                        hotel = 150,
                        patrol = 2.5,
                        max.speed = 20,
                        max.power = 7000,
                        speed = 10,
                        power = 1000) %>%
      as.numeric(),
    0.3574957



  )

})

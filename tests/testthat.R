library(testthat)
library(ptoolkit)

test_check("ptoolkit")


####p_bh_helper functionality tests

test_that("p_bh_helper basic functionality", {
  expect_equal(p_bh_helper(c(0.07), alpha = 0.05), c(0.07))

  expect_equal(p_bh_helper(c(0.07, 0.2), alpha =0.05), c(0.14, 0.2))
  expect_equal(p_bh_helper(c(0.2, 0.07), alpha = 0.05), c(0.2, 0.14))

  expect_equal(p_bh_helper(c(0.01, 0.02, 0.03), alpha = 0.05), c(0.03, 0.03,0.03))

  expect_equal(p_bh_helper(c(0.02, 0.03, 0.01), alpha =0.05), c(0.03,0.03,0.03))

  expect_equal(p_bh_helper(c(.02,.12,.24,.56,.6), alpha =0.05), c(0.1,0.6, 0.4,0.7, 0.6))
})

test_that("p_bh_helper repeated values index the same",{
  expect_equal(p_bh_helper(c(0.04, 0.04, 0.04,0.08)), c(0.16,0.16,0.16,0.08))
})




test_that("p_bh_helper all values are between 0 and 1", {
  expect_error(p_bh_helper(c(-3)), "not a valid probability")
  expect_error(p_bh_helper(c(-3,.05)), "not a valid probability")
  expect_error(p_bh_helper(c(0.05,-3)), "not a valid probability")
  expect_error(p_bh_helper(c(8)), "not a valid probability")
  expect_error(p_bh_helper(c(8,.05)), "not a valid probability")
  expect_error(p_bh_helper(c(0.05,8)), "not a valid probability")
})


###p_bonferroni_helper functionality tests

test_that("p_bonferroni_helper basic functionality", {
  expect_equal(p_bonferoni_helper(c(0.07)), c(0.07))
  expect_equal(p_bonferoni_helper(c(0.07, 0.2)), c(0.14, 0.2))
  expect_equal(p_bonferoni_helper(c(0.2, 0.07)), c(0.2, 0.14))
  expect_equal(p_bonferoni_helper(c(0.01, 0.02, 0.03)), c(0.03, 0.06,0.09))
})

test_that("p_bonferroni_helper all values are between 0 and 1", {
  expect_error(p_bonferroni_helper(c(-3)), "not a valid probability")
  expect_error(p_bonferroni_helper(c(-3,.05)), "not a valid probability")
  expect_error(p_bonferroni_helper(c(0.05,-3)), "not a valid probability")
  expect_error(p_bonferroni_helper(c(8)), "not a valid probability")
  expect_error(p_bonferroni_helper(c(8,.05)), "not a valid probability")
  expect_error(p_bonferroni_helper(c(0.05,8)), "not a valid probability")
})

test_that("p_bonferroni_helper maximum return is 1",{
  expect_equal(p_bonferoni_helper(c(0.4,0.7)), c(0.8,1))
})

####p_adjust basic functionality

test_that("p_adjust basic vector functionality", {
  expect_equal(p_adjust(data = c(0.07), method = "Bonferroni"),
    data.frame(p_value = c(0.07), adjusted = c(0.07)      )
})



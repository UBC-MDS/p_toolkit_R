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
    data.frame(p_value = c(0.07), adjusted = c(0.07))      )
  expect_equal(p_adjust(data = c(0.07), method = "BH"),
                 data.frame(p_value = c(0.07), adjusted = c(0.07))      )
   expect_equal(p_adjust(data = c(0.07,0.2), method = "BH"),
                data.frame(p_value = c(0.07, 0.2), adjusted = c(0.14,0.2)) )
   expect_equal(p_adjust(data = c(0.07,0.2), method = "Bonferroni"),
                data.frame(p_value = c(0.07, 0.2), adjusted = c(0.14,0.4)) )
})

test_that("p_adjust basic dataframe functionality", {
  expect_equal(p_adjust(data = data.frame(test = c("test 1"),p=c(0.07)),column ="p", method = "Bonferroni"),
               data.frame(test = c("test 1"), p_value = c(0.07), adjusted = c(0.07))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1"),p=c(0.07)),column ="p", method = "BH"),
               data.frame(test = c("test 1"), p_value = c(0.07), adjusted = c(0.07))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1"),p=c(0.07,0.2)),column ="p", method = "Bonferroni"),
               data.frame(test = c("test 1"), p_value = c(0.07,0.2), adjusted = c(0.14,0.4))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1"),p=c(0.07,0.2)),column ="p", method = "BH"),
               data.frame(test = c("test 1"), p_value = c(0.07,0.2), adjusted = c(0.14,0.2))      )
})

###p_methods basic functionality
test_that("p_methods basic vector functionality", {
  expect_equal(p_methods(data = c(0.07), alpha = 0.05),
               data.frame(p_value = c(0.07),
                          Bonferroni_critical_value = c(0.05),Bonferroni_reject =c(FALSE),
                          BH_critical_value = c(0.05), BH_reject = FALSE))
  expect_equal(p_methods(data = c(0.01), alpha = 0.05),
              data.frame(p_value = c(0.01),
                          Bonferroni_critical_value = c(0.01),Bonferroni_reject =c(TRUE),
                          BH_critical_value = c(0.01), BH_reject = TRUE))
})


test_that("p_methods basic dataframe functionality", {
  expect_equal(p_methods(data = data.frame(test = c("test 1"),p=c(0.07)),column ="p", alpha = 0.05),
               data.frame(test = c("test 1"),
                          p_value = c(0.07),
                          Bonferroni_critical_value = c(0.05),Bonferroni_reject =c(FALSE),
                          BH_critical_value = c(0.05), BH_reject = FALSE))
  expect_equal(p_methods(data = data.frame(test = c("test 1"),p=c(0.07)),column ="p", alpha = 0.05),
               data.frame(test = c("test 1"),
                          p_value = c(0.01),
                          Bonferroni_critical_value = c(0.05),Bonferroni_reject =c(TRUE),
                          BH_critical_value = c(0.05), BH_reject = TRUE))
})

###p_qq functionality tests
## This thread was very helpful for plot tests
## https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot
## https://stackoverflow.com/questions/13457562/how-to-determine-the-geom-type-of-each-layer-of-a-ggplot2-object

test_that("p_qq outputs a ggplot object", {
  p <- p_qq(df)
  expect_true(is.ggplot(p))
})

test_that("p_qq axis labels and title", {
  p <- p_qq(df)
  expect_identical(p$labels$y, "Observed -log(p)")
  expect_identical(p$labels$x, "Expected -log(p)")
})

test_that("p_qq uses geom_point and geom_abline", {
  p <- p_qq(df)
  ## Used code for getting geoms from this thread:
  ## https://stackoverflow.com/questions/13457562/how-to-determine-the-geom-type-of-each-layer-of-a-ggplot2-object

  geoms <- sapply(p$layers, function(x) class(x$geom)[1])
  expect_true("GeomPoint" %in% geoms)
  expect_true("GeomAbline" %in% geoms)
  expect_true(length(geoms)==2)

})

test_that("p_qq plot mapping", {
  p <- p_qq(df)
  expect_true(is.ggplot(p))
  expect_identical(p$mapping$y, "theoretical_pvalues")
  expect_identical(p$mapping$x, "real_pvalues")
})

###p_plot functionality tests

test_that("p_plot outputs a ggplot object", {
  p <- p_plot(df)
  expect_true(is.ggplot(p))
})

test_that("p_plot axis labels and title", {
  p <- p_plot(df)
  expect_identical(p$labels$y, "p(k)")
  expect_identical(p$labels$x, "k")
})

test_that("p_plot uses geom_point and geom_abline", {
  p <- p_plot(df)
  geoms <- sapply(p$layers, function(x) class(x$geom)[1])
  expect_true("GeomPoint" %in% geoms)
  expect_true("GeomAbline" %in% geoms)
  expect_true(length(geoms)==3)

})

test_that("p_plot plot mapping", {
  p <- p_qq(df)
  expect_true(is.ggplot(p))
  expect_identical(p$mapping$y, "pvalue")
  expect_identical(p$mapping$x, "k")
})


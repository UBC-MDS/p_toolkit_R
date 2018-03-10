


context("functionality test")

test_that("p_adjust basic vector functionality", {
  expect_equal(p_adjust(data = c(0.07), method = "bonferroni"),
               data.frame(p_value = c(0.07), bonf_pvalue = c(0.07))      )
  expect_equal(p_adjust(data = c(0.07), method = "BH"),
               data.frame(p_value = c(0.07), bh_pvalue = c(0.07))      )
  expect_equal(p_adjust(data = c(0.07,0.2), method = "BH"),
               data.frame(p_value = c(0.07, 0.2), bh_pvalue = c(0.14,0.2)) )
  expect_equal(p_adjust(data = c(0.07,0.2), method = "Bonferroni"),
               data.frame(p_value = c(0.07, 0.2), bonf_pvalue = c(0.14,0.4)) )
  expect_equal(p_adjust(data = c(0.2,0.07), method = "BH"),
               data.frame(p_value = c(0.07, 0.2), bh_pvalue = c(0.14,0.2)) )
  expect_equal(p_adjust(data = c(0.2,0.07), method = "Bonferroni"),
               data.frame(p_value = c(0.07, 0.2), bonf_pvalue = c(0.14,0.4)) )
})



test_that("p_adjust basic dataframe functionality", {
  expect_equal(p_adjust(data = data.frame(test = c("test 1"),p=c(0.07)),pv_index ="p", method = "bonferroni"),
               data.frame(test = c("test 1"), p_value = c(0.07), bonf_pvalue = c(0.07))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1"),p=c(0.07)),p_index ="p", method = "BH"),
               data.frame(test = c("test 1"), p_value = c(0.07), bh_pvalue = c(0.07))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1", "test 2"),p=c(0.07,0.2)),pv_index ="p", method = "Bonferroni"),
               data.frame(test = c("test 1", "test 2"), p_value = c(0.07,0.2), bonf_pvalue = c(0.14,0.4))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1", "test 2"),p=c(0.07,0.2)),pv_index ="p", method = "BH"),
               data.frame(test = c("test 1", "test 2"), p_value = c(0.07,0.2), bh_pvalue = c(0.14,0.2))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1", "test 2"),p=c(0.2,0.07)),pv_index ="p", method = "Bonferroni"),
               data.frame(test = c("test 1", "test 2"), p_value = c(0.07,0.2), bonf_pvalue = c(0.14,0.4))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1", "test 2"),p=c(0.2,0.07)),pv_index ="p", method = "BH"),
               data.frame(test = c("test 1", "test 2"), p_value = c(0.07,0.2), bh_pvalue = c(0.14,0.2))      )
})

context('testing data types')

test_that('correct data types', {
  #expect_is(p_bonferroni_helper(c(0.07, 0.2)),'data.frame')
  #expect_is(p_bonferroni_helper(c(0.07, 0.2))$pvals, 'numeric')
  #expect_is(p_bonferroni_helper(c(0.07, 0.2))$alpha, 'numeric')

  #outputs
  expect_is(p_methods(c(0.07, 0.2)),'data.frame')
  expect_is(p_methods(c(0.07, 0.2))$p_value, 'numeric')
  expect_is(p_methods(c(0.07, 0.2))$bonf_value, 'numeric')
  expect_is(p_methods(c(0.07, 0.2))$bonf_signficant, 'logical')
  expect_is(p_methods(c(0.07, 0.2))$bh_value, 'numeric')
  expect_is(p_methods(c(0.07, 0.2))$bh_signficant, 'logical')
  #inputs
  expect_is(data, 'data.frame' |'vector')  ####fix...  or vector???
  expect_is(alpha, 'numeric')
  #expect_is(col, 'integer')  #### or char
  expect_is(method, 'string')
})

###p_methods basic functionality
test_that("p_methods basic vector functionality", {
  expect_equal(p_methods(data = c(0.07), alpha = 0.05),
               data.frame(p_value = c(0.07),
                          bonf_value = c(0.05),bonf_significant =c(FALSE),
                          bh_value = c(0.05), bh_significant = FALSE))
  expect_equal(p_methods(data = c(0.01), alpha = 0.05),
               data.frame(p_value = c(0.01),
                          bonf_value = c(0.05),bonf_significant =c(TRUE),
                          bh_value = c(0.05), bh_significant = TRUE))
  expect_equal(p_methods(data = c(0.01, 0.03), alpha = 0.05),
               data.frame(p_value = c(0.01, 0.03),
                          bonf_value = c(0.025,0.025),bonf_significant =c(TRUE, FALSE),
                          bh_value = c(0.025,0.05), bh_significant = TRUE, TRUE))
})


test_that("p_methods basic dataframe functionality", {
  expect_equal(p_methods(data = data.frame(test = c("test 1"),p=c(0.07)),p_index ="p", alpha = 0.05),
               data.frame(test = c("test 1"),
                          p_value = c(0.07),
                          bonf_value = c(0.05),bonf_significant =c(FALSE),
                          bh_value = c(0.05), bh_significant = FALSE))
  expect_equal(p_methods(data = data.frame(test = c("test 1"),p=c(0.07)),p_index ="p", alpha = 0.05),
               data.frame(test = c("test 1"),
                          p_value = c(0.01),
                          bonf_value = c(0.05),bonf_significant =c(TRUE),
                          bh_value = c(0.05), bh_significant = TRUE))
  expect_equal(p_methods(data = data.frame(test = c("test 1", "test 2"),p=c(0.01,0.03)),p_index ="p", alpha = 0.05),
               data.frame(test = c("test 1", "test 2"),
                          p_value = c(0.01, 0.03),
                          bonf_value = c(0.025, 0.025),bonf_significant =c(TRUE, FALSE),
                          bh_value = c(0.025,0.05), bh_significant = TRUE, TRUE))
})

context('testing data types')

test_that('correct data types', {
  #outputs
  expect_is(p_methods(c(0.07, 0.2)),'data.frame')
  expect_is(p_methods(c(0.07, 0.2))$bh_value, 'numeric')
  expect_is(p_methods(c(0.07, 0.2))$bonf_value, 'numeric')
  expect_is(p_methods(c(0.07, 0.2))$bonf_significant, 'logical')
  expect_is(p_methods(c(0.07, 0.2))$bh_significant, 'logical')
  #inputs
  expect_is(data, 'data.frame')
  expect_is(pv_index, 'integer' |'char')  ###FIX!!!
  expect_is(alpha, 'numeric')
})

###p_qq functionality tests
## This thread was very helpful for plot tests
## https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot
## https://stackoverflow.com/questions/13457562/how-to-determine-the-geom-type-of-each-layer-of-a-ggplot2-object

test_that("p_qq outputs a ggplot object", {
  p <- p_qq(df, index)
  expect_true(is.ggplot(p))
})

test_that("p_qq axis labels and title", {
  p <- p_qq(df,index)
  expect_identical(p$labels$y, "Observed -log(p)")
  expect_identical(p$labels$x, "Expected -log(p)")
})

test_that("p_qq uses geom_point and geom_abline", {
  p <- p_qq(df,index)
  ## Used code for getting geoms from this thread:
  ## https://stackoverflow.com/questions/13457562/how-to-determine-the-geom-type-of-each-layer-of-a-ggplot2-object

  geoms <- sapply(p$layers, function(x) class(x$geom)[1])
  expect_true("GeomPoint" %in% geoms)
  expect_true("GeomAbline" %in% geoms)
  expect_true(length(geoms)==2)

})

test_that("p_qq plot mapping", {
  p <- p_qq(df, index)
  expect_true(is.ggplot(p))
  expect_identical(p$mapping$y, "theoretical_pvalues")
  expect_identical(p$mapping$x, "real_pvalues")
})

###p_plot functionality tests

test_that("p_plot outputs a ggplot object", {
  p <- p_plot(df, index)
  expect_true(is.ggplot(p))
})

test_that("p_plot axis labels and title", {
  p <- p_plot(df, index)
  expect_identical(p$labels$y, "p(k)")
  expect_identical(p$labels$x, "k")
})

test_that("p_plot uses geom_point and geom_abline", {
  p <- p_plot(df, index)
  geoms <- sapply(p$layers, function(x) class(x$geom)[1])
  expect_true("GeomPoint" %in% geoms)
  expect_true("GeomAbline" %in% geoms)
  expect_true(length(geoms)==3)

})

test_that("p_plot plot mapping", {
  p <- p_qq(df, index)
  expect_true(is.ggplot(p))
  expect_identical(p$mapping$y, "pvalue")
  expect_identical(p$mapping$x, "k")
})




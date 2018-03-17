


context("functionality test")

test_that("p_adjust basic vector functionality", {
  expect_equal(p_adjust(data = c(0.07), method = "Bonferroni"),
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

###p_adjust invalid  p_value
test_that("p_adjust with an invalid probability", {
  expect_warning(p_adjust(data = c(0.01, 3), 1), "p-values should be between 0 and 1")
  expect_warning(p_adjust(data = c(0.01, -.02), 1), "p-values should be between 0 and 1")
})



test_that("p_adjust basic dataframe functionality (pv_index is a string)", {
  expect_equal(p_adjust(data = data.frame(test = c("test 1"),p=c(0.07)),pv_index ="p", method = "Bonferroni"),
               data.frame(test = c("test 1"), p_value = c(0.07), bonf_pvalue = c(0.07))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1"),p=c(0.07)),pv_index ="p", method = "BH"),
               data.frame(test = c("test 1"), p_value = c(0.07), bh_pvalue = c(0.07))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1", "test 2"),p=c(0.07,0.2)),pv_index ="p", method = "Bonferroni"),
               data.frame(test = c("test 1", "test 2"), p_value = c(0.07,0.2), bonf_pvalue = c(0.14,0.4))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1", "test 2"),p=c(0.07,0.2)),pv_index ="p", method = "BH"),
               data.frame(test = c("test 1", "test 2"), p_value = c(0.07,0.2), bh_pvalue = c(0.14,0.2))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1", "test 2"),p=c(0.2,0.07)),pv_index ="p", method = "Bonferroni"),
               data.frame(test = c("test 2", "test 1"), p_value = c(0.07,0.2), bonf_pvalue = c(0.14,0.4))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1", "test 2"),p=c(0.2,0.07)),pv_index ="p", method = "BH"),
               data.frame(test = c("test 2", "test 1"), p_value = c(0.07,0.2), bh_pvalue = c(0.14,0.2))      )
})

test_that("p_adjust basic dataframe functionality (pv_index = 'p_value')", {
  expect_equal(p_adjust(data = data.frame(test = c("test 1"),p_value=c(0.07)),pv_index ="p_value", method = "Bonferroni"),
               data.frame(test = c("test 1"), p_value = c(0.07), bonf_pvalue = c(0.07))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1"),p_value=c(0.07)),pv_index ="p_value", method = "BH"),
               data.frame(test = c("test 1"), p_value = c(0.07), bh_pvalue = c(0.07))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1", "test 2"),p_value=c(0.07,0.2)),pv_index ="p_value", method = "Bonferroni"),
               data.frame(test = c("test 1", "test 2"), p_value = c(0.07,0.2), bonf_pvalue = c(0.14,0.4))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1", "test 2"),p_value=c(0.07,0.2)),pv_index ="p_value", method = "BH"),
               data.frame(test = c("test 1", "test 2"), p_value = c(0.07,0.2), bh_pvalue = c(0.14,0.2))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1", "test 2"),p_value=c(0.2,0.07)),pv_index ="p_value", method = "Bonferroni"),
               data.frame(test = c("test 2", "test 1"), p_value = c(0.07,0.2), bonf_pvalue = c(0.14,0.4))      )
  expect_equal(p_adjust(data = data.frame(test = c("test 1", "test 2"),p_value=c(0.2,0.07)),pv_index ="p_value", method = "BH"),
               data.frame(test = c("test 2", "test 1"), p_value = c(0.07,0.2), bh_pvalue = c(0.14,0.2))      )
})



context('testing data types')

# SAMPLE DATA
nSims <- 10 #number of simulated experiments
p <-numeric(nSims) #set up empty container for all simulated p-values

for(i in 1:nSims){ #for each simulated experiment
  x<-rnorm(n = 10, mean = 10, sd = 8) #produce 100 simulated participants
  #with mean=100 and SD=20
  y<-rnorm(n = 10, mean = 30, sd = 5) #produce 100 simulated participants
  #with mean=100 and SD=20
  z<-t.test(x,y) #perform the t-test
  p[i]<-z$p.value #get the p-value and store it
}

p_adjust(p ,1, "bonf",0.05)
test_that('correct data types', {
  # ERROR STRING: col index of dataframe contains character values
  err_str = as.data.frame(x=c('str','test'))
  expect_error(p_adjust((err_str),1, "bonf",0.05),
               "All arguments must be character vectors, not double")
  # ERROR FACTOR: col of dataframe contains factors
  err_fctr = data.frame(x=as.factor(c(1,2,3)))
  expect_error(p_adjust((err_fctr),1, "bh",0.05),
               "All arguments must be character vectors, not double")
  ## ERROR LIST: input is not a df, vector matrix, but it is a list
  err_list = as.list(p)
  expect_error(p_adjust(err_list,2, "bh",0.05),
               "Please ensure you input a vector or matrix of numeric p-values.")
  # TEST MATRIX: p-values are input as matrix and output is dataframe
  test_matrix = as.matrix(p)
  #expect_equal(is.data.frame(p_adjust(test_matrix,1, "bh",0.05), TRUE))

  # TEST MATRIX OUTPUT
  test_matrix = as.matrix(p)
  #expect_equal(p_adjust(test_matrix,1, "bh",0.05)[1,2], sort(test_matrix)[1]*length(test_matrix))

  # TEST VECTOR: p-values are input as vector and output is dataframe
  test_vector = c(.1,.4,.2)
  #expect_equal(is.data.frame(p_adjust(test_vector,1, "bh",0.05), TRUE))

})

#### FEEDBACK FROM MILESTONE 2
test_that('correct method strings', {
  # ERROR METHOD STRING: input method="bonferonu"
  err_methodstr = as.data.frame(x=c('str','test'))
  expect_error(p_adjust(c(0.07,0.2),1, "bonferonu",0.05),
               "Method should be specified as either 'Bonferroni' or 'BH'.")
 })
  

###p_methods basic functionality
test_that("p_methods basic vector functionality", {
  expect_equal(p_methods(data = c(0.07), 1, alpha = 0.05),
               data.frame(p_value = c(0.07),
                          bonf_value = c(0.05),bonf_significant =c(FALSE),
                          bh_value = c(0.05), bh_significant = FALSE))
  expect_equal(p_methods(data = c(0.01), 1, alpha = 0.05),
               data.frame(p_value = c(0.01),
                          bonf_value = c(0.05),bonf_significant =c(TRUE),
                          bh_value = c(0.05), bh_significant = c(TRUE)))
  expect_equal(p_methods(data = c(0.01, 0.03), 1, alpha = 0.05),
               data.frame(p_value = c(0.01, 0.03),
                          bonf_value = c(0.025,0.025),bonf_significant =c(TRUE, FALSE),
                          bh_value = c(0.025,0.05), bh_significant = c(TRUE, TRUE)))
})

###p_methods invalid alpha or p_value
test_that("p_methods with an invalid probability", {
  expect_warning(p_methods(data = c(0.01, 3), 1, alpha = 0.05), "p-values should be between 0 and 1")
  expect_warning(p_methods(data = c(0.01, -.02), 1, alpha = 0.05), "p-values should be between 0 and 1")
})

test_that("p_methods with an invalid alpha",{
  expect_warning(p_methods(data = c(0.01, .07), 1, alpha = 3), "alpha should be between 0 and 1")
  expect_warning(p_methods(data = c(0.01, .02), 1, alpha = -.04), "alpha should be between 0 and 1")
})




test_that("p_methods basic dataframe functionality", {
  expect_equal(p_methods(data = data.frame(test = c("test 1"),p=c(0.07)),pv_index ="p", alpha = 0.05),
               data.frame(test = c("test 1"),
                          p_value = c(0.07),
                          bonf_value = c(0.05),bonf_significant =c(FALSE),
                          bh_value = c(0.05), bh_significant = c(FALSE)))
  expect_equal(p_methods(data = data.frame(test = c("test 1"),p=c(0.01)),pv_index ="p", alpha = 0.05),
               data.frame(test = c("test 1"),
                          p_value = c(0.01),
                          bonf_value = c(0.05),bonf_significant =c(TRUE),
                          bh_value = c(0.05), bh_significant = c(TRUE)))
  expect_equal(p_methods(data = data.frame(test = c("test 1", "test 2"),p=c(0.01,0.03)),pv_index ="p", alpha = 0.05),
               data.frame(test = c("test 1", "test 2"),
                          p_value = c(0.01, 0.03),
                          bonf_value = c(0.025, 0.025),bonf_significant =c(TRUE, FALSE),
                          bh_value = c(0.025,0.05), bh_significant = c(TRUE, TRUE)))
})


test_that("p_methods basic dataframe functionality, P_index = 'p_value", {
  expect_equal(p_methods(data = data.frame(test = c("test 1"),p_value=c(0.07)),pv_index ="p_value", alpha = 0.05),
               data.frame(test = c("test 1"),
                          p_value = c(0.07),
                          bonf_value = c(0.05),bonf_significant =c(FALSE),
                          bh_value = c(0.05), bh_significant = c(FALSE)))
  expect_equal(p_methods(data = data.frame(test = c("test 1"),p_value=c(0.01)),pv_index ="p_value", alpha = 0.05),
               data.frame(test = c("test 1"),
                          p_value = c(0.01),
                          bonf_value = c(0.05),bonf_significant =c(TRUE),
                          bh_value = c(0.05), bh_significant = c(TRUE)))
  expect_equal(p_methods(data = data.frame(test = c("test 1", "test 2"),p_value=c(0.01,0.03)),pv_index ="p_value", alpha = 0.05),
               data.frame(test = c("test 1", "test 2"),
                          p_value = c(0.01, 0.03),
                          bonf_value = c(0.025, 0.025),bonf_significant =c(TRUE, FALSE),
                          bh_value = c(0.025,0.05), bh_significant = c(TRUE, TRUE)))
})



context('testing data types')

test_that('correct data types', {
  #outputs
  expect_is(p_methods(c(0.07, 0.2),1),'data.frame')
  expect_is(p_methods(c(0.07, 0.2),1)$bh_value, 'numeric')
  expect_is(p_methods(c(0.07, 0.2),1)$bonf_value, 'numeric')
  expect_is(p_methods(c(0.07, 0.2),1)$bonf_significant, 'logical')
  expect_is(p_methods(c(0.07, 0.2),1)$bh_significant, 'logical')
  expect_error(p_methods(c()),"Data is missing")
  expect_error(p_methods(c(0.07, 0.2),1,alpha='m'),"Alpha is not numeric")
})

###p_qq functionality tests
## This thread was very helpful for plot tests
## https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot
## https://stackoverflow.com/questions/13457562/how-to-determine-the-geom-type-of-each-layer-of-a-ggplot2-object

test_that("p_qq outputs a ggplot object", {
  df <- data.frame(test = c("test 1"),p=c(0.07))
  p <- p_qq(df, "p")
  expect_true(is.ggplot(p))
})

test_that("p_qq title", {
  df <- data.frame(test = c("test 1", "test2"),p=c(0.07,.1))
  p <- p_qq(df, "p")
  expect_identical(p$labels$title, "QQ")
})

test_that("p_qq uses geom_point and geom_abline", {
  df <- data.frame(test = c("test 1"),p=c(0.07))
  p <- p_qq(df, "p")
  ## Used code for getting geoms from this thread:
  ## https://stackoverflow.com/questions/13457562/how-to-determine-the-geom-type-of-each-layer-of-a-ggplot2-object

  geoms <- sapply(p$layers, function(x) class(x$geom)[1])
  expect_true("GeomPoint" %in% geoms)
  expect_true("GeomPath" %in% geoms)
  expect_true(length(geoms)==2)

})

###p_qq invalid  p_value
test_that("p_methods with an invalid probability", {
  expect_warning(p_qq(data = c(0.01, 3), 1), "p-values should be between 0 and 1")
  expect_warning(p_qq(data = c(0.01, -.02), 1), "p-values should be between 0 and 1")
})



test_that("p_qq plot mapping", {
  df <- data.frame(test = c("test 1"),p=c(0.07))
  p <- p_qq(df, "p")
  expect_identical(p$labels$x, "log_exp")
  expect_identical(p$labels$y, "log_transf")
})

test_that("p_qq plot mapping, pv_index = 'p_value'", {
  df <- data.frame(test = c("test 1"),p_value=c(0.07))
  p <- p_qq(df, "p_value")
  expect_identical(p$labels$x, "log_exp")
  expect_identical(p$labels$y, "log_transf")
})

test_that("p_plot outputs a ggplot object when input is vector", {
  p <- p_qq(c(0.01, .07))
  expect_true(is.ggplot(p))
})

###p_plot functionality tests

test_that("p_plot outputs a ggplot object", {
  df <- data.frame(test = c("test 1"),p=c(0.07))
  p <- p_plot(df,"p")
  expect_true(is.ggplot(p))
})

test_that("p_plot outputs a ggplot object, pv_index = 'p_value'", {
  df <- data.frame(test = c("test 1"),p_value=c(0.07))
  p <- p_plot(df,"p_value")
  expect_true(is.ggplot(p))
})

test_that("p_plot title", {
  df <- data.frame(test = c("test 1"),p=c(0.07))
  p <- p_plot(df, "p")
  expect_identical(p$labels$title, "Bonferroni vs BH")
})

test_that("p_plot uses geom_point and geom_abline", {
  df <- data.frame(test = c("test 1"),p=c(0.07))
  p <- p_plot(df, "p")
  geoms <- sapply(p$layers, function(x) class(x$geom)[1])
  expect_true("GeomPoint" %in% geoms)
  expect_true("GeomAbline" %in% geoms)
  expect_true(length(geoms)==3)

})

test_that("p_plot plot mapping", {
  df <- data.frame(test = c("test 1"),p=c(0.07))
  p <- p_plot(df, "p")
  expect_true(is.ggplot(p))
  expect_identical(p$labels$y, "p_value")
  expect_identical(p$labels$x, "rank")
})

###p_plot invalid alpha or p_value
test_that("p_plots with an invalid probability", {
  expect_warning(p_plot(data = c(0.01, 3), 1, alpha = 0.05), "p-values should be between 0 and 1")
  expect_warning(p_plot(data = c(0.01, -.02), 1, alpha = 0.05), "p-values should be between 0 and 1")
})

test_that("p_plot with an invalid alpha",{
  expect_warning(p_plot(data = c(0.01, .07), 1, alpha = 3), "alpha should be between 0 and 1")
  expect_warning(p_plot(data = c(0.01, .02), 1, alpha = -.04), "alpha should be between 0 and 1")
})

test_that("p_plot outputs a ggplot object when input is vector", {
  p <- p_plot(c(0.01, .07),"p")
  expect_true(is.ggplot(p))
})

#Integration test: plotting functions using the output of p_methods

test_that("p_plot outputs a ggplot object when input is the output from p_methods", {
  p <- p_plot(p_methods(c(0.07, 0.2),1),"p_value")
  expect_true(is.ggplot(p))
})

test_that("p_qq outputs a ggplot object when input is the output from p_methods", {
  p <- p_qq(p_methods(c(0.07, 0.2),1),"p_value")
  expect_true(is.ggplot(p))
})
#=======

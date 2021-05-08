#' Constructor Function to Perform Student's T-Test
#'
#' @param x samples of population A
#' @param y samples of population B
#' @param alpha level of significance
#' @param paired whether x and y are paired samples
#'
#' @return list of confidence interval, test type, p-value, decision to reject or not to reject null hypothesis
#' @importFrom ggplot2 ggplot aes geom_boxplot ggtitle
#' @importFrom stats var.test t.test
#' @export
#'
#' @examples
#' set.seed(32); x=rnorm(30,mean=10,sd=15)
#' set.seed(35); y=rnorm(30,mean=8,sd=15)
#' ans1 = myttest(x, y, alpha = 0.05, paired=FALSE)
#' print(ans1)
#' plot(ans1)
#'
myttest = function(x = numeric(), y = numeric(), alpha = double(), paired = logical())
{
  if (paired == TRUE)
  {
    result = t.test(x, y, paired=TRUE, conf.level=1-alpha)
  }

  else if (paired == FALSE)
  {
    v = var.test(x,y)

    if (v$p.value < alpha)
    {
      result = t.test(x, y, paired=FALSE, var.equal=FALSE, conf.level=1-alpha)
    }
    else if (v$p.value >= alpha )
    {
      result = t.test(x, y, paired=FALSE, var.equal=TRUE, conf.level=1-alpha)
    }
  }

  #extractable results from t.test

  ci = result$conf.int
  test_type = result$method

  t_value = result$statistic
  df = result$parameter
  p_value = result$p.value
  est_meanordiff = result$estimate
  nullhyp = result$null.value
  stderr = result$stderr
  althyp = result$alternative

  #storing these info into obj of class Rttest

  obj = list(ci = ci,
             test_type = test_type,
             x = x,
             y = y,
             alpha = alpha,
             paired = paired,
             t_value = t_value,
             df = df,
             p_value = p_value,
             est_meanordiff = est_meanordiff,
             nullhyp = nullhyp,
             stderr = stderr,
             althyp = althyp)

  class(obj) = "Rttest"
  obj
}

print.Rttest = function(x, ...)
{
  ci = x$ci
  test_type = x$test_type
  p_value = x$p_value
  alpha = x$alpha

  if (p_value < alpha)
  {
    RejectNullHypothesis = "Y"
  }

  else
  {
    RejectNullHypothesis = "N"
  }

  output = list(ci = ci, test_type = test_type, pvalue = p_value, RejectNullHypothesis = RejectNullHypothesis)
  output
}

plot.Rttest = function(x, ...)
{
  sample1 = x$x
  sample2 = x$y
  paired = x$paired
  difference = sample1-sample2

  # plot the boxplot of two population samples
  if (paired == FALSE)
  {
    pop = rep(c("sample1", "sample2"))
    distribution = c(sample1, sample2)
    df = data.frame(pop = pop, distribution = distribution)

    g = ggplot(df, aes(x = pop, y = distribution, fill = pop )) + geom_boxplot()
    g = g + ggtitle("Boxplot of Two Population Samples")
    g
  }

  # plot the boxplot of differences of x and y
  else if (paired == TRUE)
  {
    pop = rep(c("difference"))
    df = data.frame(pop = pop, difference = c(difference))

    g = ggplot(df, aes(x = pop, y = difference, fill = pop )) + geom_boxplot()
    g = g + ggtitle("Boxplot of Differences between Two Samples")
    g
  }

}

set.seed(32); x=rnorm(30,mean=10,sd=15)
set.seed(35); y=rnorm(30,mean=8,sd=15)
ans1 = myttest(x, y, alpha = 0.05, paired=FALSE)
print(ans1)
plot(ans1)

set.seed(32); x=rnorm(30,mean=10,sd=5)
set.seed(35); y=rnorm(30,mean=8,sd=15)
ans2 = myttest(x,y,alpha=0.05,paired=FALSE)
print(ans2)
plot(ans2)

set.seed(32); x=rnorm(30,mean=10,sd=15)
set.seed(35); y = x+ rnorm(30, 5 ,4)
ans3 = myttest(x,y,alpha=0.05,paired=TRUE)
print(ans3)
plot(ans3)


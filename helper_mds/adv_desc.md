## Advanced desriptive statistics

Use this option to show more advanced descriptive statistics for quatitative variables. 

---
**These include**:
  * nbr.val - number of valid (not NULL) and non-missing values (excluding nbr.null and nbr.na)
  * nbr.null - number of not-valid (NULL) observations
  * nbr.na  - number of missing values
  * min - minimum value
  * max - maximum value
  * range - range value (max - min)
  * sum - sum of all values per variable
  * median - median value
  * mean - mean value
  * SE.mean - standart error of the mean
  * CI.mean.0.95 - 95% confidence intervals for the mean
  * var - variance, measure of dispersion
  * std - standard deviation, sqaure root of var
  * corf.var - coeficient of variation, the ratio of standard deviation to the mean. [More info](https://en.wikipedia.org/wiki/Coefficient_of_variation)
  * skewness - measure of the asymmetry
  * skew.2SE - standard error of the skewness. Values much greater than 1 indicate positive skewness, while values less than 1 - negative skewness.
  * kurtosis - measure of the "tailedness" of the probability distribution
  * kurt.2SE - standard error of tne kurtosis. Values greater than 1 indicate a distribution with positive excess of kurtosis (leptokurtic), while values mcuh less than 1, indicate negative excess of kurtosis (platykurtic). 
  * normtest.W - test statistics of Shapiro–Wilk test, which is used to test if the data are normally distributed.
  * normtest.p - statistical significance of Shapiro-Wilk test (p-value). If p-value is less than 0.05 - the data are **Not** normally distributed.
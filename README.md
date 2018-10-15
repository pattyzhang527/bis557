[![Build Status](https://travis-ci.org/pattyzhang527/bis557.svg?branch=master)](https://travis-ci.org/pattyzhang527/bis557)

BIS557
===

This is a repository for storing all code, documentation, and digital 
artifacts for BIS557.

Homework 1
So far the only thing we've done is create and document a function that
calls `lm`. You can use it like this:

```{R}
library(bis557)
fit <- linear_model(Sepal.Length ~., iris)
summary(fit)
```

Homework 2
I created my own ridge_reg function to get the coefficients of a ridge regression model. I also find the relationship between the out-of-samples mean square error and log(lambda) values.

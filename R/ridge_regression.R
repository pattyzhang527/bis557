
#' Fit a Ridge Regression Model
#'
#' @description This function passes parameters to the ridge_reg function.
#' @param formula A formula
#' @param lambda A penalty value 
#' @param data A data.frame
#' @return A ridge_reg object
#' @import MASS stats
#' @examples
#' fit <- ridge_reg(Sepal.Length ~., lambda = 1, iris)
#' summary(fit)
#' @export

#the ridge_reg function is according to what we learnt in class
ridge_reg = function(formula, lambda, data){
  #avoid in messing up the row names, I set the rownames of the dataset as NULL
  rownames(data) = NULL 
  m <- model.matrix(formula, data)
  y <- matrix(data[,as.character(formula)[2]], ncol=1)
  y <- y[as.numeric(rownames(m)),, drop = FALSE]
  
  #fit directly
  #beta <- solve(t(m) %*% (m + labmda * diag(rep(1,ncol(m))))) %*% t(m) %*% y
  
  #fit via svd(singular value decomposition)
  svd_obj <- svd(m)
  U <- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d
  
  D <- diag(svals / (svals^2 + lambda))
  beta <- V %*% D %*% t(U) %*% y
  #now naming the coefficients 
  rownames(beta) = colnames(m)
  ret <- list(coefficients = beta, lambda = lambda, formula=formula)
  class(ret) = "ridge_reg"
  #return the result
  ret
}


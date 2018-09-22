#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats model.matrix terms
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export

linear_model <- function(formula, data) {
  #get the design matrix X
  X<-model.matrix(formula, data)
  
  #extract the response variable Y from the above matrix
  #since for every matrix, Y variable is the first cololum, we get
  var<-all.vars(formula)
  Y<-data[,var[1]]
  
  #use qr function to decompose the matrix and find coefficients as beta
  beta<-qr.solve(X,Y)
  beta[which(beta==0)] <- NA
  
  #fit the coponents of lm model into a list, and set the class as lm
  #if I cannot get the value, I would just assign it as NULL (got help from Hongyu Li)
  
  lm_result = list(
    coefficients = beta, residals = X %*% beta, fitted.values = Y - X %*% beta, rank = ncol(X), 
    df.residual = nrow(X) - ncol(X), call = call('lm', formula),  weights = NULL,
    y = Y, x = X, model = formula, na.action = NA, qr = qr(X),
    terms = terms(x = formula, data = data), contrasts = NA, xlevels = NA, offset = NA
    )
  
  class(lm_result) <- 'lm'
  #get the final result
  return(lm_result)
}
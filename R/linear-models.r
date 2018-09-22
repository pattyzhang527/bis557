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
  var = all.vars(formula)
  y = data[, var[1]]
  X = model.matrix(formula, data)
  
  X_QR = qr(X)
  beta = solve.qr(X_QR, y)
  beta[which(beta == 0)] = NA
  
  fitted = X %*% beta
  residuals = y - fitted
  linear_model_fit = list(coefficients = beta, residals = residuals, fitted.values = fitted,
                          rank = ncol(X), weights = NULL, df.residual = nrow(X) - ncol(X), 
                          call = call('lm', formula), terms = terms(x = formula, data = data),
                          contrasts = NA, xlevels = NA, offset = NA, y = y, x = X, 
                          model = formula, na.action = NA, qr = X_QR)
  class(linear_model_fit) = 'lm'
  return(linear_model_fit)
}


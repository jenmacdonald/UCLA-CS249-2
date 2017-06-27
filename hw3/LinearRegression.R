# HW3 Problem #2: Linear Regression
## Due Sunday May 7, at 11:55pm

# Jennifer MacDonald 604501712

## Load all libraries we need
not.installed = function(package_name)  !is.element(package_name, installed.packages()[,1])

if (not.installed("MASS")) install.packages("MASS")
if (not.installed("ISLR")) install.packages("ISLR")

library(MASS)
library(ISLR)

## Linear Regression and Ridge Regression, and the observations that have greatest influence on the Regression coefficients
Linear_Regression = function(X, y) {

    #  X is a nxp numeric matrix
    #  y = is a nx1 numeric vector
    
    # transform X and y into matrices
    X_ = as.matrix(X)
    y_ = as.matrix(y)

    Xt_X = t(X_) %*% X_
    Xt_y = t(X_) %*% y_

    minimum_lambda_value = 0
    maximum_lambda_value = 100 ##  norm( Xt_X ) / 100000
    number_of_lambda_values = 101

    lambda_values = seq( minimum_lambda_value, maximum_lambda_value, length = number_of_lambda_values )

    n = nrow(X_)
    p = ncol(X_)

    # keep track of all coefficient values -- for each value of lambda -- in a matrix:
    coefficient_values_for_each_lambda = matrix(0, nrow=number_of_lambda_values, ncol=p)

    colnames(coefficient_values_for_each_lambda) = colnames(X_)

    I_p = diag(rep(1,p))  # pxp identity matrix  (rep(1,p) = a sequence with "1" repeated p times)

    for (i in 1:number_of_lambda_values) {
        w = solve(  (Xt_X  +  I_p * lambda_values[i]),  Xt_y )
        coefficient_values_for_each_lambda[i,] = w
    }
    
    # compute the range of coefficient values  (over all values of lambda considered)
    max_coefficient_value = apply( coefficient_values_for_each_lambda, 2, max )
    min_coefficient_value = apply( coefficient_values_for_each_lambda, 2, min )

    coefficient_value_range = max_coefficient_value - min_coefficient_value
    
    # print the column number of the variable that has the largest range
    cat(which.max(as.matrix((coefficient_value_range))), "\n")

    X_minus_Xbar = scale(X_, center=TRUE, scale=FALSE)
    C_inverse = solve( cov(X_) )

    Leverage = diag( (X_minus_Xbar) %*% C_inverse %*% t(X_minus_Xbar) )  ## very wasteful -- we just want the diagonal

    i = which( Leverage == max(Leverage) )

    cat(i, "\n")
    
    # calculate w, the regression coefficients
    w =  solve(t(X_) %*% X_) %*% t(X_) %*% y_
    
    # set the max value and the row number of the max value to the smallest possible values
    # when compared to a row's value, it will update the max value and row if the current value is larger
    max_val = 0
    max_val_i = 1
    
    # loop that calculates "i" values for Cook Distance
    for (i in 1:nrow(X_)){
        # calculate wi, the regression coefficients for X_i and y_i
        w_i = solve(t(X_[-i,]) %*% X_[-i,]) %*% t(X_[-i,]) %*% y_[-i,]
        
        # the predicted y-values minus the y values predicted from w_i 
        y_hat_minus_yi_hat = X_ %*% (w - w_i)
        
        # the current Cook Distance for "i"
        cur_val = t(y_hat_minus_yi_hat) %*% y_hat_minus_yi_hat
        
        # update current value if larger than the stored max value
        # also update the max value's row number
        if(cur_val > max_val){
            max_val = cur_val
            max_val_i = i
        }       
    }
    
    # print row of value that maximize Cook Distance
    cat(max_val_i)
    
}
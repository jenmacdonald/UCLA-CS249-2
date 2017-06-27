# Final Exam Problem #2:  Attractiveness
## Due Sunday June 18, at 11:55pm

### Jennifer MacDonald
### 604501712

logistic_regression = function( formula, dataset, tolerance=1.0e-6 ) {
  # (a) delete all rows of the data that have missing values
  dataset_a = na.omit(dataset) # Delete rows that have null values
  
  # (c) modify your program further to handle missing values by ???rst deleting columns 
  # that have many missing values, and then deleting all rows that still contain any missing values 
  dataset = dataset[, -which(colMeans(is.na(dataset)) > 0.5)] # Columns with more than 50% of the data missing is dropped
  dataset = na.omit(dataset) # Delete remaining rows that still have null values in them
  
  initial.model = model.frame( formula, dataset )
  X = model.matrix( formula, data = dataset )
  y = model.response( initial.model, "numeric" )  # y values should be 0 and 1
  p = ifelse( y==0, 0.25, 0.75 )   # initial values; all y values are 0 or 1
  yhat = log(p/(1-p))
  prev_deviance = 0
  deviance = 2*sum( y*log(1/p) + (1-y)*log(1/(1-p)) )
  
  while (abs(deviance - prev_deviance) > tolerance) {
    w = p * (1-p)
    ynew = yhat + (y-p)/w
    model = lm( ynew ~ X - 1,  weights = w )   #  weighted least squares
    yhat = model$fit
    p = 1/(1 + exp(-yhat))
    prev_deviance = deviance
    deviance = 2 * sum( y*log(1/p) + (1-y)*log(1/(1-p)) )
  }
  rss = sum( residuals( model, type="pearson")^2 )  #  weighted RSS
  dispersion = rss / model$df.residual
  
  # (d) include as a comment the printed summary of the model obtained for the attractiveness data
  # See end of program for summary
  # print(summary(model))
  
  return(list( coef = coef(model),  stderr = sqrt( diag(vcov(model)) ) / sqrt(dispersion)  ))   
}

demo = function() {
  data(iris)
  zero_one_iris = transform( iris,  Species = ifelse( unclass(Species)==2, 0, 1 ) )
  print(zero_one_iris)
  logistic_regression( Species ~ ., zero_one_iris )
}

# (d) Summary of the model obtained for the attractiveness data
# Call:
# lm(formula = ynew ~ X - 1, weights = w)

# Weighted Residuals:
#     Min      1Q  Median      3Q     Max 
# -2.4972 -0.7923 -0.4869  0.9307  4.6457 

# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# X(Intercept)   2.716097   0.491198   5.530 3.87e-08 ***
# Xattractive   -1.639746   0.165611  -9.901  < 2e-16 ***
# Xintelligence  0.013547   0.109727   0.123 0.901759    
# Xage           0.029563   0.007629   3.875 0.000112 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 1.012 on 1311 degrees of freedom
# Multiple R-squared:  0.1031,	Adjusted R-squared:  0.1003 
# F-statistic: 37.66 on 4 and 1311 DF,  p-value: < 2.2e-16

#  Is the resulting model 'better' in some way? 
# Yes, this model is better since it doesnt eliminate every single row like we did in part (a)
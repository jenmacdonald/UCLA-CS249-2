#!/anaconda/bin/Rscript

# Jennifer MacDonald, 604501712
# CS249--Spring 2017--D.S. Parker © 2017
# 04/30/17

# HW2 Problem #1: Multivariate Normal Distributions
## Due Sunday April 30, at 11:55pm

## Specification of the Problem:
# You are given a tabular dataset containing feature values $X$ and classes $\boldsymbol{y}$,
# where the $y$ values are integers between 1 and $k$ (so $k$ is the number of classes).
# (1) Derive the Gaussian/Multivariate Normal Distribution parameters for each class (the centroid/mean vector, and the covariance matrix).
# (2) For each row $\boldsymbol{x}_i$ in the $X$ matrix,
# compute its Gaussian distance to each of the $k$ MVNs
# (using the Gaussian pdf / likelihood function).
# If $j_{min}$ is this closest class, and is different from $y_i$, print $i$, $j_{min}$, and $y_i$.
# More specifically, develop a program that reads in a single numeric table from stdin.
# The last column is $\boldsymbol{y}$, and the columns before this define a matrix $X$.
# Your program should identify rows in the dataset that are "misclassified" by $\boldsymbol{y}$,
# and print out information about these rows.

## The Multivariate Normal / Gaussian pdf as a function
# $$
# g({\boldsymbol{x}}, {\boldsymbol{\mu}}, \Sigma) ~~=~~
# \frac{1}{{(2\,\pi)}^{p/2}} ~
# \frac{1}{\sqrt{\det\,\Sigma}} ~
# \exp\left({ \, -\frac{1}{2} \;
# {\,({\boldsymbol{x}}-{\boldsymbol{\mu}})'}
# \; \Sigma^{-1} \,
# {\,({\boldsymbol{x}}-{\boldsymbol{\mu}})}
#  \, }\right) .
# $$

g = function(xvec, meanvec, inverseCovMatrix, Sigma, p) {
    detSigma = det(Sigma)
    
    return (1 / sqrt(2*pi)^(p/2) / sqrt(detSigma) *
        exp( -1/2 * ( t(xvec-meanvec) %*% inverseCovMatrix %*% (xvec-meanvec) )[1,1] ))
}

## Find the closest Multivariate Normal Distribution for any table (dataset)
MVN = function(Table) {
    
    X = data.matrix( Table[, 1:(ncol(Table)-1) ] )  #  data.matrix() forces data to be numeric
    classifications = unclass(Table[, ncol(Table) ])

    k = length(unique(classifications))  #  k is the number of different classifications

    y = classifications  # the class values will always be integers from 1 to k here.

    n = nrow(X)
    p = ncol(X)

    distance_value = matrix(0, nrow=n, ncol=k)  # matrix to record distance values
    
    # ... For each class j from 1 to k
    for (j in 1:k) {
        Data_for_j_th_class = subset(X, (y==j) )
        
        # ...    Derive the MVN distribution parameters for the j-th class.
        mean_vector = matrix( apply(Data_for_j_th_class, 2, mean), nrow=p, ncol=1 )  # column vector
        cov_matrix = cov(Data_for_j_th_class) # covariance matrix
        
        # ...    For each row x[i,] in the X matrix,
        for (i in 1:n) {
            # ...       distance_value[i,j] = the Gaussian distance_value of x[i,] to class j 
            # ...           (using a function like g(), defined above).
            distance_value[i,j]  =  g( X[i,], mean_vector, solve(cov_matrix), cov_matrix, p )
        }
    }
    
    # ... For each row x[i,] in the X matrix,
    for (i in 1:n) {
        
        # # ...    jmin is the number of this closest class
        jmin = which(distance_value[i,] == max(distance_value[i,]))

        # ...    If jmin is different from y[i],    
        if(jmin != y[i] && length(jmin) == 1)
            # ...    print i, jmin, and y[i].
            cat(sprintf("%i %i %i\n", i, jmin, y[i]))
    }
}

Table = data.matrix(read.csv( file("stdin"), header=TRUE ))
MVN(Table)
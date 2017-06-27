# HW3 Problem #1: PCA
## Due Sunday May 7, at 11:55pm
## However:  it is strongly recommended you master this material before the Midterm on Saturday May 6.  The Midterm will cover this material.

# Jennifer MacDonald 604501712

## ISL
not.installed <- function(pkg) !is.element(pkg, installed.packages()[,1])

if (not.installed("ISLR"))  install.packages("ISLR")

library(ISLR)  #  load the ISLR package

## Principle Components, strongest loadings, and the observations that have greatest influence on the PCs
PCA = function(Table) {
    
    lambda = svd( cor(Table) )$d
    n = nrow(Table)
    p = ncol(Table)

    # sum of number of values where lambda is above 0.7
    cat( sum((lambda >= 0.7), na.rm = TRUE), "\n" ) 
    
    # sum of number of values were lambda is above 1
    cat( sum(lambda >= 1, na.rm = TRUE), "\n" ) 
    PrincipalComponents = svd(cor(Table))$u  ### correlation matrix

    SortedSquaredComponents = apply(PrincipalComponents^2, 2, function(x) sort(x, decreasing=TRUE))
    
    CumulativeSortedSquaredComponents = apply( SortedSquaredComponents, 2, cumsum)
    
    # array to hold each principle component's variable row number    
    nStrongLoadings <- vector() 
    # for each column in CumulativeSortedSquaredComponents
    for(j in 1:p)
    {
        # for each row in CumulativeSortedSquaredComponents
        for(i in 1:p)
        {
            # check if value over 0.7
            if(CumulativeSortedSquaredComponents[i,j] > 0.7)
            {
                nStrongLoadings[j] <- i # return the row number of the value
                break # go to the next column
            }
        }
    }
    cat(nStrongLoadings, "\n") 
    
    U = PrincipalComponents
    Z = scale(Table, center=TRUE, scale=FALSE) %*% U
        
    # the correlation matrix
    S = rep(1, n) %*% t(lambda)
    
    # emphasize the size of each row    
    a = apply(Z^2, 1, sum )
    i = which( a == max(a) )
    cat(i, "\n")
    
    # emphasize influence on small principal components
    b = apply((Z^2)/S, 1, sum)
    i = which( b == max(b) )
    cat(i, "\n")
   
    # emphasize influence on large principal components    
    c = apply(S*(Z^2), 1, sum)
    i = which( c == max(c) )
    cat(i, "\n")      
}
                                    
Table = data.matrix(read.csv( file("stdin"), header=TRUE ))
PCA(Table)
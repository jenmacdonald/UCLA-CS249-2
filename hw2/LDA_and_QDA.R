# HW2 Problem #2:  LDA and QDA
## Due Sunday April 30, at 11:55pm

### Problem:  given a tabular dataset of feature values $X$ and classes $\boldsymbol{y}$, derive both LDA and QDA models, and determine how accurate they are.
# More specifically, develop a program that reads in a numeric table with X and y from stdin, determines the normal distribution parameters for X, and derive the LDA and QDA models described in Chapter 4.4 of the ISL text (an introduction), and Chapter 4.3 of the ESL text (more rigorous treatment).
# The columns of $X$ should all be numeric.  The values in the last column, $\boldsymbol{y}$,
# can be either symbolic or numeric.
# For example, with the <tt>iris</tt> dataset, the last column is symbolic.
# Your program should print the <i>confusion matrix</i> for LDA,
# and also print the confusion matrix for QDA.
# If there are $k$ different classes, the confusion matrix is a $k \times k$ table
# whose $i,j$-th entry is the number of times that an input row $\boldsymbol{x}$
# was classified (by LDA or QDA) as being in class $i$, when in fact its $y$ value is $j$.
# Your program should print the confusion matrices both for LDA and for QDA.

## lda and qda
# Your program can use the "lda" and "qda" functions in the MASS library in order to classify data.
# To use lda and qda, you can install the MASS package in R

# load the MASS package, which includes simple implementations of LDA and QDA

not.installed <- function(pkg) !is.element(pkg, installed.packages()[,1])

if (not.installed("MASS"))  install.packages("MASS")  # we need the MASS package

library(MASS)  #  load the MASS package

#  ?lda      #  help for the LDA classifier
#  ?qda      #  help for the QDA classifier

## Use predict to turn a model into a function
# If $M$ is a model, and $X$ is a (possibly new) set of $X$ values, then  <b>predict(M, X)</b> yields the vector of $\boldsymbol{y}$ values predicted by the model for the input matrix $X$.
# If $M$ is a classification model, then <b>predict(M, X)</b> yields the classifications for feature vectors in the rows of $X$.

classifier = function(Model,X)  {
   predict(Model, as.data.frame(X))$class
}

## Print the confusion matrix

print_matrix = function(Matrix) {
    for (i in 1:nrow(Matrix)) {
       cat( Matrix[i,], "\n" )  # print each row as a sequence
    }
}

## Print the confusion matrices for both LDA and for QDA

LDA_and_QDA = function(Table) {
        
    X = data.matrix( Table[, 1:(ncol(Table)-1) ])
    classifications = Table[, ncol(Table) ]

    y = unclass(classifications)  # convert the class values into numeric indices

    n = nrow(X)
    p = ncol(X)
    
    # ... construct an LDA representation of X
    LDA.model <- lda(y ~ X)
    
    # Compute the LDA predictions ("classifications") for each input row.
    LDA.classifications = classifier(LDA.model, X)

    # ... determine for which rows in X the LDA classification differs from y
    LDA.disagreements = (1:nrow(X))[ LDA.classifications != y ]
   
    LDA.confusion.matrix = table( LDA.classifications, y )
    
    # ... print the confusion matrix for LDA
    print_matrix( LDA.confusion.matrix )
    
    # ... construct a QDA representation of X
    QDA.model <- qda(y ~ X)

    # Compute the LDA predictions ("classifications") for each input row.
    QDA.classifications = classifier(QDA.model, X)
    
    # ... determine for which rows in X the QDA classification differs from y
    QDA.disagreements = (1:nrow(X))[ QDA.classifications != y ]

    QDA.confusion.matrix = table( QDA.classifications, y )
    
    # ... print the confusion matrix for QDA
    print_matrix( QDA.confusion.matrix )
}

Table = data.matrix(read.csv( file("stdin"), header=TRUE ))
LDA_and_QDA(Table)
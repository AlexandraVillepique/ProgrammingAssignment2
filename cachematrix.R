############################################################################
# cachematrix.R
#
#"Generates a cache of solve operation in different enviroment"
#
# A simple code that makes a list which stores matrix and inverse of the matrix.
# It is assumed that input matrix is squared and inversiable. Code does not ch-
# ecks validity of input matrix. 
# For more flexible solution look up package "R.cache". 
#
#
# Author: Aleksandra Villepique
# Date: 2014-11-23
#
##########################################################################


makeCacheMatrix <- function(x = matrix()) {
      ## makes list that cashes matrix and inverse matrix
      xi<=NULL
      set <- function(y){
            x <<- y
            xi <<- NULL
      }
      get <- function() x
      setinverse < - function(inverse) xi << - solve
      getinverse <- function() xi
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Following function calculates the inverse of the matrix that is part of 
## construct created with makeCacheMatrix. If the inverse is already calculated
## it skips calculation.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      xi <-x$getinverse()
      if(!is.num(xi)){
            message("getting cahsed data")
            return(xi)
      }
      data <-x$get()
      datai <-solve(data,...)
      x$setinverse(datai)
      datai
}

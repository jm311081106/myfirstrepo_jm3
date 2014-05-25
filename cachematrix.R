## This is a pair of functions that allow for the caching of the inverse
## of a matrix, regardless of size. The matrix is assumed to be invertible
## The two functions take advantage of the lexical scoping features of R.
## code tested with the following
## (1)  my_matrix <- matrix(rnorm(16), nrow=4)
## (2)  yy<-makeCacheMatrix(my_matrix)
## (3)  zz<- cacheSolve(yy)
## (4)  round(my_matrix %*% zz,4)
## identity matrix returned!
## repeating line (3) returns cache value message
## This function creates a matrix that contains an additional
## feature of that allows it to chache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL  ## define default value
      set <- function(y) {  ## declare environment
            x <<- y ## pass variable from parent environment
            m <<- NULL ## set defualt value
      }
      get <- function() x ## call function to get cached value 
      setInverse <- function(solve) m <<- solve ## use solve function to declare inverse of matrix 
      getInverse <- function() m ## get cached value
      ## option list of function call
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## This function computes the inverse of a special matrix
## created by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
      m <- x$getInverse() ## get inverse of matrix argument
      if(!is.null(m)) { ## determine if matrix inversion completed in the past
            message("getting cached data")
            return(m)
      }
      data <- x$get() ## if not, get inversion
      m <- solve(data, ...) ## invert
      x$setInverse(m) ## set cache value
      m ## return value        ## Return a matrix that is the inverse of 'x'
}

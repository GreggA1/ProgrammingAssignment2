## The first function, makeCacheMatrix creates a special "matrix" which is really a list containing a function to get the 
##inverse of that matrix


makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(inverse) invr <<- inverse
  getMatrixInverse <- function() invr
  list(set = set,get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}

##The following function calculates the inverse of the special "matrix" created with the above function. The function first 
##checks to see if the matrix inversion has already been calculated. If so, it gets the inversion results from the cache
##and skips calculating it. Otherwise, it calculates the inversion values
##and stores the it in the cache with the setMatrixInverse function. 
cacheSolve <- function(x, ...) {
  invr <- x$getMatrixInverse()
  if (!is.null(invr)) {
    message("getting cached inverse matrix data")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data, ...)
  x$setMatrixInverse(invr)
  invr
}
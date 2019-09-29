## This set of functions creates the opportunity to cache the inverse of a matrix and retreive that inversion later on.
## This function only works on square matrices.

## This function below creates a list of 
## 1 setting the matrix, 
## 2 getting the matrix
## 3 setting the inverse of the matrix
## 4 getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) s <<- solve
      getInverse <- function() s
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## The function below retreives a cached matrix (out of the list) and if unchanged, returns the cached inverse of the matrix. 
## If changed or not set, it computes the inversion and then chaches and return it.

cacheSolve <- function(x) {
            s <- x$getInverse()
            if(!is.null(s)) {
                  message("getting cached inversed matrix")
                  return(s)
            }
            data <- x$get()
            s <- solve(data, ...)
            x$setInverse(s)
            s
}

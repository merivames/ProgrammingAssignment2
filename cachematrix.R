## Will cache the inverse of a matrix to speed up execution.

## This function will create a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, 
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function will calculate the inverse of the matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv)
        inv
}

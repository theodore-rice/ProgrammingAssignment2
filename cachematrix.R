## This pair of functions cache the inverse of a matrix, so that if the 
## inverse is needed in subsequent computations, it does not need to be
## recomputed

## This creates a special matrix, which is really a list which
## 1) sets the value of the matrix
## 2) gets the value of the matrix
## 3) sets the value of the inverse
## 4) gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv<<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This functions looks to see if the inverse is cached, if so it 
## returns the cached value, otherwise it computes the inverse and
## caches it.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached value for the inverse")
            return(inv)
            ##function terminates
      }
      matrix <- x$get()
      inverse <- solve(matrix)
      x$setinverse(inverse)
      inverse
}

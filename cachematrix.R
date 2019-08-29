## This pair of functions demonstrates how one can save time by
## using a cache to save information that may be useful. 



## This function creates a special matrix object that can 
## also cache the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
    
        m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve(x)
      getinverse <- function() m
      a <- list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This fuction computes the inverse of the special matrix from
## the above function, but if the inverse is cached and the matrix
## has not changed, it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

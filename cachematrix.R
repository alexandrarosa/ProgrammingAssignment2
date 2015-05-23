##The two functions create an inverted matrix from a matrix 'x' and store it in 
##cache, making it retrievable for the future and, thus, avoiding unnecessary 
##computing.

##This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
      x <<- y
    m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
  setinverse = setinverse,
  getinverse = getinverse)
}

##This function computes the inverse of the matrix returned by the previous 
##function. If the inverse has already been calculated (and the matrix has not 
##changed), the cachesolve should retrieve the inverse from the cache.

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

##For example, the matrix 'x'    
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##returns the inverted matrix
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

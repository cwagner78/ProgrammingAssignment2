## The two functions in this file allow for caching the inverse of a matrix
## "makeCacheMatrix" creates a special object containing the matrix and can cache its inverse.
## It returns the relevant functions to set and get those values.
## "cacheSolve" returns the inverse of the matrix stored in the special object created by "makeCacheMatrix".


## makeCacheMatrix is able to store internally the matrix "x" and its inverse. 
## It returns the following list of functions
## - set the matrix
## - get the matrix
## - set the inverse matrix
## - get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {

      ## the inverse is cached in "m"
      m <- NULL
      
      set <- function(y) {
            ## Only reset values if y is not identical to x
            if (!identical(x,y)){
                  x <<- y
                  m <<- NULL
            }
            
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix given in the special matrix object. 
## It either computes the inverse or gets the precomputed inverse.
cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      ## return the cached data if available
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ## otherwise compute the inverse
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

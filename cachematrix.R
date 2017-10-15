## makeCacheMatrix creates an invertible matrix object

## makeCacheMatrix sets the value of the matrix, gets its value, 
## sets its inverse and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve retreives the inverse of the matrix created
## by makeCacheMatrix. If the inverse has already been
## computed, then cacheSolve will return the inverse of the 
## matrix from the cache. If the inverse has not yet bee
## calculaed, cacheSolve will calculate the inverse

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


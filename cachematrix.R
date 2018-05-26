## Functions that cache the inverse of Matrix
## creating a matrix object to cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse
  i <- NULL
  
  ##Set matrix
  set <- function( matrix ) {
  m <<- matrix
  i <<- NULL
  }
  
  ## Get matrix
  get <- function() {
  ## Return 
  m
  }
  
  ## Set inverse of matrix
  setInverse <- function(inverse) {
  i <<- inverse
  }
  
  ##  Get inverse of matrix
  getInverse <- function() {
  ## Return the inverse
  i
  }
  
  ## Return methods
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## Inverse of special matrix is calculated in the above function
##cachesolve retrieves the inverse from cache.
  cacheSolve <- function(x, ...) {
  
  ## Return inverse of x
  m <- x$getInverse()
  
  if( !is.null(m) ) {
  message("getting cached data")
  return(m)
  }
  
  ## Getting matrix
  data <- x$get()
  
  ##  Matrix multiplication
  m <- solve(data) %*% data
  
  ## Set inverse equal to object
  x$setInverse(m)
  
  ## Return the matrix
  m
  }
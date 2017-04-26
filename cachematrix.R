## Makes a matrix that can "cache" the inverse of the matrix.
## Function Steps
##  1: Set the matrix
##  2: Get the matrix
##  3: Set the inverse
##  4: Get the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    
    x <<- y
    inv() <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
  
}

## Calculates the inverse of the matrix. Checks first to see if the
## inverse has already been calculated, If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it will calculate the inverse
## and set the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)){
    
    message("Getting cached data...")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  
  return(inv)
  
}

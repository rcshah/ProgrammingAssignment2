## Function to calculate inverse of a matrix, caching the result 
## for efficiency

## Setup a matrix to be inverted. Provide basic access functions.

makeCacheMatrix <- function(x = matrix()) {
  ## Basic access functions to get, set matrix
  ## and get/set inverse
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse1) inverse <<- inverse1
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Compute the matrix inverse. Assumption - matrix is invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Check if inverse is already computed, return that if
  ## available
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## Inverse is not available, so compute it
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}

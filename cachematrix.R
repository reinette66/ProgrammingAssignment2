# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly

# Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  ## Return a list 
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}

# The second function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # Return the inverse from the cache if it is already calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  
  ## Return inverse of square invertible matrix
  inv <- solve(data)
  x$setinv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
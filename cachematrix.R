#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
# Below are pair of functions that cache the inverse of a matrix.

# function provides methods to get the matrix and to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  inv <- NULL
  
  ## getter/setter for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## getter/setter for matrix inverse
  getInverse <- function() inv
  setInverse <- function(inverse) inv <<- inverse
  
  ## return list of functions for matrix
  list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}


# calculate inverse of matrix x, unless the inverse has been cached - if so return cached value

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # return cached matrix inverse if it's been already computed
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  
  # compute inverse of matrix 
  m <- x$get()
  inv <- solve(m, ...)
  
  # cache inverse
  x$setInverse(inv)
  
  # return inverse of matrix
  return(inv)
}

## These functions cache the inverse of a matrix.
## cacheSolve returns the inverse by first checking if the inverse
## has already been calculated. If so, it gets the inverse via getInv
## and skips the computation. Otherwise, it will calculate the
## inverse of the data and set the inverse in the cache via setInv function.

## Create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


##  Compute the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  #Use solve() function to calculate inverse, assuming the matrix provided is invertible.
  inv <- solve(data) 
  x$setmean(inv)
  inv
}

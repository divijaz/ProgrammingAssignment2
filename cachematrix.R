## Functions attempt to solve the lexical scoping assignment.
## The functions below create a 'special' matric and solve or retrieve the inverse of the matrix.

## This function creates a 'special'matrix that is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<-y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(solve) inv <<- solve
  getinv <- function()inv
  list(set = set, get=get, setinv=getinv, getinv=getinv)

}


## This function solves for the inverse of the 'special' matrix. If the inverse is already calculated, the function retrieves it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinv()
if(!is.null(inv)){
  message("getting cached data")
  return(inv)
}
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
  }

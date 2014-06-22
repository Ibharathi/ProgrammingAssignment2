## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # invmatrix will store the cached inverse matrix
  invmatrix <- NULL
  ## Matrix setter
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  #Matrix getter
  get <- function() x
  #inverse setter 
  setinvmatrix <- function(inverse) invmatrix <<- inverse
  #inverse getter
  getinvmatrix <- function() invmatrix
  #Return the matrix
  list(set = set, get = get, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
}
# To compute the inverse of the matrix
cacheSolve <- function(x, ...) {
  invmatrix <- x$getinvmatrix()
  #Check if already calculated, then return it
  if (!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  #if not calculated, calculate it
  data <- x$get()
  invmatrix <- solve(data, ...)
  #cache it
  x$setinv(invmatrix)
  #return the cached value
  invmatrix
}




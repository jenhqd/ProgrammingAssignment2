## This program consists two functions: makeCacheMatrix() and cacheSolve().
## makeCacheMatrix() function is to store a matrix and its inverse matrix.
## The cacheSolve() function requires an argument returned through the makeCacheMatrix() function
## and get the inverse matrix from the cached data, if available, instead of re-calculating the inverse matrix

## This function make a list of functions and return it to parent environment
## For example: mcm <- makeCacheMatrix(), access other functions by mcm$set(a_matrix) or mcm$setinv(inv_a_matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv,
       getinv = getinv)
}


## This function checks if the inverse matrix is available (not NULL)
## if it is, the function retrieves the inverse matrix data from cache
## if the inverse data is not available, then the function compute the inverse matrix
## finally, the inverse matrix is returned to the screen

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message('getting cached data')
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}

##Example:
## A <- rbind(c(3,2,5), c(2,3,2), c(5,2,4))
## cacheSolve(makeCacheMatrix(A))
## this calculates inverse matrix of A
## mcm <- makeCacheMatrix()
## inv_A <- cbind(c(-0.296, -0.074, 0.407),
##                c(-0.74, 0.481, -0.148),
##                c(0.407, -0.148, -0.185))
## mcm$set(A); mcm$setinv(inv_A)
## cacheSolve(mcm) #now retrieve inverse matrix from cache

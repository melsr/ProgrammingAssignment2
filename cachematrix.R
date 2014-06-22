
## Put comments here that give an overall description of what your
## functions do

## The purpose of the assignment is to cache the inverse of a matrix
## passed to the makeCacheMatrix function. The cacheSolve function will save time 
## not having to run the inversion each time it is called as long as there is a cached version
## and it has not changed.

## Write a short comment describing this function
## The makeCacheMatrix function takes a passed matrix and caches the inverse value of the function
## for use in calling functions.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Get the matrix
  get <- function() x
  
  ## Set matrix Inverse
  setInverseMatrix <- function(solve) m <<- solve
  
  ## Get matrix Inverse
  getInverseMatrix <- function() m
  
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function
## The cacheSolve function checks for a current version of the inverted matrix
## If there is a current cached matrix, it is used. If there is no cached matrix or it is
## not current, the matrix is retrieved and inverted before returning.

cacheSolve <- function(x, ...) {
  
  ## Get inverse of the matrix x
  inv <- x$getInverseMatrix()
 
  # If the inverse matrix is present and current, returnit
  if(!is.null(inv)) {
    message("Get cached data")
    return(inv)
  }
  
  ## If not cached or not current, get the inverse of the matrix and return it
  dat <- x$get()
  
  inv <- solve(dat, ...)
  x$setInverseMatrix(inv)
  message("No cached data")
  inv
  
}
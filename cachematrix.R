## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "vector", which is really a list containing a function to:
## 1) set the value of the matrix (set)
## 2) get the value of the matrix (get)
## 3) set the value of the inverse of a matrix (setInverse)
## 4) get the value of the inverse of a matrix (getInverse)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    if(!identical(x, y)) {
      x <<- y
      inv <<- NULL
    }
    
  }
  getMatrix <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  
  
  list(set = setMatrix, get = getMatrix,
       setInverse = setInv,
       getInverse = getInv)

}

v <- makeCacheMatrix(NULL)

## cacheSolve returns a cached inverse of a matrix x.
cacheSolve <- function(x, ...) {
  v$set(x)
  m <- v$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- v$get()
  m <- solve(data, ...)
  v$setInverse(m)
  m  
}

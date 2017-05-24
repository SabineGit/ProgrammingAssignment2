## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  makeCacheMatrix creates a special "matrix"
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  
  setmatrix <- function(y) 
    {
      x <<- y
      m <<- NULL
    }
  getmatrix <- function() x
  setinverse<- function(solve) m <<- solve 
  getinverse<- function() m
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## calculates the minverse of makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$getmatrix()
  x$setmatrix(data)
  m <- solve(data, ...)
  x$setinverse(m) 
  m
}

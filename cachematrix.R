## This function creates a special "matrix" object that can 
## cache its inverse

## Assumption: Matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL ## Defining a null matrix
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x ## To return matrix x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m ## To return cached inverse of x
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}

## Computes the inverse of special matrix returned by makeCacheMatrix. 
## If inverse has already been calculated, returns the value of inverse from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    ("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## This implements two functions:
## makeCacheMatrix - creates a special matrix that can cache inverse
## cacheSolve - computes the inverse of the special matrix returned by 'makeCacheMatrix

## This function, makeCacheMatrix, creates a special "matrix, which is really a list
## containing functions to:
## 1. set the matrix
## 2. get the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}


## The function CacheSolve calculates the inverse of the 
## special matrix created by "makeCacheMatrix". It first checks 
## whether the inverse is already calculated, and if so, the inverse
## is returned from the cache. Otherwise, it solves it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getsolve()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}

## Coursera Data Science Assignment for R Language Week3
## Functions that cache the inverse of a matrix

## Creates a special Matrix that contains sub functions as below
## 1. set : set the value of the Matrix
## 2. get : get the value of the Matrix
## 3. setinv : set its inverse Matrix
## 4. getinv : get its inverse Matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Cache the inverse result of the Matrix from makeCacheMatrix function

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

##My test pattern
##Create a 2x2 matrix, called test_matrix
##    [,1] [,2]
##[1,]  3    4
##[2,]  5    6
##cache_test_matrix<-makeCacheMatrix(test_matrix)
##cacheSolve(cache_test_matrix)
##The inverse result is correct as below
##    [,1] [,2]
##[1,]-3.0  2.0
##[2,] 2.5 -1.5

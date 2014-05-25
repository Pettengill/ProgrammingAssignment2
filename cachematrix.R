## The following functions 1) create a cached matrix and 2) computes
## the inverse of the cache matrix unless the matrix has not been
## cached.

## This function creates a matrix of functions that has 4 subfunctions
## which 1) sets the values of a matrix x and creates a null cache, 
## 2) returns the values of the matrix x, 3) sets the inverse value
## of matrix x and 4) returns the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
                 m <- NULL
                 set <- function(y) {
                                 x <<- y
                                 m <<- NULL
                         }
                 get <- function() x
                 setinv <- function(inv) m <<- inv
                 getinv <- function() m
                 list(set = set, get = get,
                                     setinv = setinv,
                                     getinv = getinv)
         }

## This function gets cached data if available and prints message that it
is getting cached data, computes the inverse matrix and returns the inverse matrix.

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
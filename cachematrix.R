## this assignment writes a function (makeCacheMatrix) to cache potentially 
## time-consuming computations. 
## this function takes advantage of the scoping rules of the R language and 
## how they can be manipulated to preserve state inside of an R object.
##
## It uses solve() to find the inverse of a matrix and cache it. 
## 
##### Write a short comment describing this function
##
## makeCacheMatrix creates a list containing a function to:
#     - set the value of the matrix
#     - get the value of the matrix
#     - set the value of inverse of the matrix
#     - get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


####### Write a short comment describing this function
## The following function (cacheSolve) returns the inverse of the matrix. 
## It first checks if the inverse has already been computed. 
## If so, it gets the result and skips the computation. 
## If not, it computes the inverse, sets the value in the cache
## via setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}

## sample output:
##
## creates a 2x2 matrix
## > x<-matrix(1:4,2)
## > x
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## haizany1 is assigned the matrix
##
## > haizany1 = makeCacheMatrix(x)
## > haizany1$get()
### get the matrix
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(haizany1)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(haizany1)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 

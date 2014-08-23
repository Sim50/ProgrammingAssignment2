## creates a chached matrix 'object' w/ setter and getter methods
## example
## > source("cachematrix.R")
## > mat <- matrix(1:4,2,2)
## > matrixObject<-makeCacheMatrix(mat)
## > cacheSolve(matrixObject) ## 1st time inverse is calculated
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(matrixObject) ## 2nd time inverse is retrieved from cache
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## makeCacheMatrix takes a (square) matrix as input and calculates the inverse. A list 
## of set and get methods is returned
makeCacheMatrix <- function(x = matrix()) {
      minv <- NULL
      set <- function(y) {
            x <<- y
            minv <<- NULL
      }
      get <- function() x
      setminv <- function(inverse) minv <<- solve(x)
      getminv <- function() minv
      list(set = set, get = get,
           setminv = setminv,
           getminv = getminv)
}



## cacheMatrix takes a matrixobject created by makeCacheMatrix and returns the inverse of the
## matrix either from cache if it was calculated before or by calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      minv <- x$getminv()
      if(!is.null(minv)) {
            message("getting cached data")
            return(minv)
      }
      data <- x$get()
      minv <- solve(data, ...)
      x$setminv(minv)
      minv
}

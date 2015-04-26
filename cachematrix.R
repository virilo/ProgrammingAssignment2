## Calculate the inverse matrix
## Uses a cache using the same approach as the mean example

## makeCacheMatrix, stores a matrix, and optionaly its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculate the inverse matrix for an makeCacheMatrix object
## If reuse the same makeCacheMatrix object, cacheSolve takes
## care of returning the previously calculated inverse (caching)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}


## Usage: 
## =====
##
## m<-makeCacheMatrix(rbind(c(1, 2), c(3, 4)))
##
## > cacheSolve(m)
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
##
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

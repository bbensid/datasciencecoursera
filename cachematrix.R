## Here I create two functions:
## 1 - makeCacheMatrix: This function creates a special "matrix" object that 
##     can cache its inverse.
## 2 - cacheSolve: This function computes the inverse of the special "matrix" 
##     returned by makeCacheMatrix above. If the inverse has already been 
##     calculated (and the matrix has not changed), then cacheSolve should 
##    retrieve the inverse from the cache.
##

## makeCacheMatrix takes a square matrix x as input and outputs a list of 4 functions 
##  associated to that matrix:


makeCacheMatrix <- function(x = matrix()) {
ix <- NULL
set <- function (y = matrix()) {
          x <<- y
          ix <<- NULL
}
get <- function () x
setInverse <- function(inverse) ix <<- inverse
getInverse <- function() ix
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve is a function that takes the list ouptut of makeCacheMatrix as 
## an input and outputs the inverse of the matrix associated from the cache 
## if exists, or computes it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
          ix <- x$getInverse()
          if(!is.null(ix)) {
                    message("getting cached data")
            return(ix)
          }
          else if((length(dim(x$get()))==2) & (dim(x$get())[1]==dim(x$get())[2])) {
          data <- x$get()
          ix <- solve(data)
          x$setInverse(ix)
          ix}
          else print("The original matrix is not a square matrix !")
}

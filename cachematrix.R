## The functions below meet the requirements of Programming Assignment 2 for the JHU/Coursera R Programming course

## This function creates a special matrix that caches its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  ## x is expected to be a square, invertible matrix
  
  inv <- NULL
  
  ## 1. set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## 2. get the matrix
  get <- function() x
  
  ## 3. set the inverse
  setinv <- function(inverse) inv <<- inverse
  
  ## 4. get the inverse
  getinv <- function() inv
  
  ## return the list of functions which act as arguments to
  ## the cacheSolve function (below)
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function computes the inverse of the special
## matrix returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## x is expected to be a square, invertible matrix
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv) ## return cached inverse
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv ## return computed inverse
}

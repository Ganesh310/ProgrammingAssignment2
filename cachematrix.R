## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function returns the inverse of the matrix. First it checks if the inverse
## has been computed. If yes, then it returns the value and skips the computation.
## If not, then it computes the inverse, and sets the value in the cache via setinv
## function

cacheSolve <- function(x, ...) {
   inverse <- x$getinv()
   if(!is.null(inverse)){
     message("getting cached data")
     return(inverse)
   }
   data <- x$get()
   inverse <- solve(data)
   x$setinv(inverse)
   inverse
}

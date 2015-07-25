## makeCacheMatrix - creates a special matrix object and corresponding
##                   variables that makes possible to cache the matrix
##                   inverse
## cacheSolve      - returns inverse of the matrix created by makeCacheMatrix

## The following function creates special matrix object. It returns a list
## containing functions:
##      - for setting and getting matrix data. 
##      - for setting ang geting the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns the inverse of matrix x
## If the inverse has already been calculated it returns the cached inverse
## If the inverse has not yet been calculated it calculates it and stores it
## into the input matrix object

cacheSolve <- function(x, ...) {
        
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

## This is just like the example online, just with inverse instead of mean and a matrix instead of numeric.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This calculates the inverse of the vector created above. it first checks to see if the inverse has already been calculated. 
## If so, it gets the mean from the cache and skips the computation (all of this is just like in the example online)

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv 
}
## Returns a matrix that is the inverse of 'x'

## Put comments here that give an overall description of what your
## functions do


# this function is used to set/get the value of matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
  var1 <- NULL
  set <- function(y) {
    x <<- y
    var1 <<- NULL
  }
  get <- function() x
  assign_inverse <- function(inverse) var1 <<- inverse
  fetch_inverse_value <- function() var1
  list(set = set,
       get = get,
       assign_inverse = assign_inverse,
       fetch_inverse_value = fetch_inverse_value)
}


## this function calculates the inverse of matrix returned by above function
## if the matrix hasn't changed and inverse is already calculated
## it returns the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  var1 <- x$fetch_inverse_value()
  if (!is.null(var1)) {
    message("getting cached data")
    return(var1)
  }
  data <- x$get()
  var1 <- solve(data, ...)
  x$assign_inverse(var1)
  var1
}

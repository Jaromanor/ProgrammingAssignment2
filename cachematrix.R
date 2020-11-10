## Put comments here that give an overall description of what your
## functions do

## The following function creates an inverse array that can be cached

makeCacheMatrix <- function(x = matrix()) {
  #Step to store the reverse
  inverse <- NULL
  #formulate original matrix and recalculate the inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # get the original matrix
  get <- function() x
  #formulate the inverse value
  set_inverse <- function(inv) inverse <<- inv
  # get inverse value
  get_inverse <- function() inverse
  #returns a list of the functions, which is the special matrix
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## The following function can compute the inverse matrix set by the 
## makeCacheMatrix object. If you have already calculated the inverse
## (and the array has not changed), then the cache solution should 
## retrieve the inverse array present in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}

##tests

pruebamatrix = makeCacheMatrix(matrix(c(4,3,-2,-1), nrow=2, ncol=2))

pruebamatrix$get()     #original matrix

cacheSolve(pruebamatrix) # matrix inverse

pruebamatrix$get_inverse() #Returns matrix inverse

cacheSolve(pruebamatrix) #Returns cached matrix inverse

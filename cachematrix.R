matrix <- matrix(c(1,2,3,4), nrow=2, ncol=2)
invert <- solve(matrix)

## This Script contain a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
    # store inverse value
    inverse <- NULL
    # set the original matrix and reset inverse
    set <- function(y) {
      matrix <<- y
      inverse <<- NULL
    }
    # get the original matrix
    get <- function() matrix
    # set inverse value
    set_inverse <- function(solve) inverse <<- solve
    # get inverse value
    get_inverse <- function() inverse
  
    # Returns a list of the 4 functions, this list is the special "matrix"
    list(set = set, get = get,
        setinverse = set_inverse,
        getinverse = get_inverse)

}


## Write a short comment describing this function

cacheSolve <- function(matrix, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- matrix$getinverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- matrix$get()
    inverse <- solve(data, ...)
    matrix$setinverse(inverse)
    inverse
}

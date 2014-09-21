## Creates a matrix, takes inverse and caches it if possible
## If inverse is called for again, pulls from cache rather than recalculating it

## inputs a matrix x and sets inverse variable inv to NULL
## creates three fns to get the value of inv, set the value of inv and get value of matrix
## also allows you to reset matrix to something new (resetting inverse inv)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  getinv <- function() inv
  setinv <- function(m_inv) inv <<- m_inv
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  list(getinv = getinv, setinv = setinv, getmatrix = getmatrix, setmatrix = setmatrix)
}


## uses Solve to get inverse of square matrix
## returns inverse to level above function (makes it visible to other fns) 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
	}
  m <- x$getmatrix()
  inv <- solve(m)
  x$setinv(inv)
  inv
}

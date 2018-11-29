## ProgrammingAssignmt2. Cache an inverse matrix
##first the makeCacheMatrix creates a matrix that will cache it's inverse
## Next the cacheSolve will compute the inverse of the matrix from above,
##If the inverse is already calculated then cacheSolve will return the cached inverse matrix

## This function creates a matrix that can chche it's inverse

makeCacheMatrix <- function(x = numeric()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  set_mat_inv <- function(solve) n <<- solve
  get_mat_inv <- function() m
  list(set = set, get = get,
       set_mat_inv = set_mat_inv,
       get_mat_inv = get_mat_inv)
}




## This function will compute the inverse of the matrix from above,
##If the inverse is already calculated then cacheSolve will return the cached inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  n <- x$get_mat_inv()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$set_mat_inv(n)
  n
  
}

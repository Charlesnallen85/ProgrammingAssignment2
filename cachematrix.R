## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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




## Write a short comment describing this function

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

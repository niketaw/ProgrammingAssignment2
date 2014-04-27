## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
  inv_mat<-NULL
  set <- function(y) {
    mat <<- y
    inv_mat <<- NULL
  }
  get <- function() mat
  setinv <- function(inv_mat_new) inv_mat <<- inv_mat_new
  getinv <- function() inv_mat
  list(set = set, get = get,
       setinv  = setinv ,
       getinv = getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getinv()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat<- solve(data, ...)
  x$setinv(inv_mat)
  inv_mat
}

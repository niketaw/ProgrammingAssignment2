##Take input a matrix and generate a list of four functions
makeCacheMatrix <- function(mat = matrix()) {     #creates a matrix object
  inv_mat<-NULL                     #Sets the initial value to NULL for inverse matrix
  set <- function(y) {              #Add new matrix to makeCacheMatrix list
    mat <<- y
    inv_mat <<- NULL
  }
  get <- function() mat             #Outputs the existing matrix
  setinv <- function(inv_mat_new) inv_mat <<- inv_mat_new #adds the inverse matrix to the list
  getinv <- function() inv_mat      #Outputs the inverse matrix of the input matrix
  list(set = set, get = get,        #Generates the list of four functions
       setinv  = setinv ,
       getinv = getinv)
  
}


##Get the inverse of the input matrix if in cache else solve the matrix, output the inverse and save it in cache

cacheSolve <- function(x, ...) {    # Return a matrix that is the inverse of 'x'
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

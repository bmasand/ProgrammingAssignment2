## The following is a pair of functions that stores and computes the 
## inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix" object, which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(x) {
    matrix_reg <<- x;         
    matrix_inverse <<- NULL;
  }
  get <- function(){ 
    return(matrix_reg)
  };
  setinv <- function(inv) {
    matrix_inverse <<- inv;
  }
  getinv <- function() {
    return(matrix_inverse);
  }
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## The function cacheSolve computes inverse of the matrix of the special matrix, matrix_reg created with the above function. 
## However, it first checks to see if the matrix_inverse has already been computed. 
## If so, it gets the matrix_inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the solve(x) function.

cacheSolve <- function(m_reg, ...) {
        ## Return a matrix that is the inverse of 'm_reg'
  m_inverse <- m_reg$getinv()
  if(!is.null(m_inverse)) {
    message("Getting cached data...")
    return(m_inverse)
  }
  ## If inverse does not exists.
  data <- m_reg$get()
  m_inverse <- solve(data, ...)
  m_reg$setinv(m_inverse)
  return(m_inverse)
}

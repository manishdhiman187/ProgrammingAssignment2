## This Function makeCacheMatrix gets a matrix as an input, set the value of the matrix get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object can cache its own object.
## <<- operator is used to assign a value to an object in an environment that is different from the current environment.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  
  }
  getMatrix <- function() x 
  setInverse <- function(inverse) invMatrix <<- inverse
  getInverse <- function() invMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
            setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("Getting Cached Invertible Matrix")
    return(invMatrix)
  }
  #if value of the invertible matrix is NULL then
  MatrixData <- x$getMatrix()
  invMatrix <- solve(MatrixData, ...)
  x$setInverse(invMatrix)
  return(invMatrix)
  ## Return a matrix that is the inverse of 'x'
  
}
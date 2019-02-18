## Put comments here that give an overall description of what your
## functions do
##This function Function makeCacheMatrix gets first a matrix as an input, 
##second to set the value of the matrix,
#3-get the value of the matrix and finally set the inverse Matrix 
##and get the inverse Matrix. The matrix object
#can cache its own object. 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  
  #set the value of the Matrix
  setMatrix <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }

  #get the value of the Matrix  
  getMatrix <- function() x  
  #set the value of the invertible matrix
  setInverse <- function(inverse) inverseMatrix <<- inverse  
  #get the value of the invertible matrix
  getInverse <- function() inverseMatrix                     
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  
  #get the value of the invertible matrix from the makeCacheMatrix function
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {                       #if inverse matrix is not NULL
    message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
    return(inverseMatrix)                             #return the invertible matrix
  }
  
  #if value of the invertible matrix is NULL then  
  #get the original Matrix Data 
  MatrixData <- x$getMatrix()  
  #use solve function to inverse the matrix
  inverseMatrix <- solve(MatrixData, ...)  
  #set the invertible matrix 
  x$setInverse(inverseMatrix) 
  #return the invertible matrix
  return(inverseMatrix)                               
  
}

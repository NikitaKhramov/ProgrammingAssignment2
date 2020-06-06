## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  
  #set the value of the Matrix
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  #get the value of the Matrix
  get <- function() x   
    
  #set the value of the invertible matrix
  setinverse <- function(inverse) i <<- inverse
    
  #get the value of the invertible matrix
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  #if inverse matrix is not NULL  
  if (!is.null(i)) {
          message("getting cached data") #Type message: Getting Cached Invertible Matrix 
          return(i) #return the invertible matrix
  }
  #if value of the invertible matrix is NULL then   
  data <- x$get() #get the original Matrix Data
  i <- solve(data, ...) #use solve function to inverse the matrix
  x$setinverse(i) #set the invertible matrix
  i  #return the invertible matrix
}

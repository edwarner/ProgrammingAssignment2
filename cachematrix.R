## Put comments here that give an overall description of what your
## functions do

## This file contains two functions makeCacheMatrix and cacheSolve
## makeCacheMatrix returns a special cacheable matrix from a standard r matrix
## cacheSolve returns the inverse of a cacheable matrix that was created using the makeCacheMatrix function



## Write a short comment describing this function

## This function returns creates a special "matrix", which is a actually a list containing four functions that
## 1. Sets the value of the matrix
## 2. Gets the value of the matrix
## 3. Sets the value of the inverse matrix
## 4. Gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the default matrix inverse value to NULL
  m_inverse <- NULL
  
  ## define the set function that sets the matrix value
  set <- function(y) {
    x <<- y
    m_inverse <<- NULL
  }
  
  ## define the get function that gets the matrix value
  get <- function() x
  
  ## define the setinverse function that sets the inverse matrix to a predefined value
  setinverse <- function(inverse) m_inverse <<- inverse
  
  ## definte the getinverse function that gets the inverse matrix
  getinverse <- function() m_inverse
  
  ## returns the list with the four functions that were defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## This function returns the inverse of a matrix x
## It first checks to see if a cached version of the inverse exists
## If the cached version exists, that is returned.
## Others, the inverse matrix is calculated, stored in the cache for future retrieval, then returned

cacheSolve <- function(x, ...)
  {
    ## Return a matrix that is the inverse of 'x'
  
  
    ## try to get the cached inverse matrix
    inverse <- x$getinverse()
    
    if(!is.null(inverse))
    {
      ## cached inverse matrix exists
      
      message("getting cached data")
      
      ## return the cached inverse matrix
      return(inverse)
    }
    
    ## cached inverse matrix does not exit
    data <- x$get()
    
    ## calculate the inverse matrix
    inverse <- solve(data, ...)
    
    ## store the inverse matrix
    x$setinverse(inverse)
    
    ## return the inverse matrix
    inverse
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#function to create matrix
# this function also caches the inverse of the matrix
# function is very similar to the example in the assignment

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                             # initialize variable to NULL. Will hold the inverse value
  set <- function(z) {                    # assign new value to the matrix through this set function 
    x <<- z                            
    inv <<- NULL                        # once new matrix is created, reset the variable to NULL
  }
  get <- function() x                     # define the get function - returns value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse  # set the inv value
  getinverse <- function() inv                     # gets the inv value
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  # value needs to be set to call in the cachesolve function
}


## Write a short comment describing this function
# this function computes inverse from the above function
# if the inverse already exists then it retrieves from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("retrieving cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

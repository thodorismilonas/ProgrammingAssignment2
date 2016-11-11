## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  
                            ## This function creates a special "matrix" object that can cache its inverse
  
  inv <- NULL               ## inv is set to NULL, initializing it as an object within the makeCacheMatrix() environment to be used by later code in the function
  
  set <- function(y) {      ## define the set function to assign 
    x <<- y                 ## the input argument to the x object in the parent environment,
  inv <<- NULL              ## and the value of NULL to the inv object in the parent environment.
  }   
  get <- function() x       ## this function defines the getter for the matrix x
  
  setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinverse <- function() inv                     ## gets the value of inv where called
  
  list(set = set,                   ## gives the name 'set' to the set() function defined above               
       get = get,                   ## gives the name 'get' to the get() function defined above    
       setinverse = setinverse,     ## gives the name 'setinverse' to the setinverse() function defined above
       getinverse = getinverse)     ## gives the name 'getinverse' to the getinverse() function defined above
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()       
    if(!is.null(inv)) {                 ## checking if inv is already calculated
      message("getting cached data")    ## printing message if the logical statement is true
      return(inv)                       ## and then returns the data
    }
  data <- x$get()                       ## Otherwise, it calculates the inverse of the data
  inv <- solve(data, ...)               ## and sets the value of the invserse in the cache
  x$setinverse(inv)                     ## via the setinverse function.
  inv
}

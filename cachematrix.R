## Matrix inversion is usually a costly computation and their may be
## some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here). 

# To test this, use the following test case. Credits to Gregory D. Horne 
# at the forums.
# Taken from https://class.coursera.org/rprog-003/forum/thread?thread_id=650#post-2811

# amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# amatrix$get()         # Returns original matrix
# cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
# amatrix$getsolve()  # Returns matrix inverse
# cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse

# amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
# cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
# amatrix$get()         # Returns matrix
# amatrix$getsolve()  # Returns matrix inverse

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
# This method makes use of the special <<- assignment operator.
# This operator searches through the parent calling environment to find an 
# unlocked existing definition it can alter. If there is no previous definition,
# it creates a definition in the global environment.
  
  #It is important to note that this makeCacheMatrix function creates a list
  #of functions. This behaves in a very similar way to how object oriented
  #programming works. The list of functions it creates work as the 
  #methods for an object in OOP.
  
  # First we declare the "s" variable.
  # This variable will hold the inverse from matrix "x", which is passed
  # in as an argument
  s <- NULL
  
  # Now the next step is where the caching magic is performed.
  # The result of the makeCacheMatrix() function is a list of four functions:
    # - set: stores the original uninverted matrix and sets the inverted matrix
    #        's' to null.
    # - get: Returns the stored ("cached") matrix
    # - setsolve: Sets the solution for the inverted matrix
    # - getsolve: Gets the solution for the inverted matrix
  
  set <- function(y) {
    # This "nested"function "caches" the matrix passed into to the makeCacheMatrix
    # function. It uses the "<<-" operator instead of the "<-"operator because
    # the caller environment calls makeCacheMatrix$set(matrix), it gets bound to the
    # this code will try to find "x" in the parent environment (the makeCacheMatrix
    # function in this case) and bind "y" to "x" in that environment. This makes
    # "x"have the value of the parameter "y" in all other "subfunctions" of the
    # "makeCacheMAtrix" function.
    x <<- y
    # Same thing applies to "s". if we didn't use the "<<-" operator, it would be a 
    # local variable, and would not be bound with the "s" variable declared
    # in the makeCacheMAtrix function, which is the parent environment.
    # Also "s" needs to be set to NULL because we are changing the matrix
    # for which we cached its inverted form, so we need to invalidade the
    # cached information.
    s <<- NULL
  }
  # The "get" function is more straightforward. When "y" was bound to "x" in the
  # "set" method, it created that entry in the parent environment of all these
  # functions, so by returning x we are returning that about variable from the
  # parent environment.
  get <- function() x
  # "setsolve" is similar to "set" in the sense that we also need to use the
  # "<<-" operator so the value "solve" gets bound to the variable "s" in the 
  # parent environment. This way when we call "getsolve" it will look for "s"
  # in the parent environment and find it and be able to return this cached 
  # value
  setsolve <- function(solve) s <<- solve
  # "getsolve" is once again similar to "get". It returns "s" from the parent 
  # environment. This value is bound in the parent environment by the 
  # "setsolve" function using the "<<-" operator.
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Here we see  makeCacheMatrix in action. This method receives an instance
  # of makeCacheMAtrix as a parameter.
  
  # The first step is to check if this matrix already has a cached value.
  # For this we call the getsolve() function. The first time this is called
  # "s" will have a NULL value, since when the matrix is created (using 
  # "makeCacheMatrix(<A_REGULAR_MATRIX>) it is set to null.
  s <- x$getsolve()
  # Once we get "s" we need to check if the value was cached. This is 
  # achieved with the ïs.null function:
  if(!is.null(s)) {
    message("getting cached data")
    # If there was a value there, we assume it was the cached inverted matrix,
    # and return it.
    return(s)
  }
  # Otherwise We need to invert the matrix. For this we need to first obtain
  # the matrix instance from the makeCacheMatrix "object" using the
  # get method.
  data <- x$get()
  # Then we calculate the inverted matrix using the solve function.
  s <- solve(data, ...)
  # And we set the inverted matrix to the makeCacheMatrix "object"
  # This will cache the value as described in the makeCacheMatrix comments
  x$setsolve(s)
  # And finally, we return the inverted matrix
  s
}

testMatrix <- function(){
  message("Create the makeCacheMatrix instance/environment.\n\n")
  flush.console()
  amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
  g<-amatrix$get()         # Returns original matrix

  message("Calling cacheSolve for the first time.\n Shouldn't get a cached value.\n\n")
  flush.console()
  c<-cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
  
  message("Calling cacheSolve for the second time.\n Gets the cached value. NEXT LINE SHOULD READ \"getting cached data\"")
  flush.console()
  c<-cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
  
  message("\nModify existing matrix. Should clear \"s\" and set the new matrix to \"x\"\n\n")
  flush.console()
  amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix

  message("Calling cacheSolve for the first time with the new matrix.\n Shouldn't get a cached value.\n\n")
  flush.console()
  c<-cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse

  message("Calling cacheSolve for the second time with the new matrix.\n Gets the cached value. NEXT LINE SHOULD READ \"getting cached data\"")
  flush.console()
  c<-cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
  
}
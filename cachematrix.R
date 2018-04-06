##Two function system that will avoid recalculating the inverse
##of a matrix if it has been calculated already.
##To make it work: Create a square matrix and assign it to a 
##variable, for example "a". Then pass "a" to makeCacheMatrix
##and assign to a variable, for example 'b'.
##Finaly, pass 'b' to cacheSolve.
##The first time it will calculate the inverse of the matrix.
##If you pass again the same 'b', it will not calculate, 
##but will return the saved value and the message "getting cached
##data".


##This functino will store the result of the inverse matrix
##it will provide it to cacheSolve if the matrix hasn't changed

makeCacheMatrix <- function(x = numeric()) {
  
  #initiallize m
  m <- NULL
  
  #Initialize x and m in the global environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Get the matrix to be processed
  get <- function() x
  
  #assigned the solved matrix
  setsolve <- function(solve) m <<- solve
  
  #get the solved matrix 
  getsolve <- function() m
  
  #create a list with the functions and name them to recall with $ operator
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##cacheSolve will check if makeCacheMatrix has the correct inverse
##if it does, it will return it. Otherwise, it will calculate it.

cacheSolve <- function(x, ...) {
  
  #assign the solved matrix to m
  m <- x$getsolve()
  
  #if m is not NULL, then returned the chached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if m was NULL, get the matrix and assigned it to data, 
  ##solve it and assign it to m
  ##set m in the environment through makeCacheMatrix
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
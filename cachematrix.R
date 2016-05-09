## This set of functions accomplishes the overall goals of the 
## "Programming in R" R programming assignment 2.
## Namely, it takes a matrix argument into the makeCacheMatrix
## function.  It can then store an inverse of the matrix into
## a cached value using the cacheSolve function

## This function takes a matrix as an argument and then
## provides four options to the user(set, get, setInv, getInv)
## set() can be used to change the matrix value
## get() provides the matrix value to the user
## setInv() is used to set the global enviroment variable m2
## with the inverse value as determined by the cacheSolve function
## getInv() returns the global envirnment variable m2
makeCacheMatrix <- function(x = matrix()) {
  m2 <- NULL
  set <- function(y) {
    x <<- y
    m2 <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m2 <<- inv
  getInv <- function() m2
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function performs two tasks.  It first calls the inline function getInv()
## from the function makeCacheMatrix(x).  It first checks to see
## if the global environament variable m2 is not null.  If m2 is not null,
## the function simply returns the value of m2.
## If m2 is null, the function gets the value of the matrix from the
## makeCacheMatrix(x) function using the get() inline function.
## It then uses the solve(x) function in R to generate the inverse
## of the matrix and sends it to the inline function setInv(x) in the 
## the function makeCacheMatrix(x) function.  The setInv function then
## updates the m2 global variable as described above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m2 <- x$getInv()
  if(!is.null(m2)) {
    message("getting cached data")
    return(m2)
  }
  data <- x$get()
  m2 <- solve(data, ...)
  x$setInv(m2)
  m2    
}

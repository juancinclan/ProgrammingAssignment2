## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The following function "makeCacheMatrix" generates a mini Global Environment 
## without being in the general Global Environment because sometimes there is an optimization problem
## due to lack of infinite memory in your computer. In order to solve this problem you can optimize this by
## creating a function that its purpose is to be a Global Environment
## In order to call the variables in this function I used the "<<-" and instead of assigning the object to 
## the variable in the global environment it assigns it to the variable declared in the function (MiniGlobal Environment)
## The purpose of "makeCacheMatrix" is to preserve the matrix that I want to inverse and to add a new matrix to inverse 
## and replace the last matrix with "set". Get shows me what matrix I inputed.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}



## Write a short comment describing this function

##The "cacheSolve" function is a functions that uses de variables declared in the last function (makeCacheMatrix)
## and it is used to get the inverse matrix from the previous inputed matrix in "makeCacheMatrix". It uses the 
## function "solve()" to get the matrix inversed. This function has a downside that only inverses a 2x2 matrix
## In order to inverse greater matrices is imperant to change for a matrix specialized package function

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
mat<-makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
#mat$set(matrix(1:4,nrow=2,ncol=2))
mat$getSolve()
cacheSolve(mat)

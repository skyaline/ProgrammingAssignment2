## Programming Assignment 2: solving the inverse of a matrix by caching the result
## within a lexical scope of a function "makeCacheMatrix" and 
# "cacheSolve".

##makeCacheMatrix crates a special "matrix" object that can caches its inverse.

##makeCacheMatrix which is a list containing a function to:
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get <-function()x
  setinverse <- function(inverse)inv<<-inverse
  getinverse <- function()inv
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.The cacheSolve sould retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cache data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}

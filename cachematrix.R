## The following two functions are used to cache the inverse of a matrix.

## The function "makeCacheMatrix" creates and returns a
## special matrix object that can cache its inverse.
##
##  The makeCacheMatrix function creates a list containing a function to:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of inverse of the matrix
## 4. gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


       
## The following function returns a matrix that is the inverse of the 
## matrix x. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

x=rbind(c(1,-1/4),c(-1/4,1))
m=makeCacheMatrix(x)
m$get()
cacheSolve(m)
cacheSolve(m)




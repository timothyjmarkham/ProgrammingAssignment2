## This program creates two functions.  The first creates a "matrix"
## object that can cache its inverse.  The second function computes
## the inverse of the "matrix" or returns the cached value if it has
## already been calculated.


## Create a "matrix" that is a list containing functions to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculate inverse of matrix or return cached inverse if already created.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}

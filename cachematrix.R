## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		inv_m <- NULL
		set <- function (y) {
			x <<- y
			inv_m <<- NULL
		}
		get <- function() x
		setInverse <- function(inverse) inv_m <<- inverse
		getInverse <- function() inv_m
		list(set = set, get = get, 
			setInverse = setInverse,
			getInverse = getInverse)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## return a matrix that is the inverse of 'x'
       inv_m <- x$getInverse()
	 if (!is.null(inv_m)) {
			message ("getting cached data")
			return(inv_m)
	 }
	 data <- x$get()
	 inv_m <- solve(data, ...)
	 x$setInverse(inv_m)
	 inv_m
}


## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

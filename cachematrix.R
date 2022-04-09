

## This makeCacheMatrix function creates a special matrix which is a list that can set the values
## of the matrix, get the values of the matrix, set the inverse of the matrix and 
## get the inverse of the matrix. these functions create special matrix objects that can
## cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL

	## set and get functions for the matrix

	set <- function(y) {
		x <<- y
		
		## m becomes special matrix object that can cache/store the inverse
		m <<- NULL
	}
	get <- function()x

	## set/get for the inverse matrix
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	
	## will return list of functions for matrix
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}




## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
      
	m <- x$getinverse()
	## if the inverse was calculated already and matrix not changed, then this should retrieve
	## the inverse from the cache and return that.
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	## if the matrix was changed, then it retrives the data/matrix and uses the solve function to
	## return the inverse, if a square matrix.
	
	## compute inverse of matrix
	data <- x$get()
	m <- solve(data, ...)

	## cache the inverse 
	x$setinverse(m)

	## returns inverse matrix
	return(m)
}



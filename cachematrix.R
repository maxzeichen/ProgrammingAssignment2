## Writung functions to get the inverse of a matrix
## functions do - cache the inverse of a matrix
## 

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inve <- NULL
	set <- function(z)
	{x <<- z
	inve <<- NULL}
	get <- function()x
	setinverse <- function(inverse) inve <<- inverse
	getinverse <- function() inve
	list(	set = set,
			get = get,
			setinverse = setinverse,
			getinverse = getinverse)}


## This function computes the inverse of the special matrix returned by makeCacheMatrix from above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inve <- x$getinverse()
        if (!is.null(inve)) {message("get cached matrix")
        	return(inve)
        	}
        matrix <- x$get()
        inve <- solve(matrix, ...)
        x$setinverse(inve)
        inve}


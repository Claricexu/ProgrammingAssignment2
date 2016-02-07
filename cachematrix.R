## Caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
			x <<- y
			m <<- NULL
		}
		get <- function() x
		setmatrixinverse <- function(solve) m <<- solve
		getmatrixinverse <- function() m
		list(set = set, get = get, 
			setmatrixinverse = setmatrixinverse,
			getmatrixinverse = getmatrixinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        	m <- x$getmatrixinverse()
		if(!is.null(m)) {
			message("getting cached data")
			return(m)
		}
		matrix <- x$get()
		m <- solve(matrix, ...)
		x$setmatrixinverse(m)
		m
}

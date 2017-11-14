##  These functions provide caching procedure for calculations of
## inverse square matrix


## makeCacheMatrix: This function creates a matrix whose inverse can be cached  
## can cache its inverse.

makeCacheMatrix <- function( mat = matrix() )
{  
	facti <- NULL
	
	set <- function( matrix )
	{
		mat <<- matrix
		facti <<- NULL
	}
	
	get <- function()
	{
		mat
	}
	
	setInverse <- function(inverse)
	{
		facti <<- inverse
	}
	
	getInverse <- function()
	{
		facti
	}
	
	list(set = set, get = get
	, setInverse = setInverse
	, getInverse = getInverse)
}


## This function returns the inverse of matrix returned by makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will use cache to returns the inverse.

cacheSolve <- function(x, ...)
{  
	mat <- x$getInverse()
	
	if( !is.null(mat) )
	{
		message("getting cached data")
		return(mat)
	}
	
	data <- x$get()
	
	mat <- solve(data)
	
	x$setInverse(mat)
	
	mat
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

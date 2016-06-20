## These functions create a special matrix object and computes the 
## inverse of the special matrix

## Create a special matrix object

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get = function() x
	setinv = function(inverse) inv <<- inverse
	getinv = function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Compute the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        
        if (!is.null(inv)) {
        	message("get cached data")
        	return(inv)
        }
        
        mat.data = x$get
        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)
}

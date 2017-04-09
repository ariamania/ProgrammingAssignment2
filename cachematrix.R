## This function creates a special "matrix" object that can
## cashe its inverse

makeCacheMatrix <- function(x = matrix()) {
	iv <- NULL
	set <- function(y){
		x <<- y
		iv <<- NULL 
	}
	get <- function() x 
	setinverse <- function(solve) iv <<- solve
	getinverse <- function() iv 
	list(
		set = set, get = get, 
	     	setinverse = setinverse,
	     	getinverse = getinverse
		)
}


## This function computes the inverse of the special "matrix" 
## reurned by the makeCasheMatrix function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	iv <- x$getinverse()
	if(!is.null(iv)) {
		message("getting cashed data")
		return(iv)
	}
	data <- x$get()
	iv <- solve(data, ...)
	x$setinverse(iv)
	iv
}

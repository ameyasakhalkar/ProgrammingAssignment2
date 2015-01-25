## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(mat)  {
		x <<- mat
		inv <<- NULL
	}
	get <- function() x
	getInv <- function() inv
	setInv <- function(invMat) {
		inv <- invMat
	}
	list(set = set, get = get, getInv = getInv, setInv = setInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInv()
	orig <- x$get()
	if (!is.null(inv) && identical(x, orig)) {
		return(inv)
	}
	inv <- solve(x)
	x$setInv(inv)
	inv
}

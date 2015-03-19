## The two functions 'makeCacheMatrix' and 'cacheSolve' combined together return the inverse of a matrix
## (assumed to be invertible). 
## The peculiarity is that they cache the inverse of the matrix after it is computed,
## and just access it in case it is requested again.
## This results in a valuable reduction in the computational costs. 

## 'makeCacheMatrix' gets a matrix as an input, and returns a "special matrix object" containing the matrix 
## itself and eventually its inverse.

makeCacheMatrix <- function (x=matrix()) {
		I <- NULL
		set <- function(y) {
			x <<- y
			I <<- NULL			
		}
		get <- function() x
		setinv <- function(solve) I <<- solve
		getinv <- function() I
		list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## 'cacheSolve' gets the list defined by 'makeCacheMatrix' and returns a matrix that is the inverse of 'x'.
## If the inverse was already computed, it just accesses it
## Otherwise, it compute it and updates its value in the corresponding part in the list.

cacheSolve <- function(x,...){
		I <- x$getinv()
		if(!is.null(I)){
			message("Found the inverse. Getting the cached data")
			return(I)
		}
		data <- x$get()
		I <- solve(data,...)
		x$setinv(I)
		I
}

## Put comments here that give an overall description of what your functions do
## Given a matrix this function will return the inverse of matrix!
## If the Inverse of given matrix already exists, it returns the cached result
## Otherwise it calculates the inverse and saves it in cache

## Write a short comment describing this function
## This function is returning a list containing four functions on given matrix 
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of inverse matrix 
## 4. get the value of inverse matrix 
makeCacheMatrix <- function(x = matrix()) {
   Inv=NULL
   set <- function(y){
	   x <<- y
	   Inv <<- NULL
   }
   get <- function() x
   setSolve <- function(solve) Inv <<- solve
   getSolve <- function() Inv
   list(set = set, get = get,
		setSolve = setSolve, 
		getSolve = getSolve)
}


## Write a short comment describing this function
## Given an object of type makeCacheMatrix, this function calculates
## the inverse of matrix if none already chached. 
## otherwise it calculates the inverse and caches it. 
cacheSolve <- function(x, ...) {
    Inv <- x$getSolve()
	if(!is.null(Inv)) {
		message("getting cached data")
		return(Inv)
	}
	data <- x$get()
	Inv <- solve(data, ...)
	x$setSolve(Inv)

    ## Return a matrix that is the inverse of 'x'
	Inv 
}
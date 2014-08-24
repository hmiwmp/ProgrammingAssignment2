## These two functions allow the creation of an inverse of a square matrix 
## that can then be saved to cache and remain available for any future calls for this inverse

## The function makeCacheMatrix creates a new object (a list) that contains the 
## functions that can then be called by other functions (such as cacheSolve below)


makeCacheMatrix <- function(orig = matrix()) { 	#creates a new object
  inverse <- NULL
  set <- function(y) {				#defines ‘set’ function
  orig <<- y
  inverse <<- NULL
}

get <- function() { orig }			#defines ‘get’ function
setmatrix <- function(solve) {			#defines ‘setmatrix’ function
	inverse <<- solve(orig)
}
getmatrix <- function() { inverse }		#defines ‘getmatrix’ function

list(set = set, get = get,			#assigns names to list elements
   setmatrix = setmatrix,
   getmatrix = getmatrix)
}


## This function (cacheSolve) checks whether or not there is a cached inverse in existence
## If so it retrieves it.  If not it calculates the inverse matrix and then caches result

cacheSolve <- function(x = matrix(), ...) {
    inverse <- x$getmatrix()			#retrieves result from store
    if(!is.null(inverse)) {
      message("getting cached data")		#exists so retrieve it
      return(inverse)
    }
    matrix <- x$get()				#nothing stored so compute
    inverse <- solve(matrix, ...)
    x$setmatrix(inverse)
    inverse
}


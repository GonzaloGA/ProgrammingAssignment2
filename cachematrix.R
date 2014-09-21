## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	## initializes inv to NULL
	inv <- NULL

        ## 'set'  makes x = argument of 'set', and inv = NULL
	set <- function(y) {
                x <<- y
                inv <<- NULL
        }

	## 'get' returns the argument of makeCacheMatrix
        get <- function() x

        ## makes inv = inverse (argument of makeCacheMatrix)
	setinverse <- function(solve) inv <<- solve
        
	## 'getinverse' returns the value of inv 
	getinverse <- function() inv

        ## returns a labeled vector of functions set, get, setinverse and getinverse
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	## takes the value of inv from within makeCacheMatrix
	inv <- x$getinverse()
        
	## if inv <> NULL, returns its cached value
	if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

	## otherwise get the 'special' matrix 
        data <- x$get()
        
	## calculate the inverse
	inv <- solve(data, ...)

	## cache the result
        x$setinverse(inv)

	## and return the result
        inv
}

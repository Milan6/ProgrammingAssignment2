## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Function create a matrix where the inverse is getting cached

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# Function returns the inverse of a matrix if it already exists, otherwise it is asked whether it is possible to compute the inverse by det(x$get())!=0 (the determinant of the matrix should be unequal to zero). If the statement returns TRUE the inverse is computed an cached otherwise the message "matrix not invertible" is printed out

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
	else if(det(x$get())!=0) {
		data <- x$get()
        	i <- solve(data, ...)
        	x$setinverse(i)
        	return(i)
	}
	else {
		message("matrix not invertible")
	}
}

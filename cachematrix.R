## This function is designed to cache the inverse of a matrix 
## and avoid repeatedly computation.

## This makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The cacheSolve function calculates the inverse of the
## special matrix created with the above function.
## However, it will first examine whether the matrix has been inversed. 
## If so, it would cache the data and skip computation.
## If not, it would get the matrix inversed and set the value
## in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}

## Put comments here that give an overall description of what your
## functions do

## This function returns a list of functions that:
## 1. Sets the value of the matrix
## 2. Gets the value of the matrix
## 3. Sets the value of the inverse of the matrix
## 4. Gets the value of the inverse of the matrix

##Note that we assume the matrix is square

makeCacheMatrix <- function(x = matrix()) {
    i <-NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <-function() x
    setinverse <- function(inverse) i <<-inverse
    getinverse <- function() i
    list(set=set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This functions takes a CacheMatrix object (x) created by the function makeCacheMatrix and returns the inverse of the invertible matrix 
## stored in x. If that inverse has already been stored in x, then the function prints a message that it is "getting cached data" and 
## returns the stored value.  If the inverse has not already been stored, then the function computes the inverse, stores
## the value of the inverse into x, and returns the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <-x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setinverse(i)
    i

}

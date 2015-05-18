## Usage: cacheSolve(makeCacheMatrix(m)) 
## m must be a square invertible matrix
## function will return the inverse of m

## Gets, and sets, the value of a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    #Set i to NULL just in case
    i <- NULL
    #Initialize variables
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    #Get original matrix
    get <- function() x
    #Set the inverse
    setinverse <- function(inverse) i <<- inverse
    #Get the inverse of original matrix
    getinverse <- function() i
    #List of member functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Checks to see if inverse exists, and either calculates the value
## or returns the cached value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    #If the cached version exists, return it
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    #Cached version doesn't exist...calculate inverse
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

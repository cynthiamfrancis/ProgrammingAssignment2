## Set of functions that cache the inverse of a matrix
## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {

	## Initialize = Property of Inverse
    i <- NULL

    ## Set matrix method
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ##  Get matrix method
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Matrix Method to set the inverse for the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }



 cachematrix.R

    ## Matrix Method to get the inverse for the matrix
    getInverse <- function() {
        ## Return inverse property
        i
    }

    ## Return list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Compute the inverse of the  matrix returned by "makeCacheMatrix" above. 
## In the event inverse has already been calculated (and the matrix has not changed),
## then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Matrix return that is the inverse of 'x'
    m <- x$getInverse()

    ## Return only the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from the object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}

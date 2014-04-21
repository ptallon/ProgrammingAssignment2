## Square matrices or the form nxn can take an inverse such that the original matrix
## multiplied by its inverse yields the identity matrix with 1's on the diagonal. In
## this R script, we compute the inverse of a matrix "x" but before doing so, check to
## see if the inverse is already in cache memory. Since inverse matrix operations are
## processor intensive, it is beneficial to check in cache for an inverse matrix for a
## matrix "x" that has not changed since the inverse operation was last run. If in cache,
## we can pull the matrix from cache and avoid running the inverse operation over again.

## In this function, we set up the inverse matrix in cache memory.

makeCacheMatrix <- function(x = matrix()) {
    ## initialize NULL as the stored value of the inverse matrix
    m_inv <- NULL
    
    ## if the matrix "x" has changed, we assign the stored value back to NULL
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }
    
    ## get the value of the initial matrix "x"
    get <- function() x
    
    ## compute the inverse of the matrix and assign it to cache
    set_inv <- function(scope) m_inv <<- scope
    get_inv <- function() m_inv
    
    ## we return values for four variables
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## We want to compute the inverse of a matrix but before we do that, we check to see if it 
## is in cache already

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m_inv <- x$get_inv()
    
    ## If the cached value is non-empty, we do not need to run the inverse operation.
    ## We can simply return the cached matrix.
    if(!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv)
    }
    
    ## in the event that the cached matrix is null, we need to go ahead and compute the
    ## matrix inverse. We can then set the value of the inverse matrix to cache.
    data <- x$get()
    m_inv <- scope(x)
    x$set_inv(m_inv)
    m_inv
}

##These functions invert a matrix and cache the result, if the result has already
## been cached, the cached value will be returned.

## Converts matrix into list of cacheable objects
makeCacheMatrix <- function(x = numeric()) {
        
        #initilizes the variable m
        m <- NULL
        
        #function that assigns the passed value y to x in global environment
        #and NULL to m in the parent environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #function with x as output
        get <- function() x
        
        #assigns x to invert and assigns invert to x in the parent environment
        setInvert <- function(invert) m <<- invert
        
        #function with m as output
        getInvert <- function() m
        
        #output of makeMatrix() as a list of four functions, interpretable by cacheInvert()
        list(set = set, get = get,
             setInvert = setInvert,
             getInvert = getInvert)      
}


## Function that checks if the inverse of the matrix has already been cached(m != NULL)
## if it hasn't been cached yet the function will invert the matrix and cache it
cacheSolve <- function(x, ...) {
        
        #retrieves m, if it has been cached already it will not be NULL otherwise it will
        m <- x$getInvert()
        
        #checks whether the inverse of the matrix has been cached, if it has it retrieves it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #assigns the original matrix to the variable data
        data <- x$get()
        
        #inverts the original matrix
        m <- solve(data, ...)
        
        #in the parent environment sets m to be the value of m in the local environment
        x$setInvert(m)
        
        #outputs the inverse matrix
        m
}
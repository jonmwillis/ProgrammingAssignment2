## makeCacheMatrix is a function that returns a list of functions
## It stores a matrix and a cached value of the inverse of the 
## matrix, using these functions:
## * setMatrix      set the value of a matrix
## * getMatrix      get the value of a matrix
## * cacheInverse   get the cahced value (inverse of the matrix)
## * getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = numeric()) {
## start off with nothing cached        
        cache<-NULL
        
        ## store a matrix        
        setmatrix<-function(m) {
                x<<- m
                ## flush the cache, since the matrix has a new value
                cache<-NULL
        }
        ## return the matrix
        getmatrix<-function() {
                x
        }
        ## cache the inverse
        cacheInverse <- function(solve) {
                cache <<- solve
        }
        ## get the inverse from the cache
        getInverse <- function() {
                cache
        }
        ## return a list of named functions
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

## cacheSolve is a function that returns the inverse of a matrix (x)
## It retrieves the cached value of the inverse of the 
## matrix, unless the stored value is null, in which case it calculates
## the inverse of the matrix 

cacheSolve <- function(y, ...) {
        ## get the cached value
        inverse<-y$getInverse()
        ## if there is a value, return it
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        ## else get the cahced matrix, calculate its inverse, 
        ## and store it in the cache
        z<-y$getMatrix()
        inverse<-solve(z, ...)
        y$cacheMatrix(inverse)
        ## return the inverse
        inverse
}

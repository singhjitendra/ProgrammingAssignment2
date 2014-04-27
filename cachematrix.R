## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by 
##                makeCacheMatrix above. If the inverse has already been calculated (and the 
##                matrix has not changed), then the cachesolve should retrieve the inverse from 
##                the cache.


##  makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of matrix
## 4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        ## 1. set the value of the matrix
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## 2. get the value of the matrix
        get <- function() x
        
        ## 3. set the value of the inverse of matrix
        setinverse <- function(solve) inverse <<- solve
        
        ## 4. get the value of the inverse of matrix
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Gets the iverse of the matrix from cache.
        inverse <- x$getinverse()
        
        ## Return the inverse if a non-null cached value is returned.
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        data <- x$get()
        
        ## Calculate teh inverse of tha matrix.
        inverse <- solve(data, ...)
        
        # Set the inversed matrix value in cache
        x$setinverse(inverse)
        
        ## Return a matrix that is the inverse of 'x'
        inverse
}

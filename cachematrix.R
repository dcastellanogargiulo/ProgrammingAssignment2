#This function creates a special "matrix" object that can cache its inverse.
#declare function, assign default value to the x argument
makeCacheMatrix <- function(x = matrix()) {
        
        #declare variable inverse
        inverse <- NULL
        
        #declare function (method) set to cache value
        set <- function(y) { 
                x <<- y  #move value y to cache x
        }
        
        #declare get function (method) to return x value
        get <- function() x
        
        #declare function (method) to set inverse
        setinverse <- function(i) inverse <<- i
        
        #declare function (method) to get inverse
        getinverse <- function() inverse

        #checks the class consistency lists
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#declare function cacheSolve and assigns arguments x and forward arguments to the internal function solve.
cacheSolve <- function(x, ...) {
        
        #set inverse variable
        inverse <- x$getinverse()
        
        #verifies if the inverse is already cached.
        #if so it breaks the execution of the function providing the cached value. 
        if(!is.null(inverse)) {
        	message("getting cached inverse")
        	return(inverse)
        }

        #inverse computation block
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

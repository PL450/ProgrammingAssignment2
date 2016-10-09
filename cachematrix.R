## The 2 functions below will create a special object that stores a matrix and cache's its inverse.


## makeCacheMatrix creates a special vector that is a list containing 4 functions:
## 1) set the value of a matrix
## 2) get the value of a matrix
## 3) set the value of the inverse of the matrix
## 4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {         
        m <- NULL         
        set <- function(y) {
                x <<- y                 
                m <<- NULL        
        }         
        get <- function() x         
        setinverse <- function(inverse) m <<- inverse         
        getinverse <- function() m         
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## cacheSolve checks if the inverse of a matrix has been calculated. If yes, it gets the inverse from the cache.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()        
        if(!is.null(m)) {                 
           message("getting cached data")                 
           return(m)        
        }         
        data <- x$get()         
        m <- solve(data, ...)         
        x$setinverse(m)         
        m 
}

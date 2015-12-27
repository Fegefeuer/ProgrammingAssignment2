
## This function creates several functions and returns them in a list. 
## The functions store a matrix, set the value of a matrix, set the value
## of the inverse of a matrix, and allow you to retrieve the matrix passed in
## as well as the value of the inverse of that matrix. 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL  
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) s <<- inverse
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of the matrix passed in if it is already cached.  
## If not, it inverses the matrix and then caches it. 

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}

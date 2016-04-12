
# This function will do the inverse of a matrix



#This function will make a matrix object

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function()x
    setinverse <- function(solve)m <<- solve
    getinverse <- function()m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


# This function will calculate the inverse of the matrix. However, it will first
# see whether the inverse has been in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
            message('getting cached data')
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

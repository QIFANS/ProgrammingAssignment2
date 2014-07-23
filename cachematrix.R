## Caching the Inverse of a Matrix
## This script implements the methodes for storing,reading and updating
## matrix and its inverse

## This function creates a special "matrix" object(list) that can cache its inverse
## with the methodes to obtain them

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set <- function(y) {
                if(!is.matrix(y)){
                        y<-as.matrix(y)
                }
                x <<- y
                inv <<- solve(y)
        }
        get <- function() x
        setinv <- function(inv) inv <<- inv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above,
## and updates the result of the inverse in the special "matrix"
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        inv<- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse matrice")
                return(inv)
        }
        m <- x$get()
        inv <- solve(m)
        x$setinv(inv)
        inv
}

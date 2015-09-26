## makeCacheMatrix function takes a matrix as an argument and returns a list of function which gives information about current state of local matrix


### when matrix is passed to makeCacheMatrix it sets the local matrix as the new matrix passed
### We can retreive the local matrix using get method
### setinv receives an argument which is a matrix and sets the it as the inverse matrix for the local matrix
### getinv returns the inverse matrix for the local matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv=matrix()) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## cachesolve function uses makeCacheMatrix function and evaluates the inverse matrix of the local matrix
### If already the inverse matrix is evaluates, it returns the cached inverse matrix
### If inverse matrix for local matrix is not set, it uses solv(x, ...) function to evaluat inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("Getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
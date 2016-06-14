## The following functions are used in tandom to create a matrix along with calculating and
## caching its inverse (a time consuming operation).
##
## Example usage
## a<-makeCacheMatrix()
## a$set(matrix(1:4,2,2))
## cacheSolve(a)

## makeCacheMatrix constructs an empty matrix and stores it in a variable and contains methods to get / set a matrix
## to that variable. It also initializes a variable that can store the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(solve) inverseMatrix <<- solve
    getInverseMatrix <- function() inverseMatrix
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## cacheSolve takes in a matrix as an argument and returns the cached inverse if it exists,
## otherwise it calculates the inverse, stores it in cache and then returns it.

cacheSolve <- function(x =, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inverseMatrix <- x$getInverseMatrix()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setInverseMatrix(inverseMatrix)
    inverseMatrix
}

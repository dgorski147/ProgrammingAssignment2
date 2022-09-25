# R programming assigment 2

### Functions to cache inverse of matrix

#### function makeCacheMatrix


makeCacheMatrix <- function(x = matrix()) {

    # Initialization
    j <- NULL
    
    # setting the matrix
    set <- function(matrix) {
        m<<- matrix
        j<<- NULL
    }
    
    # get the matrix
    get <- function(){
        m
    }
    
    # set inverse of matrix
    
    setInverse <- function(inverse) {
        j <<- inverse
    }
    
    # get inverse of matrix
    getInverse <- function(){
        j
    }
    
    # return list of methods
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## computes the inverse of matrix returned by makeCacheMatrix()
## if inverse already calculated and unchanged, then cacheSolve() retrieves
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    # if already set return the inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # get the matrix
    data <- x$get()
    
    ## solve for matrix inverse
    m <- solve(data) %*% data
    
    ## set inverse to object
    x$setInverse(m)
    
    ##return matrix
        m
    
}

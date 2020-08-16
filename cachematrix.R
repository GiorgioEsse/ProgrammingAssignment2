makeCacheMatrix <- function(x = matrix()) {
    j <- NULL
    set <- function(y){
        x <<- y
        j <<- NULL
    }
    get <- function(x)
    setMatrix <- function(inverse) j <<- inverse
    getMatrix <- function(j) 
    list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}


cacheSolve <- function(x, ...) {
    j <- x$getInverse()
    if(!is.null(j)){
        message("getting data")
        return(j)
    }
    mat <- x$get()
    j <- solve(mat,...)
    x$setInverse(j)
    j
}
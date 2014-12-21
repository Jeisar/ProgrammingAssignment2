## cachematrix.R
## This function calculates the inverse of a matrix, while uses caching to avoid unecessary computing time. 
## If the inverse matrix is already calculated it is simply retrieved from cache, avoiding the re-calculation.
## It is assumed that the input matrix is always invertible.

## The makeCacheMatrix function received the input matrix and creates a special list that includes the four 
## functions required to do the calculations.
makeCacheMatrix <- function(x = numeric()) {
    inv<-NULL                                                    
    set<-function(y)
        x  <<-y                                                   
        inv<<-NULL       
    
    get<- function() x
    setinv <- function(inverse) inv <<-inverse
    getinv <- function() inv                                
    list(set=set, get = get, setinv = setinv, getinv = getinv)   
}


## The cacheSolve function uses the special list of functions from the makeCacheMatrix to actually perform the 
## inverse calculation or cache retrieval.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    ## If the inverse matrix from cache is not zero then the calculations are completed and the retrieved-from-cache inverse matrix is shown on screen.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## If the inverse matrix from cache is zero, then the inverse matrix of the input matrix is calculated using the solve() function.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    
    
    
}
# source("cachematrix.R") paste this in the promt to load the functions.


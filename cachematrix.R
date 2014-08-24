## The following two functions deal with creating a special matrix object
## that can cache its inverse and with the computing matrix inverses based on
## on that special matrix object.
## Examples:
##
##> m1 <- matrix(c(1,0,5,2,1,6,3,5,1), nrow=3, ncol=3)
##> cm1 <- makeCacheMatrix(m1)
##> cacheSolve(cm1)
##           [,1]       [,2]       [,3]
##[1,] -4.8333333  2.6666667  1.1666667
##[2,]  4.1666667 -2.3333333 -0.8333333
##[3,] -0.8333333  0.6666667  0.1666667
##>
##
## Calling cacheSolve(cm1) again, will retrieve the inverse from the  cache
##
##> cacheSolve(cm1)
##Retrieving inverse matrix from cache
##           [,1]       [,2]       [,3]
##[1,] -4.8333333  2.6666667  1.1666667
##[2,]  4.1666667 -2.3333333 -0.8333333
##[3,] -0.8333333  0.6666667  0.1666667
##>
## If the matrix is changed, the cache is cleared, the inverse is calculated and stored
##
##> nm <- matrix(c(1,0,5,2,1,6,3,5,0), nrow=3, ncol=3)
##> cm1$set(nm)
##> cacheSolve(cm1)
##Calculating the inverse matrix and storing it in the cache
##     [,1] [,2] [,3]
##[1,]   -6  3.6  1.4
##[2,]    5 -3.0 -1.0
##[3,]   -1  0.8  0.2
##> 
##> cacheSolve(cm1)
##Retrieving inverse matrix from cache
##     [,1] [,2] [,3]
##[1,]   -6  3.6  1.4
##[2,]    5 -3.0 -1.0
##[3,]   -1  0.8  0.2
##> 


## This function creates a special "matrix" object that can cache its inverse.
## The special matrix object is a list of funcions to set and retrieve the original
## matrix as well as the cached version.
makeCacheMatrix <- function(x = matrix()) {

    #variable that holds the cached invese matrix
    cachedInverseMatrix <- NULL

    #if the matrix changes, clean up the cache
    setMatrix <- function(matrix){
        x <<- matrix
        cachedInverseMatrix <<- NULL
    }
    
    #retrieves the original matrix
    getMatrix <- function(){
        x
    }
    
    #stores the inverse matrix
    setCachedMatrix <- function(cMatrix) {
        cachedInverseMatrix <<- cMatrix
    }
    
    #retrieves the inverse matrix
    getCachedMatrix <- function(){
        cachedInverseMatrix
    }
    
    #the matrix object that can cache its inverse
    list(set = setMatrix, get = getMatrix, getCache = getCachedMatrix, setCache = setCachedMatrix)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves
##the inverse from the cache.
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getCache()
    if (!is.null(inverseMatrix)){
        message("Retrieving inverse matrix from cache")
        return(inverseMatrix)
    } else {
        ##Since the cache is empty, we have to calculate the inverse and store it
        message("Calculating the inverse matrix and storing it in the cache")
        oMatrix <- x$get()
        inverse <- solve(oMatrix)
        x$setCache(inverse)
        #returning the cached inverse matrix
        return(x$getCache())
    }
}

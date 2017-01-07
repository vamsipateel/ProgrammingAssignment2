## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Sample test problem

## sd<-matrix(c(1,2,3,4),nrow = 2,ncol = 2)
## sd1<-makeCacheMatrix(sd)
## cacheSolve(sd1)

makeCacheMatrix <- function(x = matrix()) {
    ## makeCacheMtrix returns a list of functions which
    ##      set the matrix
    ##      get the matrix
    ##      set the inverse
    ##      get the inverse of invertible matrix
    
    ##  Matrix inverse is initialized to null
    mtrx_invrs <- NULL
    
    ##  set function sets the value of matrix object (x)
    ##  <<- so that the value of argument can be used in parent environment, different
    ##      from function environment
    set <- function(y){
        x <<- y
        mtrx_invrs <<- NULL 
    }
    
    ## get function access/gets the matrix object (x)
    get <- function() x
    
    ##  setinv function sets the inverse of matrix (x)
    setinv <- function(mat_invs) mtrx_invrs <<-mat_invs
    
    ## getinv function access/gets the inverse of the matrix (x)
    getinv <- function() mtrx_invrs
    
    ## returns the list containing above four functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## cachesolve input is of type makeCachematrix()
    
    ## getting the inverse of matrix, accessing the getinv function
    mtrx_invrs <- x$getinv()
    
    ## if inverse is available, it will just return it instead of computing
    if (!is.null(mtrx_invrs)) {
        message("getting cached inverse")
        return(mtrx_invrs)
    }
    
    ## if inverse is not available
    ##  get the matrix
    matrx <- x$get()
    
    ## compute the inverse of matrix
    mtrx_invrs <-solve(matrx, ...)
    
    ## set the inverse using setinv function
    x$setinv(mtrx_invrs)
    
    ## print the inverse to console
    mtrx_invrs
    
}

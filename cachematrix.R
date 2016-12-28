## We are creating 2 functions, makeCacheMatrix and cacheSolve.
## The first will create an environment in which the inverse is stored and then
## change a matrix into a list, consisting of the original matrix and two functions 
## which can access the value of the inverse matrix in the special environment.

makeCacheMatrix <- function(x = matrix()) {
    # We first initialize an inverse vector filled with NA.
    inv<-matrix(nrow=nrow(x),ncol=ncol(x))
    ## this matrix inv exists only in the environment that is makeCacheMatrix.
    ## it is not easily accessible from the outside.
    ## Hence, we need a function that sets its value in the makeCacheMatrix environment
    ## and a function that retrieves it.
    setinverse<-function(y) inv<<-y
    getinverse<-function() inv
    list(org= x,setinverse=setinverse,getinverse=getinverse)
}


## The second function, cacheSolve, takes as argument a list such as been created by makeCacheMatrix.
## The function checks if any of the elements in inv (in the makeCacheMatrix environment) is NA.
## If not, then inv is returned as the inverse of the original matrix. 
## If there is an NA entry, the inverse is calculated, stored as inv in the original environment and returned.

cacheSolve <- function(x, ...) {
    ## First we check if inv has NA
    y<-x$getinverse()
    if(!any(is.na(y))){
        ##if no value is NA, then inv (and hence y) is the inverse.
        message('The stored inverse is:')
        return(y)
    }
    ## if we get here, then the list did NOT contain an inverse to the matrix. 
    ## hence the need to calculate one in a *local* variable z:
    z<-solve(x$org)
    ## now we set the value of inv to equal z. inv is in another environment but luckily we can access
    ## it through the function setinverse()
    x$setinverse(z)
    ## finally, we return the value of the inversematrix
    z
}
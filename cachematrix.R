## Function makeCacheMatrix to create special matrix object. 
## It contains four functions set, get, setinv & getinv


makeCacheMatrix <- function(x = matrix()) {
    inv_mat = NULL
    set = function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv_mat <<- inverse 
    getinv = function() inv_mat
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}




## Function cacheSolve to return the inverse of special matrix
## IF inverse has not already been calculated, it computes the inverse and stores in cache
## ELSE it returns the inverse from cache

cacheSolve <- function(x, ...) {
    inv_mat = x$getinv()
    
    if (!is.null(inv_mat)){
        message("Getting cached data...")
        return(inv_mat)
    }
    
    data = x$get()
    inv_mat = solve(data, ...)
    x$setinv(inv_mat)
    inv_mat
}


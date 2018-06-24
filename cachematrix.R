#
# makeCacheMatric function
# pass in the original matrix to be inversed
# Set the inv_matrix to NULL
# four functions: set(), get(), setInverse(), getInverse()
#
makeCacheMatrix <- function(orig_matrix = matrix()) 
{
    #
    # initialize inv_matrix to NULL
    #
    inv_matrix <- NULL 
    #
    # set(matrix)
    # sets orig matrix to a new value f_orig_matrix and reset inv_matrix
    #
    set <- function(f_orig_matrix) 
    {
        orig_matrix <<- f_orig_matrix
        inv_matrix <<- NULL
    }
    #
    # get()
    # return the orig_matrix
    #
    get <- function()
    {
      orig_matrix
    }
    #
    # setinverse(matrix)
    # 
    setInverse <- function(new_inv_matrix) 
    {
        inv_matrix <<- new_inv_matrix
    }
    #
    # return the cached inverse of the matrix.
    #
    getInverse <- function() 
    {
        inv_matrix
    }
    #
    # return a list of getter and setter methods
    # 
    list(set = set, get = get,
         setinverse = setInverse,
         getinverse = getInverse)    
}

#
# cacheSolve(cacheMatrix, ...)
# Returns the inverse of the passed in cacheMatrix object
#
cacheSolve <- function(makeCacheMatrix_object, ...) {
    #
    # Get the inverse matrix
    #
    inv_matrix <- makeCacheMatrix_object$getinverse()
    #
    # Check if the inv_matrix is Cached. 
    # if exists, use the Cached the inv matrix
    # if not, calculate the inverse matrix
    #
    if(!is.null(inv_matrix)) 
    {
        #  
        # Inverse already calculated, value not stale
        #
        message("getting cached data")
        #
        # Return the Cached inv matrix
        #
        return(inv_matrix)
    }
    #
    # if the inv matrix does not exist, get the new matrix
    #
    new_matrix <- makeCacheMatrix_object$get()
    #
    # calculate the inverse of the new matrix
    #
    new_inv_matrix <- solve(new_matrix)  
    #
    # Set the inv_matric to a new result
    #
    makeCacheMatrix_object$setinverse(new_inv_matrix)
    #
    # Return the new_inv_matrix
    #
    new_inv_matrix   
    #
}

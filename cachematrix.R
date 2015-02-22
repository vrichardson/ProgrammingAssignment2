## The functions makeCacheMatrix and cacheSolve are used to create a special kind of matrix that
## can cache the inverse of its self after its inverse has been created with cacheSolve. The purpose
## of this is to prevent frequent recalculation of potentially processor intensive operations.

## The makeCatchMartix function takes a matrix as a parameter and returns a list containg
## containing functions to set, get the matrix and set and get its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        
        # function to set the matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        #function to get the matrix
        get <- function() x
        
        #Function to set the inverse of the matrix is assigned to setsolve
        setsolve <- function(solve) s <<- solve
        
        # Function to return the stored inverse of a matrix previosly calculated
        getsolve <- function() s
        
        #Assign finctions to list which is returned
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        

}


## The function takes the special matrix created with makeCacheMatrix as an argument.
## It first tries to retreive the cached inverse of the matrix using the getsolve function.
## if the cached inverse isnt available then the function retrieves the matrix,
## calculates its inverse, stores the inverse of the matrix for later retreval 
## with setsolve and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # If the inverse of the matrix hasnt been calculated x$getsolve will return a null value
        # else it returns the cached inverse of the matrix.
        s <- x$getsolve()
        
        # Test if inverse of matrix has been cached and if so return it
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        #If no cached matrix is returned get the original matrix. 
        data <- x$get()
        
        #Calculate the inverse of the matrix
        s <- solve(data, ...)
        
        # Store the inverse of the matrix in the special matrix created with makeCacheMatrix
        x$setsolve(s)
        
        # Return the inverse of the matrix
        s
}





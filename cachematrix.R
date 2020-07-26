
#/* functions to calculate the inverse of matrix */

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL         # /** We will first initialize the inverse **/
        #/* Set up the matrix and subsequent step will return the matrix */
        set <- function(z) {
                x <<- z
                i <<- NULL
        }
        get <- function() (x) #/* Returning the matrix */
        
        # /* Set up and return the inverse of matrix
        setinverse <- function(inverse){ i <<- inverse}
        getinverse <- function() i
        
        
        # /* Return a list of the methods */
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        i <- x$getinverse()         #/* This will return a matrix that is the inverse of x */
        if( !is.null(i) ) {
                message("getting cached data")          #/* Return the inverse if its already set */
                return(i)
        }
        #/* Bring the matrix (get from the makeCacheMatrix)
        get_matrix <- x$get()
        
        #/* Calculate the inverse using solve function */
        i <- solve(get_matrix, ...)
        #/* Set the inverse to the object and return
        x$setinverse(i)
        return(i) 
}

# /* Example */
Data_A <-  matrix(c(2,4,6,8),2,2)
Data_A2 <- makeCacheMatrix(Data_A)
cacheSolve(Data_A2)

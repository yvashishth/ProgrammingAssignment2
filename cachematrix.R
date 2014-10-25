## Put comments here that give an overall description of what your
## functions do
#################################################################
##    The Function Returns a list of Function which are        ##
##    Used to set matrix, get matrix , inverse matrix and      ##
##    return inverted matrix                                   ##
##    The Function assumes that the matrix is square matrix    ##
#################################################################
makeCacheMatrix <- function(x = matrix()) {
  
  ## invMat stores the inverte matrix is initialized with NULL ##
      invMat <- NULL
    
  ##                Function to Create Matrix                  ##  
    setMat <- function(y) {
        x <<- y
        invMat <<- NULL
    }
  
  ##                Function to Fetch Matrix                   ##   
    getMat <- function() x
  
  ##              Function to Inverse Matrix                   ##
    setInvMat <- function() invMat <<- solve(x)
  
  ##          Function to Return Inverted Matrix               ##
    getInvMat <- function() invMat
    
  ##     Return the List of Function for Matrix operations     ##
  list(setMat = setMat,getMat = getMat,
         setInvMat = setInvMat ,getInvMat = getInvMat)

}


## Write a short comment describing this function
#################################################################
##  The Function Returns a matrix that is the inverse of 'x'   ##
##  It takes the List of function as arguments and check if    ## 
##  matrix invers exist in cache. If exist it returns the same ##
##  Else calculate and returns the value.                      ##
##  The Function assumes that the matrix is square matrix      ##
#################################################################

cacheSolve <- function(x, ...) {
  
  invMat <- x$getInvMat()
  
  ## Check If Inverted Matrix exist in Cache ##
  ##   If yes than fetch and Return          ##
    if(!is.null(invMat)){
      message("getting cached data")
      return(invMat)
      
    }
 
  ##   If no than Calculate Inverted Matrix  ##
  ##                and Return               ##
  
    x$setInvMat()
    invMat <- x$getInvMat()
    invMat
}

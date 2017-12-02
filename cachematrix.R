
#Creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
 
     setMat <- function(y) {
         x <<- y
         invMat <<- NULL
        }
     
       getMat<- function() x                              #get the value of the Matrix
      setInv<- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix
       getInv<- function() invMatrix                     #get the value of the invertible matrix
       list(setMat = setMat, getMat= getMat,
              setInv = setInv, getInv = getInv)
}



#Obtains inverse by computation or retrieving cached matrix
cacheSolve <- function(x, ...) {
  invMat <- x$getInv()
         if(!is.null(invMat)) {                       #if inverse matrix is not NULL
              message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
              return(invMat)                             #return the invertible matrix
            }
       
    #if value of the invertible matrix ==NULLthen  
         MatrixData <- x$getMat()                     #get the original Matrix Data 
         invMat<- solve(MatrixData, ...)             #use solve function to inverse the matrix
           x$setInv(invMat)                         #set the invertible matrix 
          return(invMat)        
}

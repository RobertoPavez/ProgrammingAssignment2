##Final Version
##function that cache inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  
       ##setting data as empty
       matrixData <- NULL
  
       ##create set function, to set matrix into cache. Also re-set matrixData for new data
       set <- function(y) {
               x <<- y
               matrixData <<- NULL      
       }
  
       ##create get function, to get cached matrix
       get <- function() {
               x
       }
  
       ##create setInverse function, to store cached inverse matrix
       setInverse <- function() {
               matrixData <<- solve(x)
       }
  
       ##create getInverse function, to get cached inverse matrix
       getInverse <- function() {
               matrixData
       }
  
       ##create function list, can be accessed from cacheSolve
       funcList <<- list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

## Return a matrix that is the inverse of 'y'
cacheSolve <- function(y, ...) {
  
       ##check for matrix input
       if(is.matrix(y) == FALSE){
    
               stop("input needs to be a matrix")
       }
  
       ##check for square matrix
       if(nrow(y) != ncol(y)){
    
               stop("matrix input needs to be a Square Matrix")                        
       }
  
       ##checking if funcList exist, to know if there is previous data
       if(exists("funcList") == TRUE){
    
               ##acquiring cached matrix
               matrixData <- funcList$get()
    
               ##checking if cached data exist
               if(!is.null(matrixData)){
      
                       print("checking cached data")
      
                       ##if cached data exist, check for identical matrices
                       sameMats <- function(a, b){
        
                               is.matrix(a) && is.matrix(b) && dim(a) == dim(b) && all(a == b)
                       }
      
                       ##if Matrices are the same return cached data
                       if((sameMats(y, matrixData)) == TRUE ){
        
                               print("input and cached matrix are the same")
        
                               ##acquiring cached Inverse
                               matrixInvData <- funcList$getInverse()
        
                               ##cached output
                               print("cached matrix")
                               return(matrixInvData)
        
                       }
      
                       ##if matrices are different function continues to calculate new inverse
                       if((sameMats(y, matrixData)) == FALSE ){
        
                               print("input and cached matrix are not the same")
                       }
      
               }
    
       }
  
       print("computing new matrix")
  
       ##generating data for input matrix
       as.list(makeCacheMatrix(y))
  
       ##acquiring data from cache
       data <- funcList$get()    
  
       ##setting inverse matrix to cache
       funcList$setInverse()
  
       ##acquiring inverse matrix from cache
       matrixData <- funcList$getInverse()
  
       ##computed output
       matrixData
  
}

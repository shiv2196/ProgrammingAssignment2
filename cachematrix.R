
#makeCacheMatrix() creates special matrix object that caches the inverse of the matrix.

makeCacheMatrix <- function(mat = matrix())   #create special object matrix
{
  invMat <- NULL                          # initially setting value of inverse of matrix to NULL
  setMat <- function(mset)                #set the value of matrix
  {
    mat <<- mset
    invMat <<- NULL
  }
  getMat <- function()                    #get the value of matrix
    mat
  setInvMat <- function(inverse)          #set the value of inverse of matrix 'mat'
    mat <<- inverse 
  getInvMat <- function() mat             #get the inverse value
  list(setMat = setMat, getMat = getMat,setInvMat = setInvMat,getInvMat = getInvMat)
}


# cacheSolve() checks if the inverse of matrix from makeCacheMatrix() already exists or not.

cacheSolve <- function(mat, ...) 
{
  #gets the value of the inverse matrix from the makeCacheMatrix function
  
  invMat <- mat$getInvMat()
  if(!is.null(invMat))                    #if inverse matrix is not NULL
  {                       
    message("Getting Cached Inverse Matrix")   
    return(invMat)                      #return the inverse matrix
  }
  
  #if value of the inverse matrix is NULL 
  dataMat <- mat$getMat()                 #get the original Matrix 
  invMat <- solve(dataMat, ...)           #use solve function to inverse the matrix
  mat$setInvMat(invMat)                   #set the invertible matrix 
  return(invMat)                          #return the inverse matrix of 'mat'
  
}

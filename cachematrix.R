## Work Assigment Week 3
## Developed by Joel Duin

## Define the cache matrix

makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL
  
  # Define Set function
  set <- function(y){
    x<<-y
    inverse <<- NULL
  }
  
  #Define Get Function
  get <- function(){x}
  
  #Set the inverse of the matrix within the lexical scope
  set_inverse <- function(inverse_matrix) {inverse<<-inverse_matrix}
  
  #Get the inverse of the matrix
  get_inverse <- function(){inverse}
  
  #list all parts of the function
  list(set = set, get= get, set_inverse=set_inverse, get_inverse=get_inverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
  #get the value of the inverse
  inverse<-x$get_inverse()
  
  
  #check if the inverse has been calculated
  if (!is.null(inverse)){
    print("getting Cache date")
    return(inverse)
    
  }
  
  #get the matrix
  mat<-x$get()
  
  #solve the inverse matrix
  inverse<-solve(mat,...)
  
  #set the inverse
  x$set_inverse(inverse)
  
  inverse
}

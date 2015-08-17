## At the beginning generates the function an special matrix, that is able to establish an inversion
## of itself. Afterwards does the second function test, if the inversion has already been calculated and 
## if not it creates an inversion of the matrix.

## This function is the first part of creating an inversion of a given matrix. At the beginning it 
## gernerates a special matrix and furthermore generates the functions for setting and getting the matrix
## as well as functions for setting and getting the inversin

makeCacheMatrix <- function(x=matrix()){
  #Inverse value is set to NULL
  m <- NULL
  
  #Define the set function; m is set null again, if another matrix is set
  set <- function(y){
    x<<- y
    m<<-NULL
  }
  
  #Get-function for the origin matrix
  get <- function()x
  #set the inverse matrix on the private parameter 'm'
  setinversion <- function(solve) m <<- solve
  
  #get Function for the inverse matrix
  getinversion <- function() m
  
  #Creates the list
  list(set=set, get=get, setinversion=setinversion, getinversion=getinversion)
}

## The second part of the functions starts with testing if the inversion of a matrix has already been
## calculated. If so it edits a message and the inversion of the matrix. Elsewise it calculates the 
## inversion with the "makeChacheMatrix" function above.

cacheSolve <- function(x,...){
  
  #Check if the inverse matrix has already been calculated
  m <- x$getinversion
  
  #IF it is calculated, return the respective inverse matrix
  if(!is.null(m)){
    message("Inversion has already been calculated")
    return(m)
  }
  #... else calculate the inverse and cache its values
  data <- x$get()
  m <- solve(data,...)
  x$setinversion(m)
  m
}

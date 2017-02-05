# The following are two functions, "makeCacheMatrix" and "cacheSolve". The functions assume that a matrix is always invertible.

 # The first function "makeCacheMatrix" returns a list of functions to
 #1. set values to an empty matrix passed 
 #2. get the populated matrix
 #3. set the inverse of the matrix
 #4. get that inverse 
 
 # The second function is to retrieve and return cached inverse ,if it has already been created by the first function.
 # It calculates and returns the inverse otherwise.




makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL   
  
   set<-function(y){
    x<<-y                ## double headed assignment operators because variables are defined in the parent environment
    inv<<-NULL
    }
  
   get<-function() x
  
   setInverse<-function() inv<<-solve(x)    ##double headed assignment operator because variable 'inv' is defined in the parent environment 
  
   getInverse<-function()inv
  
   list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


# This function takes the list returned by "makeCacheMatrix" as an argument

cacheSolve <- function(f, ...) {
        ## Return a matrix that is the inverse of 'x'

   inv<-f$getInverse()
  
  if(!is.null(inv)){
    message("getting cache data")
    return(inv)
  }
  mat<-f$get()
  inv<-solve(mat,...)
  inv 
}




sample run:

> funs<-makeCacheMatrix(matrix(1:4,2))

> funs$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4

# cacheSolve function calculates and returns inverse
> cacheSolve(funs)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

# cacheSolve function retrieves and return cached inverse 

> funs$setInverse()
> cacheSolve(funs)
getting cache data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

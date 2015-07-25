## This script is part of a Coursera course assignment. The course is 
## "R Programming", the assignment is "Programming Assignment 2".

## In this script:

## makeCacheMatrix : A function that allows to cache the inverse of a matrix.

## cacheSolve: A function that allows to compare a matrix to a cached one and
##    either retrieve the inverse of the matrix from the cache (if possible) or
##    compute it.

## makeCacheMatrix(x) 
## - x is a square matrix
## - returns a list with four elements, each of which is again a function:
##          set(y) takes the matrix y and assigns it to x
##          get() returns x
##          setinverse(y) takes the matrix y and stores it in the variable
##                inverseofx (internal variable of makeCacheMatrix(x))
##          getinverse() returns inverseofx

## cacheSolve(x,y=makeCacheMatrix(x), overwrite=TRUE)
## - x is a non-singular(!) square matrix
## - y is a list as returned by makeCacheMatrix(x')
## - overwrite is a logical variable: It specifies if in the case that x!=x' 
##          the list y should be updated with x and the inverse of x 
##          (and hence x' and its inverse are lost) or if a new list 
##          z <- makeCacheMatrix(x) should be created with inverse of x set.
## - returns the inverse of x 
## - if overwrite=FALSE and x!=x', then the list z is created in the defining
##          environment of cacheSolve


## NOTE: The possibility that the matrix x is singular is not treated in the 
## code below. Although there is a is.singular.matrix function in R it belongs
## to the package matrixcalc, which does not belong to base. And I didn't want
## to code the whole process of guiding a user through installing matrixcalc if
## it isn't already installed. 


makeCacheMatrix <- function(x = matrix()) {
      # Initializing inverseofx with NULL, to be able to track if no computation
      # has been made
      inverseofx <- NULL
      
      # Constructing the function set. <<- has to be used so that y is passed to
      # the variable x in the namespace of makeCacheMatrix (and not to a new 
      # variable x in the environment of set). And since the value of x has been
      # changed inverseofx is set to NULL again, to be able to track if a 
      # computation was already made.
      set <- function(y){
            x <<- y
            inverseofx <<- NULL
      }
      
      # Constructing the function get. Nomes est omen this function allows the 
      # user to get the value of x once makeCacheMatrix(x) was constructed.
      get <- function() {
            x
      }
      
      # Constructing the function setinverse. This function allows the user to
      # set the value of inverseofx to computedinverse. Note that this 
      # function does not compute the inverse of x! computedinverse can be
      # anything (which doesn't give an error because of clashing data types
      # with inverseofx) the user likes.
      setinverse <- function(computedinverse) {
            inverseofx <<- computedinverse
      }
      
      # Constructing the function getinverse. 
      getinverse <- function(){
            inverseofx
      }
      
      # Returning those four functions in a list, but don't print them.
      invisible(list(set=set, get=get, setinverse=setinverse, 
                     getinverse=getinverse))
      
}




cacheSolve <- function(x, y=makeCacheMatrix(x), overwrite=TRUE) {
      # Test if x is x'. If it is there is a chance the inverse of x is in the
      # cache. Else we definitely have to compute the inverse of x 'from 
      # 'scratch'.
      if (all(x==y$get())){
            inverseofxprime <- y$getinverse()
            # Test if the inverse of x' is already computed. If so retrieve the
            # result form the cache.
            if (!is.null(inverseofxprime)){
                  message("The inverse of your input matrix was already in the
                          cache. Retrieving it from there.")
                  return(inverseofxprime)
            }
            else{
                  inverseofx <- solve(x)
                  y$setinverse(inverseofx)
                  return(inverseofx)
            }
      }
      # If we got to here then x!=x'. So we distinguish depending on the value
      # of overwrite.
      else if (overwrite ==TRUE){
            inverseofx <- solve(x)
            y$setinverse(inverseofx)
            return(inverseofx)
      }
      else {
            inverseofx <- solve(x)
            # Creating the cache object for x. <<- is used so that the list z 
            # is created in a parent environment of cacheSolve. Honestly I am
            # not so sure this does not screw things if z were defined before
            # and used for something completly unrelated.
            z <<- makeCacheMatrix(x)
            z$setinverse(inverseofx)
            return(inverseofx)
      }      
}

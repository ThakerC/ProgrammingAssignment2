#The program demonstrates lexical scoping and how various functions and variables 
#operates in current and parent environment under which it is invoked.

#The program takes the input of object type matrix and it uses a function called 
#as "solve()" to inverse the matrix. In order that this result is 'saved' 
#once the function is executed and the value may be used again,
#it is stored in a variable  defined outside of current environment
#using a special doubleassignment operator "<<-" , The use of this operator 
#within a function actually assigns or modifies the value of the variables 
#in parent environment 


#This program uses two functions, MakeCacheMatrix and cacheSolve

#makecacheMatrix takes a matrix as an input and returns the object with matrix, 
#the inversed value of matrix and also retain the copy of working environment makeCacheMAtrix()
# it returns an object that contains functions to its parent environment,
#have access to the specific functions in its list,  retains access to the entire environment defined by makeCacheMatrix()

makeCacheMatrix <- function (x = matrix()){            # x is an object of matrix passed as an argument when makeCacheMatrix is run
     
     m <- NULL                     # initiate m which will be used for caching the inverse value
     
     set <- function(y) {          # The function used to setup an input matrix value and  reset cache 'm' whenever this function is executed 
          x <<- y                  # Assigns the value to x  which is now operating at parent level 
          m <<- NULL               # always set to NULL if new matrix input is instantiated using set()
          # thus 'm', which is used for the inverse value by previous instances  will be reset
     }
     
     get <- function() x           # Get the matrix
     
     
     setinverse <- function(mat_obj)    # Assign the inversed matrix object to variable m, 
          # m is defined in the parent environment in makechacheMatrix
          m <<- mat_obj
     
     
     getinverse <- function() m         #  returns  the inverse matrix  
     
     
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) # Returns the object 
}

#Cachesolve takes a list stored in parent environment  as an input and returns the list with the values 
#and stores the result in the variable in parent environment  


cacheSolve <- function(x, ...) {
     
     m <- x$getinverse()           # This invokes getinverse function which returns the value of m defined in global envirinment
     if(!is.null(m)) {             #  when  makeCacheMatrix is instantiated first time its value is set a NULL as seen in the function
          # when m is returned with a object by invoking setinverse() it's vlaue is cached  using <<- , 
          # thus if m has stored value it will returns cached value.  
          
          message("getting cached data")
          return(m)
     }
     
     data <- x$get()               # if m is NULL, then it uses solve function to create inverse of matrix object and assigns it to m
     m <- solve(data, ...)
     x$setinverse(m)               # This step is to ensure the value of m is passed as an argument and set in m in global environment
     m
     
}


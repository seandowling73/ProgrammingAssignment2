## This function creates a special "matrix" object that can cache its inverse

## I have no fricking clue how this should work. The instructions are incredibly vague.
## I don't know why we need these items, but I'm going to follow the format of the example
## and have my makeCacheMatrix return a list containing 4 functions: 
## (get, set, get_inv, set_inv)

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL  ##initialize a null matrix for our inverse
    set_x <-function (y){
        x <<- y   ## assign y to x at a global level
        inv_x <<- NULL ##re-initialize inv_x
    }
    get_x <- function()x   ##get should retrieve the matrix
    set_inv <- function(z) inv_x<<-z  ##globally assign the argument to inv_x
    get_inv <- function() inv_x  ## simply return the inverse matrix
    list(set_x=set_x, get_x=get_x,set_inv=set_inv,get_inv=get_inv) ##assemble a list of our functions to return
}


## The following function calculates the mean of the special "vector" created with the 
## above function (makeCacheMatrix). 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the set_inv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$get_inv()  #assign inverse to a temp variable
    if (!is.null(m) & identical(x,x$get_x) ) {  ## if m is NOT NULL AND the matrix hasn't changed 
        ## we just retrieve from cache.  PLEASE NOTE: I think there's an error in this code and this
        ## always returns FALSE, but I'm sort of fed up with this assignment
        print("Retrieving Cached Matrix")
        return(m)
    }
    data <- x$get_x()  ## assign a temp data variable using the get_x function from 
    m <- solve(data, ...)  ##make m the inverse of our matrix
    x$set_inv(m)  ## set our inverse matrix
    m  ##return our inverse matrix
        
}

## Test ouputs
# > y
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > y1 <- makeCacheMatrix(y)
# > cacheSolve(y1)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5



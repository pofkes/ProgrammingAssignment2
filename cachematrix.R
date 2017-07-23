##week 3 programming assignment
##makeCacheMatrix function creates a list which has getter and setter functions as its objects
##set and setInv are setter functions, which set the value of matrix x and the inverse of x - inv
##in the makeCacheMatrix environment
##get and getInv are used to retrieve values from cache

#cacheSolve the function retrieves the cached value.
#function checks if the cached value is not null
#if inverted matrix was saved before, the function will return its value and stop
#if there is no inverted matrix saved in cache, 
#the function will proceed to calculate the inverse of the matrix
#supplied to the makeCacheMatrix function

makeCacheMatrix <- function (x = matrix()) {
        #initialize the values for x and inv - the inverse of the matrix x every time the makeCacheMatrix object is defined
        inv <- NULL
        #set function sets the new value of x and the cached inverted matrix to NULL in the parent function
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #define get function, which retrieves the value of the matrix x
        get <- function() x
        #define setInv function, which takes the calculated inverted matrix and saves its value in cache
        setInv <- function(Xinv) inv <<- Xinv
        #getInv retrieves the value of the cached inverted matrix
        getInv <- function () inv
        #define a list, which will be returned by the makeCacheMatrix object
        list(set = set, get = get, setInv = setInv, getInv = getInv)

}

cacheSolve <- function (x) {
        #retrieves the cached inv value
        #if it is null, the function proceeds to calculate an inverse of matrix x from makeCacheMatrix function
        #if it is not null, retrieves inv value from the cache and returns it
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        #executes the get function, retrieving the original matrix
        data <- x$get()
        # calculates the inverse using a solve function
        inv <- solve(data)
        #sets the cache value in the argument object
        x$setInv(inv)
        #returns the calculated inverse
        inv
}


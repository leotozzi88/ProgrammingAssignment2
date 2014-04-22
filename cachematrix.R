# The makeCacheMatrix function creates a "special object", which is actually a list containing functions. These functions can be used to store a matrix, change it, set its inverse and store it.

makeCacheMatrix <- function(x = matrix()) {#the function takes a matrix as argument
	inv<-NULL #setting the inverse value to null when creating a new special matrix
	set_matrix<-function(y){ #this functions allows to change the stored matrix
		x<<-y
		inv<<-NULL
	}
	get_matrix<-function() x #shows the currently stored matrix
	set_inverse<-function(inverse) inv<<-inverse #stores the inversion result, which will be calculated by the cacheSolve function
	get_inverse<-function() inv #prints the inversion result
	list(set_matrix= set_matrix, get_matrix= get_matrix, set_inverse= set_inverse, get_inverse= get_inverse) #this special object is a list of functions, as requested
}


# The cacheSolve function calculates the inverse of a matrix stored in a "special object" created by the makeCacheMatrix function. It then stores it in the "special object". If an inverse matrix has already been calculated before, this function can get the saved result directly from the "special object".

cacheSolve <- function(x, ...) {#x has to be a "special object" created by the makeCacheMatrix function
        inv <- x$get_inverse() #sets inv to be the inverse of the matrix that is inside the chosen special object
        if(!is.null(inv)) { #if inv is already in the cache, its value is taken from there and a friendly message is printed
                message("getting cached data! :)")
                return(inv)
        }
        #if not, it gets calculated and stored in the cache as inv as follows:
        data <- x$get_matrix() #gets the desired matrix from the makeCacheMatrix "special object"
        inv <- solve(data, ...) #calculates the inverse of the matrix and sets it as inv
        x$set_inverse(inv) #stores inv inside the specified "special object"
        inv #returns the inverse
}

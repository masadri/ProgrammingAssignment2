## This code is making a cache matrix which is cache 
## The matrix and it's inverse matrix

## Make cache matrix to store a matrix and it's Inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix(nrow=dim(x)[1],ncol=dim(x)[2])
        set<- function(y){
                x<<- y
                inv<<- matrix(nrow=dim(x)[1],ncol=dim(x)[2])
        }
        get<- function() x
        setInv<- function(y) 
                inv<<-y
        getInv<- function() inv
        list(set= set, get= get, setInv=setInv,
             getInv=getInv)

}



## This function will return solve matrix from a cached matrix

## Note: before solving cached matrix, it is going to check 
## requirements of solving matrix and if it was eligible for 
## solving it will continue to solve it

cacheSolve <- function(x, ...) {
        i<- x$getInv()
        if(!any(is.na(i))){
                message("The matrix has been solved...\ngetting cached data...")
                return(i)
        } else if(dim(i)[1]!=dim(i)[2]){
                message("Your matrix dimension is not valid...\nYour matrix should be a square one...")
                return()
        }else if(any(is.na(x$get()))){
                message("Your matrix data is not complete...\nTry to fix your data and solve it again...")
                return()
        }else if(det(x$get())==0){
                message("Your matrix determinant is 0...\nYou cannot solve this matrix...")
                return()
        }
        mat_data<- x$get()
        i<- solve(a= mat_data, ...)
        x$setInv(i)
        i
        
}

library(igraph)
# Using topological sort algorithm o find a topological ordering of the directed graph using the matrix
#     | 0 1 1 0 0 |
#     | 0 0 0 0 1 |
# A=  | 0 0 0 0 1 |
#     | 1 0 0 0 0 |
#     | 0 0 0 0 0 |

# Define function
topSort <- function(DAG){
  n <- ncol(DAG)
  visited <- vector("logical",n)
  sort <- vector("numeric",n)
  for (i in 1:n){
    for (j in 1:n){
      found <- FALSE
      if (visited[j] == FALSE && all(visited[DAG[,j] == 1]) == TRUE){
        visited[j] <- TRUE
        sort[i] <- j
        found <- TRUE
        break
      }
    }
    if (found == FALSE){
      stop("No topological sorting exists!")
    }
  }
  return(sort)
}

# Matrix A from above
A <- matrix(0,5,5)
A[1,2:3] <- 1
A[2:3,5] <- 1
A[4,1] <- 1
plot(graph(A))

# Find topological sorting
TS <- topSort(A)
TS

# Making the DAG cyclic by adding edge X3 -> X4
A[3,4] <- 1
plot(graph(A))
TS <- topSort(A)







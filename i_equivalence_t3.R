library(igraph)

#The goal was to write a function that takes two DAGs as arguments 
#and checks if they are I-equivalent. 

# Comparing two dags by checking I-equivalence 
i_equvalence <- function(A,B){
  skeleon_A <- A+t(A)
  skeleon_B <- B+t(B)
  if (all(skeleon_A == skeleon_B) == FALSE){
    return(FALSE)
  }
  for (i in 1:ncol(A)){
    parents_A <- which(A[,i] == 1)  
    if (length(parents_A) > 1){
      tmp_A <- combn(parents_A,2)
      index <- vector("logical",ncol(tmp_A))
      for (j in 1:length(index)){
        if (A[tmp_A[1,j],tmp_A[2,j]] == 0 && A[tmp_A[2,j],tmp_A[1,j]] == 0){
          index[j] <- TRUE
        }
      }
      if (any(index)){
        immoralities_A <- tmp_A[,index]
      } else {
        immoralities_A <- 0
      }
    } else {
      immoralities_A <- 0
    } 
    parents_B <- which(B[,i]==1)  
    if (length(parents_B) > 1){
      tmp_B <- combn(parents_B,2)
      index <- vector("logical",ncol(tmp_B))
      for (j in 1:length(index)){
        if (B[tmp_B[1,j],tmp_B[2,j]] == 0 && B[tmp_B[2,j],tmp_B[1,j]] == 0){
          index[j] <- TRUE
        }
      }

      if (any(index)){
        im_B <- tmp_B[,index]
      } else {
        im_B <- 0
      }
    } else {
      im_B <- 0
    } 
    if (length(im_A) != length(im_B) || !all(immoralities_A == im_B)){
      return(FALSE)
    }
  }
  return(TRUE)
}

# Running the code with DAGs
G1 <- matrix(0,5,5)
G1[1,2] <- 1
G1[2,3] <- 1
G1[c(3,5),4] <- 1
G2 <- matrix(0,5,5)
G2[2,1] <- 1
G2[3,c(2,4)] <- 1
G2[5,4] <- 1
G3 <- matrix(0,5,5)
G3[1,2] <- 1
G3[3,c(2,4)] <- 1
G3[5,4] <- 1
plot(graph_from_adjacency_matrix(G1))
plot(graph_from_adjacency_matrix(G2))
plot(graph_from_adjacency_matrix(G3))
i_equvalence(G1,G2)
i_equvalence(G1,G3)
i_equvalence(G2,G3)
G2[3,1] <- 1
G3[3,1] <- 1
plot(graph_from_adjacency_matrix(G2))
plot(graph_from_adjacency_matrix(G3))
i_equvalence(G2,G3)



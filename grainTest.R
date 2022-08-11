install.packages("gRain")
install.packages("gRbase")
install.packages("gRim")
install.packages("bnlearn")
library(gRain)
library(bnlearn)

#The goal was to get familiarized with the R package gRain to querying 
#graphical models. The code uses querygrain( type= “conditional”) to verify local 
#independencies encoded by the DAG. The student network used is from the book 
#Probabilistic Graphical models by Daphne Koller. 

# Conditional probability tables for Student network 
D <- CTP(vpar = ~D, val = c(0.6,0.4), levels = c("d0","d1"))
I <- CTP(vpar = ~I, val = c(0.7,0.3), levels = c("i0","i1"))
S <- CTP(vpar = ~S|I, val = c(0.95,0.05,0.2,0.8), levels = c("s0","s1"))
G <- CTP(vpar = ~G|D:I, val = c(0.3,0.4,0.3,0.05,0.25,0.7,0.9,0.08,0.02,0.5,0.3,0.2), levels = c("g1","g2","g3"))
L <- CTP(vpar = ~L|G, val = c(0.1,0.9,0.4,0.6,0.99,0.01), levels = c("l0","l1"))

# List of conditional probability tables 
list <- compileCPT(list(D,I,S,G,L))
list
list$G

# Create network
student_dag <- grain(list)
dag <- as.bn(student_dag)
plot(dag)

# Joint distribution 
P <- as.vector(querygrain(object = student_dag, nodes = c("I","D","G","S","L"), type = "joint"))
sum(P)

setEvidence(student_dag, nodes = c("I","D","G","S","L"), states = c("i1","d0","g2","s1","l0"))

# Conditional distributions
querygrain(object = student_dag, nodes = c("S","I"), type = "conditional")
querygrain(object = student_dag, nodes = c("S","I","D","G","L"), type = "conditional")







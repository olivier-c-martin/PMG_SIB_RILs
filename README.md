# PMGISRIL
Probabilities of multilocus genotypes in SIB recombinant inbred lines.

This code consider the probabilities of achieving any combination of alleles in 2-way
RILs (Recombinant Inbred Lines) obtained through mating of siblings until fixation
is reached.

#Install the reuired packages
install.packages(c("eply", "rlist","rmarkdown"))

library(eply)
library(rlist)
library(rmarkdown)

source('sibFun.R')
L = 3

allVar = systemVar(L)
  
varNom = allVar$symQs
varNom
  
nonSymQs = allVar$nonsymQs
nonSymQs
  
SQ = table(nonSymQs)
SQ
  
cat(paste(paste(as.vector(SQ),"Q(", names(SQ),")", collapse = "+", sep = ""),"=1", sep = ""))
  
# The all possible crossover for each contributed variable is 
  
scEq = allCrossOver(distinctQs = distinctQs)
  
res = twoWayRILsib(L, varNom, nonSymQs, scEq)
  
A = res$A
A[1:3,1:2]
  
B = res$B
B[1:3]
  
  
## To solve this linear system you should evaluate this symbolic matrix
  
AA = evalMatrix(A = A, recRates = recRates)
  
## For example, 
AA[1:3,1:2]
  
## Hence, 
  
sol = solve(AA, B)
names(sol) = varNom
sol
  
## This means, 
  
cat(" Q(0,0,0) = ", sol[1], ",", "Q(0,0,1) = ", sol[2], ",", "Q(0,0,2) = ", sol[3], "\n",
      "Q(0,1,0) = ", sol[4], ",", "Q(0,1,1) = ", sol[5], ",", "Q(0,1,2) = ", sol[6], "\n",        "Q(0,2,0) = ", sol[7], ",", "Q(0,2,1) = ", sol[8], ",",  "Q(0,2,2) = ", sol[9], "\n",
      "Q(0,2,3) = ", sol[10])
  
# Verification by simulation
  
## Note that the sum of all Q's equal to 1
  
QsProbs = rbind(sol, table(nonSymQs))
sum(QsProbs[1,] * QsProbs[2,])
  
  
## Convert Qs to Frequencies
## We should first convert the Qs to genotypes frequencies:
Fexp = QsToFreq(L, sol)
Fexp
  
## Compute the frequencies by simulation 
##To arrive more stabilty for your results you should choose hight number RIL generation, we choose $nRILS = 50000$ RIL. 
  
nRILS = 50000
  
## Then, define the binary hetrzgouse $F_2$ genertaion:
  
childGenotype = matrix(c(rep(0, L), rep(1, L), rep(0, L), rep(1, L)), ncol = L, byrow = TRUE)
childGenotype
  
  
## Now, run the simulation over $nRILS$
f = rep(0, 2^L)
for (i in 1:nRILS){
child = Get_One_RIL(L, recRates, childGenotype, type = "sib")
f[binTodec(child[1,])+1] = f[binTodec(child[1,])+1]+1
  cat(i,"of",nRILS,"\r")
 }#EndFor
 cat("\n")
 Fsim = f /nRILS
 Fsim
  
## Simulation Accuercy 
## You can compare the analytics results with the simulation one and compute the mean square error (MSE) for that
mean((Fexp - Fsim)^2)



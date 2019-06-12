# PMGISRIL
Probabilities of multilocus genotypes in SIB recombinant inbred lines.

### This code consider the probabilities of achieving any combination of alleles in 2-way RILs (Recombinant Inbred Lines) obtained through mating of siblings until fixation is reached.

Install the reuired packages:
install.packages(c("eply", "rlist","rmarkdown"))

library(eply)
library(rlist)
library(rmarkdown)

Call the function files 
source('sibFun.R')
L = 3

Find all Qs
allVar = systemVar(L)
The distinct Qs are
varNom = allVar$symQs

The nondistinct Qs are 

nonSymQs = allVar$nonsymQs

The homogeneous equation is

SQ = table(nonSymQs)  
cat(paste(paste(as.vector(SQ),"Q(", names(SQ),")", collapse = "+", sep = ""),"=1", sep = ""))
  
The self consistent indecies are 
  
scEq = allCrossOver(distinctQs = distinctQs)

The analsyit solution is 

res = twoWayRILsib(L, varNom, nonSymQs, scEq)
  
A = res$A
B = res$B
  
We can solve this linear system you should evaluate this symbolic matrix
  
AA = evalMatrix(A = A, recRates = recRates)
  
Hence, 

sol = solve(AA, B)
names(sol) = varNom
sol
  
This means, 
  
cat(" Q(0,0,0) = ", sol[1], ",", "Q(0,0,1) = ", sol[2], ",", "Q(0,0,2) = ", sol[3], "\n",
      "Q(0,1,0) = ", sol[4], ",", "Q(0,1,1) = ", sol[5], ",", "Q(0,1,2) = ", sol[6], "\n",        "Q(0,2,0) = ", sol[7], ",", "Q(0,2,1) = ", sol[8], ",",  "Q(0,2,2) = ", sol[9], "\n",
      "Q(0,2,3) = ", sol[10])
  
Verification by simulation
  
Note that the sum of all Q's equal to 1
  
QsProbs = rbind(sol, table(nonSymQs))
sum(QsProbs[1,] * QsProbs[2,])
  
  
Fisrt, Convert Qs to Frequencies:
We should first convert the Qs to genotypes frequencies:

Fexp = QsToFreq(L, sol)
Fexp
  
Second, Compute the frequencies by simulation:
we choose $nRILS = 50000$ RIL, nRILS = 50000.
  
Then, define the binary hetrzgouse $F_2$ genertaion:
  
childGenotype = matrix(c(rep(0, L), rep(1, L), rep(0, L), rep(1, L)), ncol = L, byrow = TRUE)
 
  
Now, run the simulation over nRILS

f = rep(0, 2^L)
for (i in 1:nRILS){
child = Get_One_RIL(L, recRates, childGenotype, type = "sib")
f[binTodec(child[1,])+1] = f[binTodec(child[1,])+1]+1
  cat(i,"of",nRILS,"\r")
 }#EndFor
 cat("\n")
 Fsim = f /nRILS
 Fsim
  
Simulation Accuercy 
Compare the analytics results with the simulation one and compute the mean square error (MSE) for that

mean((Fexp - Fsim)^2)



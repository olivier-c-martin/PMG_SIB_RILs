# PMGISRIL
Probabilities of multilocus genotypes in SIB recombinant inbred lines.

### This code consider the probabilities of achieving any combination of alleles in 2-way RILs (Recombinant Inbred Lines) obtained through mating of siblings until fixation is reached.


# Introduction
Set the working directory to the emplacement of the source file "SibFun.R" and load it to the working environment. You can download the "SibFun.R" file from <https://github.com/Kamel20/PMGISRIL>, 

```r
  setwd("the directory")
  source("sibFun.R")
```

Then load the required packages:

```{r}
  library(eply)
  library(rlist)
  library(rmarkdown)
```

```{r, include = FALSE}
  source('sibFun.R')
```

# Input
This code just needs to input the locus number and the recombination rates values. For example for $L=3$

```{r}
  L = 3
  recRates = c(0.4, 0.2, 0.3)
```

# The variables names
To gain time we found the list of inheritance indexes (all Qs) that contribute in the system before.

```{r, eval=FALSE}
  allQs = list.load("allVarTillL=10.rds")
  EquivalenceQs = list.load("allContrVarTillL=10.rds")
  distinctQs = allQs[[L]]$symQs
  nonSymQs = allQs[[L]]$nonsymQs
  scEq = EquivalenceQs[[L]]
```

or you can use their function to create it again

```{r}
  allQs = systemVar(L)
```


So all Qs variables are 

```{r}
  nonSymQs = allQs$nonsymQs
  nonSymQs
```

and the distinct Qs are

```{r}
  distinctQs = allQs$symQs
  distinctQs
```

Note that the first equation of the linear system is given by the fact that $\sum(Qs) = 1$

```{r}
SQ = table(nonSymQs)
SQ
```

This means that 

```{r, echo=FALSE}
cat(paste(paste(as.vector(SQ),"Q(", names(SQ),")", collapse = "+", sep = ""),"=1", sep = ""))
```

For each distinct Q, the equivlence Q in $F_2$ genration are  

```{r}
  scEq = allCrossOver(distinctQs = distinctQs)
  scEq
```

# Find the system AQ = B

The system required to compute all self-consistent equations except one that replaced by equation $\sum(Qs)=1$.
```{r}
  res = twoWayRILsib(L, distinctQs, nonSymQs, scEq)
```

Hence, the matrix A is 

```{r}
  A = res$A
  A[1:3,1:2]
```

and, the matrix B is 

```{r}
  B = res$B
  B[1:3]
```

To solve the linear system you should evaluate the symbolic matrix

```{r}
  AA = evalMatrix(A = A, recRates = recRates)
```
For example, 
```{r}
  AA[1:3,1:2]
```

Hence, 

```{r}
  sol = solve(AA, B)
  names(sol) = distinctQs
  sol
```

This means, 

```{r, echo=FALSE}
cat(" Q(0,0,0) = ", sol[1], ",", "Q(0,0,1) = ", sol[2], ",", "Q(0,0,2) = ", sol[3], "\n",
     "Q(0,1,0) = ", sol[4], ",", "Q(0,1,1) = ", sol[5], ",", "Q(0,1,2) = ", sol[6], "\n",        "Q(0,2,0) = ", sol[7], ",", "Q(0,2,1) = ", sol[8], ",",  "Q(0,2,2) = ", sol[9], "\n",
    "Q(0,2,3) = ", sol[10])
```

# Verification by simulation

Note that the sum of all Q's equal to 1

```{r}
  QsProbs = rbind(sol, table(nonSymQs))
  sum(QsProbs[1,] * QsProbs[2,])
```

## Convert Qs to Frequencies
We should first convert the Qs to genotypes frequencies:
```{r}
    Fexp = QsToFreq(L, sol)
    Fexp
```

## Compute the frequencies by simulation 
Choose high number RIL generation, we choose $nRILS = 50000$ RIL. 
```{r}
  nRILS = 50000
```
Then, define the binary hetrzgouse $F_2$ genertaion:

```{r}
  childGenotype = matrix(c(rep(0, L), rep(1, L), rep(0, L), rep(1, L)), ncol = L, byrow = TRUE)
  childGenotype
```

Now, run the simulation over $nRILS$
```{r}
  f = rep(0, 2^L)
  for (i in 1:nRILS){
      child = Get_One_RIL(L, recRates, childGenotype, type = "sib")
      f[binTodec(child[1,])+1] = f[binTodec(child[1,])+1]+1
      }#EndFor
  Fsim = f /nRILS
  Fsim
```

## Simulation Accuercy 
You can compare the analytic results with the simulation one and compute the mean square error (MSE) for that
```{r}
  mean((Fexp - Fsim)^2)
```

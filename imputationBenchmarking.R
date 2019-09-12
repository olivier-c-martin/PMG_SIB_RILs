rm(list = objects())
library(tictoc)
library(eply)
library(rlist)
library(rmarkdown)
library("mice")
library("missForest")
library("parallel")

### Recombination rates based on genetic distances and assuming:
## 1. No interference
## 2. Recombination rate between two loci < 0.5
recRates_estimation <- function(posC){
  distance_between_successive_loci <- diff(posC)
  recRates <- 0.5-0.5*exp(-distance_between_successive_loci/50)
  return(recRates)
}


## Function to counts the number of NAs between two given markers 
count.na.between.2.markers <- function(vec){
  #eliminate= FALSE
  vec.as.string <- paste0(vec, collapse="")
  na.appart <- unlist(strsplit(vec.as.string, "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)", perl=T))
  #if (max(lengths(regmatches(unlist(a), gregexpr("N", unlist(a)))))>n.missing) eliminate=TRUE
  return(max(lengths(regmatches(unlist(na.appart), gregexpr("N", unlist(na.appart))))))
}

MLGP <- function(n.loci, recRates, allQs, allvpForallup){
  nonEquivalentQs = allQs[[n.loci]]$symQs
  allQsMappedToNonEquivalent = allQs[[n.loci]]$nonsymQs
  sel.allvpForallup = allvpForallup[[n.loci]]
  multiplicityQs = table(allQsMappedToNonEquivalent)
  
  analyticEquations = twoWayRILsib(n.loci, nonEquivalentQs , allQsMappedToNonEquivalent, sel.allvpForallup, multiplicityQs)
  
  Amatrix = analyticEquations$A
  Bvector = analyticEquations$B
  ## To solve this linear system you should evaluate this symbolic matrix
  numericAmatrix = evalMatrix(A = Amatrix, recRates = recRates)
  solution = solve(numericAmatrix, Bvector)
  names(solution) = nonEquivalentQs
  
  ## Convert Qs to Frequencies
  ## This function returns the genotype in addition to its probability. 
  allProbabilitiesOfRilGenotypes= QsToGenotypeProbabilities(n.loci, solution)
  
  ## Note that the sum of all Q's is equal to 1
  # QsProbs = rbind(sol, table(nonSymQs))
  ## We convert the Qs to genotypes frequencies:
  # Fexp = QsToFreq(n.loci, sol)
  return(allProbabilitiesOfRilGenotypes)
}

## The divide and conquer procedure to deal with blocks that are too large
reduction.for.Lstar <- function(pos.sel, posC, Lstar){
  # Define an auxiliary vector that takes into account which loci would be set to the same value
  pos.init <- pos.sel
  aux <- rep(NA, length(pos.sel))
  kk <- 0
  while (Lstar > 5) {
    # Consider the smallest interval between adjacent markers
    reducing.jj  = which.min(diff(posC[pos.sel]))
    pos.on.initial.vec1 <- which(pos.sel[reducing.jj]==pos.init)
    pos.on.initial.vec2 <- which(pos.sel[reducing.jj+1]==pos.init)
    nearest.positions <-  pos.on.initial.vec1 : pos.on.initial.vec2
    is.not.next.to <- ((length(nearest.positions)>2)|(!all(is.na(aux[nearest.positions]))))
    if (is.not.next.to){
      kk <- sort(unique(na.omit(aux[nearest.positions])))
      if (length(kk)>1){
        aux[c(grep(kk[1],aux), grep(kk[2], aux))] <- kk[1]
        kk <- kk[1]
      } else {
        aux[nearest.positions] <- kk
      }
    } else {
      kk <- max(c(na.omit(aux),0))+1
      aux[nearest.positions] <- kk
    }
    # Between the two loci bounding this interval, remove the one that is nearest to its other neighbor  
    if (reducing.jj==1) {
      pos.sel <- pos.sel[-(reducing.jj+1)]
    } else if (reducing.jj==(length(pos.sel)-1)){
      pos.sel <- pos.sel[-(reducing.jj)]
    } else {
      sel <- which.min(c(diff(posC[pos.sel[(reducing.jj-1):(reducing.jj)]]), diff(posC[pos.sel[(reducing.jj+1):(reducing.jj+2)]])))
      # If the nearest is the firstm consider it and get ride of the next 
      if (sel==1) {
        pos.sel <- pos.sel[-(reducing.jj+1)]
      } else {
        pos.sel <- pos.sel[-(reducing.jj)]
      }}
    
    Lstar <- length(pos.sel)
  }
  return(list(Pos= pos.sel, Equal.loci=aux))
}


ImputationWithSibFrequencies <- function(rilsMatrix, recRates, allQs, allvpForallup, posC){
  ind.01 <- apply(rilsMatrix, 2, function(x){
    myvec <- rep(NA, length(x))
    myvec[1:length(which((x==0)|(x==1)))] <- which((x==0)|(x==1))
    return(myvec)})
  ## Boundaries:
  col.to.change <- which(ind.01[1,]!=1)
  for (cl in col.to.change){
    rilsMatrix[, cl][1:ind.01[1, cl]] <- rep(rilsMatrix[ind.01[1,cl], cl], ind.01[1, cl])
  }
  col.to.change = apply(ind.01, 2, function(x) max(na.omit(x))!=length(x))
  col.to.change = which(col.to.change)
  for (cl in col.to.change){
    rilsMatrix[, cl][max(na.omit(ind.01[,cl])):dim(rilsMatrix)[1]] <- rep(rilsMatrix[max(na.omit(ind.01[,cl])), cl], dim(rilsMatrix)[1]-max(na.omit(ind.01[,cl]))+1)
  }
  diff.pos = apply(ind.01, 2, function(x) diff(x))
  seq.along = apply(ind.01, 2, function(x) paste0(x, collapse="-"))
  patterns.in.matrix = sort(unique(as.vector(na.omit(diff.pos))))
  patterns.in.matrix = patterns.in.matrix[-which(patterns.in.matrix==1)]
  for (jj in patterns.in.matrix){
    index.jj <- which(diff.pos==jj, arr.ind = T)
    nii <- dim(index.jj)[1]
    
    for (ii in 1:nii){
      aux = NULL
      myindex <- ind.01[index.jj[ii,1]:(index.jj[ii,1]+1), index.jj[ii,2]]
      same.rec.rates <- apply(ind.01, 2, function(x, myindex) {
        res <- all(myindex %in% x)
        return(res)
      }, myindex) 
      same.rec.rates <- which(same.rec.rates)
      successive <- grep(paste0( myindex[1], "-",myindex[2], "-", collapse = ""), seq.along)
      same.rec.rates <- intersect(same.rec.rates, successive)
      
      pos.sel <- ind.01[index.jj[ii,1]:(index.jj[ii,1]+1), index.jj[ii,2]][1]:ind.01[index.jj[ii,1]:(index.jj[ii,1]+1), index.jj[ii,2]][2]
      
      Lstar <- length(pos.sel)
      pos.sel.initial = pos.sel
      ### Need to evaluate only if the interval is recombinant according to the flanking markers
      if (length(same.rec.rates)==1){
        if(!(rilsMatrix[pos.sel, same.rec.rates][1]==rilsMatrix[pos.sel, same.rec.rates][Lstar])){
          if (Lstar>5){
            pos.sel.and.equal.loci = reduction.for.Lstar(pos.sel, posC, Lstar)
            pos.sel =  pos.sel.and.equal.loci$Pos
            aux = pos.sel.and.equal.loci$Equal.loci
            Lstar = length(pos.sel)
          }
          rec.rate = recRates_estimation(posC[pos.sel])
          all.probs = MLGP(Lstar, rec.rate, allQs, allvpForallup)
        }
      } else {
        if (!all(rilsMatrix[pos.sel, same.rec.rates][1,]==rilsMatrix[pos.sel, same.rec.rates][Lstar,])){
          if (Lstar>5){
            pos.sel.and.equal.loci = reduction.for.Lstar(pos.sel, posC, Lstar)
            pos.sel =  pos.sel.and.equal.loci$Pos
            aux = pos.sel.and.equal.loci$Equal.loci
            Lstar = length(pos.sel)
          }
          rec.rate = recRates_estimation(posC[pos.sel])
          all.probs = MLGP(Lstar, rec.rate, allQs, allvpForallup)
        }
      }
      
      rilsMatrix[pos.sel.initial, same.rec.rates] <- apply(as.matrix(rilsMatrix[pos.sel, same.rec.rates]), 2, function(x, all.probs, Lstar, pos.sel.initial, pos.sel, aux=NULL){
        if (x[1]==x[length(x)]) {
          if(Lstar!=length(pos.sel.initial)){
            mv <- rep(x[1], length(pos.sel.initial))
            x <- mv
          } else {
            x[1:length(x)] <- x[1]
          }
        } else{
          genotypes <- t(sapply(0:(2^(length(pos.sel)-1)), paddedIntToBin, vectorLength= length(pos.sel)))
          sub.genotypes.indices <-  which((genotypes[,1]==x[1])&(genotypes[,Lstar]==x[length(x)]))
          best.genotype <- which.max(all.probs[sub.genotypes.indices])
          binary.best.genotype <- genotypes[sub.genotypes.indices[best.genotype],]
          x <- binary.best.genotype
          
          if (length(pos.sel.initial)> Lstar){
            mv <- rep(NA, length(pos.sel.initial))
            mv[which(pos.sel.initial %in% pos.sel)] <- binary.best.genotype
            for (aa in unique(na.omit(aux))){
              mv[which(aa==aux)] <- as.numeric(na.omit(mv[which(aa==aux)]))
            }
            x <- mv
          }
        }
        return(as.numeric(x))
      }, all.probs, Lstar, pos.sel.initial, pos.sel, aux)
      
      cat("Done: ", ii, "/", nii, "positions. For L*:", jj+1, "\n")
    }
  }
  cat("Simulation completed!")
  return(rilsMatrix)
}


## LOAD FUNCTIONS FOR COMPUTATION OF SIB-Rils Probabilities
setwd("~/Downloads")
source("PMG_SIB_RILs.R", echo = FALSE)
allQs = list.load("allVarTillL=10.rds")
allvpForallup = list.load("allContrVarTillL=10.rds")

## Set the values for the RIL production
error.per.sim = NULL
nRILS = 100
L= 100
Clength = 150  ### cM: Chromosome length
posC = sort(runif(L,min=0,max=150))  ### positions
recRates = recRates_estimation(posC)
perc_missing =  0.7  # The fraction of calls to be transformed to missing data
# n.missing = 3
nsim =97


for(perc_missing_loci in perc_missing){
  ### Simulations
  for (ns in 1:nsim){
    ## Define the binary heterozygote $F_2$ generation:
    childGenotype = matrix(c(rep(0, L), rep(1, L), rep(0, L), rep(1, L)), ncol = L, byrow = TRUE)
    ## Run the simulation over $nRILS$
    rils <- rils.missing <- NULL
    for (i in 1:nRILS){
      child = Get_One_RIL(L=L, recRates= recRates, F1Genotypes= childGenotype, rilType = "sib")
      ## Let perc_missing_loci of RIL loci be missing
      child.missing = child[1,]
      child.missing[sort(sample(1:L, L*perc_missing_loci))] = NA 
      ##store the genetic values for the rils
      rils <- cbind(rils, as.character(child[1,]))
      ##store the genetic values for the rils (after NA)
      rils.missing <- cbind(rils.missing, as.character(child.missing))
    }#EndFor
    # cnt <- apply(rils.missing, 2, count.na.between.2.markers)
    ## Delete rils with more than n.missing values between two known markers
    # keep <- cnt<=n.missing
    # rils.missing <- rils.missing[, keep]
    # rils <- rils[, keep]
    
    # rils.missing <- as.data.frame(rils.missing)
    # colnames(rils) <- colnames(rils.missing) <- paste("Ril", 1:dim(rils)[2], sep="")
    
    ####### IMPUTATION OF MISSING DATA  
    ##### With missForest: a non parametric method for imputation of missing data. It uses a random forest algorithm 
    ##### using the observed values to predict the missing values. 
    ##### Random forest algorithm:
    ##### It takes as input a matrix - on columns are the variables and on rows the observations
    t.rils.missing <- t(rils.missing)
    tic()
    rils.forest <- missForest(as.data.frame(t.rils.missing))
    toc()
    # rils.forest$ximp
    # rils.forest$OOBerror
    # comparing actual data accuracy: Proportion of falsely classified entries for the categorical variables 
    rils.forest.err <- mixError(rils.forest$ximp, t.rils.missing, t(rils))
    
    ########################################################################
    ### IMPUTATION WITH THE METHOD EXPLOITING THE EXACT PROBABILITIES 
    #########################################################################
    tic()
    rils.sib.frequencies.imputation <- ImputationWithSibFrequencies(rilsMatrix = rils.missing, recRates = recRates, allQs = allQs, allvpForallup=allvpForallup, posC = posC ) 
    toc()
    rils.sib.frequencies.imputation.error <- mixError(as.data.frame(rils.sib.frequencies.imputation), rils.missing, rils)
    error.per.sim = rbind(error.per.sim , c(perc_missing_loci, rils.forest.err, rils.sib.frequencies.imputation.error))
    cat("ForrestError: ", rils.forest.err, ", OurError: ", rils.sib.frequencies.imputation.error)
  }
}

colnames(error.per.sim) = c("%.Missing.loci", "missForrest", "SibRils")
# Wilcoxon signed-rank test: a non-parametric statistical hypothesis test used to compare two related samples
library("MASS")
wilcox.test(error.per.sim[,"missForrest"], error.per.sim[,"SibRils"], paired=TRUE)

df.arrange = rbind(error.per.sim[, 1:2], error.per.sim[, c(1,3)])
df.arrange = cbind(df.arrange, c(rep("MissForrest", dim(error.per.sim)[1]), rep("SibRils", dim(error.per.sim)[1])))
colnames(df.arrange) <- c("Missing", "Error", "Method")
df.arrange <- as.data.frame(df.arrange)
df.arrange$Error <- as.numeric(as.character(df.arrange$Error))

library(lattice)
library(ggplot2)
p <- ggplot(data = df.arrange, aes(x=Missing, y=Error)) + geom_boxplot(aes(fill=Method))
p <- p + facet_wrap( ~ Missing, scales="free")
p <- p + xlab("% of missing data") + ylab("Error rates") + ggtitle("")
p <- p + guides(fill=guide_legend(title="Method"))
p 

bwplot(Error~Method |Missing,    ## see the powerful conditional formula 
       data=df.arrange,
       between=list(y=1),
       main="")


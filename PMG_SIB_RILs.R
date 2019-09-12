#rm(list=objects())
# ==============================================
####                    s2c
# ==============================================
s2c = function (string) {
  ## Function to transform a string or vector of strings into the vector of successive characters.
  ## arguments: string = String (the string of characters (or vector of strings of characters) 
  ## to be converted). Numbers are considered as characters.
  
  ## returns a vector of characters.
  
  return(unlist(strsplit(string, "")))
}# EndFun s2c


# ===============================================
####                  c2s
# ===============================================
## Function to transforms a vector of characters or a vector of strings of characters into a string.
## arguments: vect = Vector (of characters).

## returns a string of characters.

c2s = function (vect) {
  return(paste(vect, collapse = ""))
}#EndFun c2s


# ===============================================
####            vIndex2vprimeIndex
# ===============================================
## For an arbitrary index v indexing the Q's, return the index v' such that v and v' are equivalent
## (Q(v) = Q(v') because of the symmetries of exchanges of chromosomes) and Q(v') is one of the  
## (non-equivalent) unknowns to be computed (it is in the reduced set N_Q(L) Q's).    
## Function to transform a "v" string of indices (used to access Q(v)) to 
## its equivalent vprime (in the list of size N_Q(L)) by using the system's symmetries:
## v(1) can be forced to 0 and the first occurrence of 2 or 3 can be forced to be 2
## A straightforward algorithm 
## First step: mapping the first index to 0 with the associated consequences 
## if it is 1, we exchange all 0s and 1s  (symmetry: 2 homologues in sister are equivalent) 
## if it is 2, we exchange all 0s and 2s and we also exchange all 1s and 3s
## if it is 3, we exchange all 0s and 3s and we also exchange all 1s and 2s
## where these last 2 lines use both the first symmetry (between homologues) and the exchange symmetry
## between sister and brother, valid if female and male meiosis are statistically identical
## Second step: exchanging if necessary 2 and 3
## if a 3 arises before the first occurrence of 2, then we exchange all 2s and 3s
## otherwise there is nothing to do

## arguments: index = String (the string of integer numbers specifying "v")
## In the SIB application, those numbers belong to 0, 1, 2 and 3.

## returns a string of integer numbers specifying "vprime".

vIndex2vprimeIndex =  function(v_string) {
  index = v_string
  strIndex = unlist(strsplit(index, ""))
  ln_strIndex = length(strIndex)
  #fr= table (strIndex)
  #class(names(fr))
  if (strIndex[1] == "0" & ln_strIndex > 1) {
    ## 0003 to 0002, 0013 to  0012
    if (("0" %in% strIndex  &
         "3" %in% strIndex &  !("2" %in% strIndex))) {
      indexNew = gsub("3", "2", v_string)
      # return(gsub("3", "2", v_string))
      index =  indexNew
    }
    # # 0323 to 0232
    if ("0" %in% strIndex &
        "3" %in% strIndex &
        "2" %in% strIndex &
        (which(strIndex == "2")[1] > which(strIndex == "3")[1])) {
      indexNew = gsub("2", "t", index)
      indexNew = gsub("3", "2", indexNew)
      indexNew = gsub("t", "3", indexNew)
      index =  indexNew
    }
  }
  
  if (strIndex[1] != "0" & ln_strIndex > 1) {
    ## {0,1}
    if ("0" %in% strIndex  &
        ("1" %in% strIndex) &
        !("2" %in% strIndex) & !("3" %in% strIndex)) {
      indexNew = gsub("1", "o", index)
      indexNew = gsub("0", "1", indexNew)
      indexNew = gsub("o", "0", indexNew)
      index = indexNew
    }
    ## {0,2}
    if ("0" %in% strIndex  &
        ("2" %in% strIndex) &
        !("1" %in% strIndex) & !("3" %in% strIndex)) {
      indexNew = gsub("2", "t", index)
      indexNew = gsub("0", "2", indexNew)
      indexNew = gsub("t", "0", indexNew)
      index = indexNew
    }
    ## {0,3}
    if ("0" %in% strIndex  &
        ("3" %in% strIndex) &
        !("1" %in% strIndex) & !("2" %in% strIndex)) {
      indexNew = gsub("3", "2", index)
      indexNew = gsub("2", "t", indexNew)
      indexNew = gsub("0", "2", indexNew)
      indexNew = gsub("t", "0", indexNew)
      index = indexNew
    }
    ## {1,2}
    if ("1" %in% strIndex  &
        ("2" %in% strIndex) &
        !("0" %in% strIndex) & !("3" %in% strIndex)) {
      indexNew = gsub("1", "0", index)
      if (which(s2c(indexNew) == "2")[1] < which(s2c(indexNew) == "0")[1]) {
        indexNew = gsub("2", "t", indexNew)
        indexNew = gsub("0", "2", indexNew)
        indexNew = gsub("t", "0", indexNew)
      }
      index = indexNew
    }
    ## {1,3}
    if ("1" %in% strIndex  &
        ("3" %in% strIndex) &
        !("0" %in% strIndex) & !("2" %in% strIndex)) {
      indexNew = gsub("3", "2", index)
      indexNew = gsub("1", "0", indexNew)
      
      if (which(s2c(indexNew) == "2")[1] < which(s2c(indexNew) == "0")[1]) {
        indexNew = gsub("2", "t", indexNew)
        indexNew = gsub("0", "2", indexNew)
        indexNew = gsub("t", "0", indexNew)
      }
      index = indexNew
    }
    ## {2,3}
    if ("2" %in% strIndex  &
        ("3" %in% strIndex) &
        !("0" %in% strIndex) & !("1" %in% strIndex)) {
      indexNew = gsub("2", "0", index)
      indexNew = gsub("3", "1", indexNew)
      
      if (which(s2c(indexNew) == "1")[1] < which(s2c(indexNew) == "0")[1]) {
        indexNew = gsub("1", "o", indexNew)
        indexNew = gsub("0", "1", indexNew)
        indexNew = gsub("o", "0", indexNew)
      }
      index = indexNew
    }
    ## {0, 1, 2}
    if ("0" %in% strIndex  &
        ("1" %in% strIndex) &
        ("2" %in% strIndex) & !("3" %in% strIndex)) {
      #210 or 201
      if (which(strIndex == "2")[1] < which(strIndex == "1")[1]) {
        indexNew = gsub("2", "t", index)
        indexNew = gsub("1", "3", indexNew)
        indexNew = gsub("0", "2", indexNew)
        indexNew = gsub("t", "0", indexNew)
        
        if (which(s2c(indexNew) == "3")[1] < which(s2c(indexNew) == "2")[1]) {
          indexNew = gsub("2", "t", indexNew)
          indexNew = gsub("3", "2", indexNew)
          indexNew = gsub("t", "3", indexNew)
        }
      }
      # 102 or 120
      if (which(strIndex == "1")[1] < which(strIndex == "2")[1]) {
        indexNew = gsub("1", "o", index)
        indexNew = gsub("0", "1", indexNew)
        indexNew = gsub("o", "0", indexNew)
      }
      index = indexNew
    }# End{0, 1, 2}
    
    ## {0, 1, 3}
    if ("0" %in% strIndex  &
        ("1" %in% strIndex) &
        ("3" %in% strIndex) & !("2" %in% strIndex)) {
      index = gsub("3", "2", index)
      #210 or 201
      if (s2c(index)[1] == "2") {
        indexNew = gsub("2", "t", index)
        indexNew = gsub("1", "3", indexNew)
        indexNew = gsub("0", "2", indexNew)
        indexNew = gsub("t", "0", indexNew)
        if (which(s2c(indexNew) == "3")[1] < which(s2c(indexNew) == "2")[1]) {
          indexNew = gsub("2", "t", indexNew)
          indexNew = gsub("3", "2", indexNew)
          indexNew = gsub("t", "3", indexNew)
        }
      }
      # 102 or 120
      if (s2c(index)[1] == "1") {
        indexNew = gsub("1", "o", index)
        indexNew = gsub("0", "1", indexNew)
        indexNew = gsub("o", "0", indexNew)
      }
      index = indexNew
    }# End{0,1, 3}
    
    ##{0, 2, 3}
    if ("0" %in% strIndex  &
        ("2" %in% strIndex) &
        ("3" %in% strIndex) & !("1" %in% strIndex)) {
      indexNew = gsub("3", "1", index)
      indexNew = gsub("2", "o", indexNew)
      indexNew = gsub("0", "2", indexNew)
      indexNew = gsub("o", "0", indexNew)
      
      if (s2c(indexNew)[1] != "0") {
        indexNew = gsub("1", "o", indexNew)
        indexNew = gsub("0", "1", indexNew)
        indexNew = gsub("o", "0", indexNew)
      }
      index = indexNew
    }# End{0, 2, 3}
    
    ## {1, 2, 3}
    if ("1" %in% strIndex  &
        ("2" %in% strIndex) &
        ("3" %in% strIndex) & !("0" %in% strIndex)) {
      ## 123, 132 to 023 or 032
      if (strIndex[1] == "1") {
        indexNew = gsub("1", "0", index)
        if (which(s2c(indexNew) == "2")[1] > which(s2c(indexNew) == "3")[1]) {
          indexNew = gsub("2", "t", indexNew)
          indexNew = gsub("3", "2", indexNew)
          indexNew = gsub("t", "3", indexNew)
        }
      }
      ## {231, 321} to 021 or 012
      if (strIndex[1] == "2") {
        indexNew = gsub("1", "0", index)
        indexNew = gsub("2", "t", indexNew)
        indexNew = gsub("3", "1", indexNew)
        indexNew = gsub("0", "2", indexNew)
        indexNew = gsub("t", "0", indexNew)
      }
      
      ## {321, 312} to 012 or 021
      if (strIndex[1] == "3") {
        # sawp 2 and 3 and put 2 = 0
        indexNew = gsub("2", "t", index)
        indexNew = gsub("3", "2", indexNew)
        indexNew = gsub("t", "3", indexNew)
        indexNew = gsub("2", "0", indexNew)
        
        # swap 1 and 3
        indexNew = gsub("1", "o", indexNew)
        indexNew = gsub("3", "1", indexNew)
        indexNew = gsub("o", "3", indexNew)
        
        # replace 3 by 2
        indexNew = gsub("3", "2", indexNew)
      }
      index = indexNew
    } # End{1, 2, 3}
    
    ## {0, 1, 2, 3}
    if ("0" %in% strIndex  &
        ("1" %in% strIndex) & ("2" %in% strIndex) & ("3" %in% strIndex)) {
      # 1023,  1230 to 0123 and 0231
      if (strIndex[1] == "1") {
        indexNew = gsub("1", "o", index)
        indexNew = gsub("0", "1", indexNew)
        indexNew = gsub("o", "0", indexNew)
        
        if (which(s2c(indexNew) == "2")[1] > which(s2c(indexNew) == "3")[1]) {
          indexNew = gsub("2", "t", indexNew)
          indexNew = gsub("3", "2", indexNew)
          indexNew = gsub("t", "3", indexNew)
        }
      }
      ## 2103, 2310 to 0231 and 0132
      if (strIndex[1] == "2") {
        ## swap 2 and 0
        indexNew = gsub("2", "t", index)
        indexNew = gsub("0", "2", indexNew)
        indexNew = gsub("t", "0", indexNew)
        ## swap 1 and 3
        indexNew = gsub("1", "r", indexNew)
        indexNew = gsub("3", "1", indexNew)
        indexNew = gsub("r", "3", indexNew)
        
        if (which(s2c(indexNew) == "2")[1] > which(s2c(indexNew) == "3")[1]) {
          indexNew = gsub("2", "t", indexNew)
          indexNew = gsub("3", "2", indexNew)
          indexNew = gsub("t", "3", indexNew)
        }
      }
      
      ## 3120, 3210, 3012, to 0213 and 0123, 0231
      if (strIndex[1] == "3") {
        indexNew = gsub("2", "t", index)
        indexNew = gsub("3", "2", indexNew)
        indexNew = gsub("t", "3", indexNew)
        indexNew = gsub("2", "t", indexNew)
        indexNew = gsub("0", "2", indexNew)
        indexNew = gsub("t", "0", indexNew)
        indexNew = gsub("3", "r", indexNew)
        indexNew = gsub("1", "3", indexNew)
        indexNew = gsub("r", "1", indexNew)
        
        if (which(s2c(indexNew) == "2")[1] > which(s2c(indexNew) == "3")[1]) {
          indexNew = gsub("2", "t", indexNew)
          indexNew = gsub("3", "2", indexNew)
          indexNew = gsub("t", "3", indexNew)
        }
      }
      index = indexNew
    }#End{0, 1, 2, 3}
  }# EndIf non-Start-Zero
  
  if (sum(strIndex == "1") == ln_strIndex |
      sum(strIndex == "2") == ln_strIndex |
      sum(strIndex == "3") == ln_strIndex) {
    index = c2s(rep(0, ln_strIndex))
  }
  if ((
    sum(strIndex == "1") == ln_strIndex |
    sum(strIndex == "2") == ln_strIndex |
    sum(strIndex == "3") == ln_strIndex
  ) & ln_strIndex == 1) {
    index = "0"
  }
  return(index)
}#EndFun


# ===============================================
####          all_IBD_Haplotypes
# ===============================================
## Function to construct the matrix of all 4^L possible "u" vectors ("u" = vector of indices labeling the 
## IBD inheritance probability Q(u), one "u" per line in the matrix) for L loci. 
## For convenience, "u" is thought of as an IBD haplotype. 
## The number of these vectors is 4^L and they are generated in alphanumeric order.
## arguments: L = integer (the number of loci).

## returns a matrix of all the possible IBD haplotypes, implemented as a recursive function.
all_IBD_Haplotypes = function (L){
  g = c(0:3) # labels the gametes or chromosomes (0, 1 for sister, 2, 3 for brother) 
  n = length(g)
  res = function(n, L, g){
    if (L == 1) 
      matrix(g, n, 1)
    else {
      subRes = Recall(n, L - 1, g)
      cbind(rep(g, rep(nrow(subRes), n)), matrix(t(subRes), ncol = ncol(subRes), nrow = nrow(subRes) * n, byrow = TRUE))
    }
  }
  res(n, L, g[1:n])
}#EndFun


# ===============================================
####                  allRecombRates
# ===============================================
## Function to compute all the possible pairwise recombination rates.
## arguments: recRates = Vector (vector of real numbers and of length L-1 that gives
## the recombination rates for the successive L-1 intervals assuming no genetic interference).

## returns a dataframe of L(L-1)/2 rows and two columns: 
## the first column is the string rll' (with "l" and "l'" being associated with the left and 
## right markers of the considered interval)
## the second column gives the value of that recombination rate r_ll'.

allRecombRates = function(recRates) {
  L_loci=length(recRates)+1   
  numTotalRec = ((L_loci-1)*L_loci) %/% 2  # All possible pairs of distinct loci
  totalRec = matrix(data = 0,
                    nrow = numTotalRec,
                    ncol = 3)
  for (i in 1:length(recRates)) {
    totalRec[i, 1] = i
    totalRec[i, 2] = i + 1
    totalRec[i, 3] = recRates[i]
  }
  g = length(recRates) + 1
  if (length(recRates) != 1) {
    for (i in 1:(numTotalRec - 1)) {
      if (g < numTotalRec + 1) {
        for (j in (i + 1):numTotalRec) {
          if (totalRec[i, 2] == totalRec[j, 1]) {
            totalRec[g, 1] = totalRec[i, 1]
            totalRec[g, 2] = totalRec[j, 2]
            totalRec[g, 3] = totalRec[i, 3] + totalRec[j, 3] - 2 * totalRec[i, 3] *
              totalRec[j, 3]
            g = g + 1
          }
          
          if (totalRec[i, 1] == totalRec[j, 2]) {
            totalRec[g, 1] = totalRec[j, 1]
            totalRec[g, 2] = totalRec[i, 2]
            totalRec[g, 3] = totalRec[i, 3] + totalRec[j, 3] - 2 * totalRec[i, 3] *
              totalRec[j, 3]
            g = g + 1
          }
        }
      }
    }
  }
  ## Now create the recombination rate symbols rll' that will used in the theoretical framework
  recIndex = apply(cbind(rep("r", dim(totalRec)[1]), totalRec[,1], totalRec[,2]),1, c2s) 
  ## Put into a data.frame form that will used in later computations
  totalRec = data.frame(recIndex, as.numeric(totalRec[,3]), stringsAsFactors = F )
  colnames(totalRec) =c("r", "value")
  
  return(totalRec)
}#EndFun



# ======================================
####            getAllTuples
# ======================================
## function to produce all tuples, that is all vectors of n elements of a set that is provided
## arguments: vecSet = vector of the elements to be used
##                 tupleSize = integer (number of elements in a tuple)
getAllTuples <- function(vecSet,tupleSize) {
  return(expand.grid(rep(list(vecSet),tupleSize)))
}#EndFun


# ======================================
####            contributingQs
# ======================================
## function to provide all the non-equivalent v's that have a non-zero value of T[ u -> v ]
## where T[ u -> v ] is the transition probability between IBD indices when
## going from F1 to F2 (cf. notation used in the publication)
## arguments: u_index_string = String  (the string specifying the IBD inheritance "u" at the F1 generation).

## returns vector of strings, that is all the possible "v"s compatible with the input "u", but where these
## "v"s are taken from the list of non-equivalent ones (thus the call to vIndex2vprimeIndex).

## Method: first produce all 2^L tuples of size L_loci with binary entries, thought of as a matrix
## Then, going over all loci (collumns of that matrix),
## interpret the binary entry (0 vs 1) as the first vs second entry of the possible values of "v"
## and reset those entries using the rules for u -> v (for non zero entries of T[ u -> v ])
## Because these operations are done on whole collumns, they are relatively efficient in terms of CPU
## The CPU time is mainly spent on mapping the "v"'s onto "vprime"'s (non-equivalent Q's)

contributingQs =  function(st = "02102130") {
  x = s2c(st)
  L_loci = length(x)
  
  allPs = getAllTuples(c(0,1),L_loci)      # produces the 2^L lines, each containing a binary tuple 
  # Then, collumn by collumn, apply the mapping 0 -> 1st choice, 1 -> second choice for the "v" possibilities
  for (j in 1:L_loci) {
    uj = x[j]
    if(uj %in% c(0,1)){                     # The rule for the possible "v"'s given "u"
      indx1 = which(allPs[,j] == 1)
      allPs[indx1, j] =2
    }
    if(uj %in% c(2,3)){
      indx2 = which(allPs[,j] == 0)         # The rule for the possible "v"'s given "u" 
      indx3 = which(allPs[,j] == 1)
      allPs[indx2, j] =1
      allPs[indx3, j] =3
    }  
  }#EndFor
  
  allPs = allPs[allPs[,1]==0,]             # optimization: for input string that is in non-equivalent list, gain factor 2 for rest
  res = apply(data.frame(allPs), 1, c2s)
  res = mapply(function(x) vIndex2vprimeIndex(x),res)  # where most of the CPU time is spent
  res = unique(res)              # remove redundancies in "v"s via unique
  return(res)
}#EndFun


# ========================================================================================
#### Compute the transition probability T[ u -> v ] used in the self-consistent equations
# ========================================================================================
## function to compute the transition probability for inheriting the vector of haplotype v in
## the F2 generation given the haplotype vector u in the F1 generation: T[ u -> v ]
## arguments :        "v" = Vector (of haplotypes in F2 generation).
##                    "u" = Vector (of haplotypes in F1 generation).

## Return: string of the transition probability for inheriting the vector of haplotype "v" in 
## the F2 generation from the vector of haplotype "u" in the F1 generation.

computeWeight = function(v, u) {
  w = cbind(v, u, rep(NA, length(v)))
  posU = matrix(c(0, 1, 2, 3), ncol = 2, nrow = 2, byrow = TRUE)
  jj = 1
  i = 0
  evalength = 0
  while (i <= 3) {
    index = which(v == i & u %in% posU[jj, ])
    linex = length(index)
    evalength = linex + evalength
    if (jj == 2){
      i = i + 1
      jj = 0
    }
    jj = jj + 1
    if (all(linex)) {
      w[index[1], 3] = 0.5
      if (linex != 1) {
        for (k in 2:linex) {
          if (w[index[k], 2] == w[index[k - 1], 2])
            w[index[k], 3] =
              paste("1-r", index[k - 1], index[k], sep = "")
          
          if (w[index[k], 2] != w[index[k - 1], 2])
            w[index[k], 3] =
              paste("r", index[k - 1], index[k], sep = "")
        }
      }
    }
    if (evalength == length(v))
      break
  }
  w[, 3] = paste("(", w[, 3], ")", sep = "")
  return(list(w = w, we = paste(c(2, as.vector(
    w[, 3]
  )), collapse = "*")))
}#EndFun


# ==============================
#  Evaluate matrix 
# ==============================
evalMatrix = function(A, recRates){
  ## function to "Evaluate" the input matrix, substituting the numerical values
  ## of the recombination rates into the algebraic expressions in the input matrix.
  ## arguments :        A =  the matrix used for the evaluation (substitution)
  #              recRates = vector of all the recombination rates.
  
  ## Returns a numerical matrix.
  cat("\n 5. Evaluate the input matrix, substituting the numerical values of the recombination rates: \n ")
  
  recMat = allRecombRates(recRates)  # because only adjacent recombination rates are passed
  # while the matrix involves recombination rates for all intervals
  ## Assign a substitution value to a each recombination rate based on this recMat matrix
  for (i in 1:dim(recMat)[1]){
    assign( recMat[i,1], recMat[i,2])
    cat(recMat[i,1], "=", recMat[i,2], "    ")
  }
  cat("\n")
  eMatv = as.vector(unlist(A))
  index = which(eMatv!="0" & eMatv!="1" & eMatv!="-1" & eMatv!="2")
  indexEvl = evals(eMatv[index])
  eMatv[index] = indexEvl
  eMatv = as.numeric(eMatv)
  neweMat = matrix(eMatv, ncol = ncol(A), byrow = F)
  return(neweMat)
}#EndFun

#==================================
##  all system inheritance indexes
# =================================    
systemVar = function(L){
  ## function to provide for each "u" index the associated "u_prime" index labeling Q's and also the
  ## N_Q(L) "u_prime" indices, that is the indices of the non-equivalent Q's for L loci
  ## arguments :         L = integer (number of loci)
  
  ## return a list of the "v"s and a list of the "vprime"s
  
  cat("\n 1. Find the inheritance indices that are contributing to this system, please wait: ... \n")
  allus = all_IBD_Haplotypes(L)    # 4^L of these "u" indices for IBDs in SIB RILs
  allus = apply(allus, 1, c2s)
  alluprimes = unlist(lapply(allus, vIndex2vprimeIndex))   # give the u_prime rather than the u
  # will be useful for constructing equations
  uniqueuprimes = unique(alluprimes)    # N_Q(L) non-equivalent Q's (our unknowns)
  #}#EndFor
  cat("\n")
  return(list(symQs = uniqueuprimes, indicesAllQs = alluprimes))
}#EndFun


# =======================================
# The v_primes for all possible u_primes
# =======================================
## function to calculate all the contributing v_prime values contributing to the self-consistent
## equation for Q(u_prime) (prime for the restriction to non-equivalent Q's), over all N_Q(L) u_prime strings
## arguments :         indicesNonEquivalentQs =  vector of strings representing the "u_prime"'s
##                          L = number of loci
## Return a list of all non-equivalent Q's (via their v_primes) contributing to these equations

allvprimeForEachuprime = function(nonEquivalentQs){
  cat("\n 2. calculate all the the contributing v_prime values contributing to the self-consistent equation for Q(u_prime):\n")
  N_QofL = length(nonEquivalentQs)
  contributingNonEquivalentQs = vector("list", N_QofL)
  cpt = 10               # to notify the user when each 10% of the calculation is done
  for (vr in 1:N_QofL){
    contributingNonEquivalentQs[[vr]] = contributingQs(nonEquivalentQs[vr])
    ## Print percentage of accomplished vertices
    if ( ((vr/N_QofL)*100)>=cpt ) {
      cat(cpt, "% ",sep="")
      cpt=cpt+10
    }
  }#EndFor
  return(contributingNonEquivalentQs)
}#EndFun


# ======================================================================
## Function twoWayRILsib to solve for the IBD probabilities in SIB RILs
# ======================================================================
twoWayRILsib = function(L, indicesNonEquivalentQs, indicesAllQs, all_vpForAll_up, multiplicityQs){
  ## function to specify the linear system (A Q = B) used for solving 2-way RIL multilocus
  ## probabilities when one has L loci.
  ##                L =  integer (the number of loci)
  ##           indicesNonEquivalentQs = vector of all the non-equivalent "u" indices of the Q's for L loci
  ##         indicesAllQs = vector of all (including equivalents) "u" indices of the Q's for L loci
  ##             all_vpForAll_up = list of vprime vectors to consider in T[ u -> v ] for 
  ##                               each uprime vector (prime for non-equivalent)
  
  ## Returns A and B of the linear system A Q = B, A being an N_Q(L) x N_Q(L) matrix in algebraic form (not numerical)
  
  A = matrix(0, ncol = length(indicesNonEquivalentQs), nrow = length(indicesNonEquivalentQs) )
  colnames(A) = indicesNonEquivalentQs
  
  ## For L = 1, the result is known
  if(L == 1){
    cat("# ====== 1 - Locus ====== #","\n")
    ## The values of Q(0) = Q(1) = Q(2) = Q(3) = 0.25
    rownames(A) = c("SumQs")
    A[1,] = table(indicesAllQs)
    cat("\n 1. The First equation in the system is: \n")
    cat(paste(paste(as.vector(multiplicityQs),"Q(", names(multiplicityQs),")", collapse = "+", sep = ""),"=1\n", sep = ""))
    cat("\n done.")
  }else{
    # or If you want to compute the inheritance indices contributing to the system from scratch
    cat("\n # ======", L," - Loci ====== #", "\n")
    length_indicesNonEquivalentQs = length(indicesNonEquivalentQs)
    ## The first equation of the system is Sum (Q)= 1, specify it in terms of the non-equivalent Q's 
    rownames(A) = c("SumQs",indicesNonEquivalentQs[-length_indicesNonEquivalentQs])
    A[1,] = table(indicesAllQs)
    cat("\n 3. The First equation in the system is: \n")
    cat(paste(paste(as.vector(multiplicityQs),"Q(", names(multiplicityQs),")", collapse = "+", sep = ""),"=1\n", sep = ""))
    
    ## start computing the self-consistent equation for each non-equivalent Q (each unknown) 
    cat("\n 4. Computing the self-consistent equations: ... \n")
    for (sc in 1:(length_indicesNonEquivalentQs-1) ){
      ## retrieve all the possible IBD "haplotypes" (the possible v's) for each haplotype (u = indicesNonEquivalentQs) 
      eq = all_vpForAll_up[[sc]]
      ln_eq= length(eq)
      for(q in 1:ln_eq){
        ## Compute the transition probability for going u (in F1) to v (in F2)
        we = computeWeight(v = as.numeric( s2c(eq[q])), u = as.numeric(s2c(indicesNonEquivalentQs[sc])) )$we 
        ## combine the similar terms together
        if(indicesNonEquivalentQs[sc] == eq[q]) we = paste(we,-1,sep = "");
        A[sc+1, eq[q]] = we
      }
      #cat(" ",sc,">> ")
      cat(sc, "of", (length_indicesNonEquivalentQs-1), "\r") 
      
      ## rearrange the self-consistent equation to be homogeneous
      if(!(indicesNonEquivalentQs[sc] %in% eq)) A[sc+1, indicesNonEquivalentQs[sc]] = -1
    }#EndFor
    cat("\n done \n")
  }#EndIFelseA
  
  # if you want to write the A matrix
  # write.table(A, paste("QmatL=",L,".txt",sep = ""))
  ## The vector B in system AQ = B 
  B = rep(0, length_indicesNonEquivalentQs)
  ## sum(Q_i) = 1
  B[1] = 1
  return(list(A = A, B = B))
}#EndFun


# =================================================================================
## Function to convert (decimal) number to its binary representation (as a vector) 
## Note: done using recursive call to this intToBin function
# =================================================================================
# Input: number = integer (thought of as decimal)
# Output: binary representation as a vector of bits (b_k 2^k, b_{k-1} 2^{k-1}, ..., b_0 2^0)
intToBin <- function(number){
  if (number == 1)
    1
  else if (number == 0)
    0
  else {
    mod <- number %% 2
    c(intToBin((number-mod) %/% 2), mod)
  }
}


# =============================================================================================================
## Function to pad with 0s a decimal number converted to its binary representation in a vector of length Lloci
## Note: padding after calling intToBin
## most significant bits on left (the index 1 of the array)
## Result is truncated to fit into the box of size of the array of length Lloci, using least significant bits
# =============================================================================================================
# Input: number = integer (thought of as decimal) 
#        number = number of loci (length of vector of 0s and 1s) 
# Output: binary representation as a vector of bits (b_{L-1} 2^{L-1}, b_{k-1} 2^{k-1}, ..., b_0 2^0)
paddedIntToBin <- function(number,vectorLength){
  rawBinaryVector = intToBin(number)
  rawLength = length(rawBinaryVector)
  paddedVector = rep(0,vectorLength)
  for(i in 1:min(c(vectorLength,rawLength))) {
    paddedVector[vectorLength-i+1]=rawBinaryVector[rawLength-i+1]
  }
  return(paddedVector)
}


# =============================================================================
## convert Q's (RIL IBD probabilities) to RIL multilocus genotype probabilities
# =============================================================================
## function outputing the 2^L probabilities of multilocus SIB RIL genotypes given the
## SIB RIL multilocus IBD probabilities represented by the | Q_L | distinct Q's
## Input: L = number (of loci) and nonEquivalentQs = Vector (of the | Q_L | IBD probabilities)

## Return: 2^L RIL multilocus genotype probabilities

QsToGenotypeProbabilities = function(L, nonEquivalentQs){
  cat("\n 6. Going from  Q's (RIL IBD probabilities) to RIL multilocus genotype probabilities: \n")
  cpt = 10
  vecGenoProbs =rep(0, 2^L)
  vecZeroes = rep(0, 2^L)
  
  for(iGenotypeRil in 0:(2^L-1)){ # loop over all possible RIL genotypes 
    binaryGenotype  =  intToBin(iGenotypeRil)
    if(length(binaryGenotype)< L){
      binaryGenotype = c(vecZeroes[1:(L-length(binaryGenotype))], binaryGenotype) 
    }
    for (iVecjs in 0:(2^L-1) ){
      vecjs = intToBin(iVecjs)
      if(length(vecjs)< L){
        vecjs = c(vecZeroes[1:(L-length(vecjs))], vecjs) 
      }
      vecis = binaryGenotype + 2*vecjs 
      Qindex = c2s(vecis)
      ind = vIndex2vprimeIndex(Qindex)
      vecGenoProbs[iGenotypeRil + 1] = vecGenoProbs[iGenotypeRil + 1] + nonEquivalentQs[ind]
      if ( ((iGenotypeRil/(2^L-1))*100)>=cpt ) {
        cat(cpt, "% ",sep="")
        cpt=cpt+10
      }
    }# EndForLoop1
  }#EndForLoop2
  cat("\n")
  return(vecGenoProbs)
}#EndFun


# ============================================================
## convert a number from base 2 (binary) to base 10 (decimal) 
# ============================================================
# Function from binary representation to decimal number
binTodec = function(binVector){
  num_bits = length(binVector)
  number = 0
  for (i in 1:num_bits){
    number = number + binVector[i] * 2^(num_bits-i)
  }
  return(number)
}#EndFun


# ============================================================================
## Simulate a meiosis using the no interference model (Haldane), producing one 
## gamete (a chromosome) from two homologous chromosomes
# ============================================================================
Make_Gamete<- function(L, recRates, homologue0, homologue1){
  gamete <-rep(0,L)
  chroms <-rbind(homologue0,homologue1)
  ifelse (runif(1) > 0.5, gamete[1] <- chroms[k<-1,1], gamete[1]<-chroms[k<-2,1])
  for (iLoc in 2:L){
    rand <- runif(1)
    if (rand > recRates[iLoc-1]){
      gamete[iLoc] <- chroms[k, iLoc]
    } else {
      k = 3-k # flip the value of k to the other value
      gamete[iLoc] <- chroms[k, iLoc]
    }
  }	
  return(gamete)
}#EndFun


# ============================================================
## Produce an individual of the next generation of inbreeding
# ============================================================
Make_Next_Generation = function(L, recRates, childGenotype, rilType = "sib"){
  
  if(rilType == "sib"){
    gamete0 = childGenotype[1,]; gamete1 = childGenotype[2,]; 
    gamete2 = childGenotype[3,]; gamete3 = childGenotype[4,];
    
    newGamete0 = Make_Gamete(L, recRates, gamete0, gamete1)
    newGamete1 = Make_Gamete(L, recRates, gamete2, gamete3)
    newGamete2 = Make_Gamete(L, recRates, gamete0, gamete1)
    newGamete3 = Make_Gamete(L, recRates, gamete2, gamete3)
    childGenotype = matrix(c(newGamete0, newGamete1, 
                             newGamete2, newGamete3), 
                           ncol = L, byrow = TRUE)
    
  }else if(rilType == "self"){
    gamete0 = childGenotype[1,]
    gamete1 = childGenotype[2,]; 
    
    newGamete0 = Make_Gamete(L, recRates, gamete0, gamete1)
    newGamete1 = Make_Gamete(L, recRates, gamete0, gamete1)
    childGenotype = matrix(c(newGamete0, newGamete1), 
                           ncol = L, byrow = TRUE)
  }else{
    stop("Please specify properly the type of mating ... ")
  }
  
  
  return(childGenotype)
}#EndFun


# ==========================================================
## check whether an individual has all of its alleles fixed
# ==========================================================
Is_childGenotype_Fixed = function(childGenotype){
  L = dim(childGenotype)[2]
  n = dim(childGenotype)[1]
  
  for (i in 2:n){
    if (sum(childGenotype[i,] == childGenotype[1,]) != L){
      return(FALSE)
    }
  }#EndFor
  return(TRUE)
}#EndFun


# ===================================================================
## simulate production of one RIL individual and return its genotype 
# ===================================================================
Get_One_RIL = function(L,recRates,F1Genotypes, rilType = "sib"){
  # Initialize the RIL construction 
  if(rilType == "self") childGenotype = F1Genotypes[1:2,];
  if(rilType == "sib")  childGenotype = F1Genotypes;
  
  fixed=FALSE
  while (fixed == FALSE){
    childGenotype = Make_Next_Generation(L, recRates, childGenotype, rilType)
    fixed = Is_childGenotype_Fixed(childGenotype)
  }#EndWhile
  
  return(childGenotype)
}#EndFun

#===================================================================================
# An example of using the functions to obtain the IBD or RIL genotype probabilities
#===================================================================================
t1 = Sys.time()
library(eply)
library(rlist)
library(rmarkdown)

L_loci = 4                        # Number of loci
recRates = c(0.5, 0.3, 0.4)       # Vector of length L_loci-1 of the recombination rates of successive intervals

# L_loci = 5
# recRates = rep(0.1,L_loci-1)

allQs = systemVar(L_loci)                          # Gets all indices of the variables to work with:
nonEquivalentQs = allQs$symQs                      # the N_Q(L) non-equivalent Qs
allQsMappedToNonEquivalent = allQs$indicesAllQs    # but also the 4^L Qs after mapping them to the non-equivalent ones
multiplicityQs = table(allQsMappedToNonEquivalent) # multiplicity factor (the number of times a non-equivalent Q arises)

# Construct the list of all v_primes given the u_primes, used to construct the self-consistent equations
allvpForallup = allvprimeForEachuprime(nonEquivalentQs)

# The analytic expressions of the system of linear equations to be solved
analyticEquations = twoWayRILsib(L_loci, nonEquivalentQs, allQsMappedToNonEquivalent, allvpForallup, multiplicityQs)
Amatrix = analyticEquations$A
Bvector = analyticEquations$B

# Numerical treatment
numericAmatrix = evalMatrix(A = Amatrix, recRates = recRates) # Substitute the numerical values of the recombination rates
solution = solve(numericAmatrix, Bvector)  # solution for the N_Q(L) unknown (non-equivalent) Qs
t2 =Sys.time()
t2-t1
names(solution) = nonEquivalentQs
sumAllIBDProbabilities = sum(solution*multiplicityQs)    # should be 1

allProbabilitiesOfRilGenotypes= QsToGenotypeProbabilities(L_loci, solution)  # Provides the 2^L
# RIL genotype probabilities
sumAllGenotypeProbabilities = sum(allProbabilitiesOfRilGenotypes)            # should be 1

## The solution
cat("\n The Results: \n")
print(solution)


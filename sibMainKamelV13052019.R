# For 8 loci Time difference of 5.290853 mins
# For 7 loci Time difference of 32.37045 secs
# For 6 loci Time difference of 4.602918
# For 5 loci Time difference of 0.5602674 secs
# For 4 loci Time difference of 0.1071317 secs
# For 3 loci Time difference of 0.05963397 secs
# For 2 loci Time difference of 0.04218674 secs
# locus one

    rm(list=objects())
    # source("http://bioconductor.org/biocLite.R")
    # biocLite("gpuR")
    library(eply)
    library(Matrix)
    library(rlist)

    source('~/Documents/SVmapNew/MarianyelaProject/R/sibFunKamel.R')
    ## =================================================
    ## =================== Start Inpute ================
    ## =================================================
    ## Inpute number of loci
    L = 7
    nQs = 2^(L-2) * (2^(L-1) + 1)
    ## Input the successive recombination rates
    recRates = c(0.5, 0.3, 0.4)
    recRates = c(0.4, 0.2, 0.2, 0.1, 0.1, 0.1, 0.1)
    ## If you want to read the variables contributing to the system from the file
    # setwd("~/Documents/SVmapNew/MarianyelaProject/R")
    allvar = list.load("allVarTillL=10.rds")
    SCHPE = list.load("allContrVarTillL=10.rds")
    varNom = allvar[[L]]$symQs
    nonSymQs = allvar[[L]]$nonsymQs
    scEq = SCHPE[[L]]
    ## =================================================
    ## =================== End Inpute ==================
    ## =================================================
    start.time = Sys.time()
    cat("\n")
    ## The main Functions
    ## If you want to compute all the possible haplotype in the sytem 
    # allVar = systemVar(L)
    # varNom = allVar$symQs
    # nonSymQs = allVar$nonsymQs
    # scEq = allCrossOver(varNom = varNom)
  
    res = twoWayRILsib(L, varNom, nonSymQs, scEq)
    
    A = res$A
    B = res$B
    end.time = Sys.time()
    time.taken = end.time - start.time
    ## numerical resultes 
    ## Matrix of all possible recombination rates between any two loci
    Anum = evalMatrix(A = A, recRates = recRates)
    sol = solve(Anum, B)
    names(sol) = varNom
    sol
    ## print the time taken
    time.taken
    ## close the clusters 
    setwd("~/Documents")
    ## save the matrix A
    #write.table(A, paste("resforL=",L,".txt",sep = ""), quote=FALSE, eol="\\\\\n", sep=" & ")
    ## verify that the sum over the Qs equal to one
    QsProbs = rbind(sol, table(nonSymQs))
    sum(QsProbs[1,] * QsProbs[2,])
    
    
   
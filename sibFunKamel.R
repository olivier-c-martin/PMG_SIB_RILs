    # ==============================================
    ##                   s2c
    # ==============================================
    s2c = function (string) {
      ## Function to transforms a vector into a string.
      ## arguments : string = String: the string of characters or numbers that required to convert.
      
      ## return a vector of characters.
      
      return(unlist(strsplit(string, "")))
    }# EndFun s2c
    
    # ===============================================
    ####                  c2s
    # ===============================================
    ## Function to transforms a vector into a string.
    ## arguments : vect = Vector of characters of numbers or both.
    
    ## return a string of characters or numbers or both.
    
    c2s = function (vect) {
      return(paste(vect, collapse = ""))
    }#EndFun c2s
    
    #### Return the index to its stander one after considering all the equevelent relations
    
    # ===============================================
    ####            standerIndex
    # ===============================================
    ## function to transform a string to its standard form after considering the symmetry.
    ## arguments : index = astring of integer numbers.
    
    ## return a string of integer numbers.
    
    standerIndex =  function(index) {
      strIndex = unlist(strsplit(index, ""))
      ln_strIndex = length(strIndex)
      #fr= table (strIndex)
      #class(names(fr))
      if (strIndex[1] == "0" & ln_strIndex > 1) {
        ## 0003 to 0002, 0013 to  0012
        if (("0" %in% strIndex  &
             "3" %in% strIndex &  !("2" %in% strIndex))) {
          indexNew = gsub("3", "2", index)
          # return(gsub("3", "2", index))
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
    ##          matrix4powL
    # ===============================================
    ## function to compute all the possible haplotype given the total number of loci which is equal to 4^L, where L is the number of loci.
    ## arguments : L = integr number reprersent the number of loci.
    
    ## return a matrix of all the possible haplotypes.
    matrix4powL = function(L) {
      fourPowL = 4^L
      completeMat = matrix(data = 0,
                               nrow = fourPowL,
                               ncol = L)
      for (i in 0:(fourPowL - 1)) {
        completeMat[1 + i, ] = quatNumber(i, L)
      }
      return(completeMat)
    }#EndFun
    
    # ================================================
    ##            quatNumber
    # ===============================================
    
    quatNumber = function(number, space) {
      quat = rep(0, space)
      n = 0
      k = number
      while (k != 0) {
        while (number != 0) {
          if (number %% 4 != 0) {
            number = (number - 1)
          }
          else {
            number = number / 4
            n = n + 1
          }
        }
        pos = space - n
        quat[pos] = quat[pos] + 1
        number = k - (4 ^ n)
        k = number
        n = 0
      }
      return(quat)
    }#EndFun
    
    
    
    # ===============================================
    ##                  allRecomRates
    # ===============================================
    
    ## function to compute all the possible pairwise recombination rate.
    ## arguments :   recRates = aa vector of real numbers and of length L, that represent the successive recombination rate for L loci.

    ## return a new matrix of three columns, the first col represent the first index "l", the second col represent the second index "l'" and the third col represnt the value of that recombation rate r_ll'.
    
    allRecomRates = function(recRates) {
      numTotalRec = fatt(length(recRates))
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
      ## creat the recombination rates symbles that will use in the compuation
      recIndex = apply(cbind(rep("r", dim(totalRec)[1]), totalRec[,1], totalRec[,2]),1, c2s) 
      ## Rearrange the recobination in the stander form that will use in the compuation
      totalRec = data.frame(recIndex, as.numeric(totalRec[,3]), stringsAsFactors = F )
      colnames(totalRec) =c("r", "value")
      
      return(totalRec)
    }#EndFun
    
    
    # ================================================
    ####  fatt
    # ===============================================
    ## function used for Conditional Element Selection 
    ## arguments :   x = an object which can be coerced to logical mode.
    
    ## returns a value with the same shape as test which is filled with 
    ## elements selected from either yes or no depending on whether the 
    ## element of test is TRUE or FALSE.
    fatt = function(x) {
      ifelse(x == 1, 1, x + fatt(x - 1))
    }#EndFun
    
    # ======================================
    ##            contributedQs
    # ======================================
    ## function to compute all the all the possible meiotic product of string st.
    ## arguments :   st =  string, a string that inherited from F1 generation to F2 generation.

    ## Return: vector of strings, the possible meiotic product of string st.
    
    contributedQs =  function(st = "002") {
      x = s2c(st)
      ln_x = length(x)
      xp = c()
      for (i in 1:ln_x){
        if (x[i] %in% c(0, 1))
          xp = c(xp, c(0, 2))
        
        if (x[i] %in% c(2, 3))
          xp = c(xp, c(1, 3))
      }
      
      allPs = t(combn(xp, length(x)))
      newIndex = NULL
      for (j in 1:ln_x) {
        if (x[j] %in% c(0, 1))
          index = which(allPs[, j] == 0 | allPs[, j] == 2)
        
        if (x[j] %in% c(2, 3))
          index = which(allPs[, j] == 1 | allPs[, j] == 3)
        
        allPs = allPs[index, ]
      }
      res = apply(data.frame(allPs), 1, c2s)
      res = lapply(res, standerIndex)
      res = unlist(res)
      res = unique(res)
      return(res)
    }#EndFun
    
    
    # ===============================
    # Compute the weight of SCHP
    # ===============================
    ## function to compute the transition probability from inheriting the vector of haplotype y in F1 generation to F2  generation.
    ## arguments :        v =  vector of haplotypes in F2 gneration.
    ##                    u = vector of haplotypes in F1 gneration.
    
    ## Return: string of the transition probability from inheriting the vector of haplotype y in F1 generation to F2  generation.
    
    computeWeight = function(v, u) {
      w = cbind(v, u, rep(NA, length(v)))
      posU = matrix(c(0, 1, 2, 3),
                    ncol = 2,
                    nrow = 2,
                    byrow = TRUE)
      jj = 1
      i = 0
      evalength = 0
      while (i <= 3) {
        index = which(v == i & u %in% posU[jj, ])
        linex = length(index)
        evalength = linex + evalength
        if (jj == 2) {
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
      ## function to Evaluate a character vector as a bunch of expressions.
      ## arguments :        A =  the matrix that required to evalute.
      #              recRates = vector of the value of  recobination rates.

      ## Return: a numeric matrix.
      recMat = allRecomRates(recRates)
      ## Assign a value to a each recombination rate inserted
      for (i in 1:dim(recMat)[1]){
        assign( recMat[i,1], recMat[i,2])
        cat(recMat[i,1], "=", recMat[i,2], "    ")
      }
      
      eMatv = as.vector(unlist(A))
      index = which(eMatv!="0" & eMatv!="1" & eMatv!="-1" & eMatv!="2")
      indexEvl = evals(eMatv[index])
      eMatv[index] = indexEvl
      eMatv = as.numeric(eMatv)
      neweMat = matrix(eMatv, ncol = ncol(A), byrow = F)
      return(neweMat)
    }#EndFun
    
    #=========================
    ##  all system variables
    # ========================    
    systemVar = function(L){
      ## function to calculate all the variables contributed in the system at locus l.
      ## arguments :         l =  integr of the locus index.
      
      ## return a list of all variables contributed in each locus.
      
      #for(l in 1:L){
      cat("\n Find the variables that are contributing to this system, please wait ... \n")
        Qs = matrix4powL(L)
        Qs = apply(Qs, 1, c2s)
        nonsymQs = unlist(lapply(Qs, standerIndex))
        symQs = unique(nonsymQs)
      #}#EndFor
      return(list(symQs = symQs, nonsymQs = nonsymQs))
    }#EndFun
    
    # ==========================
    # all possible crossover
    # =========================
    ## function to calculate all the the possible crossover at a given locus.
    ## arguments :         varNom =  character represtent the hapolotype in F1 genertaion.
    ##                          L = numver of locis
    ## return a list of all variables contributed in each locus.
    
    allCrossOver = function(varNom){
      allContrVar = vector("list", length(varNom))
      #for (l in 2:L) {
        noVarPerlocus = length(varNom)
        for (vr in 1:noVarPerlocus){
          allContrVar[[vr]] = contributedQs(varNom[vr])
        }#EndFor
      #}#EndFor
      return(allContrVar)
    }#EndFun
    
    # ====================================
    ##    Compute all marginal equations
    # ====================================
    ## function to compute all the marginal equation at any given loci l.
    ##                L =  integr, the loci index.
    ##                varNom = vector of all the unique variables name in the l loci system.
    
    ## Return: a matrix of all marginal equations.
    
    marginEquations = function(l, varNom) {
      A = NULL
      uniqueQsPrev = varNom[[l-1]]
      ln_uniqueQsPrev = length(uniqueQsPrev)
      uniqueQs = varNom[[l]]
      ln_uniqueQs = length(uniqueQs)
      cat("\n")
      cat("Marginal Equation:")
      for (i in 1:1) {
        q = length(s2c(uniqueQs[1]))
        while (q >= 1) {
          AA = matrix(0, ncol = ln_uniqueQs, nrow = 1)
          colnames(AA) = uniqueQs
          rownames(AA) = inserteChar(uniqueQsPrev[i], "x", q)
          marg = matrix(rep(s2c(uniqueQsPrev[i]), 4), ncol = length(s2c(uniqueQsPrev[i])) , byrow = 1)
          marg = insertVector(q = q,
                              mat = marg,
                              v = as.character(c(0, 1, 2, 3)))
          cat(" ", rownames(AA), ">>")
          eq = apply(marg, 1, c2s)
          eq = unlist(lapply(eq, standerIndex))
          w = table(eq)
          AA[, names(w)] = as.vector(w)
          A = rbind(A, AA)
          q = q - 1
        }#EndWhile
      }#EndFor
      cat("\n")
      return(A = A)
    }#EndFun
    
    # ==============================
    ## 
    # ==============================
    twoWayRILsib = function(L, varNom, nonSymQs, scEq){
      ## function to compute the linear system (AQ=B)  for solving 2way RIL problem for L loci.
      ##                l =  integr, the loci index.
      ##           varNom = vector of all the unique variables name in the l loci system.
      ##         nonSymQs = vector of all the variables name in the l loci system.
      ##             scEq = list of vectors for all the possible meiosis
      
      ## Return: The linear system AQ = B 
      
      ## For L = 1, its known
      if(L == 1){
        cat("# ====== 1 - Loci ====== #","\n")
        ## The values of Q(0) = Q(1) = Q(2) = Q(3) = 0.25
        sol = 0.25
        names(sol) = varNom
        cat("\n done.")
        return(sol)
      }else{
        # or If you want to compute the variables contributing to the system agian
        cat("\n # ======", L," - Loci ====== #", "\n")
        ln_varNom = length(varNom)
        ## Sum (Q)= 1 
        A = matrix(0, ncol = length(varNom), nrow = ln_varNom)
        colnames(A) = varNom
        rownames(A) = c("SQ",varNom[-ln_varNom])
        A[1,] = table(nonSymQs)
        ## start computing the self consistance equation (SCHP) for each varNom 
        cat("\n SCHP: ")
        for (sc in 1:(ln_varNom-1) ){
          ## find all the possible meiotic product (the set V) for each haplotypes (u) denoted by varNom 
          eq = scEq[[sc]]
          ln_eq= length(eq)
          for(q in 1:ln_eq){
            ## Compute the transition prob from going u to v where u: the state in F1, v: the state in F2
            we = computeWeight(v = as.numeric( s2c(eq[q])), u = as.numeric(s2c(varNom[sc])) )$we 
            ## combine the similar term together
            if(varNom[sc] == eq[q]) we = paste(we,-1,sep = "");
            A[sc+1, eq[q]] = we
          }
          cat(" ",sc,">> ")
          ## rearrange the SCHP equation to be equal to zero
          if(!(varNom[sc] %in% eq)) A[sc+1, varNom[sc]] = -1
        }#EndFor
        cat("\n done \n")
        # if you wan to write A matrix
        # write.table(A, paste("QmatL=",L,".txt",sep = ""))
        ## The vector B in system AQ = B 
        B = rep(0, ln_varNom)
        ## sum(Q_i) = 1
        B[1] = 1
        ## Evaluate the matrix A for patriculare values of r's
        return(list(A = A, B = B))
      }#EndIFelseA
    }#EndFun
    
    
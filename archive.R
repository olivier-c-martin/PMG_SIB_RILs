# 0     0     0   0.5   0.3     0 0.02232143
# 0     0     1   0.5   0.3     0 0.01339286
# 0     0     2   0.5   0.3     0 0.01339286
# 0     1     0   0.5   0.3     0 0.01339286
# 0     1     1   0.5   0.3     0 0.02232143
# 0     1     2   0.5   0.3     0 0.01339286
# 0     2     0   0.5   0.3     0 0.01339286
# 0     2     1   0.5   0.3     0 0.01339286
# 0     2     2   0.5   0.3     0 0.02232143
# 0     2     3   0.5   0.3     0 0.01339286

x=0.4
y=0.3

0     0     0  0.40   0.3     0 0.02678334
0     0     1  0.40   0.3     0 0.01566639
0     0     2  0.40   0.3     0 0.01553984
0     1     0  0.40   0.3     0 0.01303875
0     1     1  0.40   0.3     0 0.02094634
0     1     2  0.40   0.3     0 0.01241922
0     2     0  0.40   0.3     0 0.01333363
0     2     1  0.40   0.3     0 0.01227874
0     2     2  0.40   0.3     0 0.02077801
0     2     3  0.40   0.3     0 0.01243314


library(rSymPy)
x <- Var("x")
x


a1 <- Var("a1")
a2 <- Var("a2")
a3 <- Var("a3")
a4 <- Var("a4")
a5 <- Var("a5")
a6 <- Var("a6")
a7 <- Var("a7")



A <- Matrix(List(a1, a2, a5), List(a3, a4, a5), List(a7, a4, a6))
Det <- function(x) Sym("(", x, ").det()")
Det(A)




x <- Var("x")
y <- Var("y")
z <- Var("z")

A=Matrix(list(1,                       1,              2,         0,           0,           0,             0,        0,                0,            0),
         list(1,                       0,              0,         1,           0,           0,             2,        0,                0,            0),
         list(1,                       0,              0,         0,           1,           0,             0,        0,                2,            0),
         list(0,                       0,              0,         1,           1,           2,             0,        0,                0,            0),
         List(0,                       1,              0,         0,           1,           0,             0,        2,                0,            0),
         List(0,                       1,              0,         1,           0,           0,             0,        0,                0,            2),
         List(0,                       0,              0,         0,           0,           0,             1,        1,                1,            1),
         List(0,              0.5-0.5* x,     -0.5*x-0.5,         0,           0,           0,             0,     0.25,                0,         0.25),
         List(y-x*y,                -1,      0.5-0.5*x,         0,           0,           0,         0.5*z,        0,            0.5*y,            0),
         List(0,                       0,   0, 0.5-0.5*z,         0,         1/4, -0.5*z-0.5,             0,        0,          1/4))
Det <- function(r) Sym("(", r, ").det()")
Det(A)

## four loci 



r12 = 0.50
r23 = 0.30
r34 = 0.40
r13 = 0.50
r24 = 0.46
r14 = 0.50


0     0     0     0  0.50  0.30   0.4     0 0.006695835
0     0     0     1  0.50  0.30   0.4     0 0.005236586
0     0     0     2  0.50  0.30   0.4     0 0.005194504
0     0     1     0  0.50  0.30   0.4     0 0.003259688
0     0     1     1  0.50  0.30   0.4     0 0.003916598
0     0     1     2  0.50  0.30   0.4     0 0.003108286
0     0     2     0  0.50  0.30   0.4     0 0.003333409
0     0     2     1  0.50  0.30   0.4     0 0.003069685
0     0     2     2  0.50  0.30   0.4     0 0.003884960
0     0     2     3  0.50  0.30   0.4     0 0.003104804
0     1     0     0  0.50  0.30   0.4     0 0.003916598
0     1     0     1  0.50  0.30   0.4     0 0.003259688
0     1     0     2  0.50  0.30   0.4     0 0.003108286
0     1     1     0  0.50  0.30   0.4     0 0.005236586
0     1     1     1  0.50  0.30   0.4     0 0.006695835
0     1     1     2  0.50  0.30   0.4     0 0.005194504
0     1     2     0  0.50  0.30   0.4     0 0.003069685
0     1     2     1  0.50  0.30   0.4     0 0.003333409
0     1     2     2  0.50  0.30   0.4     0 0.003884960
0     1     2     3  0.50  0.30   0.4     0 0.003104804
0     2     0     0  0.50  0.30   0.4     0 0.003884960
0     2     0     1  0.50  0.30   0.4     0 0.003104804
0     2     0     2  0.50  0.30   0.4     0 0.003333409
0     2     0     3  0.50  0.30   0.4     0 0.003069685
0     2     1     0  0.50  0.30   0.4     0 0.003104804
0     2     1     1  0.50  0.30   0.4     0 0.003884960
0     2     1     2  0.50  0.30   0.4     0 0.003333409
0     2     1     3  0.50  0.30   0.4     0 0.003069685
0     2     2     0  0.50  0.30   0.4     0 0.005194504
0     2     2     1  0.50  0.30   0.4     0 0.005194504
0     2     2     2  0.50  0.30   0.4     0 0.006695835
0     2     2     3  0.50  0.30   0.4     0 0.005236586
0     2     3     0  0.50  0.30   0.4     0 0.003108286
0     2     3     1  0.50  0.30   0.4     0 0.003108286
0     2     3     2  0.50  0.30   0.4     0 0.003259688
0     2     3     3  0.50  0.30   0.4     0 0.003916598


> B
[1] 0.02232143 0.01662234 0.01838235 0.02678334 0.01339286 0.01529255 0.01470588 0.02094634 0.01339286 0.01529255 0.01470588 0.02077801 0.01339286 0.01529255 0.01470588 0.01303875 0.02232143
[18] 0.01662234 0.01838235 0.01566639 0.01339286 0.01529255 0.01470588 0.01243314 0.01339286 0.01529255 0.01470588 0.01333363 0.01339286 0.01529255 0.01470588 0.01227874 0.02232143 0.01662234
[35] 0.01838235 0.01553984 0.01339286 0.01529255 0.01470588 0.01241922 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
[52] 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000


# 
# > all_possible_rec_rates
# [,1] [,2] [,3]
# [1,]    1    2 0.50
# [2,]    2    3 0.30
# [3,]    3    4 0.40
# [4,]    1    3 0.50
# [5,]    2    4 0.46
# [6,]    1    4 0.50


BB[5] = QsValues(x = r12, y = r23, z = r13, indx = "001")
BB[6] = QsValues(x = r12, y = r24, z = r14, indx = "001")
BB[7] = QsValues(x = r13, y = r34, z = r14, indx = "001")

BB[8] = QsValues(x = r12, y = r24, z = r14, indx = "002")
BB[9] = QsValues(x = r13, y = r34, z = r14, indx = "002")
BB[10] = QsValues(x = r23, y = r34, z = r24, indx = "002")

BB[11] = QsValues(x = r12, y = r23, z = r13, indx = "010")
BB[12] = QsValues(x = r12, y = r24, z = r14, indx = "010")
BB[13] = QsValues(x = r13, y = r34, z = r14, indx = "010")
BB[14] = QsValues(x = r23, y = r34, z = r24, indx = "010")

BB[15] = QsValues(x = r13, y = r34, z = r14, indx = "011")

BB[16] = QsValues(x = r12, y = r23, z = r13, indx = "012")
BB[17] = QsValues(x = r13, y = r34, z = r14, indx = "012")
BB[18] = QsValues(x = r23, y = r34, z = r24, indx = "012")

BB[19] = QsValues(x = r13, y = r34, z = r14, indx = "020")
BB[20] = QsValues(x = r23, y = r34, z = r24, indx = "020")

BB[21] = QsValues(x = r12, y = r23, z = r13, indx = "021")
BB[22] = QsValues(x = r12, y = r24, z = r14, indx = "021")
BB[23] = QsValues(x = r13, y = r34, z = r14, indx = "021")

BB[24] = QsValues(x = r13, y = r34, z = r14, indx = "022")

BB[25] = QsValues(x = r12, y = r23, z = r13, indx = "023")



# # length(which(apply(aa,1, function(x){length(x!=0)})== 0))
# #length( which(apply(aa,1, function(x){sum(x!=0)})==0))
# 
# # ==============================================
# # check the indep equation on the marginal side 
# # ==============================================
# 
# b = rep(0,dim(A[1:40,])[1])
# aa=echelon(A[1:40,],b, verbose=TRUE, fractions=TRUE)
# 
# indIndex=which(apply(aa,1, sum)==0)
# length(indIndex)
# indepIndexMarginal= which(apply(aa,1, sum)!= 0)
# 
# # ==============================================
# # check the indep equation on the SCHP side 
# # ==============================================
# b = rep(0,dim(A[41:51,])[1])
# aa=echelon(A[41:51,],b, verbose=TRUE, fractions=TRUE)
# length(which(apply(aa,1, sum)==0))
# 
# AA= A[c(indepIndexMarginal, 41:51),]; BB = B[c(indepIndexMarginal, 41:51)]
# 
# # ==============================================
# # check the indep equation over all
# # ==============================================
# 
# aa=echelon(AA,B = rep(0,dim(AA)[1]), verbose=TRUE, fractions=TRUE)
# length(which(apply(aa,1, sum)==0))
# 








# A=t(A)
# 
# a=A[, qr(A)$pivot[seq_len(qr(A)$rank)]]
# dim(a)
# a=t(a)
# res = echelon(a, B=rep(0,dim(a)[1]), verbose=FALSE, fractions=TRUE) # reduced row-echelon form
# fix(res)
# 
# 
# qr(A)$rank
# A=A[1:36,]
# res = echelon(A, B=rep(0,dim(A)[1]), reduced = FALSE, verbose=FALSE, fractions=FALSE) # reduced row-echelon form
# dim(res)
# length(which(apply(res,1, sum)==0))
# inv(A)
# det(A)

# A <- matrix(data=c(1, 2, 3, 2, 5, 9, 5, 7, 8,20, 100, 200),
#             nrow=3, ncol=4, byrow=FALSE)
# A=cbind(A[1:25,],rep(0,dim(A)[1]))
# aa=rref(A)

# A=A[-indIndex,]
# dim(A)
# b=rep(0,dim(A)[1])
# aa=echelon(A,b, verbose=TRUE, fractions=TRUE)
# length(which(apply(aa,1, sum)==0))

# 15






# # length(which(apply(aa,1, function(x){length(x!=0)})== 0))
# #length( which(apply(aa,1, function(x){sum(x!=0)})==0))
# 
# # ==============================================
# # check the indep equation on the marginal side 
# # ==============================================
# 
# b = rep(0,dim(A[1:40,])[1])
# aa=echelon(A[1:40,],b, verbose=TRUE, fractions=TRUE)
# 
# indIndex=which(apply(aa,1, sum)==0)
# length(indIndex)
# indepIndexMarginal= which(apply(aa,1, sum)!= 0)
# 
# # ==============================================
# # check the indep equation on the SCHP side 
# # ==============================================
# b = rep(0,dim(A[41:51,])[1])
# aa=echelon(A[41:51,],b, verbose=TRUE, fractions=TRUE)
# length(which(apply(aa,1, sum)==0))
# 
# AA= A[c(indepIndexMarginal, 41:51),]; BB = B[c(indepIndexMarginal, 41:51)]
# 
# # ==============================================
# # check the indep equation over all
# # ==============================================
# 
# aa=echelon(AA,B = rep(0,dim(AA)[1]), verbose=TRUE, fractions=TRUE)
# length(which(apply(aa,1, sum)==0))
# 








# A=t(A)
# 
# a=A[, qr(A)$pivot[seq_len(qr(A)$rank)]]
# dim(a)
# a=t(a)
# res = echelon(a, B=rep(0,dim(a)[1]), verbose=FALSE, fractions=TRUE) # reduced row-echelon form
# fix(res)
# 
# 
# qr(A)$rank
# A=A[1:36,]
# res = echelon(A, B=rep(0,dim(A)[1]), reduced = FALSE, verbose=FALSE, fractions=FALSE) # reduced row-echelon form
# dim(res)
# length(which(apply(res,1, sum)==0))
# inv(A)
# det(A)

# A <- matrix(data=c(1, 2, 3, 2, 5, 9, 5, 7, 8,20, 100, 200),
#             nrow=3, ncol=4, byrow=FALSE)
# A=cbind(A[1:25,],rep(0,dim(A)[1]))
# aa=rref(A)

# A=A[-indIndex,]
# dim(A)
# b=rep(0,dim(A)[1])
# aa=echelon(A,b, verbose=TRUE, fractions=TRUE)
# length(which(apply(aa,1, sum)==0))

# 15




A <- matrix(c(2, 1, -1,
              -3, -1, 2,
              -2,  1, 2), 3, 3, byrow=TRUE)
colnames(A) <- paste0('x', 1:3)
b <- c(8, -11, -3)

Solve(A,b)




A <- matrix(c(2, 1, -1,
              -3, -1, 2), 2, 2, byrow=TRUE)
colnames(A) <- paste0('x', 1:3)
b <- c(8, -11)

Solve(A,b)


# if(L == 3){
#   A = matrix(c(        1,                       1,                  2,              0,           0,           0,                 0,        0,                    0,            0,  #1.  #Q00X
#                        1,                       0,                  0,              1,           0,           0,                 2,        0,                    0,            0,  #2.  #Q0X0
#                        1,                       0,                  0,              0,           1,           0,                 0,        0,                    2,            0,  #3.  #QX00
#                        0,                       0,                  0,              1,           1,           2,                 0,        0,                    0,            0,  #4.  #Q01X
#                        0,                       1,                  0,              0,           1,           0,                 0,        2,                    0,            0,  #5.  #Q0X1
#                        0,                       1,                  0,              1,           0,           0,                 0,        0,                    0,            2,  #6.  #QX01
#                        0,                       0,                  0,              0,           0,           0,                 1,        1,                    1,            1,  #7.  #Q02X
#      "(1-r12)*(1-r23)-1",                       0,      "0.5-0.5*r12",              0,           0,           0,     "0.5-0.5*r13",        0,        "0.5-0.5*r23",            0,  #10. #Q000
#          "(1-r12)*(r23)",                      -1,      "0.5-0.5*r12",              0,           0,           0,         "0.5*r13",        0,            "0.5*r23",            0,  #11. #Q001
#                        0,                       0,                  0,  "0.5*(1-r13)",           0,         1/4,   "0.5*(1-r13)-1",        0,                    0,          1/4  #16. #Q020
#   ), byrow=T, ncol=10)
#   B = matrix(c( "1/(4 + 24*r12)",  # a 1.
#                   "1/(4 + 24*r13)",  # b 2.
#                   "1/(4 + 24*r23)",  # c 3.
#                   "r12/(2 + 12*r12)",  # d 4.
#                   "r13/( 2 + 12*r13)", # e 5.
#                   "r23/(2 + 12*r23)",  # f 6.
#                   "r12/(2 + 12*r12)",  # d 7.
#                   rep(0,3)), byrow=T, ncol = 1)
#   
#   recPerm = combn(1:L, l, simplify = F) # if length(recPer[[1]]>2) else 
#   
#   # for(sb in 1:length(recPerm)){
#     
#     for( s in 1:length(recPerm)){
#       allSystems[[l]][[s]] = solveSystem( AAA = A, BBB = B, allRec = recMat, newRecIndex = recPerm[[s]], ll = l )
#       
#     }
#   names(allSystems[[l]]) = allPossibleRec(1:L, l)
#   
#   # }
# }

# if(L !=1 & L!=2){
#    A = marginEquations(l)
#    B = coefBs(l = l, A = A, allSystems = allSystems  )
# }
# recPerm = combn(1:L,l, simplify = F)
# if( length(recPerm[[1]]) == 2 ){
#   for (s in 1:length(recPerm) ) {
#     allSystems[[l]][[s]] = solveSystem( AAA = A, BBB = B, allRec = recMat, newRecIndex = recPerm[[s]], ll = l )
#   }
#   names(allSystems[[l]]) = allPossibleRec(1:L, l)
# }else{
#   
# }
# 
# ## Filter A to choose just the dependent ones
# # A = A[dependentEq(A),]
# ## Fill the B's matrix with Q000..x
# for (j in 1:dim(A)[1]) {
#   b = rownames(A)[j]
#   rIndex = which(s2c(b) !="x")
#   sol = solveSystem(AAA = allSystems[[l-1]][[1]], BBB = allSystems[[l-1]][[2]], allRec = recMat, newRecIndex = rIndex, ll = l-1  )
#   B = c(B, as.vector(sol[c2s(s2c(b)[rIndex])]))
# }



# A = matrix(c(      1,  1,    2,
#                "r12",  0, -1/2,
#                "r12", -1, 1/2), byrow = T, ncol = 3)
# B = c(0.25, 0, 0)



#
# # ==============================
# #  give values for the rec rates
# # =============================
#
# evalRec = function(varb, val){
#   for (i in 1:dim(recMat)[1]){
#     cat(varb[i], "=", val[i], "    ")
#     return(assign( varb[i], val[i]))
#   }
# }#EndFun


# ==============================
#  Vector names for the solution of the system
# =============================
#
# namesSol = function(loci = l){
#   allQsPrev = matrix4powL(loci)
#   str_all_qsPrev= apply(allQsPrev,1, c2s)
#   uniqueQsPrev = unique(unlist(lapply(str_all_qsPrev, standerIndex)))
#   return(uniqueQsPrev)
# }#EndFun



# ================================================
####  dependentEq
# ===============================================

# =============================================
# check the dependencies equation in the system
# =============================================

dependentEq = function(mat) {
  n = dim(mat)[1]
  p = dim(mat)[2]
  index = c(1)
  eq = matrix(mat[1, ], ncol = p, byrow = F)
  rankPrev = qr(eq)$rank
  cat("\n \n Check the independincies: ")
  for (i in 2:n) {
    eq = rbind(eq, mat[i, ])
    rankNew = qr(eq)$rank
    if (rankNew > rankPrev) {
      index = c(index, i)
    } else{
      eq = eq[-i, ]
    }
    rankPrev = rankNew
    cat(" ", i, ">>")
  }
  return(index)
} # EndFun


# for (i in 1:dim(allRec)[1]){
#   assign( allRec[i,1], allRec[i,2])
#    cat(allRec[i,1], "=", allRec[i,2], "    ","\n")
# }

# for (i in 1:length(origInx) ){
#   eval(parse(text = paste(origInx[i],"=", newInx[i],"=", updateRecMat[i])))
#   # assign( origInx[i], eval(parse(text = newInx[i])) )
#    cat(origInx[i], "=", newInx[i], "    ", "\n")
# }

#
#
# aa = combn(1:4,3,simplify = F)
# unlist(lapply(aa,function(x) combn(x,2,simplify = F)))




# ================================================
####  marginEquations
# ===============================================

# ==========================
# Compute the wight of SCHP
# ==========================

computeWeight = function(x, y) {
  w = cbind(x, y, rep(NA, length(x)))
  # Gamete zero
  index = which(x == 0 & y %in% c(0, 1))
  ln.index = length(index)
  if (ln.index != 0) {
    if (ln.index == 1) {
      w[index, 3] = 0.5
    } else{
      w[index[1], 3] = 0.5
      for (k in 2:ln.index) {
        if (w[index[k], 2] == w[index[k - 1], 2])
          w[index[k], 3] = paste("1-r", index[k - 1], index[k], sep = "")
        
        if (w[index[k], 2] != w[index[k - 1], 2])
          w[index[k], 3] = paste("r", index[k - 1], index[k], sep = "")
        
      }#EndFor
    }
  }
  
  # Gamete zero 2
  index = which(x == 0 & y %in% c(2, 3))
  ln.index = length(index)
  
  if (ln.index != 0) {
    if (ln.index == 1) {
      w[index, 3] = 0.5
    } else{
      w[index[1], 3] = 0.5
      for (k in 2:ln.index) {
        if (w[index[k], 2] == w[index[k - 1], 2])
          w[index[k], 3] = paste("1-r", index[k - 1], index[k], sep = "")
        
        if (w[index[k], 2] != w[index[k - 1], 2])
          w[index[k], 3] = paste("r", index[k - 1], index[k], sep = "")
        
      }#EndFor
    }
  }
  
  # Gamete one 1
  index = which(x == 1 & y %in% c(0, 1))
  ln.index = length(index)
  
  if (ln.index != 0) {
    if (ln.index == 1) {
      w[index, 3] = 0.5
    } else{
      w[index[1], 3] = 0.5
      for (k in 2:ln.index) {
        if (w[index[k], 2] == w[index[k - 1], 2])
          w[index[k], 3] = paste("1-r", index[k - 1], index[k], sep = "")
        
        if (w[index[k], 2] != w[index[k - 1], 2])
          w[index[k], 3] = paste("r", index[k - 1], index[k], sep = "")
        
      }#EndFor
    }
  }
  
  # Gamete one 2
  index = which(x == 1 & y %in% c(2, 3))
  ln.index = length(index)
  
  if (ln.index != 0) {
    if (ln.index == 1) {
      w[index, 3] = 0.5
    } else{
      w[index[1], 3] = 0.5
      for (k in 2:ln.index) {
        if (w[index[k], 2] == w[index[k - 1], 2])
          w[index[k], 3] = paste("1-r", index[k - 1], index[k], sep = "")
        
        if (w[index[k], 2] != w[index[k - 1], 2])
          w[index[k], 3] = paste("r", index[k - 1], index[k], sep = "")
        
      }#EndFor
    }
  }
  
  # Gamete two 1
  index = which(x == 2 & y %in% c(0, 1))
  ln.index = length(index)
  
  if (ln.index != 0) {
    if (ln.index == 1) {
      w[index, 3] = 0.5
    } else{
      w[index[1], 3] = 0.5
      for (k in 2:ln.index) {
        if (w[index[k], 2] == w[index[k - 1], 2])
          w[index[k], 3] = paste("1-r", index[k - 1], index[k], sep = "")
        
        if (w[index[k], 2] != w[index[k - 1], 2])
          w[index[k], 3] = paste("r", index[k - 1], index[k], sep = "")
        
      }#EndFor
    }
  }
  
  # Gamete two 2
  index = which(x == 2 & y %in% c(2, 3))
  ln.index = length(index)
  
  if (ln.index != 0) {
    if (ln.index == 1) {
      w[index, 3] = 0.5
    } else{
      w[index[1], 3] = 0.5
      for (k in 2:ln.index) {
        if (w[index[k], 2] == w[index[k - 1], 2])
          w[index[k], 3] = paste("1-r", index[k - 1], index[k], sep = "")
        
        if (w[index[k], 2] != w[index[k - 1], 2])
          w[index[k], 3] = paste("r", index[k - 1], index[k], sep = "")
        
      }#EndFor
    }
  }
  
  # Gamete three 1
  index = which(x == 3 & y %in% c(0, 1))
  ln.index = length(index)
  
  if (ln.index != 0) {
    if (ln.index == 1) {
      w[index, 3] = 0.5
    } else{
      w[index[1], 3] = 0.5
      for (k in 2:ln.index) {
        if (w[index[k], 2] == w[index[k - 1], 2])
          w[index[k], 3] = paste("1-r", index[k - 1], index[k], sep = "")
        
        if (w[index[k], 2] != w[index[k - 1], 2])
          w[index[k], 3] = paste("r", index[k - 1], index[k], sep = "")
        
      }#EndFor
    }
  }
  # Gamete three 2
  index = which(x == 3 & y %in% c(2, 3))
  ln.index = length(index)
  
  if (ln.index != 0) {
    if (ln.index == 1) {
      w[index, 3] = 0.5
    } else{
      w[index[1], 3] = 0.5
      for (k in 2:ln.index) {
        if (w[index[k], 2] == w[index[k - 1], 2])
          w[index[k], 3] = paste("1-r", index[k - 1], index[k], sep = "")
        
        if (w[index[k], 2] != w[index[k - 1], 2])
          w[index[k], 3] = paste("r", index[k - 1], index[k], sep = "")
        
      }#EndFor
    }
  }
  w[, 3] = paste("(", w[, 3], ")", sep = "")
  return(list(w = w, we = paste(c(2, as.vector(
    w[, 3]
  )), collapse = "*")))
}#EndFun


# ====================================
##    Compute all marginal equations
# ====================================
## function to compute all the marginal equation at any given loci l.
##                l =  integr, the loci index.
##                cl = integer, number of cores used in the parallel computing

## Return: a list, the matrix of all marginal equations, and a vectors of the Q's (the variables indices) names.

marginEquations = function(l) {
  A = NULL
  allQsPrev = matrix4powL(l - 1)
  allQsPrev = apply(allQsPrev, 1, c2s)
  uniqueQsPrev = unique(unlist(lapply(allQsPrev, standerIndex)))
  ln_uniqueQsPrev = length(uniqueQsPrev)
  allQs = matrix4powL(l)
  allQs = apply(allQs, 1, c2s)
  uniqueQs = unique(unlist(lapply(allQs, standerIndex)))
  ln_uniqueQs = length(uniqueQs)
  cat("\n")
  cat("Marginal Equation:")
  #for (i in 1:ln_uniqueQsPrev){
  for (i in 1:1) {
    #for (q in 1: length(s2c(unqueQs[1])) ) {
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
  return(list(A = A, uniqueQsNames = uniqueQs))
}#EndFun


# ================================
##   Compute the coefficient B's
# ================================
## function to compute the values of each marginal equation coefficient in the previous locus case (l-1) for particular recombination rates values.
## arguments :   l =  integr, the loci index.
##               A = A square nonsingulare matrix of all SCHP equations and one marginal.
##          allRec =  vector of the given original recombination rates values.
##     newRecIndex =  a vector of a new chosen recombination rates values.


## Return: an evaluated B vector after substituting the new recombination values.

coefBs = function (l, A, allSystems, newRecIndex) {
  B = NULL
  n = dim(A)[1]
  #for (j in 1:1) {
  for (j in 1:n) {
    b = rownames(A)[j]
    xNot = which(s2c(b) != "x")
    if (length(xNot) == 1) B=c(B, 0.25) # this special case for l = 2
    if (length(which(s2c(b) == "x")) != 0 & length(xNot) > 1 ){
      rIndex = newRecIndex[xNot]
      # sol = solveSystem(AAA = allSystems[[l-1]][[1]], BBB = allSystems[[l-1]][[2]], allRec = recMat, newRecIndex = rIndex, ll = l-1  )
      B = c(B, eval(parse(text = paste("allSystems[[",l - 1,"]]$r", c2s(rIndex), "[", "'", gsub("x", "", b), "'", "]" , sep = "" ) )))
    }
    if (length(which(s2c(b) == "x")) == 0){
      B = c(B, 0)
    }
  }
  return(B)
} #EndFun



# =============================
##       solveSystem
# =============================
## function to solve the linear system (AQ=B) and combined all information at each loci l for all the possible valued of recombination rates.
## arguments :   AAA = A square nonsingulare matrix of all SCHP equations and one marginal.
##               BBB =  A vetor of coeffecients.
##            allRec =  vector of the given original recombination rates values.
##          solNames = vector of charcter(Q) represnts the names of variables.

## Return: vector of Q''s values: Q = inv(A)B
solveSystem = function(AAA, BBB, allRec = recMat, solNames = varNom) {
  n = dim(allRec)[1]
  p = dim(AAA)[2]
  ## Let the memory read the values again
  for (i in 1:n)
    assign(allRec[i, 1], allRec[i, 2])
  
  # origInx = allPossibleRec(1:ll, 2)
  # ln_origInx = length(origInx)
  # newInx = allPossibleRec(newRecIndex, 2)
  # #cat("r12Old = ",r12, "   ")
  # updateRecMat = mapply(function(x) eval(parse(text = x)), newInx)
  # for (i in 1:ln_origInx)
  #   eval(parse(text = paste(origInx[i], "=", newInx[i], "=", updateRecMat[i] )))
  #cat("r12new = ",r12, "   ","\n")
  # AAA = matrix (parSapply(cl, AAA, function(x) eval(parse(text = (x)))), byrow = F, ncol = p)
  AAA = evalMatrix(AAA)
  #tt = matrix (mapply(function(x) lazy_eval(x), A), byrow = F, ncol = dim(A)[2])
  sol = solve(AAA, BBB)
  names(sol) = solNames
  return(sol)
}# EndFun


# # ================================
# ##   Compute the coefficient B's
# # ================================
# ## function to compute the values of each marginal equation coefficient in the previous locus case (l-1) for particular recombination rates values.
# ## arguments :   b = ctacter, the constance index of mariginal equation.
# ##      prevSol = list of the solutions of the perviouse system (l-1).
# ##          allRec =  vector of the given original recombination rates values.
# ##     newRecIndex =  a vector of a new chosen recombination rates values.
# 
# ## Return: an evaluated B vector after substituting the new recombination values.
# coefBs = function (b, prevSol, newRecIndex) {
#     xNot = which(s2c(b) != "x")
#     if (length(xNot) == 1) B = 0.25 # this special case for l = 2
#     if (length(which(s2c(b) == "x")) != 0 & length(xNot) > 1 ){
#       rIndex = newRecIndex[xNot]
#       B = eval(parse(text = paste("prevSol$r", c2s(rIndex), "[", "'", gsub("x", "", b), "'", "]" , sep = "" ) ))
#     }
#   return(B)
# } #EndFun


# ===================
##        evalMat
# ===================
## function to Evaluate an R expression in a specified environment.
## arguments :   recMat =  matrix of ll original recobination rate.
##                    A = A square nonsingulare matrix of all SCHP equations and one marginal.
##                    l =  intiger of loci index. 
##                   cl =  integer, number of cores used in the parallel computing.


## Return: The result of evaluating the object: for an expression vector this is the result of evaluating the last element.

evalMat = function(recMat, A, l, cl) {
  p = dim(A)[2]
  n = dim(recMat)[1]
  recMat[, 2] = rep(0, n)
  for (i in 1:n) {
    assign(recMat[i, 1], recMat[i, 2])
    #cat(recMat[i,1], "=", recMat[i,2], "    ")
  }
  if (l == 2)
    r12 = 0
  
  AA = matrix (parSapply(cl, A, function(x)
    eval(parse(text = (
      x
    )))),
    byrow = F,
    ncol = p)
  return(AA)
}#EndFun

# ====================================
##    Compute all marginal equations
# ====================================
## function to compute all the marginal equation at any given loci l.
##                l =  integr, the loci index.
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


# =============================
##       solveSystem
# =============================
## function to solve the linear system (AQ=B) and combined all information at each loci l for all the possible valued of recombination rates.
## arguments :   AAA = A square nonsingulare matrix of all SCHP equations and one marginal.
##               BBB =  A vetor of coeffecients.
##            allRec =  vector of the given original recombination rates values.
##          solNames = vector of charcter(Q) represnts the names of variables.

## Return: vector of Q''s values: Q = inv(A)B
solveSystem = function(AAA, BBB, allRec = recMat, solNames = varNom) {
  n = dim(allRec)[1]
  p = dim(AAA)[2]
  ## Let the memory read the values again
  for (i in 1:n)
    assign(allRec[i, 1], allRec[i, 2])
  
  AAA = evalMatrix(AAA)
  sol = solve(AAA, BBB)
  names(sol) = solNames
  return(sol)
}# EndFun

# =========================
#  all system variables
# ========================
systemVar2 = function(L){
  ## function to calculate all the variables contributed in the system at locus l.
  ## arguments :         l =  integr of the locus index.
  
  ## return a list of all variables contributed in each locus.
  
  allVar = vector("list", L)
  for(l in 1:L){
    Qs = matrix4powL(l)
    Qs = apply(Qs, 1, c2s)
    symQs = unlist(lapply(Qs, standerIndex))
    uniqueQs = unique(symQs)
    allVar[[l]] = uniqueQs
  }#EndFor
  return(list(allVar, symQs))
}#EndFun

# ==========================
# all possible crossover
# =========================
## function to calculate all the the possible crossover at a given locus.
## arguments :         varNom =  character represtent the hapolotype in F1 genertaion.
##                          L = numver of locis
## return a list of all variables contributed in each locus.

allCrossOver2 = function(varNom, L){
  allContrVar = vector("list", L)
  for (l in 2:L) {
    noVarPerlocus = length(varNom[[l]])
    for (vr in 1:noVarPerlocus) {
      allContrVar[[l]][[vr]] = contributedQs(varNom[[l]][vr])
    }#EndFor
  }#EndFor
  return(allContrVar)
}#EndFun




# ===============================================
##              inserteChar
# ===============================================
## function to insert a character in the string.
## arguments :   st = a string.
##              chr = a character required to inserted.
##                q = an integer representing the position of the new character.

## return a new string contain the new character.

inserteChar = function(st, chr, q) {
  n = length(s2c(st))
  if (q == 1) {
    stNew = c2s(c(chr, s2c(st)))
  }
  if (q == n + 1) {
    stNew = c2s(c(s2c(st), chr))
  }
  
  if (q != 1 & q != (n + 1)) {
    stNew = c2s(c(s2c(st)[1:(q - 1)], chr, s2c(st)[q:n]))
  }
  return(stNew)
}#EndFun



# ===============================================
##            insertVector
# ===============================================

## function to insert a column in the data frame.
## arguments :   q = an integr represent the position of the insterted column.
##             mat = a matrix that want to inserte column in it.
##               v = a vector that want to insert. 

## return a new matrix with new inserted column.

insertVector = function(q, mat, v) {
  p = dim(mat)[2]
  if (q == 1) {
    matNew = cbind(v, mat)
  }
  if (q == (p + 1)) {
    matNew = cbind(mat, v)
  }
  if (q != 1 & (q != p + 1)) {
    matNew = cbind(mat[, 1:(q - 1)], v, mat[, q:p])
  }
  return(matNew)
}#EndFun

# ================================
#  assignValues
#=================================

## function to assign a value to a name in an environment.
## arguments :   var = a character represent the variable name.
##             value = a real value to be assigned to var.

## This function is invoked for its side effect, which is assigning value to the variable var.

assignValues = function(var, value) {
  ln_var = length(var)
  for (i in 1:ln_var) {
    assign(var[i], value[i])
    cat(var[i], "=", value[i], "    ")
    eval(parse(text = var[i]))
  }
  cat("\n")
}#EndFun



# ================================
#  allPossibleRec
# ================================
## function to generate all combinations of the elements of vect taken m at a time.
## arguments :   vect = a vector source for combinations, or integer.
##              m =  a integr number of elements to choose.

## Return: returns all combinations of the elements of seq(var) taken m at a time.

allPossibleRec = function(vect, m) {
  return (unlist(lapply(lapply(combn(vect, m, simplify = FALSE), c2s), function(x)
    paste("r", x, sep = ""))))
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

#####################################
####################################

qs = c("00", "01", "02", "02", "01", "00", "02", "02", "20", "20", "22", "23", "20", "20", "23", "22" )
table(qs)



## ==============================================
## Function to determine probabiltiy to obtain 
## a recombination pattern using counting model
## ==============================================
countingRecRate = function(v, u, genePos, m) {
  L= length(v)
  w = cbind(v, u, rep(NA, length(v)))
  posU = matrix(c(0, 1, 2, 3), ncol = 2, nrow = 2, byrow = TRUE)
  Vin = rep(0, m+1)
  Vout = rep(0, m+1)
  for (jj in 1:(m+1)) Vin[jj] = 1/(m+1)
  const = 1
  flag = "d"
  for (l in 1:(L-1)){
    Vgene1 = v[l]
    Vgene2 = v[l+1]
    Ugene1 = u[l]
    Ugene2 = u[l+1]
    
    if(Vgene1 == Vgene2){
      if(Ugene1 %in% c(0,1) & Ugene2 %in% c(0,1)){
        if(Ugene1 != Ugene2) flag = "r" else flag = "nr"
      }
      if(Ugene1 %in% c(2,3) & Ugene2 %in% c(2,3)){
        if(Ugene1 != Ugene2) flag = "r" else flag = "nr"
      }
    }
    
    if(flag == "d") const = const * 0.5
    
    # Genetic distance for the interval 
    
    if(flag == "r" | flag == "nr"){
      d = genePos[l+1] - genePos[l]
      mu = 2*(m+1)*d
      for (kk in 0:1000){
        probk = PoissonK(mu, kk)
        for (jj in 1:(m+1)) {
          if(kk > 0){
            H = (jj + kk)/(m+1)
            R = (jj + kk)%%(m+1)
            #cat("R=", R,"\t")
            if(H > 0){
              Vout[R + 1] = Vout[R + 1] + 0.5 * Vin[jj]*probk
            }else{
              if(flag == "nr") Vout[R + 1] = Vout[R + 1] + Vin[jj]*probk
            }
          }else{
            if(flag == "nr") Vout[jj] = Vout[jj] + Vin[jj]*probk
          }
        }#EndFor jj
      }#EndFor kk
      Vin = Vout
      Vout = rep(0, m+1)
    }#End if r nr
    
    flag = "d"
  }#EndFor l
  
  # prob =0
  # for (jj in 1:(m+1)) {
  #   prob = prob + Vin[jj]
  # }
  prob = sum(Vin)
  if(length(unique(v)) == L) return(2 * prob * const *0.5) else return(2 * prob * const * 0.5 )
  
}#EndFun


##########################
#########################
#########################

## SIB 
recRates = c(0.3553079, 0.3553079)

sol
000        001        002        010        011        012        020        021        022        023 
0.02604775 0.01803042 0.01787346 0.01333764 0.01803042 0.01267845 0.01365157 0.01252149 0.01787346 0.01267845 


# ## for selfing 
# >     sol
# 000        001        010        011 
# 0.17275509 0.11953732 0.08817028 0.11953732 


# # ===============================================
# ##          matrix4powL
# # ===============================================
# ## function to compute all the possible haplotype given the total number of loci which is equal to 4^L, where L is the number of loci.
# ## arguments : L = integr number reprersent the number of loci.
# 
# ## return a matrix of all the possible haplotypes.
# matrix4powL = function(L) {
#   fourPowL = 4^L
#   completeMat = matrix(data = 0, nrow = fourPowL, ncol = L)
#   cpt = 10
#   I = (fourPowL - 1)
#   for (i in 0:I) {
#     completeMat[1 + i, ] = quatNumber(i, L)
#     ## Print percentage of accomplished vertices
#     if ( ((i/I)*100)>=cpt ) {
#       cat(cpt, "% ",sep="")
#       cpt=cpt+10
#     }
#   }
#   return(completeMat)
# }#EndFun
# 
# # ================================================
# ##            quatNumber
# # ===============================================
# 
# quatNumber = function(number, space) {
#   quat = rep(0, space)
#   n = 0
#   k = number
#   while (k != 0) {
#     while (number != 0) {
#       if (number %% 4 != 0) {
#         number = (number - 1)
#       }
#       else {
#         number = number / 4
#         n = n + 1
#       }
#     }
#     pos = space - n
#     quat[pos] = quat[pos] + 1
#     number = k - (4 ^ n)
#     k = number
#     n = 0
#   }
#   return(quat)
# }#EndFun


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


# # ================================================
# ####  fatt
# # ===============================================
# ## function used for Conditional Element Selection 
# ## arguments :   x = an object which can be coerced to logical mode.
# 
# ## returns a value with the same shape as test which is filled with 
# ## elements selected from either yes or no depending on whether the 
# ## element of test is TRUE or FALSE.
# fatt = function(x) {
#   ifelse(x == 1, 1, x + fatt(x - 1))
# }#EndFun
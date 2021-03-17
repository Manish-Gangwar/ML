dtm2CF <- function(dtm, focal.user, n0){
  
  ## build token_freqs wala DF from DTM
  a0 = colSums(dtm)
  a1 = sort(a0, decreasing=TRUE, index.return=TRUE)
  a2 = as.matrix(a0[a1$ix]) # Sorted freqs
  token_freqs = data.frame(brands = rownames(a2), Freq = a2, Freq.Rank = seq(1, nrow(a2)))
  rownames(token_freqs) = NULL
  
  ## Finding similarities of all the item pairs.
  a3 = sim2(as.matrix(t(dtm)), method = "cosine", norm="l2") # item-item simil matrix
  a4 = sim2(as.matrix(dtm), method = "cosine", norm="l2") # user-user simil matrix
  
  
  ## IBCF: Generating the missing ratings in the table
  a5 = a4 %*% as.matrix(dtm) %*% a3 # 0.01s  single, simple step using matrix multiplication
  a5a = apply(a5, 1, function(x) {x/max(x)}) # 0 s!
  a5a = t(a5a)
  
  ## Find unrated items focal.user i0 might most like.
  i0 = which(rownames(a5a) == focal.user) # doc index num of focal.user
  a6 = a5a[i0, (dtm[i0,] == 0)] # retain only brands focal.user hasn't yet rated
  a7 = sort(a6, decreasing=TRUE, index.return=TRUE) # sort by predicted ratings
  output0_ibcf = data.frame(IBCF.Recommended.brands = names(a6[a7$ix[1:n0]]), 
                            Pref.Prob = as.matrix(round(a6[a7$ix[1:n0]], 3)))
  
  rownames(output0_ibcf) = NULL  
  test = dplyr::left_join(output0_ibcf, token_freqs, by = c("IBCF.Recommended.brands" = "brands"))
  output0_ibcf = test
  
  ## same for UBCF
  a8 = a4 %*% as.matrix(dtm)
  a9 = a4 %*% rep(1, nrow(a4))
  a10 = matrix(rep(as.vector(a9)), nrow(a4), ncol(a8))
  a11 = a8 / a10
  a12 = a11[i0, (dtm[i0,] == 0)] 
  a13 = sort(a12, decreasing=TRUE, index.return=TRUE) # 
  output0_ubcf = data.frame(UBCF.Recommended.brands = names(a12[a13$ix[1:n0]]), 
                            Pref.Prob = as.matrix(round(a12[a13$ix[1:n0]], 3)))
  
  rownames(output0_ubcf) = NULL
  test = dplyr::left_join(output0_ubcf, token_freqs, by = c("UBCF.Recommended.brands" = "brands"))
  output0_ubcf = test
  
  ## find most similar users to focal.user
  a4a = a4; diag(a4a) = 0   # set diag to 0
  a14 = sort(a4a[i0,], decreasing=TRUE, index.return=TRUE)
  a15 = a4a[i0, a14$ix]
  a16 = (a15 > 0) # names(a4a)[1:n0]
  most_simil_users_df = data.frame(most_similar_users = names(a15)[a16], 
                                   similarity_score = round(a15[a16],2))
  rownames(most_simil_users_df) = NULL
  
  return(list(output0_ibcf, output0_ubcf, most_simil_users_df)) 
  
} # func ends


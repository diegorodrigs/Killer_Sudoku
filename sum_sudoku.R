sum_sudoku <- function(S,N,restric = NULL){
  
  ini = pmin(S-sum(seq(N-1)),9)

  if(N == 2){
    # seg_lim <- ifelse(S %% 2 == 0,(S/2)-1,(S-1)/2)
    exp_gr <- expand.grid("V01" = seq(ini,1,-1)
                          ,"V02" = seq(1,ini))
  }else if(N == 3){
    # seg_lim <- ifelse(S %% 2 == 0,(S/2)-1,(S-1)/2)
    exp_gr <- expand.grid("V01" = seq(ini,1,-1)
                          ,"V02" = seq(2,ini)
                          ,"V03" = seq(1,ini)
                          )
  }else if(N == 4){
    # seg_lim <- ifelse(S %% 2 == 0,(S/2)-1,(S-1)/2)
    exp_gr <- expand.grid("V01" = seq(ini,1,-1)
                          ,"V02" = seq(3,ini)
                          ,"V03" = seq(2,ini)
                          ,"V04" = seq(1,ini)
    )
  }else if(N == 5){
    # seg_lim <- ifelse(S %% 2 == 0,(S/2)-1,(S-1)/2)
    exp_gr <- expand.grid("V01" = seq(ini,1,-1)
                          ,"V02" = seq(4,ini-1)
                          ,"V03" = seq(3,ini-2)
                          ,"V04" = seq(2,ini-3)
                          ,"V05" = seq(1,ini-4)
    )
  }else if(N == 6){
    # seg_lim <- ifelse(S %% 2 == 0,(S/2)-1,(S-1)/2)
    exp_gr <- expand.grid("V01" = seq(ini,1,-1)
                          ,"V02" = seq(5,ini-1)
                          ,"V03" = seq(4,ini-2)
                          ,"V04" = seq(3,ini-3)
                          ,"V05" = seq(2,ini-4)
                          ,"V06" = seq(1,ini-5)
    )
  }
  
  
  ### Removendo as linhas sem sentido
  
  # soma diferente de S
  exp_gr$soma <- apply(exp_gr,1,sum)==S
  exp_gr <- exp_gr[exp_gr$soma,setdiff(names(exp_gr),"soma")]
  
  # removendo as restrições
  if(!is.null(restric)){
    exp_gr$restric <- apply(exp_gr,1,function(x){
      return(!any(x %in% restric))
    })    
    exp_gr <- exp_gr[exp_gr$restric,setdiff(names(exp_gr),"restric")]
  }

  # cada linha tem que estar na order decrescente
  exp_gr$ordem <- apply(exp_gr,1,function(x){
    return(all(x == sort(x,decreasing = T)))
  })
  exp_gr <- exp_gr[exp_gr$ordem,setdiff(names(exp_gr),"ordem")]
  
  # não podem haver números repetidos nas linhas
  exp_gr$repet <- apply(exp_gr,1,function(x){
    return(sum(duplicated(x)) == 0)
  })
  exp_gr <- exp_gr[exp_gr$repet,setdiff(names(exp_gr),"repet")]
  
  if(nrow(exp_gr) == 0){
    print("Impossível gerar essa tabela")
  }else{
    row.names(exp_gr) <- NULL
    print(exp_gr,row.names = F)
  }
  
  # return(exp_gr)
  
}

# sum_sudoku(S = 12, N = 2, restric = c(3))
# sum_sudoku(S = 20, N = 3, restric = c(3,6))
# sum_sudoku(S = 10, N = 2, restric = c(8,6))
# sum_sudoku(S = 11, N = 2, restric = c(3,4,5))
# sum_sudoku(S = 10, N = 2)

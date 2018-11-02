library(tidyverse)
library(randomForest)
library(inTrees)
library(dplyr.teradata)
library(foreach)
library(data.table)
set.seed(123) # Set seed as random forest use random variable which affects random forest result


con <- dbConnect(
  todbc(),
  driver = "/Library/Application Support/teradata/client/16.10/lib/tdata.dylib",
  DBCName = "153.65.169.70",
  uid = "****",
  pwd = "****",
  charset = "UTF8"
  )


compute_rf <- function(in_schema = "nissan", in_tbl = "NOX_ADS_RANDOM_FOREST", ntry = 1000L){
  
  #' @description 
  #' Compute RandomForest from td table.

  tbl_name <- paste(in_schema, in_tbl, sep = ".")
  
  in_df <- tbl(con, tbl_name) %>%
    select(-RES_ID) %>%
    collect() %>% # Load table from TD
    as_tibble() %>% 
    mutate(OBJ_VAL = as.factor(OBJ_VAL)) # Random forest function calls classifier when type of object variable is factor. Otherwise it calls regressor.
  
  rf <- randomForest(
    OBJ_VAL~., # Formula
    in_df, # data
    importance=T,
    do.trace=T, # If true, we can see the progress of random forest
    ntree=ntry
    )
  
  return(rf)
  
}


count_vars_stage <- function(rf = rf){
  
  #' @description 
  #' Function to get the information which variables are used in random forest 
  #' Variable combination is flatten
  
  # Get number of trees
  k <- rf$ntree
  
  create_var_comb <- function(rf, i){
    
    # 各決定木の情報取得
    rf %>% 
      getTree(k = i, labelVar = TRUE) %>% 
      data.table() %>% 
      setnames(c("left", "right", "var", "point", "status", "prediction")) -> tree
    
    # 第1分岐
    top <- tree[1]
    
    # 第2分岐
    second_l <- tree[top$left]
    second_r <- tree[top$right]
    
    # 第3分岐
    third_ll <- tree[second_l$left]
    third_lr <- tree[second_l$right]
    third_rl <- tree[second_r$left]
    third_rr <- tree[second_r$right]
    
    # 決定木が前で切れていた場合に挿入する情報
    df_null <- data_frame(left = 0, right = 0, var = NA, point = 0, status = -1, prediction = "No")
    
    # 決定木が降りてきていなかった場合の対応
    if(nrow(third_ll) == 0L) third_ll <- df_null
    if(nrow(third_lr) == 0L) third_lr <- df_null
    if(nrow(third_rl) == 0L) third_rl <- df_null
    if(nrow(third_rr) == 0L) third_rr <- df_null
    
    data_frame(
      var1 = c(rep(as.character(top$var), 4)),
      var2 = c(rep(coalesce(as.character(second_l$var), "-"), 2), rep(coalesce(as.character(second_r$var), "-"), 2)),
      var3 = c(coalesce(as.character(third_ll$var), "-"), coalesce(as.character(third_lr$var), "-"), coalesce(as.character(third_rl$var), "-"), coalesce(as.character(third_rr$var), "-"))
      ) %>% 
      mutate(idx = i) -> df_i
    
    return(df_i)
  }
  
  # For文で回す(foreachのほうが結果を貯めるオブジェクト準備しなくていいので簡単, dplyr::bind_rowsはrbindの高速版)
  res <- foreach(i = 1:k, .combine = bind_rows) %do% {
    create_var_comb(rf, i)
  } 
  
  return(res)
}


main <- function(){
  
  rf <- compute_rf()
  res <- count_vars_stage(rf) %>% write_csv("var_cnts_1000.csv")
  
}

  
main()

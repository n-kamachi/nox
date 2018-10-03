library(tidyverse)
library(randomForest)
library(inTrees)
library(RODBC)
#library(dplyr.teradata)
library(foreach)
set.seed(123)

DSN <- "***"
USERID <- "***"
PASSWORD <- "***"

# ODBC接続 to TERADATA
con <- odbcConnect(DSN, USERID, PASSWORD)


compute_rf <- function(in_schema = "nissan", in_tbl = "NOX_ADS_RANDOM_FOREST"){
  
  #' @description 
  #' Compute RandomForest from td table.

  tbl_name <- paste(in_schema, in_tbl, sep = ".")
  
  in_df <- sqlFetch(con, tbl_name) %>% select(-RES_ID) %>% collect() %>% as_tibble()
  
  rf <- randomForest(OBJ_VAL~., in_df, importance=T)
  
  return(rf)
  
}


show_var_importance <- function(rf){
  # Draw variable importance
  rf %>% varImpPlot() -> imp
  return(imp)
  
}

count_vars_stage <- function(rf = rf){
  
  k <- rf$ntree
  
  create_var_comb <- function(rf, i){
    
    rf %>% 
      getTree(k = i, labelVar = TRUE) %>% 
      data.table() %>% 
      setnames(c("left", "right", "var", "point", "status", "prediction")) -> tree
    
    df_null <- data_frame(left = 0, right = 0, var = NA, point = 0, status = -1, prediction = "No")
    
    top <- tree[1]
    second_l <- tree[top$left]
    second_r <- tree[top$right]
    third_ll <- tree[second_l$left]
    third_lr <- tree[second_l$right]
    third_rl <- tree[second_r$left]
    third_rr <- tree[second_r$right]
    
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
  
  
  res <- foreach(i = 1:k, .combine = union_all) %do% {
    create_var_comb(rf, i)
  } %>%
    group_by(var1, var2, var3) %>%
    dplyr::summarise(cnt = n()) %>%
    arrange(desc(cnt))
  
  return(res)
}


main <- function(){
  
  rf <- compute_rf()
  show_var_importance(rf) %>% save.image("./out/randomforest/var_imp.png")
  res <- count_vars_stage(rf) %>% write_csv("./out/randomforest/var_cnts.csv")
  
}
  
main()

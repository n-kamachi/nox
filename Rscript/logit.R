
library(dplyr)
library(tidyr)
library(broom)
library(glue)
library(tibble)
library(pforeach)
#library(dplyr.teradata)

#con <- dbConnect(todbc(),
#                 driver = "/Library/Application Support/teradata/client/16.10/lib/tdata.dylib",
#                 DBCName = "153.65.169.70",
#                 uid = "****",
#                 pwd = "****",
#                 charset = "UTF8")

library(RODBC)

DSN <- "nissanpoc"
USERID <- "nissan"
PASSWORD <- "nissan"

# ODBC接続 to TERADATA
con <- odbcConnect(DSN, USERID, PASSWORD)

load_org_data <- function(schema, tbl_name){
  
  #' @description Function to load data from teradata.
  #' @param schema Table schema to be loaded
  #' @param tbl_name Table name to be loaded
  
  q <- glue(
    "
    SELECT
      res_id
     ,var_id
     ,obj_val
     ,SUM(CASE WHEN VAR_SUB_ID =  0 THEN exp_val ELSE 0 END) AS lag_00
     ,SUM(CASE WHEN VAR_SUB_ID =  1 THEN exp_val ELSE 0 END) AS lag_01
     ,SUM(CASE WHEN VAR_SUB_ID =  2 THEN exp_val ELSE 0 END) AS lag_02
     ,SUM(CASE WHEN VAR_SUB_ID =  3 THEN exp_val ELSE 0 END) AS lag_03
     ,SUM(CASE WHEN VAR_SUB_ID =  4 THEN exp_val ELSE 0 END) AS lag_04
     ,SUM(CASE WHEN VAR_SUB_ID =  5 THEN exp_val ELSE 0 END) AS lag_05
     ,SUM(CASE WHEN VAR_SUB_ID =  6 THEN exp_val ELSE 0 END) AS lag_06
     ,SUM(CASE WHEN VAR_SUB_ID =  7 THEN exp_val ELSE 0 END) AS lag_07
     ,SUM(CASE WHEN VAR_SUB_ID =  8 THEN exp_val ELSE 0 END) AS lag_08
     ,SUM(CASE WHEN VAR_SUB_ID =  9 THEN exp_val ELSE 0 END) AS lag_09
     ,SUM(CASE WHEN VAR_SUB_ID = 10 THEN exp_val ELSE 0 END) AS lag_10
    FROM
      {schema}.{tbl_name}
    GROUP BY 1,2,3
    ;
    "
  )
  
  sqlQuery(con, q) %>% 
    as_tibble() -> logit_input
  
  return(logit_input)
}


load_var_mst <- function(schema, tbl_name){
  
  q <- glue("SELECT var_id, var from {schema}.{tbl_name};")
  
  var_mst <- sqlQuery(con, q) %>% 
    as_tibble()
  
  return(var_mst)
  
}


compute_logit_parallel <- function(logit_input){
  
  #'@description
  #'Function for parallel computation of logistic regression.
  
  ite <- logit_input$VAR_ID %>% unique()
  
  res <- pforeach(i = ite, .combine = bind_rows)({
    
    logit_input %>% 
      filter(VAR_ID == i) %>% 
      glm(
        OBJ_VAL ~ lag_00 + lag_01 + lag_02 + lag_03 + lag_04 + lag_05 + lag_06 + lag_07 + lag_08 + lag_09 + lag_10,
        data = .,
        family = binomial) %>%
      tidy() %>% 
      mutate(VAR_ID = i)
    
  })
  
  return(res)
  
}


main <- function(){
  
  logit_input <- load_org_data(schema = "nissan",tbl_name = "NOX_ADS_REGRESSION")
  #var_mst <- load_var_mst(schema = "nissan", tbl_name = "NOX_MST_AN_VARIABLE")
  res <- compute_logit_parallel(logit_input)
  
  res %>% write.csv("./out/nox_logit.csv")
    
}

main()

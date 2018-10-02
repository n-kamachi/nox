
library(dplyr)
library(tidyr)
library(broom)
library(glue)
library(tibble)
library(pforeach)
#library(dplyr.teradata)
library(RODBC)

#con <- dbConnect(todbc(),
#                 driver = "/Library/Application Support/teradata/client/16.10/lib/tdata.dylib",
#                 DBCName = "153.65.169.70",
#                 uid = "****",
#                 pwd = "****",
#                 charset = "UTF8")

DSN <- "***"
USERID <- "***"
PASSWORD <- "***"

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
      ,var_sub_id
      ,obj_val
      ,exp_val
    FROM
      {schema}.{tbl_name}
    ;
    "
  )
  
  sqlQuery(con, q) %>% 
    as_tibble() -> logit_input
  
  return(logit_input)
}


compute_logit_parallel <- function(logit_input){
  
  #'@description
  #'Function for parallel computation of logistic regression.
  
  ite <- logit_input %>% 
    select(VAR_ID, VAR_SUB_ID) %>%
    unique()
  
  res <- pforeach(i = 1:nrow(ite), .combine = bind_rows)({
    
    logit_input %>% 
      filter(VAR_ID == ite$VAR_ID[i],
             VAR_SUB_ID == ite$VAR_SUB_ID[i]) %>% 
      glm(
        OBJ_VAL ~ EXP_VAL,
        data = .,
        family = binomial) %>%
      tidy() %>% 
      mutate(VAR_ID = ite$VAR_ID[i],
             VAR_SUB_ID = ite$VAR_SUB_ID[i])
    
  })
  
  return(res)
  
}


main <- function(){
  
  logit_input <- load_org_data(schema = "nissan",tbl_name = "NOX_ADS_REGRESSION")
  #var_mst <- load_var_mst(schema = "nissan", tbl_name = "NOX_MST_AN_VARIABLE")
  res <- compute_logit_parallel(logit_input)
  
  res %>% write.csv(paste0("./out/nox_logit", format(Sys.time(), "%Y-%m-%d-%H-%M-%OS"), ".csv"))
  
}

main()


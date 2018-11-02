library(magrittr)
library(dplyr)
library(tidyr)
library(broom)
library(glue)
library(tibble)
library(pforeach)
library(RODBC)


DSN <- "nissanpoc"
USERID <- "nissan"
PASSWORD <- "nissan"

# ODBC接続 to TERADATA
con <- odbcConnect(DSN, USERID, PASSWORD)

q <- glue(
  "
  SELECT 
  	RES_ID,
    VAR_ID,
    VAR_SUB_ID,
    EXP_VAL,
    CAST(ROUND(OBJ_VAL) AS INTEGER) as OBJ_VAL
  FROM 
    NOX_ADS_REGRESSION 
  WHERE 
    (VAR_ID, VAR_SUB_ID) IN 
      (
        SELECT 
          VAR_ID,
          VAR_SUB_ID
        FROM 
          NOX_EXEC_LOGIT
        WHERE 
          TERM = 'EXP_VAL'
        AND 
          P_VALUE <= 0.001 --451変数
      )
  ;
  "
)

sqlQuery(con, q) %>% 
  as_tibble() -> vtbl

vtbl %>% 
  unite(cols=c(VAR_ID, VAR_SUB_ID)) -> htbl

colnames(htbl)[2] <- "VAR_COMB" 

htbl %<>% 
  spread(key = VAR_COMB, value = EXP_VAL)


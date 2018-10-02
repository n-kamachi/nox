library(tidyverse)
library(foreach)
library(dplyr.teradata)
library(glue)

con <- dbConnect(todbc(),
                 driver = "/Library/Application Support/teradata/client/16.10/lib/tdata.dylib",
                 DBCName = "****",
                 uid = "****",
                 pwd = "****",
                 charset = "UTF8")

cast_query <- function(min_nox_threshold = 40, min_span_threshold){
  
  cat(glue("{min_span_threshold}\n"))
  
  q <- glue(
    "WITH abnormal as
    (
    SELECT 
      file_id
     ,nox_time
     ,var_id
     ,val
    FROM
    (
    SELECT 
      file_id
     ,nox_time
     ,var_id
     ,val
     ,SUM(nox_time) OVER (PARTITION BY file_id ORDER BY nox_time ROWS BETWEEN 1 PRECEDING AND 1 PRECEDING) AS pre_time
    FROM
      (SELECT * FROM nissan.NOX_AN_VERT_TBL WHERE var_id = 33 AND val > {min_nox_threshold}) AS over_thresh
    ) AS compute_lag_time
    WHERE
      (nox_time - pre_time > {min_span_threshold} OR pre_time IS NULL)
     AND
      nox_time > {min_span_threshold}
    )
    SELECT
      COUNT(*) AS fails
    FROM
      abnormal
    ;"
  )
  
  # Cast query and attach parameter information
  q %>%
    db_collect(con = con,.) %>%
    mutate(min_nox_threshold = min_nox_threshold,
           min_span_threshold = min_span_threshold) -> df
  
  return(df)
  
}

dfs <- foreach(i = 10:30, .combine = bind_rows) %do% {
  
  cast_query(min_span_threshold = i)
  
}

# Group by keys
dfs %>%
  group_by(min_span_threshold) %>%
  summarise(sum_ng = sum(fails)) %>% 
  write_csv("ng.csv")

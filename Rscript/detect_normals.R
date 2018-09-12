library(tidyverse)
library(dplyr.teradata)
library(foreach)
library(glue)

con <- dbConnect(todbc(),
                 driver = "/Library/Application Support/teradata/client/16.10/lib/tdata.dylib",
                 DBCName = "153.65.169.70",
                 uid = "****",
                 pwd = "****",
                 charset = "UTF8") # For mac, charaset must be taken.

cast_query <- function(nox_ppm_threshold){
  
  print(nox_ppm_threshold)
  
  q <- glue(
    "
     SELECT
        COUNT(*) AS ok_cnt
     FROM
      (
       SELECT 
         file_id
        ,nox_time
        ,var
        ,val
        ,MAX(val) OVER (PARTITION BY file_id ORDER BY nox_time ROWS BETWEEN 1200 PRECEDING AND CURRENT ROW) AS mx -- 120秒前からの最大値取得
        ,COUNT(nox_time) OVER (PARTITION BY file_id ORDER BY nox_time ROWS BETWEEN 1200 PRECEDING AND CURRENT ROW) AS cn -- 120秒前から数えて何レコードあるか(1201だと2分前からある)
       FROM
         nissan.NOX_LOAD_TS_MERGE
       WHERE
         var = '瞬時DIR_NOX濃度' 
      ) AS two_minutes
     WHERE
       mx <= {nox_ppm_threshold}
      AND
       cn = 1201
     ;
  "
  )
  
  q %>% 
    db_collect(con = con,.) %>%
    mutate(nox_threshold = nox_ppm_threshold) -> df
  
  return(df)

}

dfs <- foreach(i = (2:8)*5, .combine = bind_rows) %do% {
  cast_query(i)
}



dfs %>%
  ggplot() +
  geom_bar(aes(x = nox_threshold, y = ok_cnt), stat = "identity") +
  geom_text(
    aes(x = nox_threshhold,
      y = ok_cnt,
      label = sprintf("%7d", ok_cnt)))
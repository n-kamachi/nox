library(tidyverse)
library(foreach)
library(dplyr.teradata)
library(glue)

con <- dbConnect(todbc(),
                 driver = "/Library/Application Support/teradata/client/16.10/lib/tdata.dylib",
                 DBCName = "********",
                 uid = "****",
                 pwd = "****")

cast_query <- function(min_nox_threshold = 40, min_span_threshold){
  
  cat(glue("{min_span_threshold}\n"))
  
  q <- glue(
    "WITH annormal as
    (
     SELECT 
       file_id
      ,nox_time
      ,var
      ,val
     FROM
     (
      SELECT 
        file_id
       ,nox_time
       ,var
       ,val
       ,SUM(nox_time) OVER (PARTITION BY file_id ORDER BY nox_time ROWS BETWEEN 1 PRECEDING AND 1 PRECEDING) AS pre_time
      FROM
        (SELECT * FROM nissan.NOX_LOAD_TS_MERGE WHERE var like '%DIR_NOX%' and val > {min_nox_threshold}) as over_thresh
     ) as compute_lag_time
     WHERE
       (nox_time - pre_time > {min_span_threshold} OR pre_time is NULL)
      AND
       nox_time > {min_span_threshold}
    )
    SELECT
      COUNT(*) AS fails
    FROM
      annormal
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

# write.csv(dfs, "dfs.csv", row.names = FALSE)

# Group by keys
dfs %>%
  group_by(min_span_threshold) %>%
  summarise(sum_ng = sum(fails)) -> tmp

# Draw Barplot
ggplot(data = tmp,
       aes(x = min_span_threshold, y = sum_ng)
       ) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(x = min_span_threshold,
        y = sum_ng,
        label = sprintf("%3d", sum_ng))) +
  ggtitle("Number of NOX NG") -> g


ggsave("number_of_nox_ng.png", g)

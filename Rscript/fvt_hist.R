library(dplyr)
library(ggplot2)
library(RODBC)


DSN <- "nissanpoc"
USERID <- "nissan"
PASSWORD <- "nissan"

# ODBC接続 to TERADATA
ch <- odbcConnect(DSN, USERID, PASSWORD)

col_list <- sqlQuery(ch, "SELECT SENSOR_NAME FROM FVT_CA_LOAD_TBL_STATS ORDER BY 1;")
col_list <- col_list$SENSOR_NAME %>%
  as.vector()

st <- proc.time()
for(i in 1:length(col_list)){
  q <- paste0("SELECT ", col_list[i], " FROM FVT_CA_LOAD_TBL_FIX WHERE ", col_list[i], " IS NOT NULL;")
  df_i <- sqlQuery(ch, q)
  
  h <- df_i[,1] %>%
    hist() %>% 
    `[`(c("mids", "counts")) %>%
    as.data.frame() %>%
    mutate(width = if_else(is.na(mids[2] - mids[1]), 0.1, mids[2] - mids[1]))
  
  
  g <- h %>% 
    ggplot(aes(x = mids, y = counts, width = width)) + 
    geom_col() +
    xlab("Value") +
    ylab("Count") +
    ggtitle(paste0("Histogram : ", col_list[i], " / Width : ", h$width[1])) +
    theme(plot.title = element_text(hjust = 0.5))
    
  ggsave(
    filename = sprintf("./fvt_hist/hist_%s.png", col_list[i]),
    device = "png",
    plot = g,
    width = 10.2,
    height = 4.6
  )
}
ed <- proc.time()
ed - st


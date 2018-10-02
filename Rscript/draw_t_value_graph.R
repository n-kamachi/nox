# Script to create t-value graph from regression result table

library(magrittr)
library(dplyr)
library(ggplot2)
#library(dplyr.teradata)
library(RODBC)

DSN <- "***"
USERID <- "***"
PASSWORD <- "***"

# ODBC接続 to TERADATA
con <- odbcConnect(DSN, USERID, PASSWORD)

#con <- dbConnect(drv = todbc(),
#                 driver = "C:/Program Files/Teradata/Client/16.10/bin/TDATA32.DLL",
#                 DBCName = "153.65.169.70",
#                 #dsn = DSN,
#                 uid = USERID,
#                 pwd = PASSWORD)

create_gg_in <- function(){
  
  ols <- sqlFetch(con, "NOX_EXEC_REGRESSION")

  ols %>% 
    select(VAR_ID, VAR_SUB_ID, T_VALUE) %>% 
    collect() %>% 
    as_tibble() -> df
  
  df %<>%
    mutate(
      block = case_when(
        VAR_SUB_ID == 0 ~ "a:0秒",
        VAR_SUB_ID == 1 ~ "b:0.1秒",
        VAR_SUB_ID == 2 ~ "c:0.2~0.3秒",
        VAR_SUB_ID == 3 ~ "d:0.4~0.6秒",
        VAR_SUB_ID == 4 ~ "e:0.7~1.1秒",
        VAR_SUB_ID == 5 ~ "f:1.2~1.9秒",
        VAR_SUB_ID == 6 ~ "g:2.0~3.2秒",
        VAR_SUB_ID == 7 ~ "h:3.3~5.3秒",
        VAR_SUB_ID == 8 ~ "i:5.4~8.7秒",
        VAR_SUB_ID == 9 ~ "j:8.8~14.2秒",
        VAR_SUB_ID == 10 ~ "k:14.3~20.0秒"
      ))
  
  var_mst <- sqlFetch(con, "NOX_MST_AN_VARIABLE")
  
  df %<>% left_join(var_mst, by = "VAR_ID")
  
  return(df)
}


df <- create_gg_in()
df$VAR_ID %>% as.vector() %>% unique() -> var_id

y_upper <- 26L
y_lower <- -1 * y_upper

for(i in var_id){
  
  df %>% filter(VAR_ID == i) -> df_i
  
  df_i$T_VALUE %>% abs() %>% max() -> max_t
  
  #y_upper <- case_when(
  #  max_t < 5L ~ 5L,
  #  max_t < 15L ~ 15L,
  #  max_t < 30L ~ 30L
  #)
  
  g <- ggplot(data = df_i, aes(x = block, y = T_VALUE, group = 1)) +
    geom_line() +
    geom_point() +
    theme(axis.text.x = element_text(size=10)) +
    #theme_gray(base_family = "HiraKakuPro-W3") + 
    ggtitle(paste0("T値_", df_i$VAR)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("集約範囲") +
    ylab("T値") + 
    ylim(c(y_lower, y_upper)) + 
    geom_hline(yintercept = -1.96, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = 1.96, colour = "red", linetype = "dashed")
  
  ggsave(
    filename = sprintf("./linear_graph/t_value_%s.png", df_i$VAR),
    device = "png",
    plot = g,
    width = 10.2,
    height = 4.6
    )
  
}

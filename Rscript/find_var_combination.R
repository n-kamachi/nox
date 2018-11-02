# ランダムフォレストで頻出する変数組合せを見つけるスクリプト
library(tidyverse)
library(openxlsx)

df <- "var_cnts_1000.csv" %>% 
  read_csv() %>% 
  rename(tree_id = idx)

# k階層目

cnt_unique_tree <- function(stage){
  
  df %>%
    select(tree_id, paste0("var", stage)) %>% 
    data.table::data.table() %>% 
    data.table::setnames(c("tree_id", "var")) %>% 
    group_by(var) %>%
    summarise(unique_id = n_distinct(tree_id)) %>%
    arrange(desc(unique_id)) %>% 
    data.table::data.table() %>% 
    data.table::setnames(old = "var", new = paste0("var", stage)) %>% 
    as_tibble() -> cnt
  
  return(cnt)
  
}

cnt_unique_tree(1) -> cnt1
cnt_unique_tree(2) -> cnt2
cnt_unique_tree(3) -> cnt3


# i階層目を条件づけたときにj階層目

cnt_unique_tree_cond <- function(cond_stage, cnt_stage = cond_stage + 1){
  
  # Validation
  if(cond_stage >= cnt_stage) stop("cond_stage must be smaller than cnt_stage.")
  if(max(cond_stage, cnt_stage) > 3L) stop("The deepest stage is 3.")
  if(min(cond_stage, cnt_stage) < 1L) stop("Stages must be positive integer less than 4.")
  
  df %>%
    select(tree_id, paste0("var", cond_stage), paste0("var", cnt_stage)) %>% 
    data.table::data.table() %>% 
    data.table::setnames(c("tree_id", "cond_var", "cnt_var")) %>% 
    group_by(cond_var, cnt_var) %>%
    summarise(unique_id = n_distinct(tree_id)) %>%
    arrange(desc(unique_id)) %>% 
    data.table::data.table() %>% 
    data.table::setnames(
      old = c("cond_var", "cnt_var"),
      new = c(
        paste0("var", cond_stage),
        paste0("var", cnt_stage)
        )
      ) %>% 
    as_tibble() -> cnt
  
  return(cnt)
}

cnt_unique_tree_cond(1) -> cnt12
cnt_unique_tree_cond(2) -> cnt23
cnt_unique_tree_cond(1, 3) -> cnt13

# 書き込み用エクセルブック準備
wb <- createWorkbook()

# ワークシートの追加
addWorksheet(wb, 'stage1')
addWorksheet(wb, 'stage2')
addWorksheet(wb, 'stage3')
addWorksheet(wb, 'stage2_cond1')
addWorksheet(wb, 'stage3_cond2')
addWorksheet(wb, 'stage3_cond1')

writeData(wb, sheet = 1, rowNames = F, cnt1)
writeData(wb, sheet = 2, rowNames = F, cnt2)
writeData(wb, sheet = 3, rowNames = F, cnt3)
writeData(wb, sheet = 4, rowNames = F, cnt12)
writeData(wb, sheet = 5, rowNames = F, cnt23)
writeData(wb, sheet = 6, rowNames = F, cnt13)

saveWorkbook(wb, "unique_tree_cnt_1000.xlsx", overwrite = TRUE)

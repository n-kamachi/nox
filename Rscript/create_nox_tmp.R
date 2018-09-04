library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(stringr)
library(purrr)
library(foreach)

dpath <- "nox_data" # loadファイル群の最上位パスを指定

get_filenames <- function(directorty){
  return(list.files(
    directorty,
    full.names = TRUE,
    recursive = TRUE)
    )
  }

make_tidy_data <- function(path){
  
  df <- path %>% 
    fread(skip = 2L) %>% # 最初2行はテスト概要のため除外
    select(-51L, -52L) %>%  # ブランクの列除外
    slice(-1L) %>% # 変数名の次の行は単位や基準値のため除外
    filter(str_detect(時間, "^(-|[0-9])")) %>%  # 最初の列が数値になるものは残す(正規表現使用)
    mutate(fname = path) %>% # パス名カラム追加
    gather(key = "key", value = "val", -fname, -時間) # 縦持ち変換
    
  return(df)

  }

fnames <- get_filenames(directorty = dpath)

main_df <- foreach(i = fnames, .combine = rbind) %do% {
    make_tidy_data(i)
} 

label_list <- main_df %>% 
  select(fname, key) %>% 
  distinct() %>%  # ファイルごと変数名一覧(縦持ち)
  mutate(tmp = 1L) %>% 
  spread(key = fname, value = tmp, fill = 0L) # ファイル×変数名

#write.csv(main_df, "main_df.csv", row.names = FALSE, fileEncoding = "utf-8")
#write.csv(label_list, "label_list.csv", row.names = FALSE, fileEncoding = "utf-8")
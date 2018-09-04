library(data.table)
library(dplyr)

options(digits = 20)

data_path <- "./add_data/"
in_path <- "./in/"
out_path <- "./out/"

file_list <- list.files(data_path)

# file_idは続きから設定
file_id <- 58
file_id_mst <- NULL

st <- proc.time()
for(file in 1:length(file_list)){
  tmp_data <- fread(paste0(data_path,file_list[file]), header = F, sep = ",", data.table = F)
  
  header_row <- charmatch("時間",tmp_data[,1])
  start_row <- header_row + 2
  
  end_row <- NULL
  for(row in start_row:nrow(tmp_data)){
    if(tmp_data[row,1] != ""){
    } else {
      end_row <- row-1
      break
    }
  }
  
  row_num <- end_row - start_row + 1
  
  tmp_df <- data.frame(FILE_ID=rep(file_id,row_num))
  tmp_file_mst <- data.frame(FILE_ID=file_id, FILE_NAME=file_list[file])
  
  tmp_label_list <- tmp_data[header_row,] %>% t() %>% na.omit() %>% as.vector()
  
  tmp_ts_data <- tmp_data[start_row:end_row,] %>% sapply(as.numeric)
  
  start_na_col <- 51
  end_na_col <- 52
  
  tmp_ts_df <- data.frame(tmp_df, tmp_ts_data[,1:(start_na_col-1)], tmp_ts_data[,(end_na_col+1):ncol(tmp_ts_data)])
  
  for(label in 1:length(tmp_label_list)){
    colnames(tmp_ts_df)[label+1] <- tmp_label_list[label]
  }
  
  tmp_ts_df_v <- tmp_ts_df %>%
    melt(id.vars=c("FILE_ID", "時間"))
  
  file_id_mst <- rbind(file_id_mst, tmp_file_mst)
  
  write.csv(tmp_ts_df_v, paste0(out_path, "NOX_LOAD_TS_", sprintf("%03d", file_id),"_", substr(file_list[file],1,9), ".csv"), row.names = F, quote = F, fileEncoding = "UTF-8", na="")
  
  file_id <- file_id + 1
}
ed <- proc.time()

print(ed-st)
write.csv(file_id_mst, paste0(out_path, "mst_file_id.csv"), row.names = F, quote = F, fileEncoding = "UTF-8")

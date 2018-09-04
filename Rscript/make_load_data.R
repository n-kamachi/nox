library(data.table)
library(dplyr)

options(digits = 20)

data_path <- "./data/"
in_path <- "./in/"
out_path <- "./out/"

file_list <- list.files(data_path)

column_list <- fread(paste0(in_path,"uniq_label_list.csv"), header = T, sep = ",", data.table = F, encoding = "UTF-8")
column_vec <- column_list$label %>% as.vector()

for(file in 1:length(file_list)){
  tmp_data <- fread(paste0(data_path,file_list[file]), header = F, sep = ",", data.table = F, encoding = "UTF-8")

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

  tmp_df <- data.frame(file_name=rep(file_list[file],row_num))

  for(label in 1:length(column_vec)){
    col_position <- charmatch(column_vec[label],tmp_data[header_row,])

    if(is.na(col_position) == F){
      col_val_vec <- tmp_data[start_row:end_row,col_position] %>% sapply(as.numeric) %>% as.vector()
    }
    else{
      col_val_vec <- rep("",row_num)
    }
    tmp_df <- data.frame(tmp_df,col_val_vec)

    colnames(tmp_df)[label+1] <- column_vec[label]

  }
  write.csv(tmp_df, paste0(out_path, "load_", file_list[file]), row.names = F, quote = F, fileEncoding = "UTF-8", na="")
}

library(data.table)
library(dplyr)
library(stringr)

data_path <- "./nox_data/"
in_path <- "./in/"
out_path <- "./out/"

com_label <- fread("common_label.csv", header = T, sep = ",", data.table = F, encoding = "UTF-8")$COM_LABEL %>% c()

fullpath_file_list <- list.files(data_path, pattern = "csv", recursive = T)
file_list <- str_split(fullpath_file_list, "/", simplify = T)
file_list <- paste0(file_list[,6], file_list[,7])

label_df <- NULL

st <- proc.time()
for(i in 1:length(fullpath_file_list)){
  tmp_data <- fread(paste0(data_path,fullpath_file_list[i]), header = F, sep = ",", data.table = F)
  
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
  
  tmp_com_label <- tmp_data[header_row, 1:length(com_label)] %>% t() %>% as.vector()
  
  upd_com_label <- NULL
  for(j in 1:length(com_label)){
    if(tmp_com_label[j] == com_label[j]){
      upd_com_label <- c(upd_com_label, com_label[j])
    }
    else{
      upd_com_label <- c(upd_com_label, paste0(com_label[j],tmp_com_label[j]))
    }
  }
  
  tmp_label_list <- tmp_data[3,53:ncol(tmp_data)] %>% t() %>% na.omit() %>% as.vector()
  tmp_label_list <- c(upd_com_label, tmp_label_list)
  tmp_df <- data.frame(file=fullpath_file_list[i], label=tmp_label_list) %>% subset(label != "時間")
  label_df <- rbind(label_df, tmp_df)
}
ed <- proc.time()
print(ed-st)

uniq_label_list <- unique(label_df$label) %>% as.data.frame()
colnames(uniq_label_list)[1] <- "label"

write.csv(uniq_label_list, paste0(in_path,"uniq_label_list_", format(Sys.time(), "%Y-%m-%d-%H-%M-%OS"), ".csv"), row.names = F, quote = F, fileEncoding = "UTF-8")
write.csv(label_df, paste0(in_path,"file-label_list_", format(Sys.time(), "%Y-%m-%d-%H-%M-%OS"), ".csv"), row.names = F, quote = F)

library(data.table)
library(dplyr)
library(stringr)

data_path <- "./nox_data/"
in_path <- "./in/"
out_path <- "./out/"

fullpath_file_list <- list.files(data_path, pattern = "csv", recursive = T)
file_list <- str_split(fullpath_file_list, "/", simplify = T)
file_list <- paste0(file_list[,6], file_list[,7])

label_df <- NULL
for(i in 1:length(fullpath_file_list)){
  tmp_data <- fread(paste0(data_path,fullpath_file_list[i]), header = F, sep = ",", data.table = F)
  tmp_label_list <- tmp_data[3,] %>% t() %>% na.omit() %>% as.vector() 
  tmp_df <- data.frame(file=fullpath_file_list[i], label=tmp_label_list) %>% subset(label != "時間")
  label_df <- rbind(label_df, tmp_df)
}

uniq_label_list <- unique(label_df$label) %>% as.data.frame()
colnames(uniq_label_list)[1] <- "label"

write.csv(uniq_label_list, paste0(in_path,"uniq_label_list_", format(Sys.time(), "%Y-%m-%d-%H-%M-%OS"), ".csv"), row.names = F, quote = F, fileEncoding = "UTF-8")
write.csv(label_df, paste0(in_path,"file-label_list_", format(Sys.time(), "%Y-%m-%d-%H-%M-%OS"), ".csv"), row.names = F, quote = F)

library(dplyr)
library(RODBC)

IN_PATH <- "./in/"
OUT_PATH <- "./out/"

#logit_data <- fread(paste0(IN_PATH,"logit_reg.csv"), header=T, sep=',', data.table=F, encoding = "UTF-8")
#View(logit_data)

DSN <- "***"
USERID <- "***"
PASSWORD <- "***"

# ODBC接続 to TERADATA
ch <- odbcConnect(DSN, USERID, PASSWORD)

logit_data <- sqlQuery(ch, 
                       "SELECT 
                          a.RES_ID,
                          a.VAR_ID,
                          a.VAR_SUB_ID,
                          DENSE_RANK() OVER(ORDER BY a.VAR_ID, a.VAR_SUB_ID) as LOGIT_VAR_ID, --ロジスティック回帰用にユニーク変数IDを作成
                          a.EXP_VAL,
                          b.FLG --NG：1、OK：0
                        FROM 
                          NOX_EXP_VARIABLE_ZCHK a
                        INNER JOIN 
                          NOX_RES_VARIABLE b
                        ON 
                          a.RES_ID = b.RES_ID
                        ;")

varid <- NULL
subid <- NULL
b0 <- NULL
b1 <- NULL
zval <- NULL
pval <- NULL
res_out <- NULL

st <- proc.time()
for(i in 1:max(logit_data$LOGIT_VAR_ID)){
  tmp_data <- logit_data %>%
    filter(LOGIT_VAR_ID == i) %>%
    select(RES_ID, VAR_ID, VAR_SUB_ID, EXP_VAL, FLG)
  
  tmp_logit <- glm(data=tmp_data, FLG ~ EXP_VAL, family = binomial(link = "logit"))
  sum_logit <- summary(tmp_logit)
  
  varid <- c(varid, tmp_data$VAR_ID[1]) %>% as.vector()
  subid <- c(subid, tmp_data$VAR_SUB_ID[1]) %>% as.vector()
  b0 <- c(b0, sum_logit$coefficients[1,1]) %>% as.vector()
  b1 <- c(b1, sum_logit$coefficients[2,1]) %>% as.vector()
  zval <- c(zval, sum_logit$coefficients[2,3]) %>% as.vector()
  pval <- c(pval, sum_logit$coefficients[2,4]) %>% as.vector()
}
ed <- proc.time()
print(ed-st)

res_out <- data.frame(VAR_ID=varid, VAR_SUB_ID=subid, b0=b0, b1=b1, Z_VALUE=zval, P_VALUE=pval)

create_gg_in <- function(){
  res_out %<>%
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
  
  var_mst <- sqlFetch(ch, "NOX_MST_AN_VARIABLE")
  
  res_out %<>% left_join(var_mst, by = "VAR_ID")
  
  return(res_out)
}


res_out <- create_gg_in()
res_out$VAR_ID %>% as.vector() %>% unique() -> var_id

y_upper <- 21L
y_lower <- -1 * y_upper

for(i in var_id){
  
  res_out %>% filter(VAR_ID == i) -> res_i
  
  res_i$Z_VALUE %>% abs() %>% max() -> max_t
  
  #y_upper <- case_when(
  #  max_t < 5L ~ 5L,
  #  max_t < 15L ~ 15L,
  #  max_t < 30L ~ 30L
  #)
  
  g <- ggplot(data = res_i, aes(x = block, y = Z_VALUE, group = 1)) +
    geom_line() +
    geom_point() +
    theme(axis.text.x = element_text(size=10)) +
    #theme_gray(base_family = "HiraKakuPro-W3") + 
    ggtitle(paste0("単変量ロジスティック回帰 T値 ", res_i$VAR)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("集約範囲") +
    ylab("T値") + 
    ylim(c(y_lower, y_upper)) + 
    geom_hline(yintercept = -1.96, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = 1.96, colour = "red", linetype = "dashed")
  
  ggsave(
    filename = sprintf("./logit_graph/t_value_%s.png", res_i$VAR),
    device = "png",
    plot = g,
    width = 10.2,
    height = 4.6
  )
  
}

write.csv(res_out, paste0(OUT_PATH,"res_logit_reg_", format(Sys.time(), "%Y-%m-%d-%H-%M-%OS"), ".csv"), row.names = F, quote = F, fileEncoding = "UTF-8", na="")

library(dplyr)
library(networkD3)
library(jsonlite)

path <- "./noxd3/"

link_df <- read.csv(paste0(path, "link_info_odds.csv"), header = T, sep = ",", fileEncoding = "utf8")
node_df <- read.csv(paste0(path,"node_info_odds_20.csv"), header = T, sep = ",", fileEncoding = "utf8")

network_d3 <- function(odds = 1.15, node_pr = 0.2, link_pr = 0.2, linkdist = 50, opacity = 0.8, charge = -30){
  nodes <-  node_df %>%
    filter(node_prov >= node_pr)
  
  links <- link_df %>%
    filter(odds_value >= odds, link_prov >= link_pr,
           label1 %in% nodes$label, label2 %in% nodes$label)
  
  nodes %<>%
    filter(label %in% links$label1 | label %in% links$label2) %>%
    arrange(clust_id, desc(odds_rate)) %>%
    mutate(
      group = case_when(
        clust_id == 1 ~ "cluster-01",
        clust_id == 2 ~ "cluster-02",
        clust_id == 3 ~ "cluster-03",
        clust_id == 4 ~ "cluster-02",
        clust_id == 5 ~ "cluster-03",
        clust_id == 6 ~ "cluster-01",
        clust_id == 7 ~ "cluster-04",
        clust_id == 8 ~ "cluster-04",
        clust_id == 9 ~ "cluster-09",
        clust_id == 10 ~ "cluster-01",
        clust_id == 11 ~ "cluster-05",
        clust_id == 12 ~ "cluster-06",
        clust_id == 13 ~ "cluster-07",
        clust_id == 14 ~ "cluster-06",
        clust_id == 15 ~ "cluster-07",
        clust_id == 16 ~ "cluster-08",
        clust_id == 17 ~ "cluster-08",
        clust_id == 18 ~ "cluster-05",
        clust_id == 19 ~ "cluster-09",
        clust_id == 20 ~ "cluster-07" ), 
      id = row_number()-1) %>%
    select(-clust_id)
  
  links %<>%
    merge(nodes, by.x = "label1", by.y = "label") %>%
    select( label1, label2, source = id, odds_value, link_prov) %>%
    merge(nodes, by.x = "label2", by.y = "label") %>%
    select(label1, label2, source, target = id, odds_value, link_prov)
  
  # trans from data.frame to json
  node_js <- toJSON(nodes, pretty = T)
  link_js <- toJSON(links, pretty = T)
  
  # Plot
  forceNetwork(Links = links, Nodes = nodes,
               Source = "source", Target = "target",
               Value = "odds_value", NodeID = "label",
               Group = "group", Nodesize = "odds_rate",
               opacity = opacity,
               linkDistance = linkdist, fontFamily = "MeiryoUI",
               #linkWidth = JS("function(d) { return Math.exp((d.odds_value * 10)) / 20000 ; }"),
               #radiusCalculation = JS("  return Math.exp((d.odds_rate * 10)^2) / 100000 + 10; "), 
               colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
               fontSize = 18, charge = charge, linkColour = "gray", legend = T
               )
}

network_d3(odds = 1.15, node_pr = 0.2, link_pr = 0.2, linkdist = 50, opacity = 0.8, charge = -30)
network_d3(odds = 1.1, node_pr = 0.2, link_pr = 0.2, linkdist = 50, opacity = 0.8, charge = -30)
network_d3(odds = 1.2, node_pr = 0.2, link_pr = 0.2, linkdist = 50, opacity = 0.8, charge = -30)


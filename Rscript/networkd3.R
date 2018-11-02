library(dplyr)
library(networkD3)
library(jsonlite)

path <- "./noxd3/"

link_df <- read.csv(paste0(path, "link_info_odds.csv"), header = T, sep = ",", fileEncoding = "utf8")
node_df <- read.csv(paste0(path,"node_info_odds.csv"), header = T, sep = ",", fileEncoding = "utf8")

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
        clust_id == 4 ~ "cluster-04",
        clust_id == 5 ~ "cluster-05",
        clust_id == 6 ~ "cluster-06",
        clust_id == 7 ~ "cluster-07",
        clust_id == 8 ~ "cluster-08",
        clust_id == 9 ~ "cluster-09",
        clust_id == 10 ~ "cluster-10",
        clust_id == 11 ~ "cluster-11",
        clust_id == 12 ~ "cluster-12",
        clust_id == 13 ~ "cluster-13",
        clust_id == 14 ~ "cluster-14",
        clust_id == 15 ~ "cluster-15",
        clust_id == 16 ~ "cluster-16",
        clust_id == 17 ~ "cluster-17",
        clust_id == 18 ~ "cluster-18",
        clust_id == 19 ~ "cluster-19",
        clust_id == 20 ~ "cluster-20",
        clust_id == 21 ~ "cluster-21",
        clust_id == 22 ~ "cluster-22",
        clust_id == 23 ~ "cluster-23",
        clust_id == 24 ~ "cluster-24",
        clust_id == 25 ~ "cluster-25",
        clust_id == 26 ~ "cluster-26",
        clust_id == 27 ~ "cluster-27",
        clust_id == 28 ~ "cluster-28",
        clust_id == 29 ~ "cluster-29",
        clust_id == 30 ~ "cluster-30",
        clust_id == 31 ~ "cluster-31",
        clust_id == 32 ~ "cluster-32",
        clust_id == 33 ~ "cluster-33" ), 
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
               #radiusCalculation = JS("function(d) { return Math.exp((d.odds_rate * 10)^2) / 100000 + 10; }"), 
               colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
               fontSize = 18, charge = charge, linkColour = "gray", legend = T
               )
}

network_d3(odds = 1.15, node_pr = 0.2, link_pr = 0.2, linkdist = 50, opacity = 0.8, charge = -30)


# sample
data(MisLinks)
data(MisNodes)
View(MisLinks)
View(MisNodes)

forceNetwork(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name", Nodesize = "size",
             Group = "group", opacity = 0.8)

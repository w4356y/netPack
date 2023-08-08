## Generate a dataframe from genus-genus interactuon corelation results
generate_link <- function(a_list, cor,p){
  source = c()
  end = c()
  weight = c()
  for(l in c(1:length(a_list))){
    if(p[[l]]< 0.05){
      source[length(source) + 1] = a_list[[l]][1]
      end[length(end) + 1] = a_list[[l]][2]
      weight[length(weight) + 1] =cor[[l]]
    }
  }
  df_link = data.frame(source, end, weight)
  return(df_link)
}

## calculate network centrality based on distance to others in the graph
get_centrality <- function(df){
  g1 = graph_from_data_frame(df, directed = FALSE)
  s = centr_clo(g1, mode="all", normalized=T)
  return(s$centralization)
}

## convert list to data frame
list_to_frame <- function(a_list){
  d1 = a_list %>% as.data.frame() %>% tidyr::gather(key = "sample", value = "avg_links")
  d1$sample = factor(d1$sample, levels = unique(d1$sample))
  return(d1)
}

plot_group_box <-function(d){
  comp = generate_comprison(unique(d1$sample))
  d1 %>% ggplot(aes(x = sample, y = avg_links)) +
    geom_boxplot() +
    ggpubr::stat_compare_means(comparisons = comp, method = "t.test")
}

plot_trend <- function(d, metric = "mean"){
  if(metric == "mean"){
    d %>% group_by(sample) %>% summarise(m = mean(avg_links), sd = mean(avg_links)) %>% ggplot(aes(x = sample, y = m)) + geom_point()
  }else if(metric == "sd"){
    d %>% group_by(sample) %>% summarise(m = mean(avg_links), sd = sd(avg_links)) %>% ggplot(aes(x = sample, y = sd)) + geom_point()
  }else{
    return("Invalid metric value.")
  }

}

## calculate the corelation of 2 genus
cal_cor <- function(x,y){
  x1 = seq_sample[,x]
  x2 = seq_sample[,y]
  if(sum(x1>0 & x2>0) >=3){
    return(cor.test(x1[x1>0 & x2>0], x2[x1>0 & x2>0],method = "pearson")$estimate)
  }else{
    return(0)
  }
}
## calculate the significance level of  corelation of 2 genus
cal_p <- function(x,y){
  x1 = seq_sample[,x]
  x2 = seq_sample[,y]
  if(sum(x1>0 & x2>0) >=3){
    return(cor.test(x1[x1>0 & x2>0], x2[x1>0 & x2>0],method = "pearson")$p.value)
  }else{
    return(1)
  }
}

## plot trend line
plot_trend_line <- function(d){
  d %>% group_by(sample) %>%  summarise(m = mean(avg_links), sd = sd(avg_links)) %>%  mutate(size = as.numeric(str_extract(sample, "\\d+"))) %>%
    ggplot(aes(x = size, y = m)) + geom_point() + geom_line()
}

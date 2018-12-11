fill_at <- function(data,col,keyword,fill_vector) {
  filled_entries <- c()
  
  data <- as.data.frame(data)
  breaks <- str_detect(data[,col],keyword)
  filled_entries <- fill_vector[(cumsum(breaks)-breaks[1])+1]
  
  data <- data %>% mutate(weeks=filled_entries)
  return(data)
  
  
}


fill_along <- function(data,col,repl_df){
  for (i in 1:length(data[,col])) {
    whichMatches <- data[i,col] == repl_df[,1]
    data[i,col] <- repl_df[,2][whichMatches]
  }
  return(data)
}

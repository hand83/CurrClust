### IMPORTED LIBRARIES ########################################################

library(jsonlite)
library(RCurl)
library(XML)
library(ggplot2)
library(ggdendro)



### GLOBAL VARIABLES ##########################################################

FIXER_URL = "http://api.fixer.io/"
BASE = "EUR"



### FUNCIONS ##################################################################

GetCurrNames = function() {
  # Currency list and abbreviations
  
  CTBL = read.csv("currencies.csv", stringsAsFactors = FALSE)
  return(CTBL)
}



GetRates = function(date = "latest") {
  # Get exchange rates from any day since 1999
  # Rates are against EUR by default
  
  url = paste0(FIXER_URL, date, "?base=", BASE) 
  RATELIST = fromJSON(url)
  
  DF = data.frame(label = names(RATELIST$rates),
                  rate = unlist(RATELIST$rates),
                  stringsAsFactors = FALSE,
                  row.names = NULL)
  DF = rbind(DF, 
             data.frame(label = c(BASE),
                        rate = c(1))
  )
  DF = DF[order(DF$label), ]
  
  return( list(Exchange = DF, 
               Date = RATELIST$date)
  )
}



GetMergedData = function(EXC0, EXC1) {
  # Creates data with conversion rates at two time points
  # Requires exchange rates with the same base
  
  # Inner join of data frames created
  MERGED = merge(EXC0$Exchange, EXC1$Exchange, by = "label", all = FALSE)
  names(MERGED)[2:3] = c("rate0", "rate1")
  
  # Get long names
  LN = sapply(MERGED$label, function(x){
    if (length(ALL_CLIST[ALL_CLIST$Code == x, "Long_Name"]) == 1){ 
      ALL_CLIST[ALL_CLIST$Code == x, "Long_Name"]
    } else {
      "na"
    } 
  })
  
  MERGED$Long_Name = LN
  MERGED = MERGED[order(MERGED$label), ]
  return(MERGED)
}



ValueChanges = function(EXC) {
  # calculates the change in the value of the currency
  # assumes that the average value changes of all currencies equals to one
  # Requires the data frame with conversion rates at two time points
  
  # All to all conversion rates
  M1 = sapply(EXC$rate1, function(x) {EXC$rate1/x})
  M0 = sapply(EXC$rate0, function(x) {EXC$rate0/x})
  
  # Ratio of conversion rates at the two time points
  R01 = log(M1) - log(M0)
  n = dim(EXC)[1]
  
  # Calculate the value changes of each currency from the averaged ratios
  vchange = apply(R01, 2, function(x) {exp(sum(x)/n)})
  vrank = as.integer(rank(-1 * vchange))
  
  # Returns percent change in the values
  return(data.frame(label = EXC$label, index = 100 * (vchange - 1), rank = vrank, stringsAsFactors = F))
}



DistFunction = function(x, y) {
  
  # Cosine similarity
  sim = sum(x * y)/sqrt(sum(x^2) * sum(y^2))
  
  # Correct floating point inaccuracies
  if (sim > 1) {
    sim = 1
  }
  if (sim < -1) {
    sim = -1
  }
  
  # Return distance
  return( acos(sim)/pi )
}



GetDist = function(EXC) {
  # Calculates the distance matrix
  # Requires the data frame with conversion rates at two time points
  
  # All to all conversion rates
  M0 = sapply(EXC$rate0, function(x) {EXC$rate0/x})
  M1 = sapply(EXC$rate1, function(x) {EXC$rate1/x})
  
  # Vectors of currency rate changes between time points
  # Normalized to the data at the second time point
  #DIFF = (M1 - M0)/M1
  DIFF = log(M1) - log(M0)
  
  # Obtain cosine distances between currencies
  DIST = apply(DIFF, 2, function(x) {apply(DIFF, 2, function(y) {DistFunction(x, y)})})
  #rownames(Dist) = df$Currency
  colnames(DIST) = EXC$label
  rownames(DIST) = EXC$label
  
  return(DIST)
}


GetDendro = function(DM, VC) {
  hc = hclust(as.dist(DM), method = "average")
  den = dendro_data(hc, type = "rectangle")
  den$labels = merge(den$labels, VC, by = "label", sort = F)
  ggden = ggplot() + 
    geom_segment(data = segment(den), aes(x = x, y = y, xend = xend, yend = yend), size = 1) +
    geom_text(data = label(den), aes(x, y, label = label, color = index), size = 6, hjust = 1, angle = 90, nudge_y = -0.02) +
    scale_y_continuous(expand = c(0.1, 0), limits = c(-0.07, NA)) +
    scale_color_gradient2(low = "blue", mid = "green", high = "red", midpoint = 0) +
    theme(axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 20),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_blank()
    )
  return(ggden)
}



GetClosest = function(DIST, VCH, EXC, curr, n=5) {
  # Returns the most similar currencies
  # Requires the distance matrix
  # Requires the value changes data frame
  # Requires the data frame with conversion rates at two time points
  # Requires a selected currency in 3 letter code format
  
  ord = order(DIST[, curr])
  toplist_names = EXC[ord, c("label", "Long_Name")]
  toplist_dist = DIST[ord, curr]
  toplist_vch = VCH[ord, c("index", "rank")]
  
  
  return(data.frame(Label = toplist_names$label[1:(1 + n)],
                    Currency = toplist_names$Long_Name[1:(1 + n)],
                    Distance = toplist_dist[1:(1 + n)],
                    Index = toplist_vch$index[1:(1 + n)],
                    Rank = toplist_vch$rank[1:(1 + n)],
                    row.names = NULL,
                    stringsAsFactors = F)
  )
}



### MAIN ######################################################################
# 'Frankenshock' : 2015-01-15
# Financial chrisis: 2008 September

ALL_CLIST = GetCurrNames() 
d0 = GetRates("2017-06-06")
d1 = GetRates("2017-06-13")
ER = GetMergedData(d0, d1)
V = ValueChanges(ER)
Dist = GetDist(ER)

gd = GetDendro(Dist, V)
gd
GetClosest(Dist, V, ER, "EUR", 31)

png("dendro.png", width = 640)
gd
dev.off()

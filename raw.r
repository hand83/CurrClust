### IMPORTED LIBRARIES ########################################################

library(jsonlite)
library(RCurl)
library(XML)



### GLOBAL VARIABLES ##########################################################

FIXER_URL = "http://api.fixer.io/"
BASE = "EUR"
CLIST_URL = "http://www.xe.com/iso4217.php"
CLIST_DF = NA #To be initialized and loaded in cache


### FUNCIONS ##################################################################

GetCurrNames = function() {
  # Downloads currency list and abbreviations and returns a data frame
  
  # cnx = getURL(CLIST_URL)
  # doc = htmlParse(cnx, asText = TRUE)
  # tbl = xpathSApply(doc, "//table[@id=\"currencyTable\"]/tbody/tr", xmlDoc)
  # CTBL = as.data.frame(t(
  #   sapply(
  #     tbl, function(x) {xpathSApply(x, "//td", xmlValue)}
  #   )
  # ), stringsAsFactors = FALSE)
  # names(CTBL) = c("Code", "Long_Name")
  CTBL = read.csv("currencies.csv", stringsAsFactors = FALSE)

  return(CTBL)
}



GetRates = function(date = "latest") {
  # Get exchange rates from any day since 1999
  # Rates are against EUR by default
  
  url = paste0(FIXER_URL, date, "?base=", BASE) 
  ratelist = fromJSON(url)

  df = data.frame(
      names(ratelist$rates), 
      unlist(ratelist$rates), 
      stringsAsFactors = FALSE, 
      row.names = NULL)
  names(df) = c("Currency", "Rate")
  df = rbind(df, 
             data.frame(
               Currency = c(BASE), 
               Rate = c(1))
             )
  df = df[order(df$Currency), ]

  return( list(Exchange = df, 
               Date = ratelist$date, 
               Base = ratelist$base)
        )
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



GetDist = function(exc0, exc1) {
  # Requires exchange rates at the same base
  
  # Inner join of data frames created
  df = merge(exc0$Exchange, exc1$Exchange, by = "Currency", all = FALSE)
  names(df)[2:3] = c("Rate0", "Rate1")
  
  # Get the long names of the currencies
  if ( is.null(nrow(ALL_CLIST)) ) {
    ALL_CLIST = GetCurrNames()
  }
  LN = sapply(df$Currency, function(x){
                                      if (length(ALL_CLIST[ALL_CLIST$Code == x, "Long_Name"]) == 1){ 
                                          ALL_CLIST[ALL_CLIST$Code == x, "Long_Name"]
                                      } else {
                                          "na"
                                      } 
                                      })
  CD = data.frame(Code = df$Currency,
                  Long_Name = unlist(LN),
                  row.names = NULL)
  
  # All to all conversion rates
  m0 = sapply(df$Rate0, function(x) {df$Rate0/x})
  m1 = sapply(df$Rate1, function(x) {df$Rate1/x})
  
  # Vectors of currency rate changes between time points
  # The index is normalized to data at the second time point 
  Diff = (m1 - m0)/m1

  # Obtain cosine distances between currencies
  Dist = apply(Diff, 2, function(x) {apply(Diff, 2, function(y) {DistFunction(x, y)})})
  rownames(Dist) = df$Currency
  colnames(Dist) = df$Currency

  rownames(Diff) = df$Currency
  colnames(Diff) = df$Currency
  
  return( list(Difference = Diff,
               Distance = Dist,
               CodeDict = CD)
  )
}



GetClosest = function(dist, curr, n=5) {
  # Returns the most similar currencies
  
  toplist = dist$Distance[order(dist$Distance[, curr]), curr]
  toplist_names = dist$CodeDict[order(dist$Distance[, curr]), ]
  
  
  return(data.frame(Code = toplist_names$Code[2:(1 + n)],
                    Currency = toplist_names$Long_Name[2:(1 + n)],
                    Distance = toplist[2:(1 + n)],
                    row.names = NULL)
  )
}

# calculates the length of the vector
vlen = function(dist) {
  cn = dist$CodeDict$Code
  cl = apply(dist$Difference, 2, function(x) { sqrt(sum(x^2)) })
  cn = cn[order(cl, decreasing = T)]
  cl = cl[order(cl, decreasing = T)]
  return(data.frame(Code = cn, Clen = cl))
}

# calculates the momentum of the currency
# sums up the changes relative to each currencies
# sums up the number of currencies to which the given currency weakened
vmom = function(dist) {
  cn = dist$CodeDict$Code
  cm = apply(dist$Difference, 2, function(x){ sum(x) })
  cneg = apply(dist$Difference, 2, function(x){ sum(x < 0) })
  cn = cn[order(cm, decreasing = T)]
  cneg = cneg[order(cm, decreasing = T)]
  cm = cm[order(cm, decreasing = T)]
  return(data.frame(Code = cn, cmom = cm, cneg = cneg))
}



### MAIN ######################################################################

# 'Frankenshock' : 2015-01-15
# Financial chrisis: 2008 September
ALL_CLIST = GetCurrNames() 
d0 = GetRates("2015-01-14")
d1 = GetRates("2015-01-16")
D = GetDist(d0, d1)
L = vlen(D)
M = vmom(D)
GetClosest(D, "CHF")
hc = hclust(as.dist(D$Distance))
plot(as.dendrogram(hc))


library(ggplot2)
library(ggdendro)

# extract properties of the dendrogram, rectangle type: rectangular lines drawn
den = dendro_data(hc, type = "rectangle")
#groups = cutree(hc, h = 0.5)
#gdf = data.frame(label = names(groups), groups = factor(groups))
names(M) = c("label", "momentum", "weakness")
den$labels = merge(den$labels, M, by = "label")

hcplot = ggplot() + 
  geom_segment(data = segment(den), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(data = label(den), aes(x, y, label = label, color = momentum), size = 6, hjust = 1, angle = 90, nudge_y = -0.02) +
  #coord_fixed(ratio = 0.5) +
  #coord_flip() +
  #scale_y_reverse(expand = c(0.5, 0)) +
  scale_y_continuous(expand = c(0.2, 0), limits = c(-0.09, NA)) +
  scale_color_gradient2(low = "blue", mid = "green", high = "red", midpoint = 0) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 15),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank()
        )
          
hcplot


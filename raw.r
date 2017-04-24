### IMPORTED LIBRARIES ########################################################

library(jsonlite)
library(RCurl)
library(XML)



### GLOBAL VARIABLES ##########################################################

FIXER_URL = "http://api.fixer.io/"
BASE = "USD"
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
  if ( is.null(nrow(CLIST_DF)) ) {
    CLIST_DF = GetCurrNames()
  }
  
  LN = sapply(df$Currency, function(x) {CLIST_DF[CLIST_DF$Code == x, "Long_Name"]})
  print(LN)
  LN = unlist(LN)
  print(LN)
  LND = data.frame(Code = df$Currency, 
                   Long_Name = , 
                   row.names = NULL
                   )
  
  # All to all conversion rates
  m0 = sapply(df$Rate0, function(x) {df$Rate0/x})
  m1 = sapply(df$Rate1, function(x) {df$Rate1/x})
  
  # Vectors of currency rate changes between time points
  Diff = m1 - m0
  
  # Obtain cosine distances between currencies
  Dist = apply(Diff, 1, function(x) {apply(Diff, 1, function(y) {DistFunction(x, y)})})
  rownames(Dist) = df$Currency
  colnames(Dist) = df$Currency
  
  return( list(Distance = Dist,
               Dates = c(exc0$Date, exc1$Date), 
               CodeDict = LND)
        )
}



GetClosest = function(dist, curr, n = 5) {
  # Returns the most similar currencies
  
  toplist = dist$Distance[order(dist$Distance[, curr]), curr]

  return( data.frame(Currencies = names(toplist)[2:(1 + n)], 
                     CurrLong = unlist(sapply(names(toplist)[2:(1 + n)], 
                                       function(x) {dist$CodeDict[dist$CodeDict$Code == x, "Long_Name"]}
                                       )), 
                     Distances = toplist[2:(1 + n)], 
                     row.names = NULL)
        )
}



### MAIN ######################################################################

# 'Frankenshock' : 2015-01-15
# Financial chrisis: 2008 September
CLIST_DF = GetCurrNames() 
d0 = GetRates("2015-01-14")
d1 = GetRates("2015-01-21")
D = GetDist(d0, d1)
GetClosest(D, "CHF")
hc = hclust(as.dist(D$Distance))
plot(as.dendrogram(hc))

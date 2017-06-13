### IMPORTED LIBRARIES ########################################################

library(jsonlite)
library(RCurl)
library(XML)
library(ggplot2)
library(ggdendro)



### GLOBAL VARIABLES ##########################################################

setwd(dir()[1])
FIXER_URL = "http://api.fixer.io/"
BASE = "EUR"



### FUNCTIONS #################################################################

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



GetData = function(date0, date1) {
  dt0 = as.Date(date0)
  dt1 = as.Date(date1)
  IR = GetRates(date0)
  TCOUR = IR$Exchange
  names(TCOUR)[2] = IR$Date
  for (i in 1:(dt1 - dt0)) {
    cdt = format(dt0 + i, "%Y-%m-%d")
    CR = GetRates(cdt)
    if (cdt == CR$Date) {
      TCOUR = merge(TCOUR, CR$Exchange, by = "label", sort = F)
      names(TCOUR)[ncol(TCOUR)] = CR$Date
    }
  }
  return(TCOUR)
}



ValueChanges = function(EXC0, EXC1) {
  # calculates the change in the value of the currency
  # assumes that the average value changes of all currencies equals to one
  # Requires the data frame with conversion rates at two time points
  
  # All to all conversion rates
  M1 = sapply(EXC1, function(x) {EXC1/x})
  M0 = sapply(EXC0, function(x) {EXC0/x})
  
  # Ratio of conversion rates at the two time points
  R01 = log(M1) - log(M0)

  n = dim(R01)[1]
  
  # Calculate the value changes of each currency from the averaged ratios
  vchange = apply(R01, 2, function(x) {exp(sum(x)/n)})

  # Returns percent change in the values
  return( vchange )
}



GetIndices = function(EXC, lag) {
  IND = data.frame(row.names = EXC$label)
  if (lag < (ncol(EXC) - 1)) {
    for (i in 2:(ncol(EXC) - lag)) {
      IND = cbind(IND, ValueChanges(EXC[, i], EXC[, i + lag]))
      names(IND)[i - 1] = names(EXC)[i + lag]
    }
  }
  IND = as.data.frame(t(IND))
  return(cbind(Date = as.Date(rownames(IND)), IND))
}



GetCumIndices = function(EXC) {
  CIND = data.frame(row.names = EXC$label)
  for (i in 3:ncol(EXC)) {
    CIND = cbind(CIND, ValueChanges(EXC[, 2], EXC[, i]))
    names(CIND)[i - 2] = names(EXC)[i]
  }
  CIND = as.data.frame(t(CIND))
  return(cbind(Date = as.Date(colnames(EXC)[3:ncol(EXC)]), CIND))
}



GetDendro = function(DM) {
  hc = hclust(as.dist(DM), method = "average")
  den = dendro_data(hc, type = "rectangle")
  ggden = ggplot() + 
    geom_segment(data = segment(den), aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(data = label(den), aes(x, y, label = label), size = 5, hjust = 1, angle = 90, nudge_y = -0.02) +
    scale_y_continuous(expand = c(0.2, 0), limits = c(-0.07, NA)) +
    theme(axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 15),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_blank()
    )
  return(ggden)
}



GetDendro_v = function(DM, VC) {
  hc = hclust(as.dist(DM), method = "average")
  den = dendro_data(hc, type = "rectangle")
  den$labels = merge(den$labels, VC, by = "label", sort = F)
  ggden = ggplot() + 
    geom_segment(data = segment(den), aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(data = label(den), aes(x, y, label = label, color  = index), size = 5, hjust = 1, angle = 90, nudge_y = -0.02) +
    scale_y_continuous(expand = c(0.2, 0), limits = c(-0.07, NA)) +
    scale_color_gradient2(low = "blue", mid = "green", high = "red", midpoint = 1) +
    theme(axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 15),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_blank()
    )
  return(ggden)
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
  Dist = matrix(0, nrow = nrow(EXC), ncol = nrow(EXC))
  DISTS = list()
  m0 = sapply(EXC[, 2], function(x) {EXC[, 2]/x})
  for (i in 3:ncol(EXC)) {
    m1 = sapply(EXC[, i], function(x) {EXC[, i]/x})
    Diff = 1 - m0/m1
    DISTS[[i - 2]] = apply(Diff, 2, function(x) {apply(Diff, 2, function(y) {DistFunction(x, y)})})
    Dist = Dist + DISTS[[i - 2]]
    m0 = m1
  }
  Dist = Dist / length(DISTS)
  Dsem = matrix(0, nrow = nrow(EXC), ncol = nrow(EXC))
  for (i in 1:length(DISTS)) {
    Dsem = (DISTS[[i]] - Dist)^2
  }
  Dsem = sqrt(Dsem / (length(DISTS) - 1) / length(DISTS))
  rownames(Dist) = EXC$label
  colnames(Dist) = EXC$label
  rownames(Dsem) = EXC$label
  colnames(Dsem) = EXC$label
  return(list(Means = Dist, Sems = Dsem))
}



PlotInd = function(IND, curs) {
  mypal = rainbow(length(curs))
  names(mypal) = curs
  gl = ggplot(data = IND, aes(x = Date))
  for (i in 1:length(curs)) {
    gl = gl + geom_line(aes_string(y = curs[i], color = shQuote(curs[i])), size = 1)
  }
  gl = gl + scale_color_manual(name = "currencies", values = mypal) + 
    ylab("Cumulative index") +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 22),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 22))
  return(gl)
}



### MAIN ######################################################################

CURRTABLE = GetCurrNames()
TDAT = GetData("2015-01-01", "2015-02-01") # HTTP error 429 if too many requests
INDICES = GetIndices(TDAT, 1)
CINDICES = GetCumIndices(TDAT)
MINDICES = data.frame(label = names(INDICES)[2:ncol(INDICES)], index = apply(INDICES[, 2:ncol(INDICES)], 2, function(x) {exp(sum(log(x))/nrow(INDICES))}))

ICM = cor(INDICES[, 2:ncol(INDICES)], method = "pearson")
IDM = sqrt(2 - 2 * ICM)/4
GetDendro_v(IDM, MINDICES)

# best version
CICM = cor(CINDICES[, 2:ncol(CINDICES)],  method = "pearson")
CDM = sqrt(2 - 2 * CICM)/4
GetDendro_v(CDM, MINDICES)

D = GetDist(TDAT)
GetDendro_v(D$Means, MINDICES)


PlotInd(CINDICES, c("HUF", "EUR", "CHF", "USD", "RUB", "GBP"))
all_ci = PlotInd(CINDICES, names(CINDICES)[2:ncol(CINDICES)])

ch_eur = data.frame(Date = as.Date(colnames(TDAT)[2:ncol(TDAT)]), 
           Rate_CHF = unlist(TDAT[TDAT$label == "CHF", 2:ncol(TDAT)]),
           Rate_HUF = unlist(TDAT[TDAT$label == "HUF", 2:ncol(TDAT)]),
           row.names = NULL)

ce = ggplot(ch_eur, aes(x = Date, y = Rate_CHF)) +
  geom_point(size = 4, color = "blue") +
  geom_line(size = 2, color = "blue") +
  ylab("CHF / EUR") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 22))
ce

he = ggplot(ch_eur, aes(x = Date, y = Rate_HUF)) +
  geom_point(size = 4, color = "red") +
  geom_line(size = 2, color = "red") +
  ylab("HUF / EUR") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 22))
he

dfi = INDICES[, c("Date", "CHF", "HUF", "EUR")]
dfi$CHF = 100 * (dfi$CHF - 1)
dfi$EUR = 100 * (dfi$EUR - 1)
dfi$HUF = 100 * (dfi$HUF - 1)
dfi = reshape(dfi, v.names = "index", varying = c("CHF", "HUF", "EUR"), timevar = "currency", times = c("CHF", "HUF", "EUR"), direction = "long")

che_i = ggplot(dfi, aes(x = Date, y = index, fill = currency)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5)) +
  geom_hline(aes(yintercept = 0)) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 22),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 22))
che_i

che_ci = ggplot(CINDICES, aes(x = Date)) +
  geom_point(aes(y = CHF, color = "CHF"),  size = 4) +
  geom_line(aes(y = CHF, color = "CHF"), size = 2) +
  geom_point(aes(y = EUR, color = "EUR"), size = 4) +
  geom_line(aes(y = EUR, color = "EUR"), size = 2)+
  geom_point(aes(y = HUF, color = "HUF"), size = 4) +
  geom_line(aes(y = HUF, color = "HUF"), size = 2)+
  ylab("Cumulative indices") +
  labs(color = "currency") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 22),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 22))
che_ci

png("chf_eur_ratio.png", width = 640)
ce
dev.off()
png("huf_eur_ratio.png", width = 640)
he
dev.off()
png("che_indices.png", width = 640)
che_i
dev.off()
png("che_cindices.png", width = 640)
che_ci
dev.off()
png("all_cindices.png", width = 640)
all_ci
dev.off()


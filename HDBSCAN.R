# Loading the packages that will be used
list.of.packages <- c("tm", "dbscan", "proxy", "colorspace")
# (downloading and) requiring packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) 
  install.packages(new.packages)
for (p in list.of.packages) 
  require(p, character.only = TRUE)

Data5 <- read.csv(file.choose(), header=TRUE)
Datah55 <- wws[, -1]

target.directory <- '/tmp/clustering-r'

# Reading the files
target.directory <- paste(target.directory, 'Datah55')
files <- list.files(path = target.directory, pattern='.csv$')

# Filling the dataframe by reading the text content
for (f in files) {
  news.filename = paste(target.directory , f, sep ='/')
  news.label <- substr(f, 0, nchar(f) - 4) 
  news.data <- read.csv(news.filename,
                        encoding = 'UTF-8',
                        header = FALSE,
                        quote = "",
                        sep = '|',
                        col.names = c('ID', 'datetime', 'content'))
  # Trick to ignore last part of tweets which content contains the split character "|"
  # No satisfying solution has been found to split (as in Python) and merging extra-columns with the last one
  news.data <- news.data[news.data$content != "", ]
  news.data['label'] = news.label # We add the label of the tweet 
  
  # Only considering a little portion of data ...
  # ... because handling sparse matrix for generic usage is a pain
  news.data <- head(news.data, floor(nrow(news.data) * 0.05))
  dataframe <- rbind(dataframe, news.data)
}
# to plot the eps values
eps_plot = kNNdistplot(Datah55, k=2)

# to draw an optimum line
eps_plot = abline(h =0.1, lty = 1)

hdbscan(Datah55, minPts = 3)
plot(Datah55, main="Datah55")

plot(Datah55, col=Datah55$cluster+1, pch=3)
colors <- mapply(function(col, i) adjustcolor(col, alpha.f = Data2$membership_prob[i]), 
                 palette()[Data2$cluster+1], seq_along(Data2$cluster))


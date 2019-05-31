library(readr)

#install.packages("klaR")
library(klaR)
setwd("D:\\Extra\\R\\NAILSTROM MEDIA")
#READ DATA
data<- read_delim("D:/Extra/python/dataset_1/dataset_1.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE, na = "OTHER")

data = na.omit(data) #REMOVE THE MISSING VALUE
data= data[-1]

data$id_iban = as.factor(data$id_iban)
data$id_cb = as.factor(data$id_cb)
clust = kmodes(data, 2, iter.max = 100, weighted = FALSE)
clust$cluster
clust$size
clust$modes
clust$size

#OVERWRITE THE CLUSTER NUMBER CORRESPOSING TO EACH DATAPOINT IN THE DATASET
data$cluster = clust$cluster

#Check the data with the pre defined pattern that in with clusters they lie. We expect them to lie in the same cluster.
d2=subset(data,data$id_iban==40031 & data$id_cb ==529097) # Here all have cluster 1

# Check for the second pattern
d3 = subset(data, data$id_iban==40031 & data$id_cb !=529097)
sum(d3$cluster==1) # so max are from cluster 1, means most of the transacation are not scam.

library(cluster)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(ggalt)
library(GGally)
library(ggthemes)
library(pheatmap)
library(ggbiplot)
library(ggdendro)
library(plotly)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggridges)
library(patchwork)
library(rworldmap)
library(kableExtra)

#Import of dataset and manipulation
data <- read.csv("Datasets/Datasets_unsupervised/merged_un.csv")
data$tot_el_TWh <- rowSums(data[, 4:11])
data$tot_fossil_TWh <- (data$Electricity.from.coal..TWh. + data$Electricity.from.gas..TWh. + data$Electricity.from.oil..TWh.) / data$Population
data$tot_renewable_TWh <- (data$tot_el_TWh/ data$Population) - data$tot_fossil_TWh


num_data <- data[,2:length(data)] %>% select(-c(Year, Electricity.from.coal..TWh., Electricity.from.hydro..TWh., 
                                                Electricity.from.solar..TWh., Electricity.from.wind..TWh., 
                                                Electricity.from.gas..TWh., Electricity.from.other.renewables..TWh.,
                                                Electricity.from.oil..TWh., Electricity.from.nuclear..TWh., Population))
num_data[is.na(num_data)] <- mean(data$Annual_Net_Saving, na.rm = TRUE) 

#scale
col_norm <- c("tot_el_TWh", "tot_fossil_TWh", "tot_renewable_TWh", "C02_emitions","Forest","Annual_Net_Saving", "AQI.Value", "CO.AQI.Value", "Ozone.AQI.Value", "NO2.AQI.Value", "PM2.5.AQI.Value")
num_data[col_norm] <- scale(num_data[col_norm])


first_10_rows <- head(num_data, 10)

# Displaying the first 10 rows
kable(first_10_rows, "html") %>%
  kable_styling(full_width = FALSE) 

############################
#Correlation Matrix
corr_matrix <- cor(num_data)
ggcorrplot(corr_matrix, lab = TRUE)


#PCA Analisys
pca_data <- princomp(corr_matrix)
summary(pca_data)


loadings <- pca_data$loadings[, 1:3]


communalities <- apply(loadings^2, 1, sum)


loadings_table <- cbind(loadings, communalities)


print(loadings_table)


loadings_table <- data.frame(
  Comp.1 = c(0.0498838720, 0.0007997001, -0.0064314246, -0.4701244238, -0.4914335562, 0.1775629101, -0.4708636077, -0.4781160734, 0.0706061152, -0.0836853095, 0.2027834420),
  Comp.2 = c(0.008571495, 0.556840509, 0.032981800, -0.135158528, 0.086128201, -0.563723533, 0.131948224, -0.130154889, -0.317527233, -0.182896002, 0.420323853),
  Comp.3 = c(0.64540842, -0.02420468, -0.30244818, -0.12349172, 0.02910431, -0.24147803, 0.09139138, -0.12346972, 0.11645866, 0.61510975, -0.03819056),
  communalities = c(0.41911390, 0.31065786, 0.09260406, 0.25453501, 0.24977207, 0.40762445, 0.24747526, 0.26078005, 0.11937139, 0.41881418, 0.21925178)
)


rownames(loadings_table) <- c("C02_emitions", "Forest", "Annual_Net_Saving", "AQI.Value", "CO.AQI.Value", "Ozone.AQI.Value", "NO2.AQI.Value", "PM2.5.AQI.Value", "tot_el_TWh", "tot_fossil_TWh", "tot_renewable_TWh")

kable(loadings_table, caption = "Loadings Table", align = "c")

pca_data$loadings[, 1:3]
 
fviz_eig(pca_data, addlabels = TRUE, barfill = "darkorange", barcolor = "#f0f0f0") + theme_fivethirtyeight()
 
fviz_pca_var(pca_data, col.var = "black") + theme_fivethirtyeight()
rownames(num_data) <- data$Country.Code
 
pca <- princomp(num_data)#cor = T)
biplot(pca, col = c("darkcyan","goldenrod1"))

########################
wss <- numeric(10)
wss[1] <- nrow(num_data) * sum(apply(num_data, 2, var))
for (i in 2:10) {
  wss[i] <- sum(kmeans(num_data, centers = i)$withinss)
}

# Create a data frame for plotting
wss_df <- data.frame(x = 1:10, wss = wss)

# Create ggplot object for Optimal number of Clusters
clusters <- ggplot(data = wss_df, aes(x = x, y = wss)) +
  geom_line() +
  geom_point(size=3, shape = 19)+
  theme_fivethirtyeight()+
  labs(x = "Number of Clusters", y = "Within Deviance", 
       title = "Optimal number of Clusters")

clusters


k <- 7 # Number of clusters
set.seed(134)  # Set a random seed for reproducibility 13
kmeans_model <- kmeans(num_data, centers = k)
cluster_labels <- kmeans_model$cluster
data$cluster <- cluster_labels
rownames(num_data)<-data[,1]
KM_clustering <- fviz_cluster(list(data=num_data,cluster=data$cluster),
                              repel = TRUE) +
  theme_fivethirtyeight() +
  ggtitle("K-Means Cluster")
KM_clustering

#Creating and plotting map for Kmeans
country_map <- joinCountryData2Map(data, 
                                   joinCode = "ISO3",
                                   nameJoinColumn = "Country.Code")

map_kmeans <- mapCountryData(country_map, nameColumnToPlot="cluster", 
                             catMethod = "categorical",
                             mapRegion = "world",
                             missingCountryCol = gray(.8),
                             colourPalette = c("green", "turquoise", "royalblue4","yellow3", "red","darkseagreen","darkorchid4"), 
                             mapTitle = "Cluster on countries using K-means", borderCol = "white")

##############################
#EUCLIDEAN DISTANCE, COMPLETE LINKAGE

#Compute distance
ds <- dist(num_data)

#Function for compute hierarchy dendogram
hierarchy <- function(ds,type, n){
  h <- hclust(ds, method=type)
  dendr<-dendro_data(h, type="rectangle")
  clust<-cutree(h,k=n)
  clust.df<-data.frame(label=names(clust), cluster=factor(clust))
  dendr[["labels"]]<-merge(dendr[["labels"]],clust.df, by="label")
  h_plot<-ggplot()+
    geom_segment(data=segment(dendr), 
                 aes(x=x,y=y,xend=xend,yend=yend)) + 
    geom_text(data=label(dendr),
              aes(x,y,label=label,hjust=1,angle = 45,color=cluster),
              size=3)+
    labs(title = paste0(toupper(substr(type, 1, 1)),
                        substr(type, 2, nchar(type)),
                        " dendrogram clustering"))+
    theme_fivethirtyeight()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  plot(h_plot)
  return(h)
}
#Cal the funcition for hierarchy
h_comp <- hierarchy(ds,"complete",7)
hierarchy(ds,"average",7)
hierarchy(ds,"single",7)
hierarchy(ds,"ward.D",7)
'hierarchy(ds,"centroid",6)'

h_clusters <- cutree(h_comp, k=7)


h_clust_plot <- fviz_cluster(list(data=num_data,cluster=h_clusters),
                             repel = TRUE) +
  theme_fivethirtyeight()+
  ggtitle("Hierarchical Cluster")
h_clust_plot


pheatmap(num_data, scale = "row") +
  theme_fivethirtyeight()



data$cluster_hclust <- h_clusters


#Hierarchy map creation and plot
country_map <- joinCountryData2Map(data, 
                                   joinCode = "ISO3",
                                   nameJoinColumn = "Country.Code")



map_hclust <- mapCountryData(country_map, nameColumnToPlot="cluster_hclust", 
                       catMethod = "categorical",
                       mapRegion = "world",
                       missingCountryCol = gray(.8),
                       colourPalette = c("green", "turquoise", "blue","yellow", "red","purple"), 
                       mapTitle = "Cluster on countries using HCLUST", borderCol = "white")

worldMap <- getMap()

#Heatmap plot for tot_fossil, Tot,renewable, C02_emitions
heatmap_data <- merge(worldMap, data.frame(ISO3 = rownames(num_data), tot_fossil_TWh = num_data$tot_fossil_TWh), 
                      by = "ISO3", all.x = TRUE)


mapCountryData(heatmap_data, nameColumnToPlot = "tot_fossil_TWh",
               catMethod = "fixedWidth", mapTitle = "Heatmap of tot_fossil_TWh",
               colourPalette = "rainbow")


heatmap_data <- merge(worldMap, data.frame(ISO3 = rownames(num_data), tot_renewable_TWh = num_data$tot_renewable_TWh), 
                      by = "ISO3", all.x = TRUE)


mapCountryData(heatmap_data, nameColumnToPlot = "tot_renewable_TWh",
               catMethod = "fixedWidth", mapTitle = "Heatmap of tot_renewable_TWh",
               colourPalette = "heat.colors()")



heatmap_data <- merge(worldMap, data.frame(ISO3 = rownames(num_data), C02_emitions = num_data$C02_emitions), 
                      by = "ISO3", all.x = TRUE)


mapCountryData(heatmap_data, nameColumnToPlot = "C02_emitions",
               catMethod = "fixedWidth", mapTitle = "Heatmap of C02_emitions",
               colourPalette = "heat.colors()")


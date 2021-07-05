  # 1. Data preparation
  df <-read.csv("DC_TMDL.csv") # Load the data set
  df <- na.omit(df)  # remove NA
  df <- df[, -2]   # remove date variable
  ggplot(df, aes(x=Site, y=TOC)) + 
    geom_boxplot()
  ggplot(df, aes(x=Site, y=TP)) + 
    geom_boxplot()
  df <- aggregate(df[, -1], list(df$Site), mean)
  library(tidyverse)
  df <- df %>% remove_rownames %>% column_to_rownames(var="Group.1")

  #  library(dplyr)
  df.scaled <- df%>%mutate_if(is.numeric,scale)

  # 2. Required R Packages
  #install.packages(c("cluster", "factoextra"))
  library(cluster)
  library(factoextra)
  
  # 3. Clustering Distance Measures
  dist.eucl <- dist(df.scaled, method = "euclidean")
  # Reformat as a matrix
  # Subset the first 3 columns and rows and Round the values
  round(as.matrix(dist.eucl)[1:3, 1:3], 1)
  # Computing correlation based distances
  dist.cor <- get_dist(df.scaled[], method = "pearson")
  dist.cor
  # Display a subset
  round(as.matrix(dist.cor)[1:3, 1:3], 1)
  
  # Computing distances for mixed data
  library(cluster)

  #Visualizing distance matrices
  fviz_dist(dist.eucl)

  # 4. K-mean Clustering
  # Estimating the optimal number of clusters
  fviz_nbclust(df.scaled, kmeans, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)
  
  # Compute k-means with k = 4
  set.seed(123)
  km.res <- kmeans(df.scaled, 4, nstart = 25)
  # Print the results
  print(km.res)
  # convert the mean of each variables by clusters to original data
  aggregate(df, by=list(cluster=km.res$cluster), mean)
  # add the cluster to the original data
  dd <- cbind(df, cluster = km.res$cluster)
  head(dd)
  
  # Visualizing k-means clusters
  fviz_cluster(km.res, data = df.scaled,
               palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
               ellipse.type = "euclid", # Concentration ellipse
               star.plot = TRUE, # Add segments from centroids to items
               repel = TRUE, # Avoid label overplotting (slow)
               ggtheme = theme_minimal()
  )
  
  # 5. K-medoids Clustering (PAM)
  #  Estimating the optimal number of clusters
  fviz_nbclust(df.scaled, pam, method = "silhouette")+
    theme_classic()
  
  pam.res <- pam(df.scaled, 2)
  print(pam.res)
  
  dd <- cbind(df, cluster = pam.res$cluster)
  head(dd, n = 3)
  
  #Visualizing PAM clusters
  fviz_cluster(pam.res,
               palette = c("#00AFBB", "#FC4E07"), # color palette
               ellipse.type = "t", # Concentration ellipse
               repel = TRUE, # Avoid label overplotting (slow)
               ggtheme = theme_classic()
  )
  
  # 6. CLARA - Clustering Large Applications

  #Estimating the optimal number of clusters
  fviz_nbclust(df.scaled, clara, method = "silhouette")+
    theme_classic()
  # Compute CLARA
  clara.res <- clara(df, 2, samples = 14, pamLike = TRUE)
  # Print components of clara.res
  print(clara.res)
  
  #Visualizing CLARA clusters
  fviz_cluster(clara.res,
               palette = c("#00AFBB", "#FC4E07"), # color palette
               ellipse.type = "t", # Concentration ellipse
               geom = "point", pointsize = 1,
               ggtheme = theme_classic(), stand = FALSE
  )
  
  # 7. HCA (AGGLOMERATIVE CLUSTERING)
  # df = the standardized data
  res.dist <- dist(df.scaled, method = "euclidean")
  as.matrix(res.dist)[1:6, 1:6]
  # Linkage
  res.hc <- hclust(d = res.dist, method = "ward.D2")
  # Dendrogram:
  library("factoextra")
  fviz_dend(res.hc, cex = 0.5)
  
  # Verify the cluster tree
  # Compute cophentic distance
  res.coph <- cophenetic(res.hc)
  # Correlation between cophenetic distance and the original distance
  cor(res.dist, res.coph)
  
  res.hc2 <- hclust(res.dist, method = "average")
  cor(res.dist, cophenetic(res.hc2))
  
  # Cut the dendrogram into different groups
  # Cut tree into 4 groups
  grp <- cutree(res.hc, k = 4)
  head(grp, n = 4)
  # Number of members in each cluster
  table(grp)
  # Get the names for the members of cluster 1
  rownames(df)[grp == 1]
  # Cut in 4 groups and color by groups
  fviz_dend(res.hc, k = 4, # Cut in four groups
            cex = 0.5, # label size
            k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
            color_labels_by_k = TRUE, # color labels by groups
            rect = TRUE # Add rectangle around groups
  )
  
  fviz_cluster(list(data = df.scaled, cluster = grp),
               palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
               ellipse.type = "convex", # Concentration ellipse
               repel = TRUE, # Avoid label overplotting (slow)
               show.clust.cent = FALSE, ggtheme = theme_minimal()
  )
  
  # Cluster R package
  library("cluster")
  # Agglomerative Nesting (Hierarchical Clustering)
  res.agnes <- agnes(x = df, # data matrix
                     stand = TRUE, # Standardize the data
                     metric = "euclidean", # metric for distance matrix
                     method = "ward" # Linkage method
  )
  fviz_dend(res.agnes, cex = 0.6, k = 4)
  
  # DIvisive ANAlysis Clustering
  res.diana <- diana(x = df, # data matrix
                     stand = TRUE, # standardize the data
                     metric = "euclidean" # metric for distance matrix
  )
  fviz_dend(res.diana, cex = 0.6, k = 4)
  grp <- cutree(res.diana, k = 4)
  fviz_dend(res.diana, k = 4, # Cut in four groups
            cex = 0.5, # label size
            k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
            color_labels_by_k = TRUE, # color labels by groups
            rect = TRUE # Add rectangle around groups
  )
  
  fviz_dend(res.diana, cex = 0.5, k = 4,
            k_colors = "jco", type = "circular")
  
  require("igraph")
  fviz_dend(res.diana, k = 4, k_colors = "jco",
            type = "phylogenic", repel = TRUE)
  
  fviz_dend(res.diana, k = 4, # Cut in four groups
            k_colors = "jco",
            type = "phylogenic", repel = TRUE,
            phylo_layout = "layout.gem")
  
  # 9. Heatmap
  library(devtools)
#  install_github("jokergoo/ComplexHeatmap")
  library(ComplexHeatmap)
  Heatmap(df.scaled,
          name = "Scaled", #title of legend
          column_title = "Water Quality Variable", row_title = "TMDL Site",
          row_names_gp = gpar(fontsize = 7) # Text size for row names
  )
  
  library(dendextend)
  row_dend = hclust(dist(df.scaled)) # row clustering
  col_dend = hclust(dist(t(df.scaled))) # column clustering
  Heatmap(df.scaled, name = "Scaled",
          column_title = "Water Quality Variable", row_title = "TMDL Site",
          row_names_gp = gpar(fontsize = 6.5),
          cluster_rows = color_branches(row_dend, k = 4),
          cluster_columns = color_branches(col_dend, k = 2))
  
  
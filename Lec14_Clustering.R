  # 1. Data preparation
  data("USArrests") # Load the data set
  df <- USArrests # Use df as shorter name
  df <- na.omit(df)
  df <- scale(df)
  head(df, n = 3)
  
  # 2. Required R Packages
  #install.packages(c("cluster", "factoextra"))
  library(cluster)
  library(factoextra)
  
  # 3. Clustering Distance Measures
  #Subset of the data
  set.seed(123)
  ss <- sample(1:50, 15) # Take 15 random rows
  df <- USArrests[ss, ] # Subset the 15 rows
  df.scaled <- scale(df) # Standardize the variable
  
  dist.eucl <- dist(df.scaled, method = "euclidean")
  # Reformat as a matrix
  # Subset the first 3 columns and rows and Round the values
  round(as.matrix(dist.eucl)[1:3, 1:3], 1)
  # Computing correlation based distances
  dist.cor <- get_dist(df.scaled, method = "pearson")
  dist.cor
  # Display a subset
  round(as.matrix(dist.cor)[1:3, 1:3], 1)
  
  # Computing distances for mixed data
  library(cluster)
  # Load data
  data(flower)
  head(flower, 3)
  
  dd <- daisy(flower)
  round(as.matrix(dd)[1:3, 1:3], 2)
  
  #Visualizing distance matrices
  fviz_dist(dist.eucl)

  # 4. K-mean Clustering
  df <- scale(USArrests) # Scaling the data
  head(df, n = 3)
  # Estimating the optimal number of clusters
  fviz_nbclust(df, kmeans, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)
  
  # Compute k-means with k = 4
  set.seed(123)
  km.res <- kmeans(df, 4, nstart = 25)
  # Print the results
  print(km.res)
  # convert the mean of each variables by clusters to original data
  aggregate(USArrests, by=list(cluster=km.res$cluster), mean)
  # add the cluster to the original data
  dd <- cbind(USArrests, cluster = km.res$cluster)
  head(dd)
  
  # Visualizing k-means clusters
  fviz_cluster(km.res, data = df,
               palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
               ellipse.type = "euclid", # Concentration ellipse
               star.plot = TRUE, # Add segments from centroids to items
               repel = TRUE, # Avoid label overplotting (slow)
               ggtheme = theme_minimal()
  )
  
  # 5. K-medoids Clustering (PAM)
  #  Estimating the optimal number of clusters
  df <- scale(USArrests)
  fviz_nbclust(df, pam, method = "silhouette")+
    theme_classic()
  
  pam.res <- pam(df, 2)
  print(pam.res)
  
  dd <- cbind(USArrests, cluster = pam.res$cluster)
  head(dd, n = 3)
  
  #Visualizing PAM clusters
  fviz_cluster(pam.res,
               palette = c("#00AFBB", "#FC4E07"), # color palette
               ellipse.type = "t", # Concentration ellipse
               repel = TRUE, # Avoid label overplotting (slow)
               ggtheme = theme_classic()
  )
  
  # 6. CLARA - Clustering Large Applications
  set.seed(1234)
  # Generate 500 objects, divided into 2 clusters.
  df <- rbind(cbind(rnorm(200,0,8), rnorm(200,0,8)),
              cbind(rnorm(300,50,8), rnorm(300,50,8)))
  # Specify column and row names
  colnames(df) <- c("x", "y")
  rownames(df) <- paste0("S", 1:nrow(df))
  # Previewing the data
  head(df, nrow = 6)
  #Estimating the optimal number of clusters
  fviz_nbclust(df, clara, method = "silhouette")+
    theme_classic()
  # Compute CLARA
  clara.res <- clara(df, 2, samples = 50, pamLike = TRUE)
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
  data("USArrests")
  df <- scale(USArrests)
  head(df, nrow = 6)
  # Similarity measures   
  # Compute the dissimilarity matrix
  # df = the standardized data
  res.dist <- dist(df, method = "euclidean")
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
  
  fviz_cluster(list(data = df, cluster = grp),
               palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
               ellipse.type = "convex", # Concentration ellipse
               repel = TRUE, # Avoid label overplotting (slow)
               show.clust.cent = FALSE, ggtheme = theme_minimal()
  )
  
  # Cluster R package
  library("cluster")
  # Agglomerative Nesting (Hierarchical Clustering)
  res.agnes <- agnes(x = USArrests, # data matrix
                     stand = TRUE, # Standardize the data
                     metric = "euclidean", # metric for distance matrix
                     method = "ward" # Linkage method
  )
  fviz_dend(res.agnes, cex = 0.6, k = 4)
  
  # DIvisive ANAlysis Clustering
  res.diana <- diana(x = USArrests, # data matrix
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
  
  # 8. Comparing Dendrograms
  df <- scale(USArrests)
  # Subset containing 10 rows
  set.seed(123)
  ss <- sample(1:50, 10)
  df <- df[ss,]
  library(dendextend)
  # Compute distance matrix
  res.dist <- dist(df, method = "euclidean")
  # Compute 2 hierarchical clusterings
  hc1 <- hclust(res.dist, method = "average")
  hc2 <- hclust(res.dist, method = "ward.D2")
  # Create two dendrograms
  dend1 <- as.dendrogram (hc1)
  dend2 <- as.dendrogram (hc2)
  # Create a list to hold dendrograms
  dend_list <- dendlist(dend1, dend2)
  # Draw a tanglegram:
  tanglegram(dend1, dend2)
  tanglegram(dend1, dend2,
             highlight_distinct_edges = FALSE, # Turn-off dashed lines
             common_subtrees_color_lines = FALSE, # Turn-off line colors
             common_subtrees_color_branches = TRUE, # Color common branches
             main = paste("entanglement =", round(entanglement(dend_list), 2))
  )
  
  # Cophenetic correlation coefficient
  cor_cophenetic(dend1, dend2)
  # Baker correlation coefficient
  cor_bakers_gamma(dend1, dend2)
  
  # 9. Heatmap
  library(devtools)
#  install_github("jokergoo/ComplexHeatmap")
  library(ComplexHeatmap)
  Heatmap(df,
          name = "mtcars", #title of legend
          column_title = "Variables", row_title = "Samples",
          row_names_gp = gpar(fontsize = 7) # Text size for row names
  )
  
  library(dendextend)
  row_dend = hclust(dist(df)) # row clustering
  col_dend = hclust(dist(t(df))) # column clustering
  Heatmap(df, name = "mtcars",
          row_names_gp = gpar(fontsize = 6.5),
          cluster_rows = color_branches(row_dend, k = 4),
          cluster_columns = color_branches(col_dend, k = 2))
  
  # 10. Cluster Validation
  # install.packages( "clustertend")
  library(clustertend)
  
  # Iris data set
  df <- iris[, -5]
  # Random data generated from the iris data set
  random_df <- apply(df, 2,
                     function(x){runif(length(x), min(x), (max(x)))})
  random_df <- as.data.frame(random_df)
  # Standardize the data sets
  df <- iris.scaled <- scale(df)
  random_df <- scale(random_df)
  
  # Visual inspection of the data
  # Plot faithful data set
  library(factoextra)
  fviz_pca_ind(prcomp(df), title = "PCA - Iris data",
               habillage = iris$Species, palette = "jco",
               geom = "point", ggtheme = theme_classic(),
               legend = "bottom")
  
  # Plot the random df
  fviz_pca_ind(prcomp(random_df), title = "PCA - Random data",
               geom = "point", ggtheme = theme_classic())
  set.seed(123)
  # K-means on iris dataset
  km.res1 <- kmeans(df, 3)
  fviz_cluster(list(data = df, cluster = km.res1$cluster),
               ellipse.type = "norm", geom = "point", stand = FALSE,
               palette = "jco", ggtheme = theme_classic())
  
  # Compute Hopkins statistic for iris dataset
  res <- get_clust_tendency(df, n = nrow(df)-1, graph = FALSE)
  res$hopkins_stat
  # Compute Hopkins statistic for a random dataset
  res <- get_clust_tendency(random_df, n = nrow(random_df)-1,
                            graph = FALSE)
  res$hopkins_stat
  #Visual methods
  fviz_dist(dist(df), show_labels = FALSE)+
    labs(title = "Iris data")
  fviz_dist(dist(random_df), show_labels = FALSE)+
    labs(title = "Random data")
  
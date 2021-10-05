# 1. Import Dataset
```r
winedf <- read.csv(
            "/home/adi/Documents/Data/wine-clustering.csv",
            header = TRUE
        )
```

# 2. Dimension of the imported dataset
```r
print( c( "Number of columns in the dataset : ", ncol(winedf) ) )
```
```
[1] "Number of columns in the dataset : " "13" 
```
```r
print( c( "Number of rows in the dataset : ", nrow(winedf) ) )
```
```
[1] "Number of rows in the dataset : " "178"
```

# 3. Columns
In the above section, it is clear that the dataset conatins 13 columns. The following output will show, the name of the columns.
```r
data.frame(
    names(winedf)
)
```
```
          names.winedf.
1               Alcohol
2            Malic_Acid
3                   Ash
4          Ash_Alcanity
5             Magnesium
6         Total_Phenols
7            Flavanoids
8  Nonflavanoid_Phenols
9       Proanthocyanins
10      Color_Intensity
11                  Hue
12                OD280
13              Proline
```
# 4. Data types of columns
```r
data.frame(
    sapply(
        winedf,
        class
    )
)
```
```
                     sapply.winedf..class.
Alcohol                            numeric
Malic_Acid                         numeric
Ash                                numeric
Ash_Alcanity                       numeric
Magnesium                          integer
Total_Phenols                      numeric
Flavanoids                         numeric
Nonflavanoid_Phenols               numeric
Proanthocyanins                    numeric
Color_Intensity                    numeric
Hue                                numeric
OD280                              numeric
Proline                            integer
```
Clearly, there are 2 columns with "integer" values  and 11 columns with "numeric" values.

# 5. Missing value treatment
```r
library(mice)
md.pattern(
    x = winedf,
    plot = TRUE,
    rotate.names = TRUE
)
```
```
 /\     /\
{  `---'  }
{  O   O  }
==>  V <==  No need for mice. This data set is completely observed.
 \  \|/  /
  `-----'

     Alcohol Malic_Acid Ash Ash_Alcanity Magnesium Total_Phenols Flavanoids
 178       1          1   1            1         1             1          1
           0          0   0            0         0             0          0
     Nonflavanoid_Phenols Proanthocyanins Color_Intensity Hue OD280 Proline 
 178                    1               1               1   1     1       1 0
                        0               0               0   0     0       0 0
```

The above output and the plot confirms that, there are no missing values in the imported dataset.

# 5. Density plots, scatter plots ad correlations among the features
library(ggplot2)
library(GGally)
ggpairs(
    data = winedf
)

# 6. Feature Scaling
library(dplyr)
scaled_df <- winedf %>% 
                mutate_all(scale)

# 7. KMeans Clustering
# Before applying KMeans method, we have to fid the optimal number of clusters to be used. To identify the optimum number of clusters, three methods will be used here :
# 1. Elbow Method
# 2. Silhoutte Scores
# 3. Gap Statistic
# 
# 7.1 Elbow Method
# In this process,Within Sum of Squares for k number of clusters. The WSS are then plotted against the corressponding KMeans with k clusters.
kmeans_withinss <- function(k) {
                        cluster <- kmeans(
                                        scaled_df,
                                        k
                                    )
                        return(
                            cluster$tot.withinss
                        )
                    }
max_k <- 21
wss <- sapply(
            2:max_k,
            kmeans_withinss
        )
ggplot(
    data.frame(
        2:max_k,
        wss
    ),
    aes(
        x = X2.max_k,
        y = wss
    )
) + 
geom_line(
    aes(
        col = "red"
    ),
    size = 2,
    show.legend = FALSE
) +
geom_point(
    aes(
        col = "blue"
    ),
    size = 4,
    show.legend = FALSE
) +
scale_x_continuous(
    breaks = seq(1, 21, by = 1)
) +
xlab(
    "Number of Clusters"
) +
ylab(
    "Within Sum of Squares"
) +
ggtitle(
    "Number of Clusters vs Within Sum of Squares"
)
# The plot obtained, shows that the optimum number of classes will be 3.

# 7.2 Silhoutte Score
library(cluster)
sil_score <- function(k) {
                    km <- kmeans(
                                scaled_df,
                                k
                            )
                    ss <- silhouette(
                                km$cluster,
                                dist(winedf)
                            )
                    mean(ss[,3])
                }
avg_sil <- sapply(
                2:max_k,
                sil_score
            )
ggplot(
    data.frame(
        2:max_k,
        avg_sil
    ),
    aes(
        x = X2.max_k,
        y = avg_sil
    )
) + 
geom_line(
    aes(
        col = "green"
    ),
    size = 2,
    show.legend = FALSE
) +
geom_point(
    aes(
        col = "yellow"
    ),
    size = 4,
    show.legend = FALSE
) +
scale_x_continuous(
    breaks = seq(1, 21, by = 1)
) +
xlab(
    "Number of Clusters"
) +
ylab(
    "Average Silhouette Score"
) +
ggtitle(
    "Number of Clusters vs Average Silhouette Score"
)
# The obtained plot shows that the optimum number of clusters will be 3.

# 7.3 Gap Statistic
gaps <- clusGap(
            x = scaled_df,
            FUNcluster = kmeans,
            K.max = max_k,
            B = 100
        )
ggplot(
    data.frame(
        1:max_k,
        gaps$Tab[, "gap"]
    ),
    aes(
        x = X1.max_k,
        y = gaps$Tab[, "gap"]
    )
) + 
geom_line(
    aes(
        col = "green"
    ),
    size = 2,
    show.legend = FALSE
) +
geom_point(
    aes(
        col = "yellow"
    ),
    size = 4,
    show.legend = FALSE
) +
scale_x_continuous(
    breaks = seq(1, 21, by = 1)
) +
xlab(
    "Number of Clusters"
) +
ylab(
    "Gap Statistic"
) +
ggtitle(
    "Optimal Number of Clusters"
)
# Here also, the optimum number of clusters obtained is 3.
# 
# So, this dataset will have 3 clusters.
# 
# 
# Fitting KMeans with 3 clusters
library(animation) 
png(
    filename = "/home/adi/Pictures/kanime%03d.png",
    width = 720,
    height = 720,
    units = "px"
)
kmeans.ani(
    x = scaled_df,
    centers = 3,
    pch = c(18, 16, 17),
    col = c("red","green","blue")
)

final <- kmeans(
            scaled_df,
            3
)
library(factoextra)
library(ggplot2)
library(grid)
library(ggplotify)
plot_list = list()
k <- 1
for (i in seq_along(winedf)) {
    for (j in seq_along(winedf)) {
        plot_list[[k]] <- as.ggplot(
                            fviz_cluster(
                                object = final,
                                data = winedf,
                                axes = c(i,j),
                                geom = "point",
                                repel = TRUE,
                                show.clust.cent = TRUE,
                                pointshape = 5,
                                xlab = names(winedf[i]),
                                ylab = names(winedf[j])
                            )
                        )
        k <- k+1    
    }
}

library(cowplot)
png(
    filename = "/home/adi/Pictures/clustplot.png",
    width = 1920,
    height = 1920,
    units = "px"
)
jpeg(
    filename = "/home/adi/Pictures/clustplot.jpeg",
    width = 1920,
    height = 1920,
    units = "px"
)
bmp(
    filename = "/home/adi/Pictures/clustplot.bmp",
    width = 1920,
    height = 1920,
    units = "px"
)
plot_grid(
    plotlist = plot_list,
    nrow = 13,
    ncol = 13
)
dev.off()

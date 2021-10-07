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
<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/miceplot.png">
</p>

The above output and the plot confirms that, there are no missing values in the imported dataset.

# 5. Density plots, scatter plots ad correlations among the features
```r
library(ggplot2)
library(GGally)
ggpairs(
    data = winedf
)
```
<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/densityplot.png">
</p>


# 6. Feature Scaling
```r
library(dplyr)
scaled_df <- winedf %>% 
                mutate_all(scale)
```

# 7. KMeans Clustering
Before applying KMeans method, we have to fid the optimal number of clusters to be used. To identify the optimum number of clusters, three methods will be used here :
1. Elbow Method
2. Silhoutte Scores
3. Gap Statistic
 
## 7.1 Elbow Method
```r
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
```
<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/elbow.png">
</p>

From the above plot, the number of optimal classes cannot be determined clearly.

## 7.2 Silhoutte Score
```r
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
```
<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/silhoutte.png">
</p>

Here also, from the above plot, the number of optimal classes cannot be determined clearly.

# 7.3 Gap Statistic
```r
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
```
<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/gap.png">
</p>

From the above plot it is clear that the optimum number of clusters will be 3.

 
# Fitting KMeans with 3 clusters
```r
library(animation) 
kmeans.ani(
    x = scaled_df,
    centers = 3,
    pch = c(18, 16, 17),
    col = c("red","green","blue")
)
```
<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/kanime001.png" width = 720 height = 720><figcaption>1</figcaption>
</p>

<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/kanime002.png" width = 720 height = 720><figcaption>2</figcaption>
</p>

<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/kanime003.png" width = 720 height = 720><figcaption>3</figcaption>
</p>

<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/kanime004.png" width = 720 height = 720><figcaption>4</figcaption>
</p>

<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/kanime005.png" width = 720 height = 720><figcaption>5</figcaption>
</p>

<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/kanime006.png" width = 720 height = 720><figcaption>6</figcaption>
</p>

<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/kanime007.png" width = 720 height = 720><figcaption>7</figcaption>
</p>

<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/kanime008.png" width = 720 height = 720><figcaption>8</figcaption>
</p>

<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/kanime009.png" width = 720 height = 720><figcaption>9</figcaption>
</p>

<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/kanime010.png" width = 720 height = 720><figcaption>10</figcaption>
</p>

<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/kanime011.png" width = 720 height = 720><figcaption>11</figcaption>
</p>

<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/kanime012.png" width = 720 height = 720><figcaption>12</figcaption>
</p>

<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/kanime013.png" width = 720 height = 720><figcaption>13</figcaption>
</p>

<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/kanime014.png" width = 720 height = 720><figcaption>14</figcaption>
</p>

```r
final <- kmeans(
            scaled_df,
            3
)
```
```r
library(factoextra)
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
plot_grid(
    plotlist = plot_list,
    nrow = 13,
    ncol = 13
)
```
<p align = "center">
            <img src = "https://github.com/Adi-ds/Wine-Clustering/blob/main/KMeans/clustplot.png" width = 1080 height = 1080>
</p>

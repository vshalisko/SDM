Regresiones de temperatura vs. elevación por cuadrantes
========================================================


```r
elev_field <- 'alt_clip_C';

#list_tmax <- c('tmax_1_cli')  # for debugging
list_tmax <- c('tmax_1_cli','tmax_2_cli','tmax_3_cli','tmax_4_cli','tmax_5_cli','tmax_6_cli','tmax_7_cli','tmax_8_cli','tmax_9_cli','tmax_10_cl','tmax_11_cl','tmax_12_cl')

list_tmin <- c('tmin_1_cli','tmin_2_cli','tmin_3_cli','tmin_4_cli','tmin_5_cli','tmin_6_cli','tmin_7_cli','tmin_8_cli','tmin_9_cli','tmin_10_cl','tmin_11_cl','tmin_12_cl')
```



```r
# read data
library(foreign)
tmax<-read.dbf("tmax_30000_sample.dbf")
tmin<-read.dbf("tmin_30000_sample.dbf")

# add variables to identify 1 degree cuadrants 
tmax$Xclass <- as.factor(floor(tmax$X))
tmax$Yclass <- as.factor(floor(tmax$Y))
tmin$Xclass <- as.factor(floor(tmin$X))
tmin$Yclass <- as.factor(floor(tmin$Y))

# make lists of cuadrant indices
tmax_X_values <- sort(unique(tmax$Xclass, incomparables = FALSE))
tmax_Y_values <- sort(unique(tmax$Yclass, incomparables = FALSE))
tmin_X_values <- sort(unique(tmin$Xclass, incomparables = FALSE))
tmin_Y_values <- sort(unique(tmin$Yclass, incomparables = FALSE))

tmax_X_values
```

```
##  [1] -107 -106 -105 -104 -103 -102 -101 -100 -99  -98  -97  -96  -95  -94 
## [15] -93  -92  -91  -90  -89  -88  -87  -86  -85  -84  -83  -82  -81  -80 
## [29] -79  -78  -77  -76  -75  -74  -73 
## 35 Levels: -107 -106 -105 -104 -103 -102 -101 -100 -99 -98 -97 -96 ... -73
```

```r
tmax_Y_values
```

```
##  [1] 5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
## Levels: 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
```

```r
tmin_X_values
```

```
##  [1] -107 -106 -105 -104 -103 -102 -101 -100 -99  -98  -97  -96  -95  -94 
## [15] -93  -92  -91  -90  -89  -88  -87  -86  -85  -84  -83  -82  -81  -80 
## [29] -79  -78  -77  -76  -75  -74  -73 
## 35 Levels: -107 -106 -105 -104 -103 -102 -101 -100 -99 -98 -97 -96 ... -73
```

```r
tmin_Y_values
```

```
##  [1] 5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
## Levels: 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
```

```r
# convert temperature values to degrees
for (i in 1:length(list_tmax)) {
  tmax[[list_tmax[i]]] <- tmax[[list_tmax[i]]] / 10
}
for (i in 1:length(list_tmin)) {
  tmin[[list_tmin[i]]] <- tmin[[list_tmin[i]]] / 10
}
```



```r
dim(tmax)
```

```
## [1] 30000    18
```

```r
summary(tmax)
```

```
##    random_poi          X                 Y            alt_clip_C    
##  Min.   :    0   Min.   :-106.93   Min.   : 5.992   Min.   : -44.7  
##  1st Qu.: 7500   1st Qu.: -99.17   1st Qu.:13.749   1st Qu.:  61.0  
##  Median :15000   Median : -90.26   Median :17.712   Median : 370.7  
##  Mean   :15000   Mean   : -90.43   Mean   :16.694   Mean   : 762.8  
##  3rd Qu.:22499   3rd Qu.: -83.79   3rd Qu.:20.578   3rd Qu.:1393.9  
##  Max.   :29999   Max.   : -72.98   Max.   :24.016   Max.   :4375.3  
##                                                                     
##    tmax_1_cli      tmax_2_cli      tmax_3_cli      tmax_4_cli   
##  Min.   : 0.00   Min.   : 0.00   Min.   : 0.00   Min.   : 0.00  
##  1st Qu.:23.53   1st Qu.:25.14   1st Qu.:27.20   1st Qu.:28.91  
##  Median :27.12   Median :28.28   Median :29.96   Median :31.29  
##  Mean   :26.08   Mean   :27.24   Mean   :28.87   Mean   :30.16  
##  3rd Qu.:29.29   3rd Qu.:30.37   3rd Qu.:31.94   3rd Qu.:33.24  
##  Max.   :35.53   Max.   :37.20   Max.   :39.22   Max.   :40.63  
##                                                                 
##    tmax_5_cli      tmax_6_cli      tmax_7_cli      tmax_8_cli   
##  Min.   : 0.00   Min.   : 0.00   Min.   : 0.00   Min.   : 0.00  
##  1st Qu.:29.39   1st Qu.:28.20   1st Qu.:27.09   1st Qu.:27.14  
##  Median :31.47   Median :30.59   Median :30.03   Median :30.02  
##  Mean   :30.35   Mean   :29.34   Mean   :28.72   Mean   :28.80  
##  3rd Qu.:33.24   3rd Qu.:32.48   3rd Qu.:32.24   3rd Qu.:32.41  
##  Max.   :41.02   Max.   :38.30   Max.   :36.20   Max.   :36.31  
##                                                                 
##    tmax_9_cli      tmax_10_cl      tmax_11_cl      tmax_12_cl   
##  Min.   : 0.00   Min.   : 0.00   Min.   : 0.00   Min.   : 0.00  
##  1st Qu.:26.47   1st Qu.:25.90   1st Qu.:24.93   1st Qu.:23.67  
##  Median :29.80   Median :29.31   Median :28.14   Median :27.29  
##  Mean   :28.23   Mean   :27.60   Mean   :26.79   Mean   :26.01  
##  3rd Qu.:31.71   3rd Qu.:30.80   3rd Qu.:29.74   3rd Qu.:29.04  
##  Max.   :35.40   Max.   :36.00   Max.   :36.10   Max.   :35.82  
##                                                                 
##      Xclass          Yclass     
##  -99    : 1501   20     : 2776  
##  -100   : 1401   18     : 2674  
##  -90    : 1387   17     : 2492  
##  -89    : 1357   19     : 2373  
##  -101   : 1303   21     : 2334  
##  -85    : 1261   22     : 2245  
##  (Other):21790   (Other):15106
```

```r
dim(tmin)
```

```
## [1] 30000    18
```

```r
summary(tmin)
```

```
##    random_poi          X                 Y            alt_clip_C    
##  Min.   :    0   Min.   :-106.93   Min.   : 5.992   Min.   : -44.7  
##  1st Qu.: 7500   1st Qu.: -99.17   1st Qu.:13.749   1st Qu.:  61.0  
##  Median :15000   Median : -90.26   Median :17.712   Median : 370.7  
##  Mean   :15000   Mean   : -90.43   Mean   :16.694   Mean   : 762.8  
##  3rd Qu.:22499   3rd Qu.: -83.79   3rd Qu.:20.578   3rd Qu.:1393.9  
##  Max.   :29999   Max.   : -72.98   Max.   :24.016   Max.   :4375.3  
##                                                                     
##    tmin_1_cli       tmin_2_cli       tmin_3_cli       tmin_4_cli    
##  Min.   :-4.985   Min.   :-4.854   Min.   :-3.864   Min.   :-2.992  
##  1st Qu.: 8.977   1st Qu.: 9.674   1st Qu.:11.109   1st Qu.:12.982  
##  Median :15.709   Median :16.019   Median :17.396   Median :19.029  
##  Mean   :13.666   Mean   :14.104   Mean   :15.400   Mean   :16.895  
##  3rd Qu.:18.330   3rd Qu.:18.598   3rd Qu.:19.860   3rd Qu.:21.265  
##  Max.   :23.706   Max.   :24.101   Max.   :24.699   Max.   :24.916  
##                                                                     
##    tmin_5_cli       tmin_6_cli       tmin_7_cli      tmin_8_cli   
##  Min.   :-2.248   Min.   :-1.474   Min.   :-1.90   Min.   :-1.99  
##  1st Qu.:14.602   1st Qu.:15.518   1st Qu.:15.06   1st Qu.:14.91  
##  Median :20.238   Median :20.740   Median :20.50   Median :20.39  
##  Mean   :18.035   Mean   :18.605   Mean   :18.31   Mean   :18.22  
##  3rd Qu.:22.206   3rd Qu.:22.474   3rd Qu.:22.30   3rd Qu.:22.21  
##  Max.   :25.121   Max.   :25.211   Max.   :25.40   Max.   :25.50  
##                                                                   
##    tmin_9_cli       tmin_10_cl       tmin_11_cl       tmin_12_cl    
##  Min.   :-1.717   Min.   :-1.874   Min.   :-2.631   Min.   :-4.388  
##  1st Qu.:14.814   1st Qu.:13.432   1st Qu.:11.206   1st Qu.: 9.566  
##  Median :20.245   Median :19.500   Median :17.659   Median :16.242  
##  Mean   :18.093   Mean   :17.112   Mean   :15.482   Mean   :14.198  
##  3rd Qu.:22.096   3rd Qu.:21.401   3rd Qu.:20.073   3rd Qu.:18.788  
##  Max.   :24.900   Max.   :24.474   Max.   :24.470   Max.   :24.304  
##                                                                     
##      Xclass          Yclass     
##  -99    : 1501   20     : 2776  
##  -100   : 1401   18     : 2674  
##  -90    : 1387   17     : 2492  
##  -89    : 1357   19     : 2373  
##  -101   : 1303   21     : 2334  
##  -85    : 1261   22     : 2245  
##  (Other):21790   (Other):15106
```





TMAX regressions


```r
par(mfcol = c(1, 4))

for (k in 1:length(list_tmax)) {
  
  print(paste('Variable ',list_tmax[k],sep=""))

  # create empty table for variable of month k
  my_table_tmax <- data.frame(t(rep(NA,5)))
  columns <- c("X_center", "Y_center", 
             paste('tmax_',k,'_A',sep=""),
             paste('tmax_',k,'_B',sep=""),
             paste('tmax_',k,'_R2',sep=""))
  names(my_table_tmax) <- columns
  
  
  for (i in 1:length(tmax_X_values) ) {
    tmax_i <- tmax[tmax$Xclass == tmax_X_values[i],]
    for (j in 1:length(tmax_Y_values) ) {
      tmax_ij <- tmax_i[tmax_i$Yclass == tmax_Y_values[j],]
      tmax_ij <- tmax_ij[complete.cases(tmax_ij),]        # take only cases with data
      tmax_ij <- tmax_ij[tmax_ij[[elev_field]] > 0,]      # take only data with positive elevation
      if (nrow(tmax_ij) > 0) {
        #print(dim(tmax_ij))
      
        # data suitable for regression analysis should fit following criteria:
        # 1) have enough samples (n > 20)
        # 2) values should be normally distributed ?
        # 3) variability in elevation should be at least 1000 m
        if ((nrow(tmax_ij) > 20) && 
              ( max(tmax_ij[[elev_field]]) - min(tmax_ij[[elev_field]]) >= 1000 )) {
            
            # data could be considered suitable for analysis
            plot(tmax_ij[[list_tmax[k]]] ~ tmax_ij[[elev_field]], 
                 main=paste('Cuadrant ',tmax_X_values[i],'° ',tmax_Y_values[j],'°, month ',k,sep=""), 
                 xlab='elevation', ylab='T max',col='darkgray')
            my_fit <- lm(tmax_ij[[list_tmax[k]]] ~ tmax_ij[[elev_field]])
            abline(my_fit, col='red', lwd=2)
            
            # print(summary(my_fit))
            
            # store the centers of cuadrants, regression coefficients and R2 in table
            # note: the conversion of factor to numeric values require use of levels attribute
            my_table_line <- c(as.numeric(levels(tmax_X_values)[tmax_X_values[i]])+0.5,
                               as.numeric(levels(tmax_Y_values)[tmax_Y_values[j]])+0.5,
                               coef(my_fit)[1],coef(my_fit)[2],
                               summary(my_fit)$r.squared)            

            my_table_tmax <- rbind(my_table_tmax,my_table_line)

        }
      }
    }     
  }

  my_table_tmax <- my_table_tmax[-1,]
  #my_table_tmax
  write.table(my_table_tmax, file = paste('table_',list_tmax[k],'.csv',sep=""), 
              sep = ",", row.names = FALSE)
}
```

```
## [1] "Variable tmax_1_cli"
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-2.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-3.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-4.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-5.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-6.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-7.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-8.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-9.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-10.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-11.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-12.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-13.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-14.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-15.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-16.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-17.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-18.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-19.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-20.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-21.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-22.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-23.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-24.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-25.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-26.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-27.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-28.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-29.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-30.png) 

```
## [1] "Variable tmax_2_cli"
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-31.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-32.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-33.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-34.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-35.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-36.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-37.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-38.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-39.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-40.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-41.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-42.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-43.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-44.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-45.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-46.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-47.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-48.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-49.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-50.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-51.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-52.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-53.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-54.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-55.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-56.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-57.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-58.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-59.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-60.png) 

```
## [1] "Variable tmax_3_cli"
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-61.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-62.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-63.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-64.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-65.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-66.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-67.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-68.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-69.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-70.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-71.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-72.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-73.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-74.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-75.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-76.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-77.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-78.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-79.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-80.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-81.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-82.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-83.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-84.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-85.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-86.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-87.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-88.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-89.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-90.png) 

```
## [1] "Variable tmax_4_cli"
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-91.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-92.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-93.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-94.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-95.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-96.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-97.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-98.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-99.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-100.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-101.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-102.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-103.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-104.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-105.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-106.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-107.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-108.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-109.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-110.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-111.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-112.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-113.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-114.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-115.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-116.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-117.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-118.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-119.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-120.png) 

```
## [1] "Variable tmax_5_cli"
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-121.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-122.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-123.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-124.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-125.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-126.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-127.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-128.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-129.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-130.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-131.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-132.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-133.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-134.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-135.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-136.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-137.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-138.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-139.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-140.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-141.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-142.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-143.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-144.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-145.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-146.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-147.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-148.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-149.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-150.png) 

```
## [1] "Variable tmax_6_cli"
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-151.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-152.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-153.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-154.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-155.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-156.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-157.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-158.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-159.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-160.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-161.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-162.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-163.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-164.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-165.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-166.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-167.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-168.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-169.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-170.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-171.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-172.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-173.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-174.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-175.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-176.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-177.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-178.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-179.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-180.png) 

```
## [1] "Variable tmax_7_cli"
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-181.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-182.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-183.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-184.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-185.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-186.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-187.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-188.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-189.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-190.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-191.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-192.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-193.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-194.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-195.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-196.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-197.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-198.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-199.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-200.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-201.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-202.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-203.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-204.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-205.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-206.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-207.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-208.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-209.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-210.png) 

```
## [1] "Variable tmax_8_cli"
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-211.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-212.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-213.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-214.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-215.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-216.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-217.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-218.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-219.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-220.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-221.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-222.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-223.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-224.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-225.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-226.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-227.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-228.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-229.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-230.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-231.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-232.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-233.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-234.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-235.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-236.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-237.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-238.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-239.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-240.png) 

```
## [1] "Variable tmax_9_cli"
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-241.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-242.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-243.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-244.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-245.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-246.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-247.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-248.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-249.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-250.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-251.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-252.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-253.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-254.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-255.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-256.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-257.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-258.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-259.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-260.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-261.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-262.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-263.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-264.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-265.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-266.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-267.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-268.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-269.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-270.png) 

```
## [1] "Variable tmax_10_cl"
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-271.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-272.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-273.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-274.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-275.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-276.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-277.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-278.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-279.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-280.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-281.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-282.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-283.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-284.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-285.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-286.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-287.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-288.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-289.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-290.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-291.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-292.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-293.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-294.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-295.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-296.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-297.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-298.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-299.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-300.png) 

```
## [1] "Variable tmax_11_cl"
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-301.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-302.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-303.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-304.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-305.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-306.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-307.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-308.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-309.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-310.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-311.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-312.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-313.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-314.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-315.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-316.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-317.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-318.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-319.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-320.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-321.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-322.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-323.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-324.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-325.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-326.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-327.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-328.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-329.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-330.png) 

```
## [1] "Variable tmax_12_cl"
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-331.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-332.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-333.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-334.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-335.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-336.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-337.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-338.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-339.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-340.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-341.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-342.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-343.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-344.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-345.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-346.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-347.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-348.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-349.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-350.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-351.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-352.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-353.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-354.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-355.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-356.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-357.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-358.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-359.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-360.png) 


TMIN regressions


```r
par(mfcol = c(1, 4))

for (k in 1:length(list_tmin)) {
  
  print(paste('Variable ',list_tmin[k],sep=""))

  # create empty table for variable of month k
  my_table_tmin <- data.frame(t(rep(NA,5)))
  columns <- c("X_center", "Y_center", 
             paste('tmin_',k,'_A',sep=""),
             paste('tmin_',k,'_B',sep=""),
             paste('tmin_',k,'_R2',sep=""))
  names(my_table_tmin) <- columns
  
  
  for (i in 1:length(tmin_X_values) ) {
    tmin_i <- tmin[tmin$Xclass == tmin_X_values[i],]
    for (j in 1:length(tmin_Y_values) ) {
      tmin_ij <- tmin_i[tmin_i$Yclass == tmin_Y_values[j],]
      tmin_ij <- tmin_ij[complete.cases(tmin_ij),]        # take only cases with data
      tmin_ij <- tmin_ij[tmin_ij[[elev_field]] > 0,]      # take only data with positive elevation
      if (nrow(tmin_ij) > 0) {
        #print(dim(tmin_ij))
      
        # data suitable for regression analysis should fit following criteria:
        # 1) have enough samples (n > 20)
        # 2) values should be normally distributed ?
        # 3) variability in elevation should be at least 1000 m
        if ((nrow(tmin_ij) > 20) && 
              ( max(tmin_ij[[elev_field]]) - min(tmin_ij[[elev_field]]) >= 1000 )) {
            
            # data could be considered suitable for analysis
            plot(tmin_ij[[list_tmin[k]]] ~ tmin_ij[[elev_field]], 
                 main=paste('Cuadrant ',tmin_X_values[i],'° ',tmin_Y_values[j],'°, month ',k,sep=""), 
                 xlab='elevation', ylab='T min',col='darkgray')
            my_fit <- lm(tmin_ij[[list_tmin[k]]] ~ tmin_ij[[elev_field]])
            abline(my_fit, col='red', lwd=2)
            
            # print(summary(my_fit))
            
            # store the centers of cuadrants, regression coefficients and R2 in table
            # note: the conversion of factor to numeric values require use of levels attribute
            my_table_line <- c(as.numeric(levels(tmin_X_values)[tmin_X_values[i]])+0.5,
                               as.numeric(levels(tmin_Y_values)[tmin_Y_values[j]])+0.5,
                               coef(my_fit)[1],coef(my_fit)[2],
                               summary(my_fit)$r.squared)            

            my_table_tmin <- rbind(my_table_tmin,my_table_line)

        }
      }
    }     
  }

  my_table_tmin <- my_table_tmin[-1,]
  #my_table_tmin
  write.table(my_table_tmin, file = paste('table_',list_tmin[k],'.csv',sep=""), 
              sep = ",", row.names = FALSE)
}
```

```
## [1] "Variable tmin_1_cli"
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-2.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-3.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-4.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-5.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-6.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-7.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-8.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-9.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-10.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-11.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-12.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-13.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-14.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-15.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-16.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-17.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-18.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-19.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-20.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-21.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-22.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-23.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-24.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-25.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-26.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-27.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-28.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-29.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-30.png) 

```
## [1] "Variable tmin_2_cli"
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-31.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-32.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-33.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-34.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-35.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-36.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-37.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-38.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-39.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-40.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-41.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-42.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-43.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-44.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-45.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-46.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-47.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-48.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-49.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-50.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-51.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-52.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-53.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-54.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-55.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-56.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-57.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-58.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-59.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-60.png) 

```
## [1] "Variable tmin_3_cli"
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-61.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-62.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-63.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-64.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-65.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-66.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-67.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-68.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-69.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-70.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-71.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-72.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-73.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-74.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-75.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-76.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-77.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-78.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-79.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-80.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-81.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-82.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-83.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-84.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-85.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-86.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-87.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-88.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-89.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-90.png) 

```
## [1] "Variable tmin_4_cli"
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-91.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-92.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-93.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-94.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-95.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-96.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-97.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-98.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-99.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-100.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-101.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-102.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-103.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-104.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-105.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-106.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-107.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-108.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-109.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-110.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-111.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-112.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-113.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-114.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-115.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-116.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-117.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-118.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-119.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-120.png) 

```
## [1] "Variable tmin_5_cli"
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-121.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-122.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-123.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-124.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-125.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-126.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-127.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-128.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-129.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-130.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-131.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-132.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-133.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-134.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-135.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-136.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-137.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-138.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-139.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-140.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-141.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-142.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-143.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-144.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-145.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-146.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-147.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-148.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-149.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-150.png) 

```
## [1] "Variable tmin_6_cli"
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-151.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-152.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-153.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-154.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-155.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-156.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-157.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-158.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-159.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-160.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-161.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-162.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-163.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-164.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-165.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-166.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-167.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-168.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-169.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-170.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-171.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-172.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-173.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-174.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-175.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-176.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-177.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-178.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-179.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-180.png) 

```
## [1] "Variable tmin_7_cli"
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-181.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-182.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-183.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-184.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-185.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-186.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-187.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-188.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-189.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-190.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-191.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-192.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-193.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-194.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-195.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-196.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-197.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-198.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-199.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-200.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-201.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-202.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-203.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-204.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-205.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-206.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-207.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-208.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-209.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-210.png) 

```
## [1] "Variable tmin_8_cli"
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-211.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-212.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-213.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-214.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-215.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-216.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-217.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-218.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-219.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-220.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-221.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-222.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-223.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-224.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-225.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-226.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-227.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-228.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-229.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-230.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-231.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-232.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-233.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-234.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-235.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-236.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-237.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-238.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-239.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-240.png) 

```
## [1] "Variable tmin_9_cli"
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-241.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-242.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-243.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-244.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-245.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-246.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-247.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-248.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-249.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-250.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-251.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-252.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-253.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-254.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-255.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-256.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-257.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-258.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-259.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-260.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-261.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-262.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-263.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-264.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-265.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-266.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-267.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-268.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-269.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-270.png) 

```
## [1] "Variable tmin_10_cl"
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-271.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-272.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-273.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-274.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-275.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-276.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-277.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-278.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-279.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-280.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-281.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-282.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-283.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-284.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-285.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-286.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-287.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-288.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-289.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-290.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-291.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-292.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-293.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-294.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-295.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-296.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-297.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-298.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-299.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-300.png) 

```
## [1] "Variable tmin_11_cl"
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-301.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-302.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-303.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-304.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-305.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-306.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-307.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-308.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-309.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-310.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-311.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-312.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-313.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-314.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-315.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-316.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-317.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-318.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-319.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-320.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-321.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-322.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-323.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-324.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-325.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-326.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-327.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-328.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-329.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-330.png) 

```
## [1] "Variable tmin_12_cl"
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-331.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-332.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-333.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-334.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-335.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-336.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-337.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-338.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-339.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-340.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-341.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-342.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-343.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-344.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-345.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-346.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-347.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-348.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-349.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-350.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-351.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-352.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-353.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-354.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-355.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-356.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-357.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-358.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-359.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-360.png) 

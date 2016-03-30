Regresiones de temperatura vs. elevación por cuadrantes
========================================================


```r
library(foreign)
library(sp)
```

```
## Warning: package 'sp' was built under R version 3.1.3
```

```r
library(raster)
```

```
## Warning: package 'raster' was built under R version 3.1.2
```

```
## Warning: no function found corresponding to methods exports from 'raster'
## for: 'overlay'
```

```r
library(dismo)
```

```
## Warning: package 'dismo' was built under R version 3.1.2
```

```r
library(rgeos)
```

```
## Warning: package 'rgeos' was built under R version 3.1.3
```

```
## rgeos version: 0.3-17, (SVN revision 520)
##  GEOS runtime version: 3.5.0-CAPI-1.9.0 r4084 
##  Linking to sp version: 1.2-2 
##  Polygon checking: TRUE
```

```r
elev_field <- 'MDE_aster'
slope_field <- 'slope'
aspect_field <- 'aspect'
relief_field <- 'Relief'
wind_slope_field <- 'wind_slop'
wind_dist_field <- 'wind_dist'
ocean_slope_field <- 'ocean_slop'
ocean_dist_field <- 'ocean_dist'
wind_speed_field <- 'wind_sp01'

list_prec <- c('prec_1_cli','prec_2_cli','prec_3_cli','prec_4_cli','prec_5_cli','prec_6_cli','prec_7_cli','prec_8_cli','prec_9_cli','prec_10_cl','prec_11_cl','prec_12_cl')


replaceOutliers <- function(x){
    quantiles <- quantile( x, c(0.25, 0.5, 0.75) )
    limits <- c(quantiles[2] - 1.5*(quantiles[3]-quantiles[1]),
                quantiles[2] + 1.5*(quantiles[3]-quantiles[1]))
    x[ x < limits[1] ] <- limits[1]
    x[ x > limits[2] ] <- limits[2]
    x
}
```



```r
# read data
prec<-read.dbf("prec_100000_sample_2.dbf")
dim(prec)
```

```
## [1] 100000     24
```

```r
# add variables to identify 1 degree cuadrants 
prec$Xclass <- as.factor(floor(prec$X))
prec$Yclass <- as.factor(floor(prec$Y))

minX <- min(floor(prec$X))
maxX <- max(ceiling(prec$X))
minY <- min(floor(prec$Y))
maxY <- max(ceiling(prec$Y))

ext <- extent(minX, maxX, minY, maxY)
ext
```

```
## class       : Extent 
## xmin        : -107 
## xmax        : -72 
## ymin        : 5 
## ymax        : 25
```

```r
# make lists of cuadrant indices
prec_X_values <- sort(unique(prec$Xclass, incomparables = FALSE))
prec_Y_values <- sort(unique(prec$Yclass, incomparables = FALSE))

prec_X_values
```

```
##  [1] -107 -106 -105 -104 -103 -102 -101 -100 -99  -98  -97  -96  -95  -94 
## [15] -93  -92  -91  -90  -89  -88  -87  -86  -85  -84  -83  -82  -81  -80 
## [29] -79  -78  -77  -76  -75  -74  -73 
## 35 Levels: -107 -106 -105 -104 -103 -102 -101 -100 -99 -98 -97 -96 ... -73
```

```r
prec_Y_values
```

```
##  [1] 5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
## Levels: 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
```

```r
Xseq <- seq(minX, maxX, 0.5)
Yseq <- seq(minY, maxY, 0.5)
length(Xseq)
```

```
## [1] 71
```

```r
length(Yseq)
```

```
## [1] 41
```



```r
prec_sp <- SpatialPointsDataFrame(prec[,c('X','Y')],prec,
                                  proj4string=CRS("+proj=longlat +ellps=WGS84"))
summary(prec_sp)
```

```
## Object of class SpatialPointsDataFrame
## Coordinates:
##          min      max
## X -107.00000 -72.9842
## Y    5.99229  24.0164
## Is projected: FALSE 
## proj4string : [+proj=longlat +ellps=WGS84]
## Number of points: 100000
## Data attributes:
##       CID      MDE_aster          slope          wind_slop      
##  Min.   :0   Min.   :   0.0   Min.   : 0.000   Min.   : 0.0000  
##  1st Qu.:0   1st Qu.:  61.0   1st Qu.: 3.769   1st Qu.: 0.8431  
##  Median :0   Median : 365.0   Median : 7.854   Median : 2.3821  
##  Mean   :0   Mean   : 763.5   Mean   :11.143   Mean   : 4.0702  
##  3rd Qu.:0   3rd Qu.:1410.0   3rd Qu.:16.051   3rd Qu.: 5.4667  
##  Max.   :0   Max.   :4861.0   Max.   :87.970   Max.   :44.5858  
##                                                                 
##    wind_dist        ocean_slop        ocean_dist       wind_sp01      
##  Min.   :0.0000   Min.   : 0.0000   Min.   :0.0000   Min.   : 0.3498  
##  1st Qu.:0.3000   1st Qu.: 0.9218   1st Qu.:0.3005   1st Qu.: 1.2826  
##  Median :0.7807   Median : 2.5579   Median :0.7777   Median : 4.1188  
##  Mean   :1.0310   Mean   : 4.2938   Mean   :1.0018   Mean   : 4.0609  
##  3rd Qu.:1.4795   3rd Qu.: 5.7629   3rd Qu.:1.4535   3rd Qu.: 6.1260  
##  Max.   :5.4989   Max.   :44.7741   Max.   :4.1128   Max.   :10.6780  
##                                                                       
##    prec_1_cli       prec_10_cl      prec_11_cl      prec_12_cl    
##  Min.   :  0.00   Min.   : 13.0   Min.   :  2.0   Min.   :  0.00  
##  1st Qu.: 16.00   1st Qu.: 82.0   1st Qu.: 22.0   1st Qu.: 15.00  
##  Median : 30.00   Median :166.0   Median : 65.0   Median : 37.00  
##  Mean   : 51.05   Mean   :192.5   Mean   :109.7   Mean   : 69.02  
##  3rd Qu.: 63.00   3rd Qu.:276.0   3rd Qu.:167.0   3rd Qu.: 98.00  
##  Max.   :504.00   Max.   :820.0   Max.   :799.0   Max.   :749.00  
##                                                                   
##    prec_2_cli       prec_3_cli       prec_4_cli       prec_5_cli   
##  Min.   :  0.00   Min.   :  0.00   Min.   :  0.00   Min.   :  0.0  
##  1st Qu.:  8.00   1st Qu.:  8.00   1st Qu.: 18.00   1st Qu.: 59.0  
##  Median : 24.00   Median : 24.00   Median : 41.00   Median :111.0  
##  Mean   : 36.42   Mean   : 35.85   Mean   : 64.15   Mean   :136.2  
##  3rd Qu.: 51.00   3rd Qu.: 49.00   3rd Qu.: 74.00   3rd Qu.:190.0  
##  Max.   :483.00   Max.   :486.00   Max.   :797.00   Max.   :709.0  
##                                                                    
##    prec_6_cli      prec_7_cli    prec_8_cli      prec_9_cli   
##  Min.   : 15.0   Min.   : 16   Min.   : 29.0   Min.   : 31.0  
##  1st Qu.:147.0   1st Qu.:139   1st Qu.:148.0   1st Qu.:169.0  
##  Median :200.0   Median :192   Median :198.0   Median :225.0  
##  Mean   :219.6   Mean   :219   Mean   :220.2   Mean   :243.2  
##  3rd Qu.:280.0   3rd Qu.:273   3rd Qu.:276.0   3rd Qu.:310.0  
##  Max.   :777.0   Max.   :942   Max.   :803.0   Max.   :807.0  
##                                                               
##        X                 Y              Relief           aspect      
##  Min.   :-107.00   Min.   : 5.992   Min.   :   0.0   Min.   : -1.00  
##  1st Qu.: -99.17   1st Qu.:13.813   1st Qu.:  86.0   1st Qu.: 83.07  
##  Median : -90.27   Median :17.730   Median : 282.0   Median :174.91  
##  Mean   : -90.48   Mean   :16.719   Mean   : 395.7   Mean   :174.62  
##  3rd Qu.: -83.88   3rd Qu.:20.598   3rd Qu.: 611.0   3rd Qu.:264.35  
##  Max.   : -72.98   Max.   :24.016   Max.   :2769.0   Max.   :359.90  
##                                                                      
##      Xclass          Yclass     
##  -90    : 4832   20     : 9342  
##  -99    : 4770   18     : 9301  
##  -100   : 4714   17     : 8460  
##  -89    : 4608   21     : 7729  
##  -101   : 4396   19     : 7637  
##  -102   : 4142   22     : 7545  
##  (Other):72538   (Other):49986
```


```r
#for (k in 1:2) {
#for (k in 1:length(list_prec)) {
for (k in 10:length(list_prec)) {
  print(paste('Variable ',list_prec[k],sep=""))

  # create empty table for variable of month k
  col_pr_2var_R2 <- paste('pr_',k,'_R2',sep="")
  col_pr_2var_I <- paste('pr_',k,'_I',sep="")
  col_pr_2var_El2 <- paste('pr_',k,'_el2',sep="")
  col_pr_2var_El1 <- paste('pr_',k,'_el1',sep="")
  col_pr_2var_Sl1 <- paste('pr_',k,'_sl1',sep="")
  my_table_prec <- data.frame(t(rep(NA,7)))
  columns <- c("X_center", "Y_center", 
             col_pr_2var_I,
             col_pr_2var_El2,
             col_pr_2var_El1,
             col_pr_2var_Sl1,
             col_pr_2var_R2)
  names(my_table_prec) <- columns
  
  for (i in 1:length(Xseq)) {
#  for (i in 19:20) {
    for (j in 1:length(Yseq)) {
#   for (j in 51:53) {
      
      center_XY <- matrix(c(Xseq[i],Yseq[j]),nrow=1,ncol=2,byrow = TRUE)
      center_point <- SpatialPoints(center_XY,
                    proj4string=CRS("+proj=longlat +ellps=WGS84"))
      
      buffer <- circles(center_point, d=50000, lonlat=TRUE)

      selected_points <- prec_sp[!is.na(over(prec_sp,buffer@polygons[1])),]

      selected_points_count <- nrow(selected_points@data)
      
      #print(selected_points)
      #str(selected_points)
      
      
          y <- selected_points@data[[list_prec[k]]]  
          el <- selected_points@data[[elev_field]]
          el_main <- replaceOutliers(el)
          sl <- selected_points@data[[slope_field]]
          ws <- selected_points@data[[wind_slope_field]]
          wd <- selected_points@data[[wind_dist_field]]
          os <- selected_points@data[[ocean_slope_field]]
          od <- selected_points@data[[ocean_dist_field]]
          sp <- selected_points@data[[wind_speed_field]]
          as <- selected_points@data[[aspect_field]]
          rl <- selected_points@data[[relief_field]]
          asx <- cos(selected_points@data[[aspect_field]]*pi/180)
          asy <- sin(selected_points@data[[aspect_field]]*pi/180)      
      
      if ((selected_points_count > 20) && ( max(el_main) - min(el_main) >= 1000 )) {
        print(paste('Point',Xseq[i],Yseq[j],sep=' '))
        print(paste('N =',selected_points_count),sep=' ')
        
        
        par(mfcol = c(1,4))
        plot(buffer, axes=TRUE)
        points(center_point, cex=1.2, pch=20, col='blue')
        points(prec_sp, cex=0.75, pch=20, col='red')
        boxplot(y, ylab='P')
        boxplot(el, ylab='el')
        boxplot(sl, ylab='sl')            
        
        my_fit <- lm(y ~ el)
        plot(y ~ el, 
             main=paste('Point ',Xseq[i],'° ',Yseq[j],'°, month ',k,sep=""), 
             xlab=paste('elevation, R²=',round(summary(my_fit)$r.squared,digits=2),sep=''), 
             ylab='P',col='darkgray')
        abline(my_fit, col='red', lwd=2)

        # elevation + slope + second order
        my_fit2 <- lm(y ~ el + sl + I(el^2))
        plot(y ~ el, 
            main=paste('Point ',Xseq[i],'° ',Yseq[j],'°, month ',k,sep=""), 
            xlab=paste('elevation, el+sl+rl+as 2-nd order, R²=',round(summary(my_fit2)$r.squared,digits=2),sep=''), 
            ylab='P',col='darkgray')
            pol2 <- function(el) my_fit2$coefficient[4]*el^2 + my_fit2$coefficient[2]*el + my_fit2$coefficient[1]
            curve(pol2, col="blue", lwd=2, xname="el", add = TRUE)
        plot(y ~ sl, 
            main=paste('Cuadrant ',prec_X_values[i],'° ',prec_Y_values[j],'°, month ',k,sep=""), 
            xlab=paste('slope, el+sl+rl+as 2-nd order, R²=',round(summary(my_fit2)$r.squared,digits=2),sep=''), 
            ylab='P',col='darkgray')
        pol2a <- function(sl) my_fit2$coefficient[3]*sl + my_fit2$coefficient[1]
        curve(pol2a, col="blue", lwd=2, xname="sl", add = TRUE)          

        print('Elevation + slope 2-nd order')
        print(summary(my_fit2)) 

        my_table_line <- c(Xseq[i],
                           Yseq[j],
                           coef(my_fit2)[1],                               
                           coef(my_fit2)[4],
                           coef(my_fit2)[2],
                           coef(my_fit2)[3],
                           summary(my_fit2)$r.squared)            
        my_table_prec <- rbind(my_table_prec,my_table_line)          
      }
    }
  }

  my_table_prec <- my_table_prec[-1,]
  write.table(my_table_prec, file = paste('table_2var1_',list_prec[k],'.csv',sep=""), 
              sep = ",", row.names = FALSE)
}
```

```
## [1] "Variable prec_10_cl"
## [1] "Point -106 23.5"
## [1] "N = 424"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-2.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -37.456  -6.837   1.049   7.551  24.816 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.029e+01  9.863e-01  81.403  < 2e-16 ***
## el           8.834e-03  2.900e-03   3.046  0.00246 ** 
## sl          -1.223e-01  5.750e-02  -2.126  0.03406 *  
## I(el^2)     -2.511e-06  1.153e-06  -2.177  0.03000 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.32 on 420 degrees of freedom
## Multiple R-squared:  0.0384,	Adjusted R-squared:  0.03153 
## F-statistic:  5.59 on 3 and 420 DF,  p-value: 0.0009107
## 
## [1] "Point -106 24"
## [1] "N = 213"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-3.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-4.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -33.795  -6.242   0.365   8.348  21.058 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.777e+01  2.116e+00  36.746  < 2e-16 ***
## el           8.828e-03  4.532e-03   1.948 0.052762 .  
## sl          -2.446e-01  7.102e-02  -3.445 0.000691 ***
## I(el^2)     -2.771e-06  1.562e-06  -1.775 0.077420 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.75 on 209 degrees of freedom
## Multiple R-squared:  0.0538,	Adjusted R-squared:  0.04022 
## F-statistic: 3.961 on 3 and 209 DF,  p-value: 0.008944
## 
## [1] "Point -105.5 20.5"
## [1] "N = 217"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-5.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-6.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -31.364 -12.383  -0.581   9.793  43.531 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.653e+01  2.248e+00  38.493   <2e-16 ***
## el           9.435e-02  6.583e-03  14.333   <2e-16 ***
## sl          -1.163e-01  1.070e-01  -1.086    0.279    
## I(el^2)     -4.197e-05  3.553e-06 -11.813   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15 on 213 degrees of freedom
## Multiple R-squared:  0.5599,	Adjusted R-squared:  0.5537 
## F-statistic: 90.31 on 3 and 213 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105.5 23"
## [1] "N = 386"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-7.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-8.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -46.317 -11.944   3.004  13.094  32.404 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.368e+01  1.806e+00  51.866  < 2e-16 ***
## el          -4.466e-02  5.640e-03  -7.919 2.64e-14 ***
## sl          -8.769e-02  9.010e-02  -0.973    0.331    
## I(el^2)      1.801e-05  2.699e-06   6.675 8.72e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 17.4 on 382 degrees of freedom
## Multiple R-squared:  0.2675,	Adjusted R-squared:  0.2618 
## F-statistic: 46.51 on 3 and 382 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105.5 23.5"
## [1] "N = 439"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-9.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-10.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -39.732 -10.356   0.538  10.387  29.765 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.310e+01  3.280e+00  25.338   <2e-16 ***
## el          -8.896e-03  4.533e-03  -1.962   0.0503 .  
## sl          -5.712e-02  5.603e-02  -1.019   0.3086    
## I(el^2)      2.157e-06  1.411e-06   1.529   0.1270    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.26 on 435 degrees of freedom
## Multiple R-squared:  0.01983,	Adjusted R-squared:  0.01307 
## F-statistic: 2.933 on 3 and 435 DF,  p-value: 0.03323
## 
## [1] "Point -105.5 24"
## [1] "N = 226"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-11.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-12.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -27.658 -11.171   1.346  10.640  26.398 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 5.531e+01  3.748e+00  14.756   <2e-16 ***
## el          4.199e-03  5.575e-03   0.753   0.4521    
## sl          1.496e-01  7.447e-02   2.009   0.0457 *  
## I(el^2)     1.637e-07  1.765e-06   0.093   0.9262    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.24 on 222 degrees of freedom
## Multiple R-squared:  0.07379,	Adjusted R-squared:  0.06127 
## F-statistic: 5.895 on 3 and 222 DF,  p-value: 0.0006887
## 
## [1] "Point -105 20"
## [1] "N = 431"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-13.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-14.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -63.696 -24.877  -1.119  21.190  74.761 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.098e+02  3.318e+00  33.084   <2e-16 ***
## el           9.483e-02  9.135e-03  10.381   <2e-16 ***
## sl          -3.008e-01  1.707e-01  -1.762   0.0788 .  
## I(el^2)     -4.660e-05  4.185e-06 -11.135   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.82 on 427 degrees of freedom
## Multiple R-squared:  0.2263,	Adjusted R-squared:  0.2208 
## F-statistic: 41.63 on 3 and 427 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105 20.5"
## [1] "N = 458"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-15.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-16.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -56.071 -19.849  -2.212  15.880  76.645 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.225e+02  3.434e+00  35.656   <2e-16 ***
## el           1.438e-03  7.104e-03   0.202   0.8396    
## sl           4.247e-02  1.097e-01   0.387   0.6987    
## I(el^2)     -6.784e-06  3.107e-06  -2.183   0.0295 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 24.66 on 454 degrees of freedom
## Multiple R-squared:  0.1118,	Adjusted R-squared:  0.106 
## F-statistic: 19.05 on 3 and 454 DF,  p-value: 1.182e-11
## 
## [1] "Point -105 21"
## [1] "N = 367"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-17.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-18.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -36.000  -9.566   0.056   9.651  44.353 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.030e+02  1.847e+00  55.761  < 2e-16 ***
## el          -6.600e-02  4.370e-03 -15.103  < 2e-16 ***
## sl           4.042e-01  6.891e-02   5.866 1.01e-08 ***
## I(el^2)      2.964e-05  2.378e-06  12.465  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.94 on 363 degrees of freedom
## Multiple R-squared:  0.4221,	Adjusted R-squared:  0.4173 
## F-statistic: 88.36 on 3 and 363 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105 21.5"
## [1] "N = 374"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-19.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-20.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -30.634  -9.152   1.298   9.217  28.078 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.108e+02  1.293e+00  85.700   <2e-16 ***
## el          -7.582e-02  4.498e-03 -16.856   <2e-16 ***
## sl          -4.006e-02  6.978e-02  -0.574    0.566    
## I(el^2)      3.169e-05  3.127e-06  10.136   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.43 on 370 degrees of freedom
## Multiple R-squared:  0.6551,	Adjusted R-squared:  0.6523 
## F-statistic: 234.2 on 3 and 370 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105 22"
## [1] "N = 437"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-21.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-22.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -30.919  -9.196  -0.290   8.488  36.983 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.937e+01  1.033e+00  96.158  < 2e-16 ***
## el          -5.983e-02  3.790e-03 -15.784  < 2e-16 ***
## sl          -2.003e-01  6.915e-02  -2.896  0.00397 ** 
## I(el^2)      1.954e-05  1.819e-06  10.745  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.57 on 433 degrees of freedom
## Multiple R-squared:  0.6553,	Adjusted R-squared:  0.6529 
## F-statistic: 274.4 on 3 and 433 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105 22.5"
## [1] "N = 427"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-23.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-24.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -27.134  -7.951  -0.219   7.767  34.842 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.213e+01  1.310e+00   70.33   <2e-16 ***
## el          -5.372e-02  3.091e-03  -17.38   <2e-16 ***
## sl          -7.763e-02  5.246e-02   -1.48     0.14    
## I(el^2)      1.729e-05  1.377e-06   12.56   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.11 on 423 degrees of freedom
## Multiple R-squared:  0.6291,	Adjusted R-squared:  0.6265 
## F-statistic: 239.2 on 3 and 423 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105 23"
## [1] "N = 417"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-25.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-26.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -39.786 -10.697  -0.910   9.747  41.256 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.253e+01  3.509e+00  26.373  < 2e-16 ***
## el          -4.437e-02  4.650e-03  -9.542  < 2e-16 ***
## sl          -2.441e-01  6.333e-02  -3.855 0.000134 ***
## I(el^2)      1.246e-05  1.552e-06   8.028 1.04e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.8 on 413 degrees of freedom
## Multiple R-squared:  0.2405,	Adjusted R-squared:  0.235 
## F-statistic: 43.59 on 3 and 413 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105 23.5"
## [1] "N = 427"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-27.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-28.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -27.862 -10.579  -1.505   6.404  42.054 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  1.584e+01  9.231e+00   1.716  0.08691 . 
## el           2.921e-02  9.354e-03   3.123  0.00192 **
## sl           1.537e-01  6.026e-02   2.552  0.01108 * 
## I(el^2)     -5.876e-06  2.352e-06  -2.499  0.01283 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.91 on 423 degrees of freedom
## Multiple R-squared:  0.05257,	Adjusted R-squared:  0.04586 
## F-statistic: 7.824 on 3 and 423 DF,  p-value: 4.294e-05
## 
## [1] "Point -104.5 19.5"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-29.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-30.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -61.877 -20.936  -4.933  23.485  66.378 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.125e+02  3.498e+00  32.151  < 2e-16 ***
## el           4.468e-02  8.673e-03   5.152 4.08e-07 ***
## sl          -2.978e-01  1.677e-01  -1.776   0.0765 .  
## I(el^2)     -2.026e-05  3.992e-06  -5.075 5.99e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 28.92 on 396 degrees of freedom
## Multiple R-squared:  0.06486,	Adjusted R-squared:  0.05778 
## F-statistic: 9.156 on 3 and 396 DF,  p-value: 7.177e-06
## 
## [1] "Point -104.5 20"
## [1] "N = 439"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-31.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-32.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -47.183 -13.782   0.618  15.044  42.926 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.494e+02  4.160e+00  59.941  < 2e-16 ***
## el          -2.215e-01  7.978e-03 -27.763  < 2e-16 ***
## sl           5.913e-01  9.288e-02   6.366 4.94e-10 ***
## I(el^2)      7.409e-05  3.358e-06  22.064  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.88 on 435 degrees of freedom
## Multiple R-squared:  0.753,	Adjusted R-squared:  0.7513 
## F-statistic:   442 on 3 and 435 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 20.5"
## [1] "N = 433"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-33.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-34.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -38.402 -14.268  -2.763  14.324  91.086 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.544e+01  1.139e+01   6.626 1.04e-10 ***
## el          -1.621e-02  1.539e-02  -1.053  0.29279    
## sl           2.767e-01  8.779e-02   3.151  0.00174 ** 
## I(el^2)      1.197e-05  5.148e-06   2.326  0.02050 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.61 on 429 degrees of freedom
## Multiple R-squared:  0.1456,	Adjusted R-squared:  0.1397 
## F-statistic: 24.37 on 3 and 429 DF,  p-value: 1.396e-14
## 
## [1] "Point -104.5 21"
## [1] "N = 424"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-35.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-36.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -36.872  -9.306  -1.879   7.789  47.370 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.355e+01  4.602e+00  18.155  < 2e-16 ***
## el          -4.866e-02  7.122e-03  -6.832 2.96e-11 ***
## sl          -2.812e-02  5.447e-02  -0.516    0.606    
## I(el^2)      2.079e-05  2.727e-06   7.624 1.66e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.01 on 420 degrees of freedom
## Multiple R-squared:  0.1372,	Adjusted R-squared:  0.1311 
## F-statistic: 22.27 on 3 and 420 DF,  p-value: 2.119e-13
## 
## [1] "Point -104.5 21.5"
## [1] "N = 442"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-37.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-38.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -20.900  -7.024  -1.646   4.987  29.396 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.407e+01  2.195e+00  38.306  < 2e-16 ***
## el          -3.235e-02  3.643e-03  -8.879  < 2e-16 ***
## sl          -3.299e-01  4.561e-02  -7.232 2.13e-12 ***
## I(el^2)      8.227e-06  1.509e-06   5.452 8.34e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.894 on 438 degrees of freedom
## Multiple R-squared:  0.3865,	Adjusted R-squared:  0.3823 
## F-statistic: 91.97 on 3 and 438 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 22"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-39.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-40.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.113  -5.590   0.259   6.437  34.670 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.892e+01  1.884e+00  41.885   <2e-16 ***
## el          -4.109e-02  3.407e-03 -12.061   <2e-16 ***
## sl          -6.356e-02  4.321e-02  -1.471    0.142    
## I(el^2)      1.244e-05  1.454e-06   8.558   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.438 on 437 degrees of freedom
## Multiple R-squared:  0.4236,	Adjusted R-squared:  0.4196 
## F-statistic:   107 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 22.5"
## [1] "N = 419"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-41.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-42.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -21.356  -6.107  -1.026   6.582  21.667 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.961e+01  2.946e+00  13.447   <2e-16 ***
## el           2.185e-03  3.902e-03   0.560    0.576    
## sl           5.660e-02  3.935e-02   1.438    0.151    
## I(el^2)     -1.333e-06  1.240e-06  -1.075    0.283    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.537 on 415 degrees of freedom
## Multiple R-squared:  0.0273,	Adjusted R-squared:  0.02026 
## F-statistic: 3.882 on 3 and 415 DF,  p-value: 0.009311
## 
## [1] "Point -104.5 23"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-43.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-44.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.645  -5.510  -1.691   5.294  17.443 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.613e+01  4.028e+00   8.970  < 2e-16 ***
## el          -7.589e-03  4.187e-03  -1.813  0.07062 .  
## sl           8.197e-02  3.136e-02   2.614  0.00928 ** 
## I(el^2)      3.319e-06  1.089e-06   3.048  0.00245 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.03 on 411 degrees of freedom
## Multiple R-squared:  0.1357,	Adjusted R-squared:  0.1294 
## F-statistic: 21.52 on 3 and 411 DF,  p-value: 5.786e-13
## 
## [1] "Point -104.5 23.5"
## [1] "N = 433"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-45.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-46.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -14.5942  -5.2321  -0.1708   4.3309  18.2013 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.576e+01  7.613e+00   6.011 3.95e-09 ***
## el          -3.084e-02  7.084e-03  -4.353 1.68e-05 ***
## sl           1.292e-01  2.883e-02   4.482 9.50e-06 ***
## I(el^2)      1.129e-05  1.625e-06   6.948 1.38e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.167 on 429 degrees of freedom
## Multiple R-squared:  0.5847,	Adjusted R-squared:  0.5818 
## F-statistic: 201.3 on 3 and 429 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 24"
## [1] "N = 229"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-47.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-48.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.4647 -2.7529 -0.0433  2.4870 11.2722 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.439e+01  1.366e+00  25.175   <2e-16 ***
## el          -2.112e-02  1.590e-03 -13.284   <2e-16 ***
## sl           3.576e-03  2.834e-02   0.126      0.9    
## I(el^2)      9.696e-06  5.318e-07  18.232   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.882 on 225 degrees of freedom
## Multiple R-squared:  0.6884,	Adjusted R-squared:  0.6843 
## F-statistic: 165.7 on 3 and 225 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 19"
## [1] "N = 275"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-49.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-50.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -35.649 -11.038  -1.594   9.417  48.918 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.260e+01  1.837e+00  50.396  < 2e-16 ***
## el          -4.539e-03  6.756e-03  -0.672  0.50230    
## sl          -9.218e-02  1.063e-01  -0.867  0.38653    
## I(el^2)      1.276e-05  4.439e-06   2.874  0.00437 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.73 on 271 degrees of freedom
## Multiple R-squared:  0.108,	Adjusted R-squared:  0.09809 
## F-statistic: 10.93 on 3 and 271 DF,  p-value: 8.424e-07
## 
## [1] "Point -104 19.5"
## [1] "N = 442"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-51.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-52.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -48.78 -17.32  -6.86  16.20  86.89 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.162e+02  4.847e+00  23.979  < 2e-16 ***
## el          -4.510e-02  7.694e-03  -5.862 9.03e-09 ***
## sl           4.218e-01  1.231e-01   3.427 0.000668 ***
## I(el^2)      1.480e-05  2.676e-06   5.530 5.50e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.9 on 438 degrees of freedom
## Multiple R-squared:  0.08552,	Adjusted R-squared:  0.07926 
## F-statistic: 13.65 on 3 and 438 DF,  p-value: 1.572e-08
## 
## [1] "Point -104 20"
## [1] "N = 458"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-53.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-54.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -31.280  -8.078  -1.468   7.050  42.573 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 5.409e+01  6.968e+00   7.763 5.55e-14 ***
## el          1.396e-02  8.973e-03   1.555    0.121    
## sl          3.850e-01  6.390e-02   6.025 3.50e-09 ***
## I(el^2)     4.889e-07  2.736e-06   0.179    0.858    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.2 on 454 degrees of freedom
## Multiple R-squared:  0.2779,	Adjusted R-squared:  0.2731 
## F-statistic: 58.24 on 3 and 454 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 21"
## [1] "N = 467"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-55.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-56.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -14.6915  -6.0430  -0.2302   4.9810  24.0690 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.642e+01  4.277e+00  10.853  < 2e-16 ***
## el           3.158e-03  5.833e-03   0.541    0.588    
## sl          -1.630e-01  3.141e-02  -5.189 3.18e-07 ***
## I(el^2)     -9.617e-08  2.007e-06  -0.048    0.962    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.796 on 463 degrees of freedom
## Multiple R-squared:  0.08027,	Adjusted R-squared:  0.07431 
## F-statistic: 13.47 on 3 and 463 DF,  p-value: 1.929e-08
## 
## [1] "Point -104 21.5"
## [1] "N = 458"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-57.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-58.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.4399  -4.5608   0.2258   3.7780  16.8203 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.935e+01  2.388e+00  20.663  < 2e-16 ***
## el          -8.512e-03  3.257e-03  -2.613  0.00926 ** 
## sl          -3.400e-02  2.358e-02  -1.442  0.15000    
## I(el^2)      1.754e-06  1.081e-06   1.623  0.10532    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.62 on 454 degrees of freedom
## Multiple R-squared:  0.07544,	Adjusted R-squared:  0.06933 
## F-statistic: 12.35 on 3 and 454 DF,  p-value: 8.884e-08
## 
## [1] "Point -104 22"
## [1] "N = 437"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-59.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-60.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -18.0270  -3.3973  -0.2536   3.1084  20.8461 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.763e+01  3.187e+00   8.670  < 2e-16 ***
## el           2.348e-02  4.024e-03   5.835 1.05e-08 ***
## sl          -4.616e-02  2.416e-02  -1.911   0.0567 .  
## I(el^2)     -9.487e-06  1.232e-06  -7.699 9.40e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.711 on 433 degrees of freedom
## Multiple R-squared:  0.3062,	Adjusted R-squared:  0.3014 
## F-statistic: 63.69 on 3 and 433 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 22.5"
## [1] "N = 437"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-61.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-62.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -13.1579  -2.3758  -0.6173   2.0297  12.7600 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.754e+01  2.838e+00  13.226  < 2e-16 ***
## el          -5.081e-03  3.194e-03  -1.591  0.11235    
## sl           5.744e-02  1.978e-02   2.904  0.00388 ** 
## I(el^2)      5.700e-07  8.910e-07   0.640  0.52267    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.4 on 433 degrees of freedom
## Multiple R-squared:  0.1143,	Adjusted R-squared:  0.1081 
## F-statistic: 18.62 on 3 and 433 DF,  p-value: 2.227e-11
## 
## [1] "Point -104 23"
## [1] "N = 417"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-63.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-64.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.4819 -1.5077 -0.4818  1.2420  7.4769 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.566e+01  2.259e+00  15.783  < 2e-16 ***
## el          -1.205e-02  2.251e-03  -5.355 1.43e-07 ***
## sl           2.550e-02  1.206e-02   2.115   0.0351 *  
## I(el^2)      4.173e-06  5.478e-07   7.618 1.78e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.361 on 413 degrees of freedom
## Multiple R-squared:  0.4959,	Adjusted R-squared:  0.4922 
## F-statistic: 135.4 on 3 and 413 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 23.5"
## [1] "N = 396"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-65.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-66.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -6.261 -3.139 -1.481  1.238 16.352 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -6.304e+00  1.020e+01  -0.618  0.53708   
## el           2.048e-02  9.382e-03   2.183  0.02962 * 
## sl          -8.067e-02  2.768e-02  -2.914  0.00377 **
## I(el^2)     -1.329e-06  2.148e-06  -0.619  0.53640   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.6 on 392 degrees of freedom
## Multiple R-squared:  0.4294,	Adjusted R-squared:  0.425 
## F-statistic: 98.32 on 3 and 392 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 18.5"
## [1] "N = 240"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-67.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-68.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -37.832  -8.649  -0.005  10.919  29.085 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.365e+01  2.141e+00  43.745  < 2e-16 ***
## el           2.062e-02  6.154e-03   3.351 0.000936 ***
## sl          -1.274e-01  1.084e-01  -1.175 0.241356    
## I(el^2)      8.245e-06  3.062e-06   2.693 0.007600 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.59 on 236 degrees of freedom
## Multiple R-squared:  0.6558,	Adjusted R-squared:  0.6515 
## F-statistic: 149.9 on 3 and 236 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 19"
## [1] "N = 376"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-69.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-70.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -69.633 -13.518  -1.641  13.693  43.571 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.743e+01  2.474e+00  31.299  < 2e-16 ***
## el           2.989e-02  5.947e-03   5.026  7.8e-07 ***
## sl           1.159e-01  1.031e-01   1.125    0.261    
## I(el^2)     -3.607e-06  2.933e-06  -1.230    0.220    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.04 on 372 degrees of freedom
## Multiple R-squared:  0.3343,	Adjusted R-squared:  0.3289 
## F-statistic: 62.27 on 3 and 372 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 19.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-71.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-72.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -41.289 -12.529   0.828  13.526  36.623 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.534e+01  4.709e+00  20.246  < 2e-16 ***
## el          -2.054e-02  6.525e-03  -3.148 0.001768 ** 
## sl           1.433e-01  9.601e-02   1.493 0.136225    
## I(el^2)      8.125e-06  2.151e-06   3.778 0.000182 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 17.98 on 407 degrees of freedom
## Multiple R-squared:  0.05395,	Adjusted R-squared:  0.04698 
## F-statistic: 7.737 on 3 and 407 DF,  p-value: 4.893e-05
## 
## [1] "Point -103.5 20"
## [1] "N = 432"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-73.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-74.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -34.291 -13.921   0.426   9.890  44.764 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.089e+01  1.379e+01   6.591 1.28e-10 ***
## el          -4.519e-02  1.528e-02  -2.957 0.003275 ** 
## sl           2.870e-01  8.048e-02   3.567 0.000403 ***
## I(el^2)      1.806e-05  4.091e-06   4.415 1.28e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.77 on 428 degrees of freedom
## Multiple R-squared:  0.2783,	Adjusted R-squared:  0.2733 
## F-statistic: 55.02 on 3 and 428 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 21"
## [1] "N = 418"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-75.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-76.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -20.1222  -3.4301   0.4201   3.3808  17.5719 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.658e+01  5.057e+00   5.256 2.36e-07 ***
## el           3.451e-02  6.379e-03   5.410 1.07e-07 ***
## sl          -1.743e-01  2.562e-02  -6.802 3.63e-11 ***
## I(el^2)     -1.179e-05  2.023e-06  -5.828 1.13e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.651 on 414 degrees of freedom
## Multiple R-squared:  0.1999,	Adjusted R-squared:  0.1941 
## F-statistic: 34.47 on 3 and 414 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 21.5"
## [1] "N = 427"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-77.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-78.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.2727 -3.3106  0.6021  3.5131 14.6908 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.140e+00  2.969e+00   0.384    0.701    
## el           4.565e-02  3.573e-03  12.774   <2e-16 ***
## sl          -1.175e-03  1.933e-02  -0.061    0.952    
## I(el^2)     -1.293e-05  1.051e-06 -12.310   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.071 on 423 degrees of freedom
## Multiple R-squared:  0.286,	Adjusted R-squared:  0.2809 
## F-statistic: 56.48 on 3 and 423 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 22"
## [1] "N = 468"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-79.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-80.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.6593  -2.7714  -0.2036   2.8170  12.3339 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.184e+01  3.855e+00   5.666 2.57e-08 ***
## el           2.193e-02  4.186e-03   5.239 2.46e-07 ***
## sl          -2.925e-02  1.838e-02  -1.591    0.112    
## I(el^2)     -7.294e-06  1.131e-06  -6.451 2.79e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.286 on 464 degrees of freedom
## Multiple R-squared:  0.1933,	Adjusted R-squared:  0.188 
## F-statistic: 37.05 on 3 and 464 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 18"
## [1] "N = 113"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-81.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-82.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -20.466  -5.592  -1.737   4.291  18.637 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.997e+01  1.898e+00  52.668  < 2e-16 ***
## el          -7.570e-03  5.874e-03  -1.289    0.200    
## sl          -6.129e-02  8.364e-02  -0.733    0.465    
## I(el^2)      3.126e-05  3.880e-06   8.055 1.08e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.583 on 109 degrees of freedom
## Multiple R-squared:  0.8214,	Adjusted R-squared:  0.8165 
## F-statistic: 167.1 on 3 and 109 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 18.5"
## [1] "N = 392"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-83.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-84.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -28.5903  -7.2760  -0.0551   8.1171  28.7094 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.243e+01  2.070e+00  34.985   <2e-16 ***
## el           6.575e-02  3.546e-03  18.544   <2e-16 ***
## sl           3.847e-02  5.965e-02   0.645    0.519    
## I(el^2)     -1.411e-05  1.464e-06  -9.638   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.8 on 388 degrees of freedom
## Multiple R-squared:  0.7641,	Adjusted R-squared:  0.7623 
## F-statistic: 418.9 on 3 and 388 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 19"
## [1] "N = 406"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-85.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-86.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -39.940  -6.885   0.131   8.299  30.563 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.451e+01  2.447e+00  10.014   <2e-16 ***
## el           1.232e-01  5.036e-03  24.455   <2e-16 ***
## sl           7.294e-02  7.290e-02   1.001    0.318    
## I(el^2)     -3.530e-05  2.036e-06 -17.337   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.76 on 402 degrees of freedom
## Multiple R-squared:  0.801,	Adjusted R-squared:  0.7995 
## F-statistic: 539.3 on 3 and 402 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 19.5"
## [1] "N = 448"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-87.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-88.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -42.470  -8.359  -0.507   7.251  50.970 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.067e+01  3.226e+00  12.609  < 2e-16 ***
## el           8.187e-02  5.521e-03  14.829  < 2e-16 ***
## sl           1.895e-01  7.136e-02   2.655  0.00821 ** 
## I(el^2)     -2.976e-05  2.129e-06 -13.980  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.71 on 444 degrees of freedom
## Multiple R-squared:  0.3685,	Adjusted R-squared:  0.3643 
## F-statistic: 86.37 on 3 and 444 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 20"
## [1] "N = 409"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-89.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-90.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -45.937 -11.709  -1.863  12.323  50.412 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.049e+02  2.546e+01   8.045 9.61e-15 ***
## el          -1.738e-01  2.901e-02  -5.993 4.57e-09 ***
## sl           8.381e-01  8.959e-02   9.355  < 2e-16 ***
## I(el^2)      5.064e-05  8.190e-06   6.183 1.54e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.83 on 405 degrees of freedom
## Multiple R-squared:  0.277,	Adjusted R-squared:  0.2717 
## F-statistic: 51.73 on 3 and 405 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 21.5"
## [1] "N = 406"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-91.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-92.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.0964 -2.4197 -0.0518  2.3359  9.1397 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -4.457e+01  5.600e+00  -7.959 1.79e-14 ***
## el           8.615e-02  5.765e-03  14.945  < 2e-16 ***
## sl           6.230e-02  1.808e-02   3.445 0.000631 ***
## I(el^2)     -2.112e-05  1.468e-06 -14.391  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.404 on 402 degrees of freedom
## Multiple R-squared:  0.3887,	Adjusted R-squared:  0.3841 
## F-statistic:  85.2 on 3 and 402 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 22"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-93.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-94.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.874  -3.749  -1.079   3.425  16.490 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.278e+01  1.065e+01   2.140   0.0329 *  
## el           1.487e-02  1.044e-02   1.424   0.1552    
## sl           1.163e-01  2.950e-02   3.942 9.43e-05 ***
## I(el^2)     -3.490e-06  2.532e-06  -1.379   0.1687    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.203 on 430 degrees of freedom
## Multiple R-squared:  0.04258,	Adjusted R-squared:  0.03591 
## F-statistic: 6.375 on 3 and 430 DF,  p-value: 0.0003102
## 
## [1] "Point -102.5 18"
## [1] "N = 183"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-95.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-96.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -18.2342  -4.9609  -0.0768   3.7073  26.5218 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.174e+02  1.252e+00  93.780  < 2e-16 ***
## el          -2.778e-02  3.994e-03  -6.955 6.38e-11 ***
## sl          -2.393e-01  7.423e-02  -3.224   0.0015 ** 
## I(el^2)      2.825e-05  2.447e-06  11.548  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.896 on 179 degrees of freedom
## Multiple R-squared:  0.5583,	Adjusted R-squared:  0.5509 
## F-statistic: 75.43 on 3 and 179 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102.5 18.5"
## [1] "N = 408"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-97.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-98.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -29.922 -10.562   2.343   9.589  27.757 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.312e+01  2.734e+00  30.396  < 2e-16 ***
## el           1.904e-02  6.120e-03   3.111 0.001999 ** 
## sl          -6.277e-02  7.741e-02  -0.811 0.417929    
## I(el^2)      1.089e-05  3.037e-06   3.587 0.000376 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.11 on 404 degrees of freedom
## Multiple R-squared:  0.6255,	Adjusted R-squared:  0.6227 
## F-statistic: 224.9 on 3 and 404 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102.5 19"
## [1] "N = 438"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-99.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-100.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -44.146  -9.334   1.482  10.192  25.630 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.505e+01  1.810e+00  19.365   <2e-16 ***
## el           8.886e-02  4.533e-03  19.602   <2e-16 ***
## sl           1.398e-01  6.895e-02   2.027   0.0433 *  
## I(el^2)     -2.312e-05  1.952e-06 -11.848   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.06 on 434 degrees of freedom
## Multiple R-squared:  0.7584,	Adjusted R-squared:  0.7567 
## F-statistic:   454 on 3 and 434 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102.5 19.5"
## [1] "N = 438"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-101.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-102.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -39.762 -10.504  -2.402   7.033  57.365 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.434e+01  2.892e+00  15.335  < 2e-16 ***
## el           5.538e-02  4.424e-03  12.519  < 2e-16 ***
## sl           2.494e-01  9.302e-02   2.681  0.00762 ** 
## I(el^2)     -1.367e-05  1.474e-06  -9.275  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.66 on 434 degrees of freedom
## Multiple R-squared:  0.445,	Adjusted R-squared:  0.4412 
## F-statistic:   116 on 3 and 434 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102.5 20"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-103.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-104.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -48.844 -12.528  -3.059   9.923  62.888 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.004e+02  2.122e+01   4.732 3.01e-06 ***
## el          -7.292e-02  2.313e-02  -3.153  0.00173 ** 
## sl           7.586e-01  1.103e-01   6.877 2.13e-11 ***
## I(el^2)      2.832e-05  6.291e-06   4.501 8.69e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.43 on 437 degrees of freedom
## Multiple R-squared:  0.372,	Adjusted R-squared:  0.3677 
## F-statistic: 86.29 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102 18.5"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-105.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-106.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -26.079 -10.421  -3.631   7.612  55.167 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.693e+01  2.432e+00  27.525   <2e-16 ***
## el           1.767e-02  9.026e-03   1.958   0.0509 .  
## sl          -2.635e-03  7.545e-02  -0.035   0.9722    
## I(el^2)      1.630e-05  6.450e-06   2.528   0.0119 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.85 on 396 degrees of freedom
## Multiple R-squared:  0.4299,	Adjusted R-squared:  0.4255 
## F-statistic: 99.52 on 3 and 396 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102 19"
## [1] "N = 440"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-107.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-108.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -35.419  -6.374  -0.431   4.233  46.462 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.430e+01  1.663e+00  26.633  < 2e-16 ***
## el           6.608e-02  4.052e-03  16.311  < 2e-16 ***
## sl          -1.800e-01  6.243e-02  -2.884  0.00412 ** 
## I(el^2)     -1.497e-05  1.778e-06  -8.423 5.36e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.59 on 436 degrees of freedom
## Multiple R-squared:  0.7588,	Adjusted R-squared:  0.7571 
## F-statistic: 457.1 on 3 and 436 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102 19.5"
## [1] "N = 450"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-109.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-110.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -41.31 -13.74  -2.31  11.04  64.61 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.693e+01  5.970e+00   9.536  < 2e-16 ***
## el           3.232e-02  7.073e-03   4.569 6.34e-06 ***
## sl           6.348e-01  1.254e-01   5.064 6.01e-07 ***
## I(el^2)     -8.007e-06  2.073e-06  -3.862 0.000129 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.67 on 446 degrees of freedom
## Multiple R-squared:  0.1052,	Adjusted R-squared:  0.09921 
## F-statistic: 17.48 on 3 and 446 DF,  p-value: 9.502e-11
## 
## [1] "Point -102 20"
## [1] "N = 433"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-111.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-112.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.972  -7.671  -1.309   5.267  40.435 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 6.146e+00  1.825e+01   0.337   0.7364  
## el          2.178e-02  1.735e-02   1.255   0.2101  
## sl          2.143e-01  8.712e-02   2.460   0.0143 *
## I(el^2)     4.320e-06  4.035e-06   1.071   0.2849  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.94 on 429 degrees of freedom
## Multiple R-squared:  0.6107,	Adjusted R-squared:  0.608 
## F-statistic: 224.3 on 3 and 429 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101.5 17.5"
## [1] "N = 152"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-113.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-114.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -40.706 -10.338  -1.264   9.640  37.699 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.071e+02  2.414e+00  44.373   <2e-16 ***
## el           1.494e-01  1.042e-02  14.333   <2e-16 ***
## sl          -2.562e-02  1.526e-01  -0.168    0.867    
## I(el^2)     -8.585e-05  8.062e-06 -10.649   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.23 on 148 degrees of freedom
## Multiple R-squared:  0.7004,	Adjusted R-squared:  0.6943 
## F-statistic: 115.3 on 3 and 148 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101.5 18"
## [1] "N = 376"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-115.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-116.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -47.571 -15.364  -4.743  10.884  84.504 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.163e+02  3.621e+00  32.124  < 2e-16 ***
## el          -1.838e-02  8.334e-03  -2.205  0.02808 *  
## sl           1.324e-01  1.395e-01   0.949  0.34300    
## I(el^2)      1.230e-05  3.977e-06   3.094  0.00213 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 26.73 on 372 degrees of freedom
## Multiple R-squared:  0.04258,	Adjusted R-squared:  0.03486 
## F-statistic: 5.515 on 3 and 372 DF,  p-value: 0.001027
## 
## [1] "Point -101.5 18.5"
## [1] "N = 426"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-117.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-118.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -19.969  -6.215  -2.019   5.326  55.113 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 5.540e+01  1.479e+00  37.467   <2e-16 ***
## el          3.637e-02  3.469e-03  10.485   <2e-16 ***
## sl          6.557e-02  4.577e-02   1.433    0.153    
## I(el^2)     1.098e-08  1.669e-06   0.007    0.995    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.396 on 422 degrees of freedom
## Multiple R-squared:  0.7514,	Adjusted R-squared:  0.7497 
## F-statistic: 425.2 on 3 and 422 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101.5 19"
## [1] "N = 468"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-119.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-120.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -20.868  -7.398  -2.406   6.555  40.927 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.309e+01  1.897e+00  33.252  < 2e-16 ***
## el           2.766e-02  3.081e-03   8.977  < 2e-16 ***
## sl          -2.501e-02  5.109e-02  -0.490  0.62462    
## I(el^2)     -3.080e-06  1.066e-06  -2.890  0.00403 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.53 on 464 degrees of freedom
## Multiple R-squared:  0.5791,	Adjusted R-squared:  0.5764 
## F-statistic: 212.8 on 3 and 464 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101.5 19.5"
## [1] "N = 460"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-121.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-122.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -35.665 -12.329  -2.255   8.790  61.839 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.262e+02  1.066e+01  11.846  < 2e-16 ***
## el          -6.037e-02  1.108e-02  -5.450 8.26e-08 ***
## sl           3.472e-01  9.512e-02   3.651 0.000292 ***
## I(el^2)      1.802e-05  2.884e-06   6.250 9.44e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 17.36 on 456 degrees of freedom
## Multiple R-squared:  0.151,	Adjusted R-squared:  0.1454 
## F-statistic: 27.04 on 3 and 456 DF,  p-value: 4.139e-16
## 
## [1] "Point -101 17.5"
## [1] "N = 314"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-123.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-124.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -52.44 -10.60  -3.21  10.16  55.24 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.106e+02  2.234e+00  49.506   <2e-16 ***
## el           6.389e-02  5.284e-03  12.091   <2e-16 ***
## sl           1.906e-01  1.161e-01   1.641    0.102    
## I(el^2)     -1.954e-05  2.177e-06  -8.976   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.01 on 310 degrees of freedom
## Multiple R-squared:  0.5215,	Adjusted R-squared:  0.5168 
## F-statistic: 112.6 on 3 and 310 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101 18"
## [1] "N = 389"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-125.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-126.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -43.528 -21.440  -5.425  14.053  87.748 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.158e+01  5.811e+00  15.761   <2e-16 ***
## el           2.618e-02  1.049e-02   2.497   0.0130 *  
## sl           2.686e-01  1.372e-01   1.957   0.0511 .  
## I(el^2)     -5.574e-07  3.946e-06  -0.141   0.8877    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 28.22 on 385 degrees of freedom
## Multiple R-squared:  0.2385,	Adjusted R-squared:  0.2326 
## F-statistic: 40.19 on 3 and 385 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101 19"
## [1] "N = 456"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-127.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-128.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -29.387 -12.472  -2.486  11.951  50.968 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.595e+01  4.129e+00  15.973  < 2e-16 ***
## el           5.688e-02  8.375e-03   6.791 3.51e-11 ***
## sl           7.932e-02  8.727e-02   0.909    0.364    
## I(el^2)     -2.277e-05  3.575e-06  -6.371 4.64e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.4 on 452 degrees of freedom
## Multiple R-squared:  0.1076,	Adjusted R-squared:  0.1017 
## F-statistic: 18.17 on 3 and 452 DF,  p-value: 3.764e-11
## 
## [1] "Point -101 19.5"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-129.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-130.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -41.628 -10.999   1.721  11.294  36.247 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.153e+02  6.411e+00  33.578  < 2e-16 ***
## el          -1.696e-01  7.541e-03 -22.490  < 2e-16 ***
## sl           5.497e-01  7.466e-02   7.363  9.9e-13 ***
## I(el^2)      4.745e-05  2.178e-06  21.787  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.11 on 411 degrees of freedom
## Multiple R-squared:  0.5947,	Adjusted R-squared:  0.5918 
## F-statistic:   201 on 3 and 411 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101 23"
## [1] "N = 430"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-131.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-132.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.4666  -3.3807  -0.1882   3.1270  11.8800 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -6.900e-01  9.210e+00  -0.075 0.940320    
## el           3.696e-02  9.771e-03   3.783 0.000177 ***
## sl          -5.058e-02  3.293e-02  -1.536 0.125266    
## I(el^2)     -9.700e-06  2.575e-06  -3.768 0.000188 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.8 on 426 degrees of freedom
## Multiple R-squared:  0.04172,	Adjusted R-squared:  0.03497 
## F-statistic: 6.182 on 3 and 426 DF,  p-value: 0.0004046
## 
## [1] "Point -101 23.5"
## [1] "N = 446"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-133.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-134.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.4090  -3.4173   0.4466   3.0962  11.0536 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.633e+01  7.931e+00   3.321 0.000973 ***
## el           1.414e-02  8.079e-03   1.750 0.080800 .  
## sl          -7.407e-02  3.097e-02  -2.392 0.017186 *  
## I(el^2)     -5.010e-06  2.052e-06  -2.442 0.015008 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.875 on 442 degrees of freedom
## Multiple R-squared:  0.1549,	Adjusted R-squared:  0.1491 
## F-statistic:    27 on 3 and 442 DF,  p-value: 4.758e-16
## 
## [1] "Point -100.5 17"
## [1] "N = 164"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-135.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-136.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -29.1635  -7.1364  -0.1767   8.1044  23.6700 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.012e+02  1.486e+00  68.087  < 2e-16 ***
## el           1.138e-01  6.767e-03  16.818  < 2e-16 ***
## sl          -5.132e-01  1.057e-01  -4.854 2.86e-06 ***
## I(el^2)     -3.229e-05  5.121e-06  -6.304 2.70e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.62 on 160 degrees of freedom
## Multiple R-squared:  0.8722,	Adjusted R-squared:  0.8698 
## F-statistic: 363.9 on 3 and 160 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 17.5"
## [1] "N = 390"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-137.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-138.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -43.831 -13.766  -0.694  13.876  43.290 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.187e+02  2.666e+00  44.529  < 2e-16 ***
## el           3.495e-02  4.763e-03   7.337 1.30e-12 ***
## sl          -1.252e-01  1.062e-01  -1.180    0.239    
## I(el^2)     -6.847e-06  1.719e-06  -3.982 8.16e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.94 on 386 degrees of freedom
## Multiple R-squared:  0.3052,	Adjusted R-squared:  0.2998 
## F-statistic: 56.51 on 3 and 386 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 18"
## [1] "N = 399"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-139.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-140.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -37.629 -10.300   0.726   8.876  50.159 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.030e+01  2.199e+00  31.967   <2e-16 ***
## el           4.104e-02  4.759e-03   8.623   <2e-16 ***
## sl          -7.321e-02  7.352e-02  -0.996    0.320    
## I(el^2)     -6.610e-07  1.758e-06  -0.376    0.707    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.31 on 395 degrees of freedom
## Multiple R-squared:  0.7653,	Adjusted R-squared:  0.7636 
## F-statistic: 429.4 on 3 and 395 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 18.5"
## [1] "N = 435"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-141.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-142.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.350  -6.358  -1.271   5.532  53.085 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.289e+01  1.889e+00  27.990  < 2e-16 ***
## el           8.036e-02  5.201e-03  15.451  < 2e-16 ***
## sl          -1.416e-01  6.321e-02  -2.240   0.0256 *  
## I(el^2)     -2.009e-05  2.470e-06  -8.134 4.48e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.15 on 431 degrees of freedom
## Multiple R-squared:  0.7165,	Adjusted R-squared:  0.7146 
## F-statistic: 363.2 on 3 and 431 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 19"
## [1] "N = 420"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-143.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-144.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -44.885 -10.975  -1.145   9.920  90.924 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.153e+01  4.396e+00  11.723   <2e-16 ***
## el           9.649e-02  7.817e-03  12.343   <2e-16 ***
## sl           9.223e-02  8.666e-02   1.064    0.288    
## I(el^2)     -3.426e-05  2.982e-06 -11.490   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.1 on 416 degrees of freedom
## Multiple R-squared:  0.297,	Adjusted R-squared:  0.2919 
## F-statistic: 58.59 on 3 and 416 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 19.5"
## [1] "N = 386"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-145.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-146.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -29.333  -9.044  -2.304   7.749  39.339 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.535e+02  5.133e+00  29.901  < 2e-16 ***
## el          -7.165e-02  5.620e-03 -12.748  < 2e-16 ***
## sl           3.290e-01  6.680e-02   4.926 1.25e-06 ***
## I(el^2)      1.662e-05  1.475e-06  11.269  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.27 on 382 degrees of freedom
## Multiple R-squared:  0.4044,	Adjusted R-squared:  0.3997 
## F-statistic: 86.45 on 3 and 382 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 20"
## [1] "N = 413"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-147.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-148.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -25.373  -8.615  -1.866   6.529  50.021 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.300e+01  2.985e+01   2.110 0.035433 *  
## el          -2.481e-02  2.552e-02  -0.972 0.331496    
## sl           2.708e-01  7.904e-02   3.425 0.000676 ***
## I(el^2)      9.792e-06  5.372e-06   1.823 0.069072 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.86 on 409 degrees of freedom
## Multiple R-squared:  0.2733,	Adjusted R-squared:  0.2679 
## F-statistic: 51.27 on 3 and 409 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 22"
## [1] "N = 467"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-149.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-150.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -14.403  -5.112  -0.629   3.960  23.322 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.533e+01  5.876e+00  14.521  < 2e-16 ***
## el          -4.898e-02  7.484e-03  -6.545 1.58e-10 ***
## sl           3.566e-01  3.628e-02   9.829  < 2e-16 ***
## I(el^2)      1.049e-05  2.291e-06   4.579 6.00e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.673 on 463 degrees of freedom
## Multiple R-squared:  0.4125,	Adjusted R-squared:  0.4087 
## F-statistic: 108.3 on 3 and 463 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 22.5"
## [1] "N = 426"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-151.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-152.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.061  -3.654  -0.713   3.966  15.494 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.069e+01  6.438e+00  14.087  < 2e-16 ***
## el          -5.905e-02  8.242e-03  -7.165 3.49e-12 ***
## sl           1.641e-01  2.973e-02   5.521 5.90e-08 ***
## I(el^2)      1.424e-05  2.591e-06   5.497 6.71e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.034 on 422 degrees of freedom
## Multiple R-squared:  0.4022,	Adjusted R-squared:  0.398 
## F-statistic: 94.64 on 3 and 422 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 23"
## [1] "N = 458"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-153.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-154.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.7425  -1.6665   0.0605   1.6801   8.7620 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -2.387e+01  3.907e+00  -6.110 2.14e-09 ***
## el           6.233e-02  4.685e-03  13.304  < 2e-16 ***
## sl          -3.433e-02  1.696e-02  -2.023   0.0436 *  
## I(el^2)     -1.590e-05  1.379e-06 -11.532  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.281 on 454 degrees of freedom
## Multiple R-squared:  0.4594,	Adjusted R-squared:  0.4558 
## F-statistic: 128.6 on 3 and 454 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 23.5"
## [1] "N = 469"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-155.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-156.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.621 -2.440 -0.114  2.955 21.149 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -2.358e+01  4.878e+00  -4.833 1.83e-06 ***
## el           6.703e-02  5.265e-03  12.731  < 2e-16 ***
## sl          -3.144e-02  2.349e-02  -1.339    0.181    
## I(el^2)     -1.845e-05  1.415e-06 -13.041  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.073 on 465 degrees of freedom
## Multiple R-squared:  0.2996,	Adjusted R-squared:  0.295 
## F-statistic: 66.29 on 3 and 465 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 17"
## [1] "N = 258"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-157.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-158.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -39.210  -9.496  -1.567   8.793  53.337 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.877e+01  1.620e+00  60.966   <2e-16 ***
## el           8.888e-02  4.199e-03  21.169   <2e-16 ***
## sl           9.990e-03  9.826e-02   0.102    0.919    
## I(el^2)     -3.076e-05  2.251e-06 -13.661   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.51 on 254 degrees of freedom
## Multiple R-squared:  0.7623,	Adjusted R-squared:  0.7595 
## F-statistic: 271.5 on 3 and 254 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 17.5"
## [1] "N = 420"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-159.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-160.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -67.762 -22.330   0.991  21.501  56.164 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.620e+02  7.282e+00  22.249  < 2e-16 ***
## el          -4.990e-02  1.037e-02  -4.811 2.11e-06 ***
## sl          -1.348e-01  1.432e-01  -0.941    0.347    
## I(el^2)      1.880e-05  3.287e-06   5.721 2.03e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.37 on 416 degrees of freedom
## Multiple R-squared:  0.09506,	Adjusted R-squared:  0.08853 
## F-statistic: 14.57 on 3 and 416 DF,  p-value: 4.875e-09
## 
## [1] "Point -100 18"
## [1] "N = 438"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-161.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-162.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -33.401 -10.531  -2.244   9.418  48.074 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.026e+02  3.210e+00   31.97  < 2e-16 ***
## el          -5.027e-02  5.303e-03   -9.48  < 2e-16 ***
## sl           2.211e-01  6.353e-02    3.48 0.000552 ***
## I(el^2)      2.792e-05  1.958e-06   14.25  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.77 on 434 degrees of freedom
## Multiple R-squared:  0.5617,	Adjusted R-squared:  0.5586 
## F-statistic: 185.4 on 3 and 434 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 18.5"
## [1] "N = 437"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-163.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-164.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -37.413 -13.545  -3.952   7.977 115.161 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 8.457e+01  5.204e+00  16.252   <2e-16 ***
## el          6.650e-03  8.155e-03   0.815   0.4153    
## sl          2.118e-01  9.555e-02   2.217   0.0272 *  
## I(el^2)     1.825e-07  2.858e-06   0.064   0.9491    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.51 on 433 degrees of freedom
## Multiple R-squared:  0.054,	Adjusted R-squared:  0.04745 
## F-statistic: 8.239 on 3 and 433 DF,  p-value: 2.42e-05
## 
## [1] "Point -100 19"
## [1] "N = 436"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-165.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-166.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -40.086 -15.621  -0.509  13.612  97.475 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.194e+02  7.966e+00  14.984  < 2e-16 ***
## el          -3.868e-03  7.652e-03  -0.505    0.613    
## sl           4.392e-01  9.635e-02   4.559 6.71e-06 ***
## I(el^2)     -3.297e-06  1.775e-06  -1.857    0.064 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 21.01 on 432 degrees of freedom
## Multiple R-squared:  0.3057,	Adjusted R-squared:  0.3009 
## F-statistic:  63.4 on 3 and 432 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 20"
## [1] "N = 424"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-167.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-168.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -16.5073  -7.2918   0.3714   6.0086  21.4672 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.536e+02  2.954e+01   5.200 3.13e-07 ***
## el          -9.746e-02  2.363e-02  -4.124 4.49e-05 ***
## sl          -5.368e-02  5.575e-02  -0.963    0.336    
## I(el^2)      2.370e-05  4.706e-06   5.036 7.08e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.261 on 420 degrees of freedom
## Multiple R-squared:  0.3178,	Adjusted R-squared:  0.3129 
## F-statistic: 65.22 on 3 and 420 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 20.5"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-169.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-170.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -13.4378  -3.5535  -0.2274   2.9141  30.4254 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)  
## (Intercept)  3.750e+01  1.561e+01   2.402   0.0167 *
## el          -8.899e-03  1.438e-02  -0.619   0.5363  
## sl          -7.729e-02  3.470e-02  -2.227   0.0264 *
## I(el^2)      5.825e-06  3.285e-06   1.773   0.0769 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.42 on 430 degrees of freedom
## Multiple R-squared:  0.3783,	Adjusted R-squared:  0.374 
## F-statistic: 87.23 on 3 and 430 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 21"
## [1] "N = 445"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-171.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-172.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -47.441  -6.045  -2.135   3.868  49.953 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.239e+01  1.079e+01   8.566  < 2e-16 ***
## el          -8.032e-02  1.060e-02  -7.577 2.10e-13 ***
## sl           3.170e-01  4.764e-02   6.654 8.48e-11 ***
## I(el^2)      2.698e-05  2.621e-06  10.293  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.68 on 441 degrees of freedom
## Multiple R-squared:  0.5204,	Adjusted R-squared:  0.5172 
## F-statistic: 159.5 on 3 and 441 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 21.5"
## [1] "N = 454"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-173.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-174.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -24.624  -7.870  -2.075   6.100  35.647 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.474e+02  5.537e+00  26.631   <2e-16 ***
## el          -1.317e-01  7.414e-03 -17.764   <2e-16 ***
## sl           4.966e-02  4.644e-02   1.069    0.285    
## I(el^2)      4.052e-05  2.301e-06  17.608   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.25 on 450 degrees of freedom
## Multiple R-squared:  0.4167,	Adjusted R-squared:  0.4128 
## F-statistic: 107.1 on 3 and 450 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 23"
## [1] "N = 469"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-175.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-176.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.213  -4.688  -1.520   2.895  25.666 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.402e+01  5.436e+00  15.457  < 2e-16 ***
## el          -6.758e-02  6.955e-03  -9.717  < 2e-16 ***
## sl           1.616e-01  3.790e-02   4.264 2.43e-05 ***
## I(el^2)      2.222e-05  2.111e-06  10.525  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.18 on 465 degrees of freedom
## Multiple R-squared:  0.2565,	Adjusted R-squared:  0.2517 
## F-statistic: 53.47 on 3 and 465 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 23.5"
## [1] "N = 445"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-177.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-178.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -24.076  -4.415  -0.925   3.459  33.121 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.686e+01  6.565e+00   7.138 3.92e-12 ***
## el          -3.125e-02  6.606e-03  -4.730 3.03e-06 ***
## sl           1.894e-01  3.582e-02   5.288 1.95e-07 ***
## I(el^2)      1.396e-05  1.608e-06   8.682  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.543 on 441 degrees of freedom
## Multiple R-squared:  0.6904,	Adjusted R-squared:  0.6883 
## F-statistic: 327.9 on 3 and 441 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 24"
## [1] "N = 233"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-179.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-180.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -28.205  -8.242  -1.819   4.466  63.749 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.420e+01  3.655e+00  14.831  < 2e-16 ***
## el          -4.020e-02  4.182e-03  -9.612  < 2e-16 ***
## sl           7.052e-01  1.056e-01   6.681 1.78e-10 ***
## I(el^2)      1.551e-05  1.344e-06  11.542  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.2 on 229 degrees of freedom
## Multiple R-squared:  0.5548,	Adjusted R-squared:  0.5489 
## F-statistic: 95.11 on 3 and 229 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 17"
## [1] "N = 351"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-181.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-182.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -68.792 -10.942   1.419  13.406  90.228 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.832e+01  2.193e+00  44.831   <2e-16 ***
## el           9.270e-02  6.851e-03  13.531   <2e-16 ***
## sl           2.315e-01  1.404e-01   1.649      0.1    
## I(el^2)     -4.262e-05  3.630e-06 -11.742   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 21.38 on 347 degrees of freedom
## Multiple R-squared:  0.4566,	Adjusted R-squared:  0.452 
## F-statistic: 97.21 on 3 and 347 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 17.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-183.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-184.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -72.863 -15.970  -0.169  16.860  70.299 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.928e+02  7.083e+00  27.219  < 2e-16 ***
## el          -1.465e-01  1.056e-02 -13.875  < 2e-16 ***
## sl           4.118e-01  1.389e-01   2.964  0.00321 ** 
## I(el^2)      4.977e-05  3.792e-06  13.124  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27.61 on 407 degrees of freedom
## Multiple R-squared:  0.3266,	Adjusted R-squared:  0.3216 
## F-statistic:  65.8 on 3 and 407 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 18"
## [1] "N = 444"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-185.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-186.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -26.134  -6.424   1.298   6.094  20.748 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.867e+01  3.231e+00  30.538   <2e-16 ***
## el          -6.759e-02  5.553e-03 -12.172   <2e-16 ***
## sl           4.916e-02  4.232e-02   1.162    0.246    
## I(el^2)      3.488e-05  2.221e-06  15.705   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.529 on 440 degrees of freedom
## Multiple R-squared:  0.5613,	Adjusted R-squared:  0.5583 
## F-statistic: 187.6 on 3 and 440 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 18.5"
## [1] "N = 437"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-187.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-188.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -24.716  -7.203   0.470   6.337  32.499 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.977e+01  4.830e+00  12.376   <2e-16 ***
## el           9.253e-03  6.961e-03   1.329    0.184    
## sl          -4.634e-02  4.989e-02  -0.929    0.354    
## I(el^2)      1.880e-06  2.320e-06   0.811    0.418    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.25 on 433 degrees of freedom
## Multiple R-squared:  0.277,	Adjusted R-squared:  0.272 
## F-statistic: 55.29 on 3 and 433 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 19"
## [1] "N = 403"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-189.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-190.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -25.493  -9.796  -1.610   6.941  47.628 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.344e+01  6.271e+00   8.521 3.28e-16 ***
## el           1.467e-02  5.927e-03   2.475   0.0137 *  
## sl           3.275e-01  7.036e-02   4.655 4.42e-06 ***
## I(el^2)     -2.373e-06  1.319e-06  -1.798   0.0729 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.59 on 399 degrees of freedom
## Multiple R-squared:  0.1006,	Adjusted R-squared:  0.09381 
## F-statistic: 14.87 on 3 and 399 DF,  p-value: 3.391e-09
## 
## [1] "Point -99.5 20"
## [1] "N = 396"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-191.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-192.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.5435  -5.3593  -0.0507   4.2228  28.1912 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.501e+02  1.986e+01  -7.557 2.93e-13 ***
## el           1.380e-01  1.585e-02   8.710  < 2e-16 ***
## sl           8.256e-02  5.708e-02   1.446    0.149    
## I(el^2)     -2.203e-05  3.139e-06  -7.018 9.93e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.59 on 392 degrees of freedom
## Multiple R-squared:  0.5474,	Adjusted R-squared:  0.544 
## F-statistic: 158.1 on 3 and 392 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 20.5"
## [1] "N = 425"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-193.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-194.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -29.837  -6.401  -1.452   3.454  55.507 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.378e+02  1.593e+01   8.648  < 2e-16 ***
## el          -1.212e-01  1.507e-02  -8.044 8.95e-15 ***
## sl           3.954e-01  4.568e-02   8.655  < 2e-16 ***
## I(el^2)      3.491e-05  3.575e-06   9.765  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.958 on 421 degrees of freedom
## Multiple R-squared:  0.509,	Adjusted R-squared:  0.5055 
## F-statistic: 145.5 on 3 and 421 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 21"
## [1] "N = 466"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-195.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-196.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -52.990 -18.871  -9.141  11.658  93.335 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.429e+02  1.159e+01   20.96  < 2e-16 ***
## el          -2.038e-01  1.355e-02  -15.04  < 2e-16 ***
## sl           3.926e-01  1.033e-01    3.80 0.000164 ***
## I(el^2)      5.318e-05  3.967e-06   13.41  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 26.65 on 462 degrees of freedom
## Multiple R-squared:  0.3961,	Adjusted R-squared:  0.3922 
## F-statistic:   101 on 3 and 462 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 21.5"
## [1] "N = 458"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-197.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-198.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -62.96 -25.19 -11.00  14.02 145.03 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.930e+02  8.480e+00  22.756   <2e-16 ***
## el          -1.610e-01  1.315e-02 -12.244   <2e-16 ***
## sl           2.595e-01  1.544e-01   1.681   0.0934 .  
## I(el^2)      5.142e-05  4.758e-06  10.808   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 36.68 on 454 degrees of freedom
## Multiple R-squared:  0.2668,	Adjusted R-squared:  0.2619 
## F-statistic: 55.06 on 3 and 454 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 22"
## [1] "N = 474"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-199.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-200.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -42.779 -17.290  -2.151  17.314  61.206 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.509e+02  3.824e+00  39.456  < 2e-16 ***
## el          -1.322e-01  1.020e-02 -12.962  < 2e-16 ***
## sl           8.210e-01  1.113e-01   7.376 7.44e-13 ***
## I(el^2)      3.756e-05  6.206e-06   6.053 2.91e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 21.59 on 470 degrees of freedom
## Multiple R-squared:  0.6723,	Adjusted R-squared:  0.6702 
## F-statistic: 321.4 on 3 and 470 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 22.5"
## [1] "N = 430"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-201.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-202.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -36.170 -13.091  -2.754  11.048  64.347 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.087e+02  3.190e+00  34.085  < 2e-16 ***
## el          -6.871e-02  8.173e-03  -8.407 6.39e-16 ***
## sl           8.038e-01  9.604e-02   8.370 8.35e-16 ***
## I(el^2)      1.559e-05  4.657e-06   3.347  0.00089 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.85 on 426 degrees of freedom
## Multiple R-squared:  0.5396,	Adjusted R-squared:  0.5363 
## F-statistic: 166.4 on 3 and 426 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 23"
## [1] "N = 456"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-203.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-204.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -27.021 -11.585  -1.725   9.223  43.167 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.208e+02  2.118e+00  57.050  < 2e-16 ***
## el          -9.719e-02  3.970e-03 -24.483  < 2e-16 ***
## sl           4.143e-01  6.332e-02   6.543 1.64e-10 ***
## I(el^2)      2.880e-05  1.728e-06  16.669  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.06 on 452 degrees of freedom
## Multiple R-squared:  0.6965,	Adjusted R-squared:  0.6945 
## F-statistic: 345.7 on 3 and 452 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 23.5"
## [1] "N = 455"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-205.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-206.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -28.394  -8.868  -1.832   7.150  55.006 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.835e+01  2.956e+00  26.506  < 2e-16 ***
## el          -5.075e-02  4.027e-03 -12.602  < 2e-16 ***
## sl           2.985e-01  5.102e-02   5.851 9.41e-09 ***
## I(el^2)      1.775e-05  1.240e-06  14.319  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.21 on 451 degrees of freedom
## Multiple R-squared:  0.3723,	Adjusted R-squared:  0.3682 
## F-statistic: 89.18 on 3 and 451 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 24"
## [1] "N = 224"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-207.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-208.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -25.155  -9.624  -1.301   7.882  53.747 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.904e+01  2.404e+00  32.875  < 2e-16 ***
## el          -2.929e-02  4.879e-03  -6.003 7.90e-09 ***
## sl           7.393e-02  9.535e-02   0.775    0.439    
## I(el^2)      1.131e-05  1.514e-06   7.471 1.85e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.59 on 220 degrees of freedom
## Multiple R-squared:  0.2682,	Adjusted R-squared:  0.2583 
## F-statistic: 26.88 on 3 and 220 DF,  p-value: 7.484e-15
## 
## [1] "Point -99 17"
## [1] "N = 385"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-209.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-210.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -84.811 -14.036   0.195  15.552  73.528 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.732e+01  2.787e+00  34.919   <2e-16 ***
## el           1.561e-01  7.331e-03  21.295   <2e-16 ***
## sl          -2.902e-01  1.650e-01  -1.759   0.0793 .  
## I(el^2)     -6.612e-05  3.029e-06 -21.830   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.29 on 381 degrees of freedom
## Multiple R-squared:  0.6027,	Adjusted R-squared:  0.5995 
## F-statistic: 192.6 on 3 and 381 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99 17.5"
## [1] "N = 391"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-211.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-212.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -68.255 -20.932  -4.265  13.774 111.398 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.994e+02  1.356e+01  14.706  < 2e-16 ***
## el          -1.545e-01  1.860e-02  -8.307 1.68e-15 ***
## sl           7.977e-01  1.783e-01   4.474 1.01e-05 ***
## I(el^2)      4.654e-05  6.304e-06   7.383 9.59e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 32.9 on 387 degrees of freedom
## Multiple R-squared:  0.2083,	Adjusted R-squared:  0.2021 
## F-statistic: 33.93 on 3 and 387 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99 18"
## [1] "N = 440"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-213.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-214.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -19.2774  -5.3318   0.0963   5.0415  20.9624 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.364e+01  4.423e+00  21.171   <2e-16 ***
## el          -6.768e-02  7.288e-03  -9.286   <2e-16 ***
## sl          -5.504e-02  4.128e-02  -1.333    0.183    
## I(el^2)      3.303e-05  2.888e-06  11.436   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.183 on 436 degrees of freedom
## Multiple R-squared:  0.3891,	Adjusted R-squared:  0.3849 
## F-statistic: 92.57 on 3 and 436 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99 19"
## [1] "N = 398"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-215.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-216.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -25.924  -8.581  -0.062   8.249  31.650 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.612e+01  5.011e+00  15.189  < 2e-16 ***
## el          -1.605e-02  4.657e-03  -3.447 0.000628 ***
## sl           2.006e-01  7.436e-02   2.697 0.007287 ** 
## I(el^2)      4.967e-06  1.026e-06   4.839 1.87e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.42 on 394 degrees of freedom
## Multiple R-squared:  0.2103,	Adjusted R-squared:  0.2043 
## F-statistic: 34.97 on 3 and 394 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99 20.5"
## [1] "N = 455"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-217.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-218.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -53.972 -19.282 -10.479   9.166 128.474 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.558e+02  2.457e+01   6.340 5.57e-10 ***
## el          -1.080e-01  2.609e-02  -4.140 4.15e-05 ***
## sl           6.055e-01  1.213e-01   4.993 8.52e-07 ***
## I(el^2)      2.721e-05  6.955e-06   3.913 0.000105 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.31 on 451 degrees of freedom
## Multiple R-squared:  0.1033,	Adjusted R-squared:  0.09732 
## F-statistic: 17.32 on 3 and 451 DF,  p-value: 1.171e-10
## 
## [1] "Point -99 21"
## [1] "N = 443"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-219.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-220.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -88.580 -32.370  -2.197  31.117 134.392 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.217e+02  7.429e+00  29.840  < 2e-16 ***
## el          -1.084e-01  1.254e-02  -8.644  < 2e-16 ***
## sl           1.816e-01  1.638e-01   1.109    0.268    
## I(el^2)      2.019e-05  4.807e-06   4.200 3.23e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 41.08 on 439 degrees of freedom
## Multiple R-squared:  0.4375,	Adjusted R-squared:  0.4337 
## F-statistic: 113.8 on 3 and 439 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99 21.5"
## [1] "N = 444"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-221.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-222.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -73.727 -26.101  -3.277  22.233  81.041 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.363e+02  3.077e+00  44.312  < 2e-16 ***
## el           2.264e-02  9.981e-03   2.268 0.023786 *  
## sl           6.456e-01  1.730e-01   3.732 0.000215 ***
## I(el^2)     -1.448e-05  4.918e-06  -2.945 0.003403 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34.28 on 440 degrees of freedom
## Multiple R-squared:  0.07274,	Adjusted R-squared:  0.06641 
## F-statistic:  11.5 on 3 and 440 DF,  p-value: 2.831e-07
## 
## [1] "Point -99 23.5"
## [1] "N = 451"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-223.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-224.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -29.103  -7.727  -0.421   7.557  44.424 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.372e+01  1.464e+00  50.348  < 2e-16 ***
## el          -4.790e-02  5.380e-03  -8.904  < 2e-16 ***
## sl           2.463e-01  6.775e-02   3.635  0.00031 ***
## I(el^2)      2.208e-05  2.760e-06   7.997  1.1e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.31 on 447 degrees of freedom
## Multiple R-squared:  0.1652,	Adjusted R-squared:  0.1596 
## F-statistic: 29.48 on 3 and 447 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 17"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-225.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-226.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -103.428  -22.007   -1.096   23.752   69.380 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.240e+02  4.807e+00  25.790   <2e-16 ***
## el           9.827e-02  8.771e-03  11.203   <2e-16 ***
## sl          -8.767e-02  1.830e-01  -0.479    0.632    
## I(el^2)     -4.794e-05  3.282e-06 -14.606   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.44 on 396 degrees of freedom
## Multiple R-squared:  0.448,	Adjusted R-squared:  0.4439 
## F-statistic: 107.2 on 3 and 396 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 17.5"
## [1] "N = 420"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-227.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-228.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -54.05 -20.68 -10.04  10.83 109.81 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.401e+02  2.499e+01   5.605 3.80e-08 ***
## el          -9.341e-02  2.881e-02  -3.242 0.001282 ** 
## sl           7.270e-01  1.663e-01   4.372 1.55e-05 ***
## I(el^2)      2.893e-05  8.117e-06   3.564 0.000407 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.94 on 416 degrees of freedom
## Multiple R-squared:  0.08226,	Adjusted R-squared:  0.07564 
## F-statistic: 12.43 on 3 and 416 DF,  p-value: 8.456e-08
## 
## [1] "Point -98.5 18"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-229.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-230.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -21.9965  -5.2635  -0.5577   4.8312  20.9221 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.177e+02  7.417e+00  15.869  < 2e-16 ***
## el          -8.297e-02  1.134e-02  -7.313 1.29e-12 ***
## sl          -4.535e-02  3.707e-02  -1.223    0.222    
## I(el^2)      2.834e-05  4.274e-06   6.631 1.00e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.172 on 430 degrees of freedom
## Multiple R-squared:  0.1595,	Adjusted R-squared:  0.1536 
## F-statistic:  27.2 on 3 and 430 DF,  p-value: 3.993e-16
## 
## [1] "Point -98.5 18.5"
## [1] "N = 408"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-231.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-232.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.5055  -4.7647  -0.4454   3.4975  26.6190 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.181e+02  5.342e+00  22.107   <2e-16 ***
## el          -8.718e-02  7.243e-03 -12.036   <2e-16 ***
## sl          -6.717e-02  3.716e-02  -1.807   0.0714 .  
## I(el^2)      3.178e-05  2.373e-06  13.389   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.085 on 404 degrees of freedom
## Multiple R-squared:  0.3749,	Adjusted R-squared:  0.3703 
## F-statistic: 80.78 on 3 and 404 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 19"
## [1] "N = 384"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-233.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-234.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -26.3194  -4.8222  -0.9562   3.7818  30.8711 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.950e+01  5.921e+00   8.360  1.2e-15 ***
## el           7.933e-03  5.024e-03   1.579    0.115    
## sl           1.881e-01  6.808e-02   2.763    0.006 ** 
## I(el^2)     -1.356e-07  1.049e-06  -0.129    0.897    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.09 on 380 degrees of freedom
## Multiple R-squared:  0.1886,	Adjusted R-squared:  0.1822 
## F-statistic: 29.45 on 3 and 380 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 20"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-235.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-236.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -76.794 -13.180  -5.029   7.125 118.338 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.396e+02  3.863e+01   8.792  < 2e-16 ***
## el          -2.070e-01  3.357e-02  -6.166 1.68e-09 ***
## sl           1.286e+00  1.578e-01   8.147 4.52e-15 ***
## I(el^2)      3.571e-05  7.333e-06   4.870 1.60e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.25 on 411 degrees of freedom
## Multiple R-squared:  0.3893,	Adjusted R-squared:  0.3849 
## F-statistic: 87.35 on 3 and 411 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 20.5"
## [1] "N = 465"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-237.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-238.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -124.587  -28.967   -4.461   32.098  128.346 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.869e+02  1.023e+01  18.281  < 2e-16 ***
## el          -6.457e-02  1.425e-02  -4.532 7.44e-06 ***
## sl           1.068e+00  1.868e-01   5.716 1.96e-08 ***
## I(el^2)      5.653e-06  4.789e-06   1.180    0.238    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 46.27 on 461 degrees of freedom
## Multiple R-squared:  0.3998,	Adjusted R-squared:  0.3959 
## F-statistic: 102.4 on 3 and 461 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 21"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-239.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-240.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -122.809  -20.557   -2.216   20.729   80.601 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.163e+02  3.221e+00  36.100   <2e-16 ***
## el           9.982e-02  1.050e-02   9.503   <2e-16 ***
## sl           1.347e-01  1.582e-01   0.851    0.395    
## I(el^2)     -4.755e-05  4.973e-06  -9.563   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 30.88 on 430 degrees of freedom
## Multiple R-squared:  0.2424,	Adjusted R-squared:  0.2371 
## F-statistic: 45.86 on 3 and 430 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98 16.5"
## [1] "N = 362"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-241.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-242.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -65.08 -19.60   0.70  20.23  66.89 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.691e+02  3.275e+00  51.632  < 2e-16 ***
## el           2.768e-02  1.151e-02   2.404   0.0167 *  
## sl          -3.251e-01  1.844e-01  -1.763   0.0788 .  
## I(el^2)     -3.005e-05  7.577e-06  -3.966 8.82e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27.72 on 358 degrees of freedom
## Multiple R-squared:  0.09394,	Adjusted R-squared:  0.08635 
## F-statistic: 12.37 on 3 and 358 DF,  p-value: 1.019e-07
## 
## [1] "Point -98 17"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-243.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-244.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -67.770 -20.266  -3.217  16.472  81.832 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.933e+02  5.373e+00  35.981  < 2e-16 ***
## el          -5.492e-02  8.418e-03  -6.523 2.04e-10 ***
## sl           3.315e-01  1.549e-01   2.139    0.033 *  
## I(el^2)      2.441e-06  2.726e-06   0.895    0.371    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27.13 on 407 degrees of freedom
## Multiple R-squared:  0.6514,	Adjusted R-squared:  0.6489 
## F-statistic: 253.5 on 3 and 407 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98 17.5"
## [1] "N = 422"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-245.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-246.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -41.543 -14.788  -3.906   8.752  90.788 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.675e+02  1.754e+01   9.551  < 2e-16 ***
## el          -1.123e-01  1.927e-02  -5.826 1.14e-08 ***
## sl           5.242e-01  1.153e-01   4.549 7.09e-06 ***
## I(el^2)      2.786e-05  5.239e-06   5.319 1.70e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 22.26 on 418 degrees of freedom
## Multiple R-squared:  0.1375,	Adjusted R-squared:  0.1313 
## F-statistic: 22.22 on 3 and 418 DF,  p-value: 2.285e-13
## 
## [1] "Point -98 18"
## [1] "N = 426"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-247.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-248.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.2668  -3.8860   0.2502   5.2853  18.3775 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.147e+02  6.520e+00  17.596  < 2e-16 ***
## el          -6.359e-02  7.887e-03  -8.062 7.82e-15 ***
## sl          -1.568e-02  3.887e-02  -0.404    0.687    
## I(el^2)      1.501e-05  2.347e-06   6.394 4.29e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.281 on 422 degrees of freedom
## Multiple R-squared:  0.3377,	Adjusted R-squared:  0.333 
## F-statistic: 71.72 on 3 and 422 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98 18.5"
## [1] "N = 413"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-249.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-250.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.763  -4.678   1.199   6.243  21.680 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.048e+02  1.048e+01   9.998  < 2e-16 ***
## el          -5.437e-02  1.300e-02  -4.181 3.55e-05 ***
## sl          -4.236e-02  5.168e-02  -0.820 0.412925    
## I(el^2)      1.344e-05  3.910e-06   3.437 0.000648 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.62 on 409 degrees of freedom
## Multiple R-squared:  0.156,	Adjusted R-squared:  0.1498 
## F-statistic:  25.2 on 3 and 409 DF,  p-value: 5.601e-15
## 
## [1] "Point -98 19"
## [1] "N = 392"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-251.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-252.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -26.0307  -8.3121   0.9111   9.7076  25.7766 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.063e+02  1.257e+01   8.456 5.70e-16 ***
## el          -5.054e-02  1.086e-02  -4.654 4.48e-06 ***
## sl           1.708e-01  9.343e-02   1.828   0.0683 .  
## I(el^2)      1.185e-05  2.374e-06   4.991 9.11e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.21 on 388 degrees of freedom
## Multiple R-squared:  0.08852,	Adjusted R-squared:  0.08147 
## F-statistic: 12.56 on 3 and 388 DF,  p-value: 7.46e-08
## 
## [1] "Point -98 20"
## [1] "N = 452"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-253.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-254.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -88.552 -32.710  -4.746  25.763 160.350 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.644e+02  1.026e+01  25.769  < 2e-16 ***
## el          -1.356e-02  1.344e-02  -1.009    0.313    
## sl           7.824e-01  1.946e-01   4.020 6.82e-05 ***
## I(el^2)     -2.554e-05  3.976e-06  -6.424 3.40e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 45.27 on 448 degrees of freedom
## Multiple R-squared:  0.7766,	Adjusted R-squared:  0.7752 
## F-statistic: 519.3 on 3 and 448 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98 20.5"
## [1] "N = 466"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-255.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-256.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -113.273  -26.979   -3.224   24.386  109.840 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.028e+02  4.009e+00  25.644   <2e-16 ***
## el           2.200e-01  1.124e-02  19.578   <2e-16 ***
## sl           1.484e-01  1.880e-01   0.789     0.43    
## I(el^2)     -9.723e-05  4.800e-06 -20.254   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 37.88 on 462 degrees of freedom
## Multiple R-squared:  0.5373,	Adjusted R-squared:  0.5343 
## F-statistic: 178.9 on 3 and 462 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 16"
## [1] "N = 228"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-257.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-258.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -69.354  -9.044  -0.245   9.389  58.969 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.526e+02  3.124e+00  48.846  < 2e-16 ***
## el           1.856e-02  9.383e-03   1.978   0.0492 *  
## sl          -3.687e-01  1.919e-01  -1.922   0.0559 .  
## I(el^2)     -2.768e-05  4.858e-06  -5.698 3.79e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 23.06 on 224 degrees of freedom
## Multiple R-squared:  0.4119,	Adjusted R-squared:  0.404 
## F-statistic: 52.29 on 3 and 224 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 16.5"
## [1] "N = 421"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-259.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-260.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -51.109 -16.672  -3.194  13.845  64.214 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.842e+02  3.960e+00  46.532  < 2e-16 ***
## el          -4.376e-02  6.608e-03  -6.622 1.09e-10 ***
## sl          -3.569e-01  1.136e-01  -3.143  0.00179 ** 
## I(el^2)     -5.596e-07  2.441e-06  -0.229  0.81881    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 22.4 on 417 degrees of freedom
## Multiple R-squared:  0.6515,	Adjusted R-squared:  0.649 
## F-statistic: 259.9 on 3 and 417 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 17"
## [1] "N = 414"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-261.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-262.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -37.229 -17.101  -6.079  12.662  67.071 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.063e+02  8.409e+00  24.528  < 2e-16 ***
## el          -1.145e-01  9.814e-03 -11.665  < 2e-16 ***
## sl           4.093e-01  1.109e-01   3.692 0.000253 ***
## I(el^2)      2.147e-05  2.791e-06   7.691  1.1e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 22.16 on 410 degrees of freedom
## Multiple R-squared:  0.5629,	Adjusted R-squared:  0.5597 
## F-statistic:   176 on 3 and 410 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 18"
## [1] "N = 412"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-263.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-264.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.772  -7.952  -0.990   6.094  75.291 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  1.088e+01  7.012e+00   1.551  0.12163   
## el           2.517e-02  8.254e-03   3.049  0.00245 **
## sl           4.674e-02  5.948e-02   0.786  0.43242   
## I(el^2)     -4.091e-06  2.351e-06  -1.740  0.08262 . 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.28 on 408 degrees of freedom
## Multiple R-squared:  0.1722,	Adjusted R-squared:  0.1661 
## F-statistic: 28.29 on 3 and 408 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 18.5"
## [1] "N = 424"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-265.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-266.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -35.922 -13.976  -5.157   7.578 111.692 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.815e-01  1.843e+01   0.015    0.988    
## el           2.774e-02  1.986e-02   1.397    0.163    
## sl           5.747e-01  1.101e-01   5.222 2.79e-07 ***
## I(el^2)     -3.474e-06  5.288e-06  -0.657    0.512    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 22.34 on 420 degrees of freedom
## Multiple R-squared:  0.1449,	Adjusted R-squared:  0.1388 
## F-statistic: 23.72 on 3 and 420 DF,  p-value: 3.359e-14
## 
## [1] "Point -97.5 19"
## [1] "N = 408"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-267.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-268.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -58.555 -11.817  -1.587  10.222  76.750 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.383e+02  1.593e+01   27.51   <2e-16 ***
## el          -3.156e-01  1.246e-02  -25.32   <2e-16 ***
## sl           1.297e+00  9.712e-02   13.35   <2e-16 ***
## I(el^2)      6.061e-05  2.432e-06   24.92   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.8 on 404 degrees of freedom
## Multiple R-squared:  0.678,	Adjusted R-squared:  0.6756 
## F-statistic: 283.5 on 3 and 404 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 20"
## [1] "N = 446"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-269.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-270.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -133.885  -24.327   -4.986   23.258  146.517 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.664e+02  4.757e+00  34.973   <2e-16 ***
## el           1.860e-01  1.042e-02  17.847   <2e-16 ***
## sl          -7.161e-02  2.202e-01  -0.325    0.745    
## I(el^2)     -8.661e-05  3.763e-06 -23.018   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 47.05 on 442 degrees of freedom
## Multiple R-squared:  0.6925,	Adjusted R-squared:  0.6904 
## F-statistic: 331.8 on 3 and 442 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 16"
## [1] "N = 331"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-271.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-272.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -48.002 -23.225  -3.031  22.415 107.777 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.828e+01  4.126e+00  21.398  < 2e-16 ***
## el           5.080e-02  9.008e-03   5.639  3.7e-08 ***
## sl           2.310e-01  1.966e-01   1.175    0.241    
## I(el^2)     -3.374e-05  3.885e-06  -8.685  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.63 on 327 degrees of freedom
## Multiple R-squared:  0.3169,	Adjusted R-squared:  0.3106 
## F-statistic: 50.56 on 3 and 327 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 16.5"
## [1] "N = 466"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-273.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-274.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -64.622 -17.254  -4.354  10.090 104.138 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.374e+02  1.056e+01  22.485  < 2e-16 ***
## el          -1.850e-01  1.249e-02 -14.813  < 2e-16 ***
## sl           7.681e-01  1.195e-01   6.428 3.22e-10 ***
## I(el^2)      4.248e-05  3.646e-06  11.651  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 24.82 on 462 degrees of freedom
## Multiple R-squared:  0.5052,	Adjusted R-squared:  0.5019 
## F-statistic: 157.2 on 3 and 462 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 17"
## [1] "N = 432"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-275.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-276.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -27.402  -9.808  -2.176   6.467  59.812 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.917e+02  1.710e+01  11.210   <2e-16 ***
## el          -1.553e-01  1.706e-02  -9.107   <2e-16 ***
## sl           6.200e-01  7.189e-02   8.625   <2e-16 ***
## I(el^2)      3.913e-05  4.102e-06   9.537   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.5 on 428 degrees of freedom
## Multiple R-squared:  0.3064,	Adjusted R-squared:  0.3015 
## F-statistic: 63.01 on 3 and 428 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 17.5"
## [1] "N = 416"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-277.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-278.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -59.965 -19.578  -9.007   6.898 134.151 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.605e+01  1.885e+01   4.034 6.53e-05 ***
## el          -4.509e-02  2.091e-02  -2.156   0.0316 *  
## sl           1.037e+00  1.538e-01   6.744 5.22e-11 ***
## I(el^2)      1.421e-05  5.743e-06   2.474   0.0138 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 32.29 on 412 degrees of freedom
## Multiple R-squared:  0.1133,	Adjusted R-squared:  0.1068 
## F-statistic: 17.54 on 3 and 412 DF,  p-value: 9.772e-11
## 
## [1] "Point -97 18"
## [1] "N = 417"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-279.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-280.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -199.44  -66.72  -22.39   37.18  403.01 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.582e+02  2.613e+01   9.881  < 2e-16 ***
## el          -2.191e-01  3.621e-02  -6.051 3.23e-09 ***
## sl           2.722e+00  4.314e-01   6.310 7.20e-10 ***
## I(el^2)      5.426e-05  1.175e-05   4.617 5.20e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 100.7 on 413 degrees of freedom
## Multiple R-squared:  0.1991,	Adjusted R-squared:  0.1933 
## F-statistic: 34.23 on 3 and 413 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 18.5"
## [1] "N = 414"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-281.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-282.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -158.871  -53.751   -0.516   46.664  297.497 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.104e+02  1.121e+01  18.762  < 2e-16 ***
## el          -1.013e-01  1.848e-02  -5.481 7.40e-08 ***
## sl           2.426e+00  3.426e-01   7.082 6.22e-12 ***
## I(el^2)      1.270e-05  6.600e-06   1.924    0.055 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 77.28 on 410 degrees of freedom
## Multiple R-squared:  0.2956,	Adjusted R-squared:  0.2905 
## F-statistic: 57.36 on 3 and 410 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 19"
## [1] "N = 428"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-283.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-284.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -98.393 -41.653  -7.082  37.768 155.201 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.222e+02  6.906e+00  17.700  < 2e-16 ***
## el           1.045e-02  9.782e-03   1.068 0.286190    
## sl           9.443e-01  2.316e-01   4.077 5.45e-05 ***
## I(el^2)     -9.112e-06  2.726e-06  -3.343 0.000904 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 50.96 on 424 degrees of freedom
## Multiple R-squared:  0.1563,	Adjusted R-squared:  0.1503 
## F-statistic: 26.17 on 3 and 424 DF,  p-value: 1.5e-15
## 
## [1] "Point -97 19.5"
## [1] "N = 425"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-285.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-286.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -91.617 -46.045  -7.709  29.678 213.287 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.984e+01  1.035e+01   5.780 1.46e-08 ***
## el           8.958e-02  1.415e-02   6.330 6.28e-10 ***
## sl           1.427e+00  2.495e-01   5.719 2.03e-08 ***
## I(el^2)     -3.275e-05  4.156e-06  -7.882 2.79e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 57.12 on 421 degrees of freedom
## Multiple R-squared:  0.2604,	Adjusted R-squared:  0.2552 
## F-statistic: 49.42 on 3 and 421 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 20"
## [1] "N = 393"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-287.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-288.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -121.678  -25.418   -3.233   21.202  181.878 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.782e+02  3.685e+00  48.361   <2e-16 ***
## el           9.400e-02  1.057e-02   8.894   <2e-16 ***
## sl          -2.035e-01  2.244e-01  -0.907    0.365    
## I(el^2)     -4.960e-05  4.401e-06 -11.271   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 42.57 on 389 degrees of freedom
## Multiple R-squared:  0.3204,	Adjusted R-squared:  0.3152 
## F-statistic: 61.14 on 3 and 389 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 16"
## [1] "N = 423"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-289.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-290.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -74.877 -21.620  -5.722  17.889 153.914 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.962e+01  4.053e+00  17.179  < 2e-16 ***
## el          -5.084e-03  7.327e-03  -0.694    0.488    
## sl           1.529e+00  1.819e-01   8.408 6.57e-16 ***
## I(el^2)     -1.704e-06  2.660e-06  -0.640    0.522    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 36.04 on 419 degrees of freedom
## Multiple R-squared:  0.1537,	Adjusted R-squared:  0.1476 
## F-statistic: 25.36 on 3 and 419 DF,  p-value: 4.287e-15
## 
## [1] "Point -96.5 16.5"
## [1] "N = 422"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-291.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-292.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -24.552  -6.649  -1.340   5.166  88.645 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.205e+02  8.041e+00  14.990  < 2e-16 ***
## el          -1.124e-01  8.487e-03 -13.241  < 2e-16 ***
## sl           4.530e-01  5.589e-02   8.105 5.88e-15 ***
## I(el^2)      3.614e-05  2.170e-06  16.654  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.55 on 418 degrees of freedom
## Multiple R-squared:  0.6719,	Adjusted R-squared:  0.6695 
## F-statistic: 285.3 on 3 and 418 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 17"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-293.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-294.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -68.563 -17.450  -7.809   8.476 122.628 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.387e+02  2.433e+01   5.701 2.21e-08 ***
## el          -1.295e-01  2.475e-02  -5.235 2.59e-07 ***
## sl           1.012e+00  1.442e-01   7.015 8.96e-12 ***
## I(el^2)      4.100e-05  6.066e-06   6.760 4.52e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 30.79 on 430 degrees of freedom
## Multiple R-squared:  0.3616,	Adjusted R-squared:  0.3572 
## F-statistic: 81.19 on 3 and 430 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 17.5"
## [1] "N = 404"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-295.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-296.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -164.797  -44.480   -7.981   41.751  181.021 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.160e+02  1.278e+01  40.379  < 2e-16 ***
## el          -4.082e-01  1.651e-02 -24.724  < 2e-16 ***
## sl           1.045e+00  2.973e-01   3.515 0.000491 ***
## I(el^2)      9.447e-05  5.161e-06  18.306  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 60.28 on 400 degrees of freedom
## Multiple R-squared:  0.737,	Adjusted R-squared:  0.735 
## F-statistic: 373.6 on 3 and 400 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 18"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-297.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-298.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -205.265  -70.015   -5.586   75.104  311.455 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.728e+02  8.340e+00  32.716  < 2e-16 ***
## el           3.697e-02  2.213e-02   1.670   0.0956 .  
## sl           4.838e-01  4.567e-01   1.059   0.2900    
## I(el^2)     -4.338e-05  8.982e-06  -4.830 1.96e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 89.56 on 396 degrees of freedom
## Multiple R-squared:  0.2331,	Adjusted R-squared:  0.2273 
## F-statistic: 40.12 on 3 and 396 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 19.5"
## [1] "N = 285"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-299.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-300.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -42.498 -20.319  -5.238  12.221 103.960 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 6.972e+01  3.149e+00  22.139   <2e-16 ***
## el          1.964e-02  1.011e-02   1.943   0.0531 .  
## sl          3.197e-01  1.739e-01   1.839   0.0670 .  
## I(el^2)     1.353e-05  5.785e-06   2.339   0.0200 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27.66 on 281 degrees of freedom
## Multiple R-squared:  0.3923,	Adjusted R-squared:  0.3858 
## F-statistic: 60.45 on 3 and 281 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 20"
## [1] "N = 173"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-301.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-302.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -93.207 -33.465   5.145  37.881  92.924 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.692e+02  6.342e+00  26.672   <2e-16 ***
## el          -5.553e-02  2.569e-02  -2.162    0.032 *  
## sl          -1.366e-01  4.169e-01  -0.328    0.744    
## I(el^2)      2.452e-05  1.335e-05   1.837    0.068 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 49.55 on 169 degrees of freedom
## Multiple R-squared:  0.05169,	Adjusted R-squared:  0.03485 
## F-statistic:  3.07 on 3 and 169 DF,  p-value: 0.02936
## 
## [1] "Point -96 16"
## [1] "N = 342"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-303.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-304.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -53.115 -23.731  -5.354  16.109 158.610 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.862e+01  4.339e+00   8.901  < 2e-16 ***
## el           5.306e-02  8.629e-03   6.149  2.2e-09 ***
## sl           3.005e-01  1.927e-01   1.560     0.12    
## I(el^2)     -1.227e-05  2.911e-06  -4.216  3.2e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.53 on 338 degrees of freedom
## Multiple R-squared:  0.2757,	Adjusted R-squared:  0.2693 
## F-statistic: 42.89 on 3 and 338 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96 16.5"
## [1] "N = 414"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-305.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-306.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -59.051 -17.692  -2.463  15.065  94.861 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.867e+01  6.310e+00   4.544 7.29e-06 ***
## el           2.313e-02  8.615e-03   2.685  0.00756 ** 
## sl           5.820e-01  1.316e-01   4.424 1.24e-05 ***
## I(el^2)     -6.718e-07  2.788e-06  -0.241  0.80969    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27.15 on 410 degrees of freedom
## Multiple R-squared:  0.2304,	Adjusted R-squared:  0.2248 
## F-statistic: 40.92 on 3 and 410 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96 17"
## [1] "N = 381"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-307.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-308.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -163.249  -58.813   -7.278   49.197  212.104 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.828e+02  1.951e+01   9.369  < 2e-16 ***
## el          -1.021e-01  2.567e-02  -3.978 8.34e-05 ***
## sl           2.259e+00  3.862e-01   5.850 1.07e-08 ***
## I(el^2)      2.169e-05  8.452e-06   2.566   0.0107 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 74.48 on 377 degrees of freedom
## Multiple R-squared:  0.1614,	Adjusted R-squared:  0.1547 
## F-statistic: 24.18 on 3 and 377 DF,  p-value: 2.484e-14
## 
## [1] "Point -96 17.5"
## [1] "N = 408"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-309.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-310.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -150.628  -55.686   -5.768   46.034  184.926 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.567e+02  7.054e+00  36.384  < 2e-16 ***
## el           2.629e-03  1.798e-02   0.146 0.883812    
## sl           1.068e+00  3.832e-01   2.786 0.005588 ** 
## I(el^2)     -2.464e-05  7.009e-06  -3.515 0.000489 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 67.37 on 404 degrees of freedom
## Multiple R-squared:  0.2465,	Adjusted R-squared:  0.2409 
## F-statistic: 44.06 on 3 and 404 DF,  p-value: < 2.2e-16
## 
## [1] "Point -95.5 16"
## [1] "N = 243"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-311.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-312.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -49.045  -9.356  -1.878   8.867  33.319 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.526e+01  1.639e+00  21.508  < 2e-16 ***
## el           5.480e-03  7.472e-03   0.733    0.464    
## sl          -5.487e-02  1.063e-01  -0.516    0.606    
## I(el^2)      3.323e-05  5.286e-06   6.286 1.53e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.96 on 239 degrees of freedom
## Multiple R-squared:  0.6717,	Adjusted R-squared:  0.6676 
## F-statistic:   163 on 3 and 239 DF,  p-value: < 2.2e-16
## 
## [1] "Point -95.5 16.5"
## [1] "N = 443"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-313.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-314.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -56.931 -22.374  -9.042  17.902 117.840 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.684e+01  3.425e+00  10.757  < 2e-16 ***
## el           6.001e-02  1.033e-02   5.808 1.21e-08 ***
## sl           9.696e-02  1.798e-01   0.539    0.590    
## I(el^2)     -3.099e-06  5.657e-06  -0.548    0.584    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 32.8 on 439 degrees of freedom
## Multiple R-squared:  0.3999,	Adjusted R-squared:  0.3958 
## F-statistic:  97.5 on 3 and 439 DF,  p-value: < 2.2e-16
## 
## [1] "Point -95.5 17"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-315.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-316.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -128.101  -29.006    7.078   31.605  130.914 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.633e+02  7.153e+00  22.823   <2e-16 ***
## el          -5.501e-03  2.287e-02  -0.240   0.8101    
## sl          -7.037e-01  3.098e-01  -2.272   0.0236 *  
## I(el^2)      2.219e-05  1.341e-05   1.655   0.0988 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 54.04 on 407 degrees of freedom
## Multiple R-squared:  0.04873,	Adjusted R-squared:  0.04171 
## F-statistic: 6.949 on 3 and 407 DF,  p-value: 0.0001432
## 
## [1] "Point -94.5 16.5"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-317.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-318.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -52.812 -16.903  -5.992   9.779 170.619 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.013e+01  2.460e+00  36.633  < 2e-16 ***
## el           1.235e-01  1.529e-02   8.077 8.07e-15 ***
## sl           2.360e-01  2.336e-01   1.010    0.313    
## I(el^2)     -6.439e-05  9.525e-06  -6.761 4.94e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.38 on 396 degrees of freedom
## Multiple R-squared:  0.3653,	Adjusted R-squared:  0.3605 
## F-statistic: 75.98 on 3 and 396 DF,  p-value: < 2.2e-16
## 
## [1] "Point -94.5 17"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-319.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-320.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -292.786  -46.948    5.008   67.238  237.771 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.139e+02  1.035e+01  40.005  < 2e-16 ***
## el          -4.506e-01  4.207e-02 -10.711  < 2e-16 ***
## sl           8.515e-01  5.929e-01   1.436    0.152    
## I(el^2)      1.737e-04  2.682e-05   6.475 2.55e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 101.4 on 437 degrees of freedom
## Multiple R-squared:  0.4447,	Adjusted R-squared:  0.4409 
## F-statistic: 116.6 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -94 16.5"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-321.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-322.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -79.63 -22.41  -4.53  14.15 171.44 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.205e+02  3.364e+00  35.803  < 2e-16 ***
## el          -8.911e-02  1.177e-02  -7.569 2.25e-13 ***
## sl           1.396e+00  1.842e-01   7.577 2.13e-13 ***
## I(el^2)      4.782e-05  8.116e-06   5.892 7.61e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34.87 on 437 degrees of freedom
## Multiple R-squared:  0.1649,	Adjusted R-squared:  0.1592 
## F-statistic: 28.76 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -94 17"
## [1] "N = 449"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-323.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-324.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -159.478  -46.143    2.161   46.259  208.721 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.703e+02  9.072e+00  62.857  < 2e-16 ***
## el          -8.587e-01  2.655e-02 -32.341  < 2e-16 ***
## sl           1.555e+00  3.208e-01   4.846 1.74e-06 ***
## I(el^2)      3.718e-04  1.749e-05  21.255  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 67.18 on 445 degrees of freedom
## Multiple R-squared:  0.8242,	Adjusted R-squared:  0.823 
## F-statistic: 695.6 on 3 and 445 DF,  p-value: < 2.2e-16
## 
## [1] "Point -93.5 16"
## [1] "N = 330"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-325.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-326.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -78.015 -27.809  -6.406  20.072 132.082 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.711e+02  4.129e+00  41.454  < 2e-16 ***
## el          -1.734e-01  1.305e-02 -13.290  < 2e-16 ***
## sl           1.627e+00  2.685e-01   6.059 3.78e-09 ***
## I(el^2)      7.412e-05  8.144e-06   9.102  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 39.28 on 326 degrees of freedom
## Multiple R-squared:  0.3961,	Adjusted R-squared:  0.3905 
## F-statistic: 71.26 on 3 and 326 DF,  p-value: < 2.2e-16
## 
## [1] "Point -93.5 16.5"
## [1] "N = 408"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-327.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-328.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -69.017 -16.095  -1.179  13.043 142.915 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.894e+02  8.852e+00  21.392  < 2e-16 ***
## el          -2.625e-01  1.877e-02 -13.986  < 2e-16 ***
## sl           6.985e-01  1.464e-01   4.771 2.57e-06 ***
## I(el^2)      1.334e-04  9.512e-06  14.019  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 24.74 on 404 degrees of freedom
## Multiple R-squared:  0.3676,	Adjusted R-squared:  0.3629 
## F-statistic: 78.28 on 3 and 404 DF,  p-value: < 2.2e-16
## 
## [1] "Point -93.5 17"
## [1] "N = 416"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-329.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-330.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -302.78  -53.15   -8.75   42.92  325.97 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.812e+02  1.498e+01   32.12  < 2e-16 ***
## el          -7.948e-01  3.691e-02  -21.53  < 2e-16 ***
## sl           2.333e+00  4.072e-01    5.73 1.94e-08 ***
## I(el^2)      3.756e-04  2.253e-05   16.67  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 84.84 on 412 degrees of freedom
## Multiple R-squared:  0.6018,	Adjusted R-squared:  0.5989 
## F-statistic: 207.5 on 3 and 412 DF,  p-value: < 2.2e-16
## 
## [1] "Point -93 15.5"
## [1] "N = 278"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-331.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-332.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -127.045  -35.489   -8.771   43.105  120.585 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.010e+02  5.360e+00  56.156  < 2e-16 ***
## el          -6.802e-03  1.655e-02  -0.411    0.681    
## sl           1.913e-01  4.087e-01   0.468    0.640    
## I(el^2)     -3.239e-05  7.132e-06  -4.541 8.38e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 52.27 on 274 degrees of freedom
## Multiple R-squared:  0.4456,	Adjusted R-squared:  0.4396 
## F-statistic: 73.42 on 3 and 274 DF,  p-value: < 2.2e-16
## 
## [1] "Point -93 16"
## [1] "N = 423"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-333.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-334.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -132.273  -49.349   -7.259   46.262  188.919 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.064e+02  1.137e+01  18.155  < 2e-16 ***
## el          -1.047e-01  2.531e-02  -4.138 4.23e-05 ***
## sl           1.753e+00  3.726e-01   4.705 3.46e-06 ***
## I(el^2)      3.475e-05  1.315e-05   2.642  0.00856 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 64.93 on 419 degrees of freedom
## Multiple R-squared:  0.07693,	Adjusted R-squared:  0.07032 
## F-statistic: 11.64 on 3 and 419 DF,  p-value: 2.427e-07
## 
## [1] "Point -93 17"
## [1] "N = 410"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-335.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-336.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -266.26  -87.10  -23.89   64.50  393.26 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.362e+02  2.501e+01   9.445  < 2e-16 ***
## el          -1.486e-01  4.456e-02  -3.334 0.000935 ***
## sl           3.406e+00  5.810e-01   5.863 9.45e-09 ***
## I(el^2)      4.665e-05  1.772e-05   2.633 0.008799 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 117.2 on 406 degrees of freedom
## Multiple R-squared:  0.1017,	Adjusted R-squared:  0.09503 
## F-statistic: 15.32 on 3 and 406 DF,  p-value: 1.849e-09
## 
## [1] "Point -93 17.5"
## [1] "N = 430"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-337.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-338.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -265.10  -49.52   -1.59   51.55  194.48 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.314e+02  6.258e+00  68.929  < 2e-16 ***
## el          -1.416e-02  3.120e-02  -0.454  0.65016    
## sl           6.285e-01  5.396e-01   1.165  0.24476    
## I(el^2)     -5.277e-05  1.761e-05  -2.997  0.00289 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 84.79 on 426 degrees of freedom
## Multiple R-squared:  0.2452,	Adjusted R-squared:  0.2399 
## F-statistic: 46.12 on 3 and 426 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92.5 15"
## [1] "N = 315"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-339.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-340.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -211.83  -81.54  -11.40   62.37  478.33 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.655e+02  9.235e+00  39.582  < 2e-16 ***
## el           2.008e-01  3.071e-02   6.538 2.56e-10 ***
## sl          -1.551e+00  9.450e-01  -1.641    0.102    
## I(el^2)     -1.010e-04  1.108e-05  -9.118  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 113.1 on 311 degrees of freedom
## Multiple R-squared:  0.2406,	Adjusted R-squared:  0.2332 
## F-statistic: 32.84 on 3 and 311 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92.5 15.5"
## [1] "N = 401"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-341.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-342.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -135.18  -50.18   -6.51   39.58  326.71 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.901e+02  8.596e+00  45.377  < 2e-16 ***
## el          -1.491e-01  1.794e-02  -8.310 1.53e-15 ***
## sl           1.044e+00  3.987e-01   2.618  0.00919 ** 
## I(el^2)      1.563e-05  6.727e-06   2.323  0.02068 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 74.78 on 397 degrees of freedom
## Multiple R-squared:  0.4955,	Adjusted R-squared:  0.4917 
## F-statistic:   130 on 3 and 397 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92.5 16.5"
## [1] "N = 449"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-343.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-344.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -56.51 -17.50  -2.94  12.43  91.49 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.355e+01  6.707e+00   7.984 1.22e-14 ***
## el           1.399e-01  1.201e-02  11.655  < 2e-16 ***
## sl           2.348e-01  1.713e-01   1.371    0.171    
## I(el^2)     -4.685e-05  4.157e-06 -11.269  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27.36 on 445 degrees of freedom
## Multiple R-squared:  0.2678,	Adjusted R-squared:  0.2629 
## F-statistic: 54.26 on 3 and 445 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92.5 17"
## [1] "N = 417"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-345.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-346.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -178.670  -40.934   -6.533   37.853  264.961 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.177e+02  1.664e+01  19.093   <2e-16 ***
## el          -2.578e-02  2.494e-02  -1.034   0.3019    
## sl           3.652e-01  4.242e-01   0.861   0.3898    
## I(el^2)     -2.224e-05  9.069e-06  -2.452   0.0146 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 78.46 on 413 degrees of freedom
## Multiple R-squared:  0.3297,	Adjusted R-squared:  0.3248 
## F-statistic:  67.7 on 3 and 413 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92.5 17.5"
## [1] "N = 443"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-347.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-348.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -165.228  -46.706    1.116   39.213  212.398 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.983e+02  4.979e+00  79.990  < 2e-16 ***
## el          -1.732e-01  2.485e-02  -6.972 1.16e-11 ***
## sl           1.006e+00  4.282e-01   2.349   0.0192 *  
## I(el^2)      9.758e-05  1.640e-05   5.949 5.53e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 64.81 on 439 degrees of freedom
## Multiple R-squared:  0.1226,	Adjusted R-squared:  0.1166 
## F-statistic: 20.45 on 3 and 439 DF,  p-value: 2.017e-12
## 
## [1] "Point -92 15"
## [1] "N = 445"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-349.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-350.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -212.74 -106.30  -24.74  101.75  320.91 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.914e+02  1.212e+01  32.285  < 2e-16 ***
## el           9.371e-02  2.460e-02   3.809 0.000159 ***
## sl          -5.433e-02  6.585e-01  -0.083 0.934284    
## I(el^2)     -6.409e-05  7.759e-06  -8.261 1.71e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 129.5 on 441 degrees of freedom
## Multiple R-squared:  0.4181,	Adjusted R-squared:  0.4141 
## F-statistic: 105.6 on 3 and 441 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92 15.5"
## [1] "N = 395"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-351.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-352.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -98.02 -41.79 -15.06  27.83 306.80 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.208e+02  1.600e+01  13.796   <2e-16 ***
## el           5.173e-04  2.142e-02   0.024   0.9807    
## sl           8.528e-01  3.303e-01   2.582   0.0102 *  
## I(el^2)     -8.207e-06  5.492e-06  -1.494   0.1359    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 65.37 on 391 degrees of freedom
## Multiple R-squared:  0.1255,	Adjusted R-squared:  0.1188 
## F-statistic:  18.7 on 3 and 391 DF,  p-value: 2.36e-11
## 
## [1] "Point -92 16"
## [1] "N = 391"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-353.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-354.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -109.396  -39.834   -5.858   33.646  164.878 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.396e+01  1.672e+01   3.825 0.000152 ***
## el           1.827e-01  2.861e-02   6.386 4.87e-10 ***
## sl           2.174e+00  3.356e-01   6.479 2.81e-10 ***
## I(el^2)     -7.136e-05  1.080e-05  -6.610 1.28e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 55.88 on 387 degrees of freedom
## Multiple R-squared:  0.2048,	Adjusted R-squared:  0.1986 
## F-statistic: 33.22 on 3 and 387 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92 16.5"
## [1] "N = 419"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-355.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-356.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -167.025  -24.860    1.623   26.698   98.486 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.044e+02  2.030e+01  19.919  < 2e-16 ***
## el          -2.060e-01  2.667e-02  -7.724 8.57e-14 ***
## sl           1.675e+00  2.395e-01   6.993 1.08e-11 ***
## I(el^2)      3.297e-05  8.509e-06   3.875 0.000124 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 44.28 on 415 degrees of freedom
## Multiple R-squared:  0.5623,	Adjusted R-squared:  0.5592 
## F-statistic: 177.7 on 3 and 415 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92 17"
## [1] "N = 378"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-357.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-358.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -133.25  -29.42   -6.12   20.35  187.10 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.411e+02  8.727e+00  39.082  < 2e-16 ***
## el          -5.781e-02  1.742e-02  -3.320 0.000989 ***
## sl           2.431e-01  2.965e-01   0.820 0.412762    
## I(el^2)     -9.574e-06  7.766e-06  -1.233 0.218413    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 48.02 on 374 degrees of freedom
## Multiple R-squared:  0.4257,	Adjusted R-squared:  0.4211 
## F-statistic: 92.42 on 3 and 374 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 14.5"
## [1] "N = 390"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-359.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-360.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -227.07 -101.61  -10.81   93.97  273.92 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.876e+02  8.947e+00  43.328  < 2e-16 ***
## el           1.338e-01  2.448e-02   5.468 8.19e-08 ***
## sl           1.192e+00  7.197e-01   1.657   0.0984 .  
## I(el^2)     -8.648e-05  8.425e-06 -10.265  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 112.4 on 386 degrees of freedom
## Multiple R-squared:  0.4725,	Adjusted R-squared:  0.4684 
## F-statistic: 115.3 on 3 and 386 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 15"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-361.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-362.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -205.787  -33.087   -3.591   22.304  189.991 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.072e+03  2.032e+01  52.769  < 2e-16 ***
## el          -7.399e-01  2.110e-02 -35.070  < 2e-16 ***
## sl           1.346e+00  2.513e-01   5.356 1.43e-07 ***
## I(el^2)      1.431e-04  5.263e-06  27.197  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 55.57 on 407 degrees of freedom
## Multiple R-squared:  0.8552,	Adjusted R-squared:  0.8541 
## F-statistic: 801.2 on 3 and 407 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 15.5"
## [1] "N = 397"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-363.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-364.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -207.510  -28.100   -6.004   22.417  264.131 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.749e+02  2.688e+01  21.388  < 2e-16 ***
## el          -3.026e-01  2.595e-02 -11.660  < 2e-16 ***
## sl           1.204e+00  2.790e-01   4.315 2.02e-05 ***
## I(el^2)      5.273e-05  6.052e-06   8.713  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 59.53 on 393 degrees of freedom
## Multiple R-squared:  0.4884,	Adjusted R-squared:  0.4845 
## F-statistic: 125.1 on 3 and 393 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 16"
## [1] "N = 395"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-365.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-366.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -180.826  -49.143    0.351   39.187  254.611 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.900e+02  1.153e+01  33.836  < 2e-16 ***
## el          -1.071e-01  1.656e-02  -6.468 2.97e-10 ***
## sl           1.694e+00  3.390e-01   4.998 8.76e-07 ***
## I(el^2)      7.171e-06  5.124e-06   1.400    0.162    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 74.66 on 391 degrees of freedom
## Multiple R-squared:  0.4666,	Adjusted R-squared:  0.4625 
## F-statistic:   114 on 3 and 391 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 16.5"
## [1] "N = 425"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-367.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-368.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -87.434 -23.330  -3.764  23.632 131.528 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.969e+02  6.844e+00  43.386  < 2e-16 ***
## el           1.147e-01  1.503e-02   7.630 1.58e-13 ***
## sl           8.281e-01  1.779e-01   4.656 4.33e-06 ***
## I(el^2)     -9.829e-05  7.124e-06 -13.796  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 35.11 on 421 degrees of freedom
## Multiple R-squared:  0.6503,	Adjusted R-squared:  0.6478 
## F-statistic:   261 on 3 and 421 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 17"
## [1] "N = 412"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-369.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-370.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -63.810 -11.961  -2.165  12.830  55.598 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.740e+02  3.010e+00  91.016   <2e-16 ***
## el           1.013e-01  9.804e-03  10.328   <2e-16 ***
## sl          -2.956e-02  1.191e-01  -0.248    0.804    
## I(el^2)     -6.148e-05  6.966e-06  -8.825   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.64 on 408 degrees of freedom
## Multiple R-squared:  0.2336,	Adjusted R-squared:  0.228 
## F-statistic: 41.45 on 3 and 408 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91 14.5"
## [1] "N = 424"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-371.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-372.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -206.956  -66.364   -3.445   55.078  266.059 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.834e+02  9.303e+00  51.956  < 2e-16 ***
## el          -1.995e-01  1.845e-02 -10.815  < 2e-16 ***
## sl           1.588e+00  4.784e-01   3.319 0.000982 ***
## I(el^2)      1.681e-05  6.854e-06   2.452 0.014610 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 89.78 on 420 degrees of freedom
## Multiple R-squared:  0.6853,	Adjusted R-squared:  0.683 
## F-statistic: 304.8 on 3 and 420 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91 15"
## [1] "N = 429"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-373.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-374.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -111.321  -25.757   -7.492   20.750  162.422 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.350e+02  2.578e+01  12.998  < 2e-16 ***
## el          -1.613e-01  2.594e-02  -6.216 1.22e-09 ***
## sl           5.238e-01  2.040e-01   2.567   0.0106 *  
## I(el^2)      3.257e-05  6.453e-06   5.048 6.64e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 43.79 on 425 degrees of freedom
## Multiple R-squared:  0.1731,	Adjusted R-squared:  0.1673 
## F-statistic: 29.66 on 3 and 425 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91 15.5"
## [1] "N = 403"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-375.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-376.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -162.98  -52.17  -10.90   38.98  242.79 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.962e+02  1.257e+01  47.413  < 2e-16 ***
## el          -2.830e-01  1.751e-02 -16.158  < 2e-16 ***
## sl           1.047e+00  3.236e-01   3.234  0.00132 ** 
## I(el^2)      4.023e-05  5.652e-06   7.119 5.11e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 70.82 on 399 degrees of freedom
## Multiple R-squared:  0.7584,	Adjusted R-squared:  0.7566 
## F-statistic: 417.5 on 3 and 399 DF,  p-value: < 2.2e-16
## 
## [1] "Point -90.5 14"
## [1] "N = 301"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-377.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-378.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -103.03  -39.74  -12.43   25.06  207.79 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.776e+02  5.286e+00  52.519  < 2e-16 ***
## el           7.561e-02  1.947e-02   3.883 0.000127 ***
## sl           1.356e+00  4.805e-01   2.821 0.005105 ** 
## I(el^2)     -8.524e-05  1.186e-05  -7.188 5.35e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 57.13 on 297 degrees of freedom
## Multiple R-squared:  0.2347,	Adjusted R-squared:  0.227 
## F-statistic: 30.36 on 3 and 297 DF,  p-value: < 2.2e-16
## 
## [1] "Point -90.5 14.5"
## [1] "N = 404"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-379.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-380.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -175.05  -44.01    5.59   40.34  345.36 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.293e+02  1.541e+01  27.862  < 2e-16 ***
## el          -2.708e-01  2.522e-02 -10.739  < 2e-16 ***
## sl           8.428e-02  4.233e-01   0.199    0.842    
## I(el^2)      6.761e-05  9.683e-06   6.983 1.21e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 79.54 on 400 degrees of freedom
## Multiple R-squared:  0.4048,	Adjusted R-squared:  0.4003 
## F-statistic: 90.66 on 3 and 400 DF,  p-value: < 2.2e-16
## 
## [1] "Point -90.5 15"
## [1] "N = 404"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-381.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-382.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -79.73 -38.63 -13.19  27.96 273.54 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.605e+01  2.041e+01   4.706 3.49e-06 ***
## el           4.206e-02  3.105e-02   1.355    0.176    
## sl           1.105e+00  2.597e-01   4.255 2.61e-05 ***
## I(el^2)     -7.091e-06  1.110e-05  -0.639    0.523    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 52.43 on 400 degrees of freedom
## Multiple R-squared:  0.08219,	Adjusted R-squared:  0.07531 
## F-statistic: 11.94 on 3 and 400 DF,  p-value: 1.672e-07
## 
## [1] "Point -90.5 15.5"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-383.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-384.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -271.277  -63.953   -2.451   65.745  227.187 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.344e+02  1.591e+01  33.586  < 2e-16 ***
## el          -2.083e-01  3.422e-02  -6.088 2.53e-09 ***
## sl           2.139e-01  4.723e-01   0.453    0.651    
## I(el^2)      2.242e-05  1.477e-05   1.518    0.130    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 103.7 on 430 degrees of freedom
## Multiple R-squared:  0.4562,	Adjusted R-squared:  0.4524 
## F-statistic: 120.3 on 3 and 430 DF,  p-value: < 2.2e-16
## 
## [1] "Point -90 14"
## [1] "N = 362"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-385.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-386.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -112.555  -30.814   -3.736   30.586  162.121 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.449e+02  4.840e+00  50.598  < 2e-16 ***
## el          -3.492e-02  1.457e-02  -2.396  0.01707 *  
## sl           7.789e-01  2.743e-01   2.840  0.00477 ** 
## I(el^2)      7.821e-06  9.075e-06   0.862  0.38936    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 41.84 on 358 degrees of freedom
## Multiple R-squared:  0.04993,	Adjusted R-squared:  0.04197 
## F-statistic: 6.272 on 3 and 358 DF,  p-value: 0.0003701
## 
## [1] "Point -90 14.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-387.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-388.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -89.694 -43.672  -5.057  27.276 164.934 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.663e+01  1.585e+01   5.465  8.1e-08 ***
## el           1.026e-01  2.764e-02   3.714 0.000232 ***
## sl           2.391e-01  3.058e-01   0.782 0.434722    
## I(el^2)     -2.286e-05  1.072e-05  -2.133 0.033540 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 52.31 on 407 degrees of freedom
## Multiple R-squared:  0.148,	Adjusted R-squared:  0.1417 
## F-statistic: 23.57 on 3 and 407 DF,  p-value: 4.39e-14
## 
## [1] "Point -90 15"
## [1] "N = 414"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-389.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-390.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -110.93  -54.34  -17.91   36.18  222.81 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.202e+02  1.244e+01   9.663  < 2e-16 ***
## el          3.157e-02  2.228e-02   1.417 0.157242    
## sl          1.255e+00  3.547e-01   3.539 0.000448 ***
## I(el^2)     9.437e-06  8.543e-06   1.105 0.269957    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 71.46 on 410 degrees of freedom
## Multiple R-squared:  0.2355,	Adjusted R-squared:  0.2299 
## F-statistic:  42.1 on 3 and 410 DF,  p-value: < 2.2e-16
## 
## [1] "Point -90 15.5"
## [1] "N = 435"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-391.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-392.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -210.800  -44.944   -9.751   46.943  239.687 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.973e+02  9.963e+00  29.839  < 2e-16 ***
## el           1.784e-01  2.209e-02   8.079 6.61e-15 ***
## sl          -5.267e-01  3.592e-01  -1.466    0.143    
## I(el^2)     -9.167e-05  9.599e-06  -9.550  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 79.91 on 431 degrees of freedom
## Multiple R-squared:    0.2,	Adjusted R-squared:  0.1944 
## F-statistic: 35.92 on 3 and 431 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89.5 13.5"
## [1] "N = 209"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-393.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-394.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -57.739 -13.174   2.555  17.248  66.923 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.409e+02  4.101e+00  58.737  < 2e-16 ***
## el          -6.216e-02  1.395e-02  -4.457 1.37e-05 ***
## sl           8.582e-01  2.631e-01   3.261   0.0013 ** 
## I(el^2)      6.147e-05  1.064e-05   5.780 2.75e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 26.68 on 205 degrees of freedom
## Multiple R-squared:  0.2116,	Adjusted R-squared:    0.2 
## F-statistic: 18.34 on 3 and 205 DF,  p-value: 1.405e-10
## 
## [1] "Point -89.5 14"
## [1] "N = 385"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-395.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-396.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -103.531  -26.485   -4.285   28.456  104.348 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.124e+02  9.313e+00  22.812  < 2e-16 ***
## el          -9.251e-02  2.568e-02  -3.603 0.000357 ***
## sl           5.034e-01  2.595e-01   1.940 0.053129 .  
## I(el^2)      8.616e-05  1.571e-05   5.484 7.59e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 39.77 on 381 degrees of freedom
## Multiple R-squared:  0.1631,	Adjusted R-squared:  0.1565 
## F-statistic: 24.75 on 3 and 381 DF,  p-value: 1.19e-14
## 
## [1] "Point -89.5 14.5"
## [1] "N = 402"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-397.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-398.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -68.123 -23.190  -3.729  26.295  72.793 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.418e+02  9.604e+00  14.766  < 2e-16 ***
## el          -6.166e-02  1.905e-02  -3.236  0.00131 ** 
## sl           3.192e-01  1.772e-01   1.802  0.07237 .  
## I(el^2)      5.896e-05  8.643e-06   6.822 3.36e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 32.06 on 398 degrees of freedom
## Multiple R-squared:  0.3893,	Adjusted R-squared:  0.3847 
## F-statistic: 84.56 on 3 and 398 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89.5 15"
## [1] "N = 429"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-399.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-400.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -116.063  -46.744   -0.755   41.243  166.102 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.695e+02  6.585e+00  25.738  < 2e-16 ***
## el          -8.124e-02  1.427e-02  -5.692 2.34e-08 ***
## sl           1.005e+00  2.729e-01   3.681 0.000262 ***
## I(el^2)      5.803e-05  6.219e-06   9.330  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 55.67 on 425 degrees of freedom
## Multiple R-squared:  0.2834,	Adjusted R-squared:  0.2783 
## F-statistic: 56.02 on 3 and 425 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89.5 15.5"
## [1] "N = 420"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-401.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-402.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -183.72  -21.65   10.12   31.47  135.50 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.450e+02  5.038e+00  48.637  < 2e-16 ***
## el           1.408e-01  1.671e-02   8.427 5.87e-16 ***
## sl          -4.525e-01  2.856e-01  -1.585    0.114    
## I(el^2)     -4.812e-05  7.881e-06  -6.106 2.35e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 54.72 on 416 degrees of freedom
## Multiple R-squared:  0.2113,	Adjusted R-squared:  0.2056 
## F-statistic: 37.14 on 3 and 416 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89 13.5"
## [1] "N = 285"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-403.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-404.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -68.275 -17.060   0.647  17.824  67.968 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.883e+02  3.272e+00  88.126  < 2e-16 ***
## el          -1.153e-01  1.348e-02  -8.554 7.79e-16 ***
## sl           3.642e-01  2.631e-01   1.385    0.167    
## I(el^2)      8.155e-05  1.144e-05   7.129 8.56e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 28.67 on 281 degrees of freedom
## Multiple R-squared:  0.2178,	Adjusted R-squared:  0.2094 
## F-statistic: 26.08 on 3 and 281 DF,  p-value: 6.519e-15
## 
## [1] "Point -89 14.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-405.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-406.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -77.687 -18.116  -6.117  16.097  86.431 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.331e+02  7.459e+00  31.255  < 2e-16 ***
## el          -1.573e-01  1.271e-02 -12.376  < 2e-16 ***
## sl           6.088e-01  1.323e-01   4.602 5.61e-06 ***
## I(el^2)      7.095e-05  5.078e-06  13.971  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 26.4 on 407 degrees of freedom
## Multiple R-squared:  0.3855,	Adjusted R-squared:  0.3809 
## F-statistic:  85.1 on 3 and 407 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89 15"
## [1] "N = 418"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-407.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-408.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -88.400 -15.369  -2.709  16.300 100.382 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.107e+02  4.000e+00  52.689  < 2e-16 ***
## el          -1.610e-01  1.074e-02 -14.986  < 2e-16 ***
## sl           6.071e-01  1.403e-01   4.327  1.9e-05 ***
## I(el^2)      9.033e-05  6.752e-06  13.377  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.89 on 414 degrees of freedom
## Multiple R-squared:  0.3597,	Adjusted R-squared:  0.355 
## F-statistic: 77.51 on 3 and 414 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89 16.5"
## [1] "N = 427"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-409.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-410.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -65.247  -6.096   0.798   9.484  29.782 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.890e+02  1.847e+00 156.457   <2e-16 ***
## el          -2.042e-01  1.009e-02 -20.242   <2e-16 ***
## sl          -1.803e-02  1.097e-01  -0.164    0.869    
## I(el^2)      2.084e-04  1.201e-05  17.353   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.74 on 423 degrees of freedom
## Multiple R-squared:  0.5344,	Adjusted R-squared:  0.5311 
## F-statistic: 161.9 on 3 and 423 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88.5 14"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-411.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-412.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -88.615 -26.166  -5.573  23.814 123.353 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.669e+02  5.190e+00  51.433  < 2e-16 ***
## el          -1.095e-03  1.434e-02  -0.076  0.93913    
## sl          -1.104e-01  2.043e-01  -0.540  0.58923    
## I(el^2)     -2.310e-05  7.481e-06  -3.088  0.00214 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 37.21 on 437 degrees of freedom
## Multiple R-squared:  0.2636,	Adjusted R-squared:  0.2585 
## F-statistic: 52.14 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88.5 14.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-413.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-414.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -73.590 -23.601  -3.123  22.339  75.479 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.262e+02  9.801e+00  23.077  < 2e-16 ***
## el          -1.090e-01  1.596e-02  -6.830 3.11e-11 ***
## sl           6.423e-01  1.714e-01   3.747 0.000205 ***
## I(el^2)      4.261e-05  6.213e-06   6.859 2.59e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.07 on 407 degrees of freedom
## Multiple R-squared:  0.1356,	Adjusted R-squared:  0.1292 
## F-statistic: 21.28 on 3 and 407 DF,  p-value: 7.955e-13
## 
## [1] "Point -88.5 15"
## [1] "N = 435"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-415.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-416.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -49.502 -20.401  -6.265  12.333 150.040 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.772e+02  5.583e+00  31.748  < 2e-16 ***
## el          -5.151e-02  1.210e-02  -4.257 2.54e-05 ***
## sl           5.332e-01  1.651e-01   3.230 0.001335 ** 
## I(el^2)      2.258e-05  6.135e-06   3.681 0.000261 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 30.26 on 431 degrees of freedom
## Multiple R-squared:  0.05568,	Adjusted R-squared:  0.0491 
## F-statistic: 8.471 on 3 and 431 DF,  p-value: 1.767e-05
## 
## [1] "Point -88.5 15.5"
## [1] "N = 372"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-417.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-418.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -87.146 -23.792   0.517  20.664 164.189 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.563e+02  3.715e+00  69.010   <2e-16 ***
## el          -2.572e-01  1.602e-02 -16.055   <2e-16 ***
## sl           3.357e-01  2.546e-01   1.318    0.188    
## I(el^2)      1.444e-04  1.214e-05  11.895   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 38.09 on 368 degrees of freedom
## Multiple R-squared:  0.5388,	Adjusted R-squared:  0.535 
## F-statistic: 143.3 on 3 and 368 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88 14"
## [1] "N = 432"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-419.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-420.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -99.127 -27.084  -4.593  25.917 143.040 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.149e+02  7.284e+00  43.223  < 2e-16 ***
## el          -1.374e-01  1.625e-02  -8.454 4.46e-16 ***
## sl           6.029e-01  2.390e-01   2.523    0.012 *  
## I(el^2)      3.586e-05  7.696e-06   4.660 4.24e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 41.94 on 428 degrees of freedom
## Multiple R-squared:  0.4058,	Adjusted R-squared:  0.4016 
## F-statistic: 97.43 on 3 and 428 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88 14.5"
## [1] "N = 442"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-421.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-422.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -84.647 -16.862  -4.451  17.957 109.553 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.539e+02  9.955e+00  25.509  < 2e-16 ***
## el          -7.315e-02  1.791e-02  -4.084 5.26e-05 ***
## sl           1.324e-01  1.696e-01   0.781   0.4354    
## I(el^2)      1.680e-05  7.155e-06   2.348   0.0193 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 32.71 on 438 degrees of freedom
## Multiple R-squared:  0.1824,	Adjusted R-squared:  0.1768 
## F-statistic: 32.57 on 3 and 438 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88 15"
## [1] "N = 484"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-423.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-424.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -65.87 -26.14  -8.07  19.76 121.04 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.749e+02  4.488e+00  38.977  < 2e-16 ***
## el           9.949e-02  1.188e-02   8.372 6.27e-16 ***
## sl          -4.220e-01  1.846e-01  -2.287   0.0226 *  
## I(el^2)     -5.893e-05  6.549e-06  -8.999  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 36.79 on 480 degrees of freedom
## Multiple R-squared:  0.145,	Adjusted R-squared:  0.1397 
## F-statistic: 27.14 on 3 and 480 DF,  p-value: 3.14e-16
## 
## [1] "Point -87.5 14"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-425.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-426.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -76.452 -16.298  -1.636  15.444  71.103 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.013e+02  4.800e+00  62.773  < 2e-16 ***
## el          -2.426e-01  9.813e-03 -24.719  < 2e-16 ***
## sl           8.222e-01  1.448e-01   5.679 2.56e-08 ***
## I(el^2)      9.484e-05  4.832e-06  19.626  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 26.62 on 411 degrees of freedom
## Multiple R-squared:  0.6765,	Adjusted R-squared:  0.6741 
## F-statistic: 286.5 on 3 and 411 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87.5 14.5"
## [1] "N = 412"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-427.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-428.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -56.841 -16.654  -0.985  15.304  80.005 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.954e+02  9.053e+00  21.579  < 2e-16 ***
## el          -2.863e-02  1.669e-02  -1.715   0.0871 .  
## sl           5.548e-01  1.354e-01   4.097 5.04e-05 ***
## I(el^2)      5.509e-06  7.144e-06   0.771   0.4411    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.73 on 408 degrees of freedom
## Multiple R-squared:  0.07495,	Adjusted R-squared:  0.06815 
## F-statistic: 11.02 on 3 and 408 DF,  p-value: 5.698e-07
## 
## [1] "Point -87.5 15"
## [1] "N = 443"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-429.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-430.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -63.022 -21.492  -7.497  13.289 122.041 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.711e+02  5.970e+00  28.662   <2e-16 ***
## el           3.494e-02  1.380e-02   2.532   0.0117 *  
## sl          -1.289e-01  1.711e-01  -0.753   0.4516    
## I(el^2)     -1.668e-05  6.909e-06  -2.415   0.0162 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.77 on 439 degrees of freedom
## Multiple R-squared:  0.01443,	Adjusted R-squared:  0.007691 
## F-statistic: 2.142 on 3 and 439 DF,  p-value: 0.09425
## 
## [1] "Point -87.5 15.5"
## [1] "N = 412"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-431.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-432.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -117.020  -31.345    5.625   34.107  128.548 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.818e+02  4.363e+00  64.599  < 2e-16 ***
## el          -2.558e-01  2.137e-02 -11.970  < 2e-16 ***
## sl           6.575e-01  3.387e-01   1.941   0.0529 .  
## I(el^2)      1.218e-04  1.472e-05   8.279  1.8e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 52.79 on 408 degrees of freedom
## Multiple R-squared:  0.4594,	Adjusted R-squared:  0.4555 
## F-statistic: 115.6 on 3 and 408 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87 13.5"
## [1] "N = 413"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-433.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-434.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -139.728  -37.568   -4.141   37.510  164.475 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.232e+02  6.602e+00  48.954   <2e-16 ***
## el          -3.345e-01  2.299e-02 -14.551   <2e-16 ***
## sl           1.903e-01  3.371e-01   0.565    0.573    
## I(el^2)      1.970e-04  1.633e-05  12.064   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 56.37 on 409 degrees of freedom
## Multiple R-squared:  0.4003,	Adjusted R-squared:  0.3959 
## F-statistic: 91.01 on 3 and 409 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87 14"
## [1] "N = 397"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-435.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-436.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -51.217 -19.373   0.262  17.698 119.277 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.054e+02  7.880e+00  13.376  < 2e-16 ***
## el           7.815e-02  1.570e-02   4.978 9.65e-07 ***
## sl           6.331e-01  1.354e-01   4.676 4.04e-06 ***
## I(el^2)     -2.447e-05  7.573e-06  -3.231  0.00134 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 24.18 on 393 degrees of freedom
## Multiple R-squared:  0.2124,	Adjusted R-squared:  0.2064 
## F-statistic: 35.32 on 3 and 393 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87 15"
## [1] "N = 423"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-437.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-438.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -51.360 -12.108   0.013  11.408  52.143 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.556e+02  5.552e+00  28.033  < 2e-16 ***
## el           3.310e-02  1.099e-02   3.012  0.00275 ** 
## sl          -1.457e-01  1.012e-01  -1.440  0.15075    
## I(el^2)     -2.634e-06  4.940e-06  -0.533  0.59418    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.73 on 419 degrees of freedom
## Multiple R-squared:  0.2158,	Adjusted R-squared:  0.2101 
## F-statistic: 38.42 on 3 and 419 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87 15.5"
## [1] "N = 384"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-439.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-440.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -101.097  -29.596    5.542   36.971   89.981 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.074e+02  5.112e+00  60.131  < 2e-16 ***
## el          -2.819e-01  1.456e-02 -19.361  < 2e-16 ***
## sl           8.509e-01  2.280e-01   3.732 0.000219 ***
## I(el^2)      1.287e-04  8.071e-06  15.939  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 44.47 on 380 degrees of freedom
## Multiple R-squared:  0.5427,	Adjusted R-squared:  0.5391 
## F-statistic: 150.3 on 3 and 380 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 13"
## [1] "N = 385"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-441.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-442.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -84.13 -29.44  -4.72  25.92 129.42 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.686e+02  4.613e+00  79.899  < 2e-16 ***
## el          -3.626e-01  1.948e-02 -18.616  < 2e-16 ***
## sl           1.224e+00  3.051e-01   4.011 7.29e-05 ***
## I(el^2)      1.971e-04  1.530e-05  12.885  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 42.13 on 381 degrees of freedom
## Multiple R-squared:  0.6211,	Adjusted R-squared:  0.6181 
## F-statistic: 208.2 on 3 and 381 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 13.5"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-443.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-444.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -90.216 -19.206  -2.217  13.335 139.714 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.892e+02  1.190e+01  15.892  < 2e-16 ***
## el          -4.223e-02  2.631e-02  -1.605    0.109    
## sl           6.252e-02  1.983e-01   0.315    0.753    
## I(el^2)      6.531e-05  1.409e-05   4.636 4.77e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 35.74 on 411 degrees of freedom
## Multiple R-squared:  0.2903,	Adjusted R-squared:  0.2851 
## F-statistic: 56.03 on 3 and 411 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 14"
## [1] "N = 426"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-445.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-446.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -43.115 -15.992  -3.142  15.397  61.624 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.840e+01  8.193e+00   5.907 7.18e-09 ***
## el           2.001e-01  1.825e-02  10.964  < 2e-16 ***
## sl          -1.545e-01  1.236e-01  -1.250    0.212    
## I(el^2)     -5.118e-05  9.340e-06  -5.479 7.35e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 21.37 on 422 degrees of freedom
## Multiple R-squared:  0.6498,	Adjusted R-squared:  0.6473 
## F-statistic:   261 on 3 and 422 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 15"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-447.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-448.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -33.690 -11.208  -1.046   7.496  66.928 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.111e+02  6.033e+00  18.408  < 2e-16 ***
## el           1.060e-01  1.282e-02   8.271 1.62e-15 ***
## sl           7.412e-02  7.662e-02   0.967 0.333865    
## I(el^2)     -2.374e-05  6.484e-06  -3.661 0.000282 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.92 on 437 degrees of freedom
## Multiple R-squared:  0.5525,	Adjusted R-squared:  0.5495 
## F-statistic: 179.9 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 15.5"
## [1] "N = 396"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-449.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-450.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -79.530 -26.051   0.616  22.722  93.836 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.461e+02  4.040e+00  60.908  < 2e-16 ***
## el          -1.090e-01  1.159e-02  -9.401  < 2e-16 ***
## sl           2.599e-01  1.697e-01   1.531    0.127    
## I(el^2)      5.315e-05  6.615e-06   8.034 1.12e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34.36 on 392 degrees of freedom
## Multiple R-squared:  0.2057,	Adjusted R-squared:  0.1997 
## F-statistic: 33.85 on 3 and 392 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 16"
## [1] "N = 106"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-451.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-452.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -46.310 -19.804  -2.952  11.926 128.106 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.104e+02  5.947e+00  52.184  < 2e-16 ***
## el          -1.623e-01  2.064e-02  -7.861 4.09e-12 ***
## sl           3.364e-01  3.473e-01   0.969    0.335    
## I(el^2)      6.114e-05  1.048e-05   5.833 6.44e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 30 on 102 degrees of freedom
## Multiple R-squared:  0.5062,	Adjusted R-squared:  0.4917 
## F-statistic: 34.86 on 3 and 102 DF,  p-value: 1.366e-15
## 
## [1] "Point -86 12.5"
## [1] "N = 401"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-453.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-454.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -36.818 -13.463  -1.864   9.299  73.236 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.496e+02  1.731e+00 144.198   <2e-16 ***
## el          -1.484e-01  1.004e-02 -14.780   <2e-16 ***
## sl          -1.904e-01  1.397e-01  -1.363    0.174    
## I(el^2)      1.314e-04  1.005e-05  13.076   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 17.93 on 397 degrees of freedom
## Multiple R-squared:  0.4553,	Adjusted R-squared:  0.4512 
## F-statistic: 110.6 on 3 and 397 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86 13"
## [1] "N = 409"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-455.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-456.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -65.891 -21.573  -7.737  18.474 123.804 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.273e+02  1.240e+01  18.324   <2e-16 ***
## el          -3.529e-02  3.344e-02  -1.055    0.292    
## sl           3.385e-01  1.883e-01   1.798    0.073 .  
## I(el^2)      3.069e-05  2.085e-05   1.472    0.142    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.48 on 405 degrees of freedom
## Multiple R-squared:  0.02863,	Adjusted R-squared:  0.02143 
## F-statistic: 3.979 on 3 and 405 DF,  p-value: 0.008183
## 
## [1] "Point -86 13.5"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-457.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-458.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -68.419 -20.119  -7.314  17.152 101.030 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.193e+02  1.314e+01  16.682   <2e-16 ***
## el          -8.342e-03  3.212e-02  -0.260    0.795    
## sl           8.645e-02  1.824e-01   0.474    0.636    
## I(el^2)      1.752e-05  1.881e-05   0.932    0.352    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.06 on 411 degrees of freedom
## Multiple R-squared:  0.03002,	Adjusted R-squared:  0.02294 
## F-statistic:  4.24 on 3 and 411 DF,  p-value: 0.005737
## 
## [1] "Point -86 14"
## [1] "N = 438"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-459.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-460.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -74.553 -10.235   3.226  18.383  58.935 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.713e+02  7.358e+00  23.281  < 2e-16 ***
## el          2.821e-03  1.958e-02   0.144 0.885503    
## sl          5.023e-01  1.499e-01   3.350 0.000878 ***
## I(el^2)     2.717e-05  1.183e-05   2.296 0.022170 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27.29 on 434 degrees of freedom
## Multiple R-squared:  0.2393,	Adjusted R-squared:  0.234 
## F-statistic: 45.51 on 3 and 434 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86 15"
## [1] "N = 436"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-461.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-462.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -40.656 -10.996  -2.401   9.995  66.145 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.327e+02  5.063e+00  26.213  < 2e-16 ***
## el           8.288e-02  1.236e-02   6.707 6.23e-11 ***
## sl          -2.108e-01  9.790e-02  -2.153   0.0319 *  
## I(el^2)     -7.564e-06  6.413e-06  -1.180   0.2388    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 17.34 on 432 degrees of freedom
## Multiple R-squared:  0.5974,	Adjusted R-squared:  0.5946 
## F-statistic: 213.7 on 3 and 432 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86 15.5"
## [1] "N = 446"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-463.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-464.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -62.812 -16.204  -0.677  12.508  85.114 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.286e+02  3.196e+00  71.524  < 2e-16 ***
## el          -1.191e-01  1.245e-02  -9.568  < 2e-16 ***
## sl           2.621e-01  1.503e-01   1.744   0.0818 .  
## I(el^2)      7.929e-05  1.013e-05   7.826 3.76e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27.9 on 442 degrees of freedom
## Multiple R-squared:  0.2013,	Adjusted R-squared:  0.1959 
## F-statistic: 37.13 on 3 and 442 DF,  p-value: < 2.2e-16
## 
## [1] "Point -85.5 15"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-465.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-466.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -39.315  -7.199   0.618   7.777  51.122 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.667e+02  2.236e+00  74.571  < 2e-16 ***
## el           4.516e-02  6.126e-03   7.371 8.52e-13 ***
## sl          -7.582e-03  6.732e-02  -0.113    0.910    
## I(el^2)     -3.091e-06  3.753e-06  -0.824    0.411    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.49 on 437 degrees of freedom
## Multiple R-squared:  0.5374,	Adjusted R-squared:  0.5343 
## F-statistic: 169.2 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -85.5 15.5"
## [1] "N = 433"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-467.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-468.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -55.625 -12.277  -0.396  11.799  72.627 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.018e+02  2.331e+00  86.579  < 2e-16 ***
## el          -3.401e-02  8.281e-03  -4.107 4.80e-05 ***
## sl          -1.270e-01  1.143e-01  -1.111    0.267    
## I(el^2)      3.759e-05  6.219e-06   6.044 3.26e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.01 on 429 degrees of freedom
## Multiple R-squared:  0.1107,	Adjusted R-squared:  0.1045 
## F-statistic:  17.8 on 3 and 429 DF,  p-value: 6.633e-11
## 
## [1] "Point -84.5 9.5"
## [1] "N = 143"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-469.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-470.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -157.562  -27.920    6.852   32.614  129.784 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.471e+02  9.393e+00  58.246  < 2e-16 ***
## el          -1.673e-01  3.056e-02  -5.474 1.99e-07 ***
## sl          -4.101e-01  5.853e-01  -0.701  0.48467    
## I(el^2)      4.665e-05  1.721e-05   2.711  0.00756 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 56.78 on 139 degrees of freedom
## Multiple R-squared:  0.4464,	Adjusted R-squared:  0.4344 
## F-statistic: 37.36 on 3 and 139 DF,  p-value: < 2.2e-16
## 
## [1] "Point -84.5 10"
## [1] "N = 356"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-471.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-472.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -130.088  -37.992    1.903   41.045  147.740 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.204e+02  7.551e+00  55.678  < 2e-16 ***
## el           2.758e-02  1.688e-02   1.633    0.103    
## sl           1.552e+00  3.386e-01   4.583 6.37e-06 ***
## I(el^2)     -3.743e-05  8.032e-06  -4.660 4.48e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 58.75 on 352 degrees of freedom
## Multiple R-squared:  0.2052,	Adjusted R-squared:  0.1984 
## F-statistic: 30.29 on 3 and 352 DF,  p-value: < 2.2e-16
## 
## [1] "Point -84.5 10.5"
## [1] "N = 382"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-473.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-474.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -115.411  -21.269   -2.305   15.674  173.083 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.144e+02  3.305e+00  95.133   <2e-16 ***
## el           2.943e-01  1.050e-02  28.021   <2e-16 ***
## sl           1.476e-01  2.361e-01   0.625    0.532    
## I(el^2)     -1.430e-04  5.983e-06 -23.904   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 35.58 on 378 degrees of freedom
## Multiple R-squared:  0.7116,	Adjusted R-squared:  0.7093 
## F-statistic: 310.9 on 3 and 378 DF,  p-value: < 2.2e-16
## 
## [1] "Point -84 9"
## [1] "N = 60"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-475.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-476.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -64.298 -24.020  -3.966  20.794 146.419 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.736e+02  7.870e+00  85.587  < 2e-16 ***
## el          -5.375e-01  5.113e-02 -10.512 7.26e-15 ***
## sl           6.008e-01  6.225e-01   0.965    0.339    
## I(el^2)      3.773e-04  4.784e-05   7.886 1.19e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 36.29 on 56 degrees of freedom
## Multiple R-squared:  0.8048,	Adjusted R-squared:  0.7943 
## F-statistic: 76.96 on 3 and 56 DF,  p-value: < 2.2e-16
## 
## [1] "Point -84 9.5"
## [1] "N = 270"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-477.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-478.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -236.56  -72.14  -15.27   48.64  323.68 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.661e+02  1.588e+01  35.648  < 2e-16 ***
## el          -1.710e-01  2.549e-02  -6.708 1.18e-10 ***
## sl           1.297e+00  6.619e-01   1.959   0.0511 .  
## I(el^2)      4.543e-05  8.877e-06   5.118 5.93e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 105.7 on 266 degrees of freedom
## Multiple R-squared:  0.1831,	Adjusted R-squared:  0.1739 
## F-statistic: 19.87 on 3 and 266 DF,  p-value: 1.181e-11
## 
## [1] "Point -84 10"
## [1] "N = 407"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-479.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-480.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -160.08  -39.41   -5.12   34.80  343.11 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.599e+02  9.065e+00  39.696  < 2e-16 ***
## el           1.826e-02  1.565e-02   1.166  0.24414    
## sl           1.018e+00  3.092e-01   3.293  0.00108 ** 
## I(el^2)     -1.036e-05  5.795e-06  -1.789  0.07444 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 64.12 on 403 degrees of freedom
## Multiple R-squared:  0.04046,	Adjusted R-squared:  0.03331 
## F-statistic: 5.664 on 3 and 403 DF,  p-value: 0.0008289
## 
## [1] "Point -83.5 9"
## [1] "N = 278"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-481.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-482.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -244.770  -64.245   -9.769   68.772  201.909 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.649e+02  1.051e+01  63.289  < 2e-16 ***
## el          -2.195e-01  2.224e-02  -9.869  < 2e-16 ***
## sl           5.660e-01  5.767e-01   0.981    0.327    
## I(el^2)      6.226e-05  8.196e-06   7.596 4.83e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 85.57 on 274 degrees of freedom
## Multiple R-squared:  0.3131,	Adjusted R-squared:  0.3056 
## F-statistic: 41.64 on 3 and 274 DF,  p-value: < 2.2e-16
## 
## [1] "Point -83.5 9.5"
## [1] "N = 370"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-483.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-484.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -222.513  -88.569    7.357   86.319  233.744 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.873e+02  2.031e+01  19.064  < 2e-16 ***
## el           1.703e-01  2.841e-02   5.993 4.92e-09 ***
## sl          -1.205e+00  5.561e-01  -2.167   0.0309 *  
## I(el^2)     -4.289e-05  8.548e-06  -5.017 8.19e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 112.1 on 366 degrees of freedom
## Multiple R-squared:  0.1063,	Adjusted R-squared:  0.09898 
## F-statistic: 14.51 on 3 and 366 DF,  p-value: 5.96e-09
## 
## [1] "Point -83.5 10"
## [1] "N = 378"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-485.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-486.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -164.847  -65.856   -9.544   45.662  276.543 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.522e+02  8.371e+00  30.130  < 2e-16 ***
## el           1.557e-01  1.918e-02   8.115 7.08e-15 ***
## sl           3.393e-01  5.014e-01   0.677    0.499    
## I(el^2)     -2.999e-05  7.592e-06  -3.950 9.34e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 91.33 on 374 degrees of freedom
## Multiple R-squared:  0.4078,	Adjusted R-squared:  0.403 
## F-statistic: 85.85 on 3 and 374 DF,  p-value: < 2.2e-16
## 
## [1] "Point -83 8.5"
## [1] "N = 243"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-487.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-488.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -133.774  -41.720    3.248   30.611  199.641 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.693e+02  7.690e+00  74.033  < 2e-16 ***
## el           1.328e-01  3.312e-02   4.009 8.14e-05 ***
## sl           4.930e-01  5.484e-01   0.899  0.36959    
## I(el^2)     -9.733e-05  2.618e-05  -3.718  0.00025 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 62.79 on 239 degrees of freedom
## Multiple R-squared:  0.08376,	Adjusted R-squared:  0.07226 
## F-statistic: 7.283 on 3 and 239 DF,  p-value: 0.0001078
## 
## [1] "Point -83 9"
## [1] "N = 388"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-489.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-490.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -250.856  -49.267   -1.684   62.345  184.020 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.344e+02  1.027e+01  61.758  < 2e-16 ***
## el          -1.122e-01  1.761e-02  -6.374 5.27e-10 ***
## sl          -1.834e+00  3.791e-01  -4.837 1.91e-06 ***
## I(el^2)      2.437e-05  6.276e-06   3.884 0.000121 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 75.94 on 384 degrees of freedom
## Multiple R-squared:  0.3121,	Adjusted R-squared:  0.3067 
## F-statistic: 58.08 on 3 and 384 DF,  p-value: < 2.2e-16
## 
## [1] "Point -83 9.5"
## [1] "N = 365"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-491.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-492.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -158.493  -40.141   -0.996   30.443  272.586 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.532e+02  7.545e+00  20.308   <2e-16 ***
## el           3.859e-01  1.462e-02  26.401   <2e-16 ***
## sl          -7.867e-01  3.258e-01  -2.415   0.0163 *  
## I(el^2)     -9.465e-05  5.332e-06 -17.749   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 63.71 on 361 degrees of freedom
## Multiple R-squared:  0.8238,	Adjusted R-squared:  0.8223 
## F-statistic: 562.6 on 3 and 361 DF,  p-value: < 2.2e-16
## 
## [1] "Point -82.5 8.5"
## [1] "N = 322"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-493.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-494.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -155.379  -60.590   -7.205   66.243  214.821 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.112e+02  7.598e+00  67.282  < 2e-16 ***
## el           1.107e-01  2.079e-02   5.327 1.89e-07 ***
## sl          -5.530e-01  6.228e-01  -0.888    0.375    
## I(el^2)     -7.406e-05  8.980e-06  -8.247 4.35e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 78.67 on 318 degrees of freedom
## Multiple R-squared:  0.2571,	Adjusted R-squared:  0.2501 
## F-statistic: 36.69 on 3 and 318 DF,  p-value: < 2.2e-16
## 
## [1] "Point -82.5 9"
## [1] "N = 352"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-495.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-496.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -167.82  -79.76  -22.21   57.08  324.17 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.376e+02  1.359e+01  24.830  < 2e-16 ***
## el           2.939e-01  2.427e-02  12.110  < 2e-16 ***
## sl          -2.740e+00  5.887e-01  -4.655 4.61e-06 ***
## I(el^2)     -1.002e-04  9.388e-06 -10.676  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 102.9 on 348 degrees of freedom
## Multiple R-squared:  0.3065,	Adjusted R-squared:  0.3005 
## F-statistic: 51.27 on 3 and 348 DF,  p-value: < 2.2e-16
## 
## [1] "Point -82.5 9.5"
## [1] "N = 198"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-497.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-498.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -107.033  -23.154   -1.471   29.043  119.512 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.697e+02  5.709e+00  29.727  < 2e-16 ***
## el           2.733e-01  2.113e-02  12.938  < 2e-16 ***
## sl           7.728e-01  3.954e-01   1.955   0.0521 .  
## I(el^2)     -7.218e-05  1.122e-05  -6.431 9.66e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 44.92 on 194 degrees of freedom
## Multiple R-squared:  0.8023,	Adjusted R-squared:  0.7992 
## F-statistic: 262.4 on 3 and 194 DF,  p-value: < 2.2e-16
## 
## [1] "Point -82 8.5"
## [1] "N = 363"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-499.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-500.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -164.91  -57.22   -1.50   53.58  212.73 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.926e+02  7.831e+00  62.912  < 2e-16 ***
## el           8.997e-02  2.941e-02   3.060  0.00238 ** 
## sl          -4.781e-01  5.300e-01  -0.902  0.36759    
## I(el^2)     -1.093e-04  1.799e-05  -6.076 3.14e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 78.09 on 359 degrees of freedom
## Multiple R-squared:  0.2149,	Adjusted R-squared:  0.2084 
## F-statistic: 32.76 on 3 and 359 DF,  p-value: < 2.2e-16
## 
## [1] "Point -82 9"
## [1] "N = 240"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-501.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-502.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -151.12  -44.70    5.49   31.91  259.53 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.210e+02  7.602e+00  42.224   <2e-16 ***
## el           2.988e-01  2.610e-02  11.449   <2e-16 ***
## sl           5.500e-01  5.278e-01   1.042    0.298    
## I(el^2)     -1.670e-04  1.466e-05 -11.392   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 62 on 236 degrees of freedom
## Multiple R-squared:  0.4541,	Adjusted R-squared:  0.4472 
## F-statistic: 65.45 on 3 and 236 DF,  p-value: < 2.2e-16
## 
## [1] "Point -81.5 8.5"
## [1] "N = 393"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-503.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-504.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -160.300  -32.475    1.446   31.217  179.875 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.248e+02  6.966e+00  75.337  < 2e-16 ***
## el          -8.612e-02  2.247e-02  -3.832 0.000148 ***
## sl          -4.365e-01  3.870e-01  -1.128 0.260027    
## I(el^2)     -1.222e-05  1.352e-05  -0.904 0.366690    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 59.19 on 389 degrees of freedom
## Multiple R-squared:  0.3937,	Adjusted R-squared:  0.3891 
## F-statistic: 84.21 on 3 and 389 DF,  p-value: < 2.2e-16
## 
## [1] "Point -81.5 9"
## [1] "N = 153"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-505.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-506.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -116.041  -27.696    6.295   27.647   75.422 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.725e+02  5.613e+00  66.358  < 2e-16 ***
## el           1.646e-01  2.260e-02   7.282 1.77e-11 ***
## sl           1.887e-01  4.275e-01   0.441     0.66    
## I(el^2)     -1.240e-04  1.467e-05  -8.455 2.37e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 36.25 on 149 degrees of freedom
## Multiple R-squared:  0.3574,	Adjusted R-squared:  0.3444 
## F-statistic: 27.62 on 3 and 149 DF,  p-value: 2.924e-14
## 
## [1] "Point -81 7"
## [1] "N = 30"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-507.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-508.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -94.63 -19.09  13.33  34.53  46.02 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.273e+02  1.561e+01  40.192  < 2e-16 ***
## el          -4.065e-01  7.318e-02  -5.555 7.81e-06 ***
## sl          -1.745e-01  9.243e-01  -0.189    0.852    
## I(el^2)      2.457e-04  5.330e-05   4.609 9.43e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 44.98 on 26 degrees of freedom
## Multiple R-squared:  0.6891,	Adjusted R-squared:  0.6532 
## F-statistic: 19.21 on 3 and 26 DF,  p-value: 8.961e-07
## 
## [1] "Point -81 8.5"
## [1] "N = 390"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-509.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-510.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -115.119  -46.618   -6.857   47.984  159.713 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.212e+02  5.995e+00  70.259   <2e-16 ***
## el           2.845e-01  2.737e-02  10.395   <2e-16 ***
## sl          -6.406e-01  3.993e-01  -1.604    0.109    
## I(el^2)     -2.251e-04  1.939e-05 -11.610   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 57.96 on 386 degrees of freedom
## Multiple R-squared:  0.2644,	Adjusted R-squared:  0.2587 
## F-statistic: 46.25 on 3 and 386 DF,  p-value: < 2.2e-16
## 
## [1] "Point -80.5 7"
## [1] "N = 54"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-511.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-512.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -50.995 -20.666  -4.984  12.875 121.824 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.547e+02  9.072e+00  39.098  < 2e-16 ***
## el           2.456e-01  4.633e-02   5.301  2.6e-06 ***
## sl           4.334e-01  5.623e-01   0.771  0.44453    
## I(el^2)     -1.190e-04  3.531e-05  -3.369  0.00146 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34.46 on 50 degrees of freedom
## Multiple R-squared:  0.662,	Adjusted R-squared:  0.6417 
## F-statistic: 32.65 on 3 and 50 DF,  p-value: 7.844e-12
## 
## [1] "Point -78.5 7.5"
## [1] "N = 42"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-513.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-514.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -212.386  -27.813    4.637   43.005  183.772 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.233e+02  3.789e+01  16.451   <2e-16 ***
## el          -2.682e-01  1.403e-01  -1.912   0.0635 .  
## sl          -2.564e-01  1.625e+00  -0.158   0.8754    
## I(el^2)      1.414e-04  1.016e-04   1.392   0.1720    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 80.54 on 38 degrees of freedom
## Multiple R-squared:  0.196,	Adjusted R-squared:  0.1325 
## F-statistic: 3.087 on 3 and 38 DF,  p-value: 0.03853
## 
## [1] "Point -76.5 6"
## [1] "N = 183"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-515.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-516.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -234.189  -32.256   -1.347   35.924   93.538 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.652e+02  5.401e+00 104.643  < 2e-16 ***
## el          -1.354e-01  1.366e-02  -9.915  < 2e-16 ***
## sl           8.828e-01  2.400e-01   3.678 0.000311 ***
## I(el^2)      1.095e-05  5.084e-06   2.154 0.032561 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 46.23 on 179 degrees of freedom
## Multiple R-squared:  0.783,	Adjusted R-squared:  0.7793 
## F-statistic: 215.2 on 3 and 179 DF,  p-value: < 2.2e-16
## 
## [1] "Point -76.5 6.5"
## [1] "N = 374"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-517.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-518.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -101.435  -35.400   -7.189   31.044  148.504 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.939e+02  4.468e+00 110.535  < 2e-16 ***
## el          -1.138e-01  1.064e-02 -10.702  < 2e-16 ***
## sl           5.831e-01  2.215e-01   2.633 0.008821 ** 
## I(el^2)      1.601e-05  4.116e-06   3.889 0.000119 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 48.41 on 370 degrees of freedom
## Multiple R-squared:  0.5652,	Adjusted R-squared:  0.5617 
## F-statistic: 160.3 on 3 and 370 DF,  p-value: < 2.2e-16
## 
## [1] "Point -76.5 7"
## [1] "N = 401"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-519.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-520.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -84.15 -26.95  -1.85  19.68 136.66 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.809e+02  3.619e+00 132.878   <2e-16 ***
## el          -1.470e-01  9.283e-03 -15.832   <2e-16 ***
## sl           1.109e-02  1.920e-01   0.058    0.954    
## I(el^2)      3.709e-05  3.945e-06   9.402   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 39.39 on 397 degrees of freedom
## Multiple R-squared:  0.6469,	Adjusted R-squared:  0.6443 
## F-statistic: 242.5 on 3 and 397 DF,  p-value: < 2.2e-16
## 
## [1] "Point -76.5 7.5"
## [1] "N = 403"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-521.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-522.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -150.491  -45.525   -3.014   33.583  192.271 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.167e+02  5.927e+00  70.307  < 2e-16 ***
## el          -1.370e-01  2.128e-02  -6.436 3.53e-10 ***
## sl           3.609e-01  4.574e-01   0.789    0.431    
## I(el^2)      4.914e-05  1.002e-05   4.902 1.38e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 68.01 on 399 degrees of freedom
## Multiple R-squared:  0.1625,	Adjusted R-squared:  0.1562 
## F-statistic:  25.8 on 3 and 399 DF,  p-value: 2.826e-15
## 
## [1] "Point -76.5 18"
## [1] "N = 128"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-523.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-524.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -71.302 -28.941   0.273  23.487  77.331 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.281e+02  6.331e+00  36.023   <2e-16 ***
## el           6.350e-01  3.378e-02  18.799   <2e-16 ***
## sl          -2.925e-01  3.583e-01  -0.816    0.416    
## I(el^2)     -4.024e-04  2.801e-05 -14.368   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 35.76 on 124 degrees of freedom
## Multiple R-squared:  0.8513,	Adjusted R-squared:  0.8477 
## F-statistic: 236.6 on 3 and 124 DF,  p-value: < 2.2e-16
## 
## [1] "Point -76 6"
## [1] "N = 186"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-525.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-526.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -154.82  -45.34    1.48   20.43  236.46 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.208e+02  2.533e+01  16.610  < 2e-16 ***
## el          -1.459e-01  3.148e-02  -4.634 6.82e-06 ***
## sl           5.695e-01  4.105e-01   1.387 0.167102    
## I(el^2)      3.731e-05  9.566e-06   3.900 0.000135 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 77.29 on 182 degrees of freedom
## Multiple R-squared:  0.1282,	Adjusted R-squared:  0.1138 
## F-statistic:  8.92 on 3 and 182 DF,  p-value: 1.518e-05
## 
## [1] "Point -76 6.5"
## [1] "N = 371"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-527.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-528.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -140.199  -43.107   -0.105   34.430  177.009 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.480e+02  1.669e+01  20.852  < 2e-16 ***
## el          -6.689e-02  2.147e-02  -3.115 0.001984 ** 
## sl          -3.976e-01  2.558e-01  -1.554 0.121020    
## I(el^2)      2.234e-05  6.153e-06   3.631 0.000322 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 60.52 on 367 degrees of freedom
## Multiple R-squared:  0.05172,	Adjusted R-squared:  0.04397 
## F-statistic: 6.673 on 3 and 367 DF,  p-value: 0.0002136
## 
## [1] "Point -76 7"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-529.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-530.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -143.331  -27.642    4.397   25.673  133.458 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.025e+02  1.241e+01  32.433  < 2e-16 ***
## el          -9.645e-02  1.547e-02  -6.235 1.13e-09 ***
## sl          -2.333e-01  2.053e-01  -1.137    0.256    
## I(el^2)      3.126e-05  4.359e-06   7.171 3.54e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 46.62 on 407 degrees of freedom
## Multiple R-squared:  0.1566,	Adjusted R-squared:  0.1504 
## F-statistic: 25.19 on 3 and 407 DF,  p-value: 5.747e-15
## 
## [1] "Point -76 7.5"
## [1] "N = 398"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-531.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-532.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -78.388 -24.662  -2.337  26.220 113.963 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.975e+02  4.120e+00  72.202  < 2e-16 ***
## el           7.058e-02  8.709e-03   8.105 6.71e-15 ***
## sl           5.583e-02  1.754e-01   0.318     0.75    
## I(el^2)     -1.500e-05  2.979e-06  -5.037 7.21e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34.49 on 394 degrees of freedom
## Multiple R-squared:  0.3584,	Adjusted R-squared:  0.3536 
## F-statistic: 73.38 on 3 and 394 DF,  p-value: < 2.2e-16
## 
## [1] "Point -76 18"
## [1] "N = 38"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-533.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-534.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -60.147 -14.588   6.354  14.698  38.763 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.745e+02  7.009e+00  39.158  < 2e-16 ***
## el           4.603e-01  4.226e-02  10.892 1.25e-12 ***
## sl          -9.874e-02  4.679e-01  -0.211    0.834    
## I(el^2)     -2.764e-04  4.052e-05  -6.820 7.56e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.76 on 34 degrees of freedom
## Multiple R-squared:  0.9283,	Adjusted R-squared:  0.9219 
## F-statistic: 146.7 on 3 and 34 DF,  p-value: < 2.2e-16
## 
## [1] "Point -75.5 6"
## [1] "N = 208"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-535.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-536.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -112.93  -40.53  -19.07   16.61  250.39 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.709e+02  3.180e+01   8.519 3.53e-15 ***
## el           6.948e-02  3.914e-02   1.775   0.0774 .  
## sl          -8.554e-02  4.659e-01  -0.184   0.8545    
## I(el^2)     -2.882e-05  1.188e-05  -2.426   0.0162 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 72.64 on 204 degrees of freedom
## Multiple R-squared:  0.05822,	Adjusted R-squared:  0.04437 
## F-statistic: 4.204 on 3 and 204 DF,  p-value: 0.006524
## 
## [1] "Point -75.5 6.5"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-537.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-538.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -98.32 -50.37 -20.72  60.10 151.07 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.169e+02  2.239e+01   9.686   <2e-16 ***
## el           5.748e-02  2.485e-02   2.313   0.0212 *  
## sl          -4.962e-01  3.023e-01  -1.641   0.1015    
## I(el^2)     -6.551e-06  6.772e-06  -0.967   0.3340    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 62.24 on 411 degrees of freedom
## Multiple R-squared:  0.1144,	Adjusted R-squared:  0.1079 
## F-statistic:  17.7 on 3 and 411 DF,  p-value: 7.999e-11
## 
## [1] "Point -75.5 7"
## [1] "N = 397"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-539.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-540.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -201.249  -38.659    5.669   48.536  182.691 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.843e+02  2.229e+01  21.731  < 2e-16 ***
## el          -8.843e-02  2.664e-02  -3.320 0.000986 ***
## sl          -2.724e-01  3.119e-01  -0.873 0.383010    
## I(el^2)      1.459e-05  7.526e-06   1.939 0.053205 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 73.55 on 393 degrees of freedom
## Multiple R-squared:  0.1248,	Adjusted R-squared:  0.1182 
## F-statistic: 18.69 on 3 and 393 DF,  p-value: 2.377e-11
## 
## [1] "Point -75.5 7.5"
## [1] "N = 365"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-541.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-542.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -152.41  -51.98  -10.27   54.73  193.27 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.295e+02  7.003e+00  47.055   <2e-16 ***
## el           2.051e-01  1.911e-02  10.732   <2e-16 ***
## sl          -4.882e-01  3.243e-01  -1.505    0.133    
## I(el^2)     -7.550e-05  8.559e-06  -8.821   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 66.32 on 361 degrees of freedom
## Multiple R-squared:    0.3,	Adjusted R-squared:  0.2941 
## F-statistic: 51.56 on 3 and 361 DF,  p-value: < 2.2e-16
## 
## [1] "Point -75 6"
## [1] "N = 224"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-543.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-544.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -91.160 -33.138  -8.199  26.798 162.104 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.419e+02  1.004e+01  24.084   <2e-16 ***
## el           3.346e-01  1.932e-02  17.321   <2e-16 ***
## sl           2.489e-01  3.778e-01   0.659    0.511    
## I(el^2)     -1.369e-04  7.457e-06 -18.359   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 46.62 on 220 degrees of freedom
## Multiple R-squared:  0.6424,	Adjusted R-squared:  0.6376 
## F-statistic: 131.8 on 3 and 220 DF,  p-value: < 2.2e-16
## 
## [1] "Point -75 6.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-545.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-546.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -178.14  -19.18   -4.11   15.57  150.91 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.213e+02  1.166e+01  18.979   <2e-16 ***
## el           2.988e-01  1.727e-02  17.308   <2e-16 ***
## sl           1.353e-02  2.517e-01   0.054    0.957    
## I(el^2)     -1.153e-04  5.945e-06 -19.395   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 40.7 on 407 degrees of freedom
## Multiple R-squared:  0.5226,	Adjusted R-squared:  0.5191 
## F-statistic: 148.5 on 3 and 407 DF,  p-value: < 2.2e-16
## 
## [1] "Point -75 7"
## [1] "N = 381"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-547.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-548.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -104.774  -29.164   -1.035   29.621  152.988 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.216e+02  1.021e+01  51.108  < 2e-16 ***
## el          -1.365e-01  1.860e-02  -7.339 1.33e-12 ***
## sl           7.306e-01  2.839e-01   2.573 0.010459 *  
## I(el^2)      2.779e-05  7.992e-06   3.477 0.000565 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 49.21 on 377 degrees of freedom
## Multiple R-squared:  0.4217,	Adjusted R-squared:  0.4171 
## F-statistic: 91.65 on 3 and 377 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74.5 7"
## [1] "N = 389"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-549.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-550.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -106.844  -30.850    0.604   21.553  135.479 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.402e+02  7.511e+00  58.608   <2e-16 ***
## el          -4.240e-02  2.898e-02  -1.463    0.144    
## sl           9.178e-01  3.443e-01   2.666    0.008 ** 
## I(el^2)     -1.268e-05  2.520e-05  -0.503    0.615    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 47.18 on 385 degrees of freedom
## Multiple R-squared:  0.09474,	Adjusted R-squared:  0.08768 
## F-statistic: 13.43 on 3 and 385 DF,  p-value: 2.363e-08
## 
## [1] "Point -74.5 7.5"
## [1] "N = 425"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-551.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-552.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -140.133  -19.681    3.669   21.313   90.718 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.554e+02  5.057e+00 109.814  < 2e-16 ***
## el          -2.013e-01  1.504e-02 -13.386  < 2e-16 ***
## sl           1.523e-01  2.122e-01   0.718 0.473446    
## I(el^2)      4.095e-05  1.051e-05   3.896 0.000114 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 35.78 on 421 degrees of freedom
## Multiple R-squared:  0.6962,	Adjusted R-squared:  0.6941 
## F-statistic: 321.6 on 3 and 421 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74.5 8"
## [1] "N = 409"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-553.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-554.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -157.479  -41.053    5.893   38.885  159.007 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.896e+02  6.380e+00  92.406  < 2e-16 ***
## el          -3.090e-01  2.427e-02 -12.734  < 2e-16 ***
## sl           1.889e+00  3.606e-01   5.239  2.6e-07 ***
## I(el^2)      6.886e-05  1.755e-05   3.924 0.000102 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 64.47 on 405 degrees of freedom
## Multiple R-squared:  0.6992,	Adjusted R-squared:  0.6969 
## F-statistic: 313.7 on 3 and 405 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74 6"
## [1] "N = 219"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-555.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-556.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -42.441 -15.116  -0.406  13.650  56.352 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.443e+02  2.706e+00 127.237   <2e-16 ***
## el           8.131e-02  8.293e-03   9.805   <2e-16 ***
## sl           3.798e-01  1.749e-01   2.171    0.031 *  
## I(el^2)     -3.831e-05  3.356e-06 -11.415   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.56 on 215 degrees of freedom
## Multiple R-squared:  0.4572,	Adjusted R-squared:  0.4496 
## F-statistic: 60.37 on 3 and 215 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74 8"
## [1] "N = 422"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-557.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-558.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -93.839 -23.729   3.859  20.030 289.068 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.398e+02  3.610e+00  94.140  < 2e-16 ***
## el           7.494e-02  1.912e-02   3.920 0.000103 ***
## sl           8.906e-01  3.021e-01   2.948 0.003378 ** 
## I(el^2)     -7.087e-05  1.414e-05  -5.013 7.94e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 42.83 on 418 degrees of freedom
## Multiple R-squared:  0.104,	Adjusted R-squared:  0.09761 
## F-statistic: 16.18 on 3 and 418 DF,  p-value: 5.74e-10
## 
## [1] "Point -74 10.5"
## [1] "N = 405"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-559.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-560.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -131.053  -25.410    0.467   27.169  124.145 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.242e+02  3.191e+00  70.263   <2e-16 ***
## el           1.851e-01  7.188e-03  25.747   <2e-16 ***
## sl          -2.917e-01  2.454e-01  -1.189    0.235    
## I(el^2)     -3.709e-05  1.630e-06 -22.756   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 41.78 on 401 degrees of freedom
## Multiple R-squared:  0.7728,	Adjusted R-squared:  0.7711 
## F-statistic: 454.7 on 3 and 401 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74 11"
## [1] "N = 345"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-561.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-562.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -166.370  -52.101    5.683   46.686  206.806 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.262e+02  6.511e+00  34.741  < 2e-16 ***
## el           1.974e-01  1.175e-02  16.795  < 2e-16 ***
## sl           1.521e+00  3.767e-01   4.038 6.65e-05 ***
## I(el^2)     -4.383e-05  2.608e-06 -16.804  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 70.93 on 341 degrees of freedom
## Multiple R-squared:  0.6512,	Adjusted R-squared:  0.6481 
## F-statistic: 212.2 on 3 and 341 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74 11.5"
## [1] "N = 87"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-563.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-564.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -170.740  -46.421   -2.833   45.716  219.556 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.134e+02  1.744e+01  12.241  < 2e-16 ***
## el           4.183e-01  5.823e-02   7.184 2.67e-10 ***
## sl           6.495e-01  9.697e-01   0.670    0.505    
## I(el^2)     -1.491e-04  3.068e-05  -4.860 5.48e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 90.11 on 83 degrees of freedom
## Multiple R-squared:  0.5819,	Adjusted R-squared:  0.5668 
## F-statistic:  38.5 on 3 and 83 DF,  p-value: 1.087e-15
## 
## [1] "Point -74 18"
## [1] "N = 87"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-565.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-566.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -85.562 -26.382  -9.885  23.970  98.183 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.510e+02  6.898e+00  50.884   <2e-16 ***
## el           7.467e-03  3.434e-02   0.217    0.828    
## sl           1.682e-01  4.615e-01   0.365    0.716    
## I(el^2)     -1.830e-05  2.385e-05  -0.767    0.445    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 35.5 on 83 degrees of freedom
## Multiple R-squared:  0.03236,	Adjusted R-squared:  -0.002619 
## F-statistic: 0.9251 on 3 and 83 DF,  p-value: 0.4324
## 
## [1] "Point -73.5 6"
## [1] "N = 202"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-567.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-568.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -83.547 -16.596   5.763  20.053  52.584 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.874e+02  9.189e+00   31.28   <2e-16 ***
## el           1.645e-01  1.181e-02   13.93   <2e-16 ***
## sl          -1.098e-01  2.072e-01   -0.53    0.597    
## I(el^2)     -6.126e-05  3.790e-06  -16.16   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 26.3 on 198 degrees of freedom
## Multiple R-squared:  0.602,	Adjusted R-squared:  0.596 
## F-statistic: 99.83 on 3 and 198 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73.5 6.5"
## [1] "N = 392"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-569.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-570.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -177.38  -40.46   12.14   42.75   96.17 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.786e+02  9.447e+00  40.074  < 2e-16 ***
## el          -5.616e-02  1.611e-02  -3.486 0.000546 ***
## sl          -6.135e-01  3.583e-01  -1.712 0.087652 .  
## I(el^2)      1.857e-05  6.749e-06   2.751 0.006224 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 62.86 on 388 degrees of freedom
## Multiple R-squared:  0.05231,	Adjusted R-squared:  0.04498 
## F-statistic: 7.138 on 3 and 388 DF,  p-value: 0.0001119
## 
## [1] "Point -73.5 7"
## [1] "N = 374"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-571.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-572.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -141.645  -20.040    3.472   21.356  151.910 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.371e+02  3.979e+00 109.850   <2e-16 ***
## el          -3.441e-01  1.046e-02 -32.911   <2e-16 ***
## sl           7.312e-02  2.702e-01   0.271    0.787    
## I(el^2)      1.156e-04  4.808e-06  24.047   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 40.12 on 370 degrees of freedom
## Multiple R-squared:  0.8188,	Adjusted R-squared:  0.8174 
## F-statistic: 557.4 on 3 and 370 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73.5 8"
## [1] "N = 446"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-573.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-574.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -102.75  -16.44    6.56   25.62   73.29 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.442e+02  2.529e+00 136.116  < 2e-16 ***
## el          -1.168e-01  8.566e-03 -13.641  < 2e-16 ***
## sl           1.227e-01  2.190e-01   0.560    0.576    
## I(el^2)      2.474e-05  3.673e-06   6.735 5.13e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34.72 on 442 degrees of freedom
## Multiple R-squared:  0.6779,	Adjusted R-squared:  0.6757 
## F-statistic: 310.1 on 3 and 442 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73.5 8.5"
## [1] "N = 439"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-575.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-576.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -71.546 -25.491  -4.077  20.392 107.327 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.030e+02  2.930e+00 103.422  < 2e-16 ***
## el          -1.044e-01  9.423e-03 -11.084  < 2e-16 ***
## sl           7.842e-01  1.836e-01   4.270 2.40e-05 ***
## I(el^2)      2.407e-05  4.892e-06   4.919 1.24e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34.37 on 435 degrees of freedom
## Multiple R-squared:  0.4964,	Adjusted R-squared:  0.4929 
## F-statistic: 142.9 on 3 and 435 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73.5 9"
## [1] "N = 406"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-577.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-578.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -94.984 -30.885   2.071  26.969 132.703 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.551e+02  3.604e+00  98.517  < 2e-16 ***
## el          -7.780e-02  1.186e-02  -6.560 1.66e-10 ***
## sl           5.366e-01  2.348e-01   2.285   0.0228 *  
## I(el^2)      1.005e-05  6.023e-06   1.668   0.0960 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 43.02 on 402 degrees of freedom
## Multiple R-squared:  0.3543,	Adjusted R-squared:  0.3495 
## F-statistic: 73.54 on 3 and 402 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73.5 10.5"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-579.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-580.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -74.839 -20.850  -0.431  16.530 108.102 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.743e+02  3.216e+00  54.187   <2e-16 ***
## el           2.148e-01  5.323e-03  40.354   <2e-16 ***
## sl          -2.547e-01  1.863e-01  -1.367    0.172    
## I(el^2)     -4.172e-05  1.157e-06 -36.062   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.2 on 396 degrees of freedom
## Multiple R-squared:  0.8812,	Adjusted R-squared:  0.8803 
## F-statistic: 979.1 on 3 and 396 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73.5 11"
## [1] "N = 374"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-581.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-582.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -172.30  -26.42    3.96   40.91  116.48 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.165e+02  6.634e+00  47.708   <2e-16 ***
## el           8.398e-02  8.922e-03   9.412   <2e-16 ***
## sl           5.526e-01  2.958e-01   1.868   0.0625 .  
## I(el^2)     -1.941e-05  1.945e-06  -9.977   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 57.93 on 370 degrees of freedom
## Multiple R-squared:  0.2904,	Adjusted R-squared:  0.2847 
## F-statistic: 50.48 on 3 and 370 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 6"
## [1] "N = 100"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-583.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-584.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -90.837 -16.755   8.017  18.755  86.743 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.343e+02  1.549e+01  15.126   <2e-16 ***
## el           2.463e-01  1.714e-02  14.370   <2e-16 ***
## sl          -7.781e-01  4.245e-01  -1.833   0.0699 .  
## I(el^2)     -8.662e-05  5.069e-06 -17.087   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.75 on 96 degrees of freedom
## Multiple R-squared:  0.7812,	Adjusted R-squared:  0.7743 
## F-statistic: 114.2 on 3 and 96 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 6.5"
## [1] "N = 184"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-585.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-586.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -141.524  -65.981    2.415   74.730  167.053 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.349e+02  2.926e+01   4.611 7.57e-06 ***
## el           1.577e-01  3.672e-02   4.295 2.86e-05 ***
## sl          -1.184e+00  6.269e-01  -1.888   0.0606 .  
## I(el^2)     -2.794e-05  1.263e-05  -2.211   0.0283 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 79.4 on 180 degrees of freedom
## Multiple R-squared:  0.2214,	Adjusted R-squared:  0.2085 
## F-statistic: 17.07 on 3 and 180 DF,  p-value: 8.482e-10
## 
## [1] "Point -73 7"
## [1] "N = 216"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-587.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-588.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -109.633  -39.598    6.145   39.983  119.983 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.396e+02  1.183e+01  20.265  < 2e-16 ***
## el          -6.083e-02  1.885e-02  -3.227  0.00145 ** 
## sl           3.144e-01  3.347e-01   0.939  0.34864    
## I(el^2)      1.983e-05  7.044e-06   2.815  0.00534 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 48.31 on 212 degrees of freedom
## Multiple R-squared:  0.0506,	Adjusted R-squared:  0.03717 
## F-statistic: 3.766 on 3 and 212 DF,  p-value: 0.01154
## 
## [1] "Point -73 7.5"
## [1] "N = 206"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-589.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-590.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -160.701  -23.634    5.945   33.360   89.938 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.177e+02  8.728e+00  36.399  < 2e-16 ***
## el          -8.656e-02  1.646e-02  -5.259 3.68e-07 ***
## sl           9.698e-01  3.664e-01   2.647  0.00876 ** 
## I(el^2)      8.344e-06  5.421e-06   1.539  0.12532    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 54.32 on 202 degrees of freedom
## Multiple R-squared:  0.4888,	Adjusted R-squared:  0.4812 
## F-statistic: 64.38 on 3 and 202 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 8"
## [1] "N = 215"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-591.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-592.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -128.563  -23.418    5.089   26.576   89.008 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.856e+02  8.064e+00  35.413  < 2e-16 ***
## el          -1.498e-02  1.192e-02  -1.256  0.21039    
## sl           5.183e-01  2.475e-01   2.094  0.03741 *  
## I(el^2)     -1.161e-05  3.792e-06  -3.061  0.00249 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 38.59 on 211 degrees of freedom
## Multiple R-squared:  0.4791,	Adjusted R-squared:  0.4717 
## F-statistic: 64.69 on 3 and 211 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 8.5"
## [1] "N = 218"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-593.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-594.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -211.761  -24.985    1.795   27.021  124.527 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.968e+02  1.157e+01  34.292  < 2e-16 ***
## el          -1.648e-01  2.512e-02  -6.563 3.93e-10 ***
## sl           4.482e-01  3.296e-01   1.360   0.1754    
## I(el^2)      2.850e-05  1.155e-05   2.468   0.0144 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 50.19 on 214 degrees of freedom
## Multiple R-squared:  0.5452,	Adjusted R-squared:  0.5388 
## F-statistic: 85.51 on 3 and 214 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 9"
## [1] "N = 214"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-595.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-596.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -82.028 -32.583  -3.044  27.882 138.705 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.737e+02  8.850e+00  53.524  < 2e-16 ***
## el          -2.145e-01  1.947e-02 -11.015  < 2e-16 ***
## sl           9.318e-02  2.727e-01   0.342    0.733    
## I(el^2)      5.865e-05  9.276e-06   6.323 1.52e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 43.22 on 210 degrees of freedom
## Multiple R-squared:  0.5945,	Adjusted R-squared:  0.5888 
## F-statistic: 102.6 on 3 and 210 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 9.5"
## [1] "N = 202"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-597.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-598.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -79.128 -32.466  -8.528  28.109 148.992 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.140e+02  7.064e+00  44.453  < 2e-16 ***
## el           1.317e-01  1.788e-02   7.366 4.63e-12 ***
## sl           3.822e-01  3.233e-01   1.182    0.239    
## I(el^2)     -6.124e-05  7.617e-06  -8.040 8.04e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 47.81 on 198 degrees of freedom
## Multiple R-squared:  0.2796,	Adjusted R-squared:  0.2687 
## F-statistic: 25.61 on 3 and 198 DF,  p-value: 4.808e-14
## 
## [1] "Point -73 10"
## [1] "N = 232"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-599.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-600.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -68.060 -31.253  -5.717  14.120 186.921 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.291e+02  5.184e+00  44.192  < 2e-16 ***
## el           1.761e-01  1.731e-02  10.174  < 2e-16 ***
## sl          -5.928e-01  3.744e-01  -1.584    0.115    
## I(el^2)     -4.789e-05  7.324e-06  -6.539 4.02e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 46.06 on 228 degrees of freedom
## Multiple R-squared:  0.5765,	Adjusted R-squared:  0.5709 
## F-statistic: 103.5 on 3 and 228 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 10.5"
## [1] "N = 228"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-601.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-602.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -74.378 -17.264  -5.844   9.071 188.039 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.920e+02  5.317e+00  36.107  < 2e-16 ***
## el           1.564e-01  1.815e-02   8.618 1.24e-15 ***
## sl          -2.745e-01  3.628e-01  -0.757   0.4501    
## I(el^2)     -2.425e-05  7.675e-06  -3.159   0.0018 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 41.18 on 224 degrees of freedom
## Multiple R-squared:  0.6964,	Adjusted R-squared:  0.6924 
## F-statistic: 171.3 on 3 and 224 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 11"
## [1] "N = 202"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-603.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-604.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -98.424 -50.668   0.166  39.588 137.960 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.394e+02  6.287e+00  38.084  < 2e-16 ***
## el           8.950e-02  1.757e-02   5.094 8.14e-07 ***
## sl           5.191e-01  5.410e-01   0.959   0.3385    
## I(el^2)     -1.213e-05  5.097e-06  -2.380   0.0183 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 56.08 on 198 degrees of freedom
## Multiple R-squared:  0.4822,	Adjusted R-squared:  0.4743 
## F-statistic: 61.45 on 3 and 198 DF,  p-value: < 2.2e-16
## 
## [1] "Variable prec_11_cl"
## [1] "Point -106 23.5"
## [1] "N = 424"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-605.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-606.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.2900  -3.5653  -0.4449   3.2621  18.5533 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.418e+01  4.752e-01  50.877   <2e-16 ***
## el           2.179e-02  1.397e-03  15.595   <2e-16 ***
## sl           2.551e-02  2.770e-02   0.921    0.358    
## I(el^2)     -6.695e-06  5.556e-07 -12.050   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.97 on 420 degrees of freedom
## Multiple R-squared:  0.6227,	Adjusted R-squared:  0.6201 
## F-statistic: 231.1 on 3 and 420 DF,  p-value: < 2.2e-16
## 
## [1] "Point -106 24"
## [1] "N = 213"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-607.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-608.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.5734  -3.3194  -0.7163   3.7873  19.2005 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.680e+01  1.020e+00  26.264  < 2e-16 ***
## el           1.801e-02  2.185e-03   8.242 1.85e-14 ***
## sl          -6.477e-02  3.424e-02  -1.892   0.0599 .  
## I(el^2)     -5.508e-06  7.529e-07  -7.316 5.37e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.185 on 209 degrees of freedom
## Multiple R-squared:  0.317,	Adjusted R-squared:  0.3072 
## F-statistic: 32.33 on 3 and 209 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105.5 20.5"
## [1] "N = 217"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-609.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-610.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.3202 -2.3740  0.6051  2.0671 11.5082 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.240e+01  4.770e-01  46.953  < 2e-16 ***
## el           1.034e-02  1.397e-03   7.402 3.07e-12 ***
## sl          -5.432e-02  2.271e-02  -2.391   0.0177 *  
## I(el^2)     -1.749e-06  7.540e-07  -2.319   0.0213 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.184 on 213 degrees of freedom
## Multiple R-squared:  0.5832,	Adjusted R-squared:  0.5773 
## F-statistic: 99.34 on 3 and 213 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105.5 23"
## [1] "N = 386"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-611.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-612.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -31.280  -6.474  -0.505   6.965  41.808 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.783e+01  1.120e+00  51.631  < 2e-16 ***
## el          -1.996e-02  3.497e-03  -5.707 2.31e-08 ***
## sl          -8.520e-02  5.587e-02  -1.525    0.128    
## I(el^2)      7.242e-06  1.674e-06   4.327 1.93e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.79 on 382 degrees of freedom
## Multiple R-squared:  0.2177,	Adjusted R-squared:  0.2115 
## F-statistic: 35.43 on 3 and 382 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105.5 23.5"
## [1] "N = 439"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-613.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-614.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.2877  -4.0335  -0.7081   4.4638  16.1052 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.939e+01  1.536e+00  25.648  < 2e-16 ***
## el           3.352e-03  2.123e-03   1.579  0.11501    
## sl           2.144e-02  2.623e-02   0.817  0.41428    
## I(el^2)     -2.007e-06  6.607e-07  -3.038  0.00252 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.209 on 435 degrees of freedom
## Multiple R-squared:  0.1392,	Adjusted R-squared:  0.1332 
## F-statistic: 23.44 on 3 and 435 DF,  p-value: 4.407e-14
## 
## [1] "Point -105.5 24"
## [1] "N = 226"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-615.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-616.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.2645  -6.5802   0.5426   4.8737  15.0731 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.116e+01  1.907e+00  16.340   <2e-16 ***
## el           9.167e-04  2.836e-03   0.323   0.7468    
## sl           9.556e-02  3.788e-02   2.522   0.0124 *  
## I(el^2)     -3.318e-07  8.978e-07  -0.370   0.7120    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.737 on 222 degrees of freedom
## Multiple R-squared:  0.04552,	Adjusted R-squared:  0.03262 
## F-statistic: 3.529 on 3 and 222 DF,  p-value: 0.0157
## 
## [1] "Point -105 20"
## [1] "N = 431"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-617.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-618.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.8481 -2.2773  0.1436  1.9248  7.7414 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.902e+01  3.017e-01  96.182  < 2e-16 ***
## el          -1.219e-03  8.305e-04  -1.468 0.142814    
## sl           5.185e-02  1.552e-02   3.341 0.000908 ***
## I(el^2)      2.311e-06  3.805e-07   6.073 2.77e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.711 on 427 degrees of freedom
## Multiple R-squared:  0.4684,	Adjusted R-squared:  0.4646 
## F-statistic: 125.4 on 3 and 427 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105 20.5"
## [1] "N = 458"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-619.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-620.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.8384  -3.8318   0.8168   4.3550  13.7966 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.240e+01  7.869e-01  28.469   <2e-16 ***
## el           3.315e-03  1.628e-03   2.037   0.0423 *  
## sl          -1.869e-02  2.513e-02  -0.744   0.4573    
## I(el^2)      7.751e-07  7.119e-07   1.089   0.2768    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.65 on 454 degrees of freedom
## Multiple R-squared:  0.2367,	Adjusted R-squared:  0.2317 
## F-statistic: 46.93 on 3 and 454 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105 21"
## [1] "N = 367"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-621.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-622.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.0362 -2.2015 -0.4035  2.1081  9.9041 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.030e+01  4.111e-01   49.38  < 2e-16 ***
## el          -1.694e-02  9.726e-04  -17.42  < 2e-16 ***
## sl           7.622e-02  1.534e-02    4.97 1.03e-06 ***
## I(el^2)      9.639e-06  5.292e-07   18.21  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.102 on 363 degrees of freedom
## Multiple R-squared:  0.4835,	Adjusted R-squared:  0.4793 
## F-statistic: 113.3 on 3 and 363 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105 21.5"
## [1] "N = 374"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-623.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-624.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.9389  -2.8084  -0.2743   2.7195  11.5969 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.050e+01  4.480e-01  45.759  < 2e-16 ***
## el          -1.071e-02  1.559e-03  -6.867 2.79e-11 ***
## sl           4.539e-02  2.419e-02   1.877   0.0613 .  
## I(el^2)      5.244e-06  1.084e-06   4.839 1.92e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.308 on 370 degrees of freedom
## Multiple R-squared:  0.1725,	Adjusted R-squared:  0.1658 
## F-statistic: 25.71 on 3 and 370 DF,  p-value: 3.985e-15
## 
## [1] "Point -105 22"
## [1] "N = 437"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-625.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-626.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -15.002  -4.909  -1.148   4.571  20.502 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.133e+01  5.437e-01  39.226  < 2e-16 ***
## el           8.378e-03  1.994e-03   4.201 3.22e-05 ***
## sl          -2.537e-02  3.638e-02  -0.698    0.486    
## I(el^2)     -4.925e-08  9.569e-07  -0.051    0.959    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.611 on 433 degrees of freedom
## Multiple R-squared:  0.3163,	Adjusted R-squared:  0.3115 
## F-statistic: 66.76 on 3 and 433 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105 22.5"
## [1] "N = 427"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-627.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-628.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.351  -2.673  -0.548   2.744  23.769 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.273e+01  6.532e-01  50.115  < 2e-16 ***
## el           7.620e-03  1.541e-03   4.945 1.10e-06 ***
## sl           1.378e-03  2.616e-02   0.053    0.958    
## I(el^2)     -2.722e-06  6.864e-07  -3.965 8.62e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.537 on 423 degrees of freedom
## Multiple R-squared:  0.09101,	Adjusted R-squared:  0.08456 
## F-statistic: 14.12 on 3 and 423 DF,  p-value: 8.718e-09
## 
## [1] "Point -105 23"
## [1] "N = 417"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-629.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-630.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -19.8036  -5.1635   0.0596   4.9462  17.0264 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.202e+01  1.669e+00  31.163  < 2e-16 ***
## el          -8.957e-03  2.212e-03  -4.049 6.15e-05 ***
## sl          -8.375e-02  3.013e-02  -2.780  0.00569 ** 
## I(el^2)      3.201e-07  7.385e-07   0.433  0.66495    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.041 on 413 degrees of freedom
## Multiple R-squared:  0.329,	Adjusted R-squared:  0.3241 
## F-statistic: 67.49 on 3 and 413 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105 23.5"
## [1] "N = 427"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-631.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-632.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -17.4418  -5.2873  -0.1823   3.9509  20.5661 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.132e+01  4.561e+00   9.059   <2e-16 ***
## el          -6.319e-03  4.622e-03  -1.367   0.1723    
## sl           6.725e-02  2.978e-02   2.259   0.0244 *  
## I(el^2)      5.312e-07  1.162e-06   0.457   0.6478    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.873 on 423 degrees of freedom
## Multiple R-squared:  0.1177,	Adjusted R-squared:  0.1114 
## F-statistic: 18.81 on 3 and 423 DF,  p-value: 1.814e-11
## 
## [1] "Point -104.5 19.5"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-633.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-634.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.8605  -2.6759   0.2815   2.4531   9.4808 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.078e+01  4.804e-01  64.085  < 2e-16 ***
## el          -4.597e-03  1.191e-03  -3.860 0.000132 ***
## sl           6.465e-02  2.302e-02   2.808 0.005233 ** 
## I(el^2)      3.359e-06  5.481e-07   6.128 2.16e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.971 on 396 degrees of freedom
## Multiple R-squared:  0.1868,	Adjusted R-squared:  0.1806 
## F-statistic: 30.31 on 3 and 396 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 20"
## [1] "N = 439"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-635.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-636.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.5346  -2.1993   0.4696   2.8306   9.3834 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.427e+01  8.344e-01  41.073  < 2e-16 ***
## el          -1.869e-02  1.600e-03 -11.683  < 2e-16 ***
## sl           8.870e-02  1.863e-02   4.761 2.62e-06 ***
## I(el^2)      9.604e-06  6.734e-07  14.261  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.786 on 435 degrees of freedom
## Multiple R-squared:  0.4486,	Adjusted R-squared:  0.4448 
## F-statistic:   118 on 3 and 435 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 20.5"
## [1] "N = 433"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-637.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-638.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -14.2611  -4.3819   0.1442   4.7788  18.2645 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.701e-01  3.502e+00   0.220    0.826    
## el           1.869e-02  4.732e-03   3.949 9.17e-05 ***
## sl          -2.510e-02  2.700e-02  -0.930    0.353    
## I(el^2)     -1.477e-06  1.583e-06  -0.933    0.351    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.032 on 429 degrees of freedom
## Multiple R-squared:  0.4238,	Adjusted R-squared:  0.4198 
## F-statistic: 105.2 on 3 and 429 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 21"
## [1] "N = 424"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-639.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-640.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.354 -1.764 -0.512  1.146 10.036 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.000e+01  9.384e-01  21.317   <2e-16 ***
## el          -1.782e-02  1.452e-03 -12.273   <2e-16 ***
## sl          -1.068e-02  1.111e-02  -0.962    0.337    
## I(el^2)      9.718e-06  5.560e-07  17.477   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.654 on 420 degrees of freedom
## Multiple R-squared:  0.663,	Adjusted R-squared:  0.6606 
## F-statistic: 275.4 on 3 and 420 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 21.5"
## [1] "N = 442"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-641.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-642.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.5746 -2.4901 -0.4576  2.1972 15.1412 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.792e+01  8.021e-01  22.345  < 2e-16 ***
## el          -9.274e-03  1.331e-03  -6.966  1.2e-11 ***
## sl           1.906e-02  1.667e-02   1.143    0.254    
## I(el^2)      5.119e-06  5.514e-07   9.284  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.616 on 438 degrees of freedom
## Multiple R-squared:  0.2577,	Adjusted R-squared:  0.2526 
## F-statistic: 50.67 on 3 and 438 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 22"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-643.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-644.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.7404  -4.1810  -0.1494   4.8040  12.8880 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.210e+01  1.227e+00   9.866  < 2e-16 ***
## el           8.809e-03  2.218e-03   3.971 8.37e-05 ***
## sl           1.265e-01  2.813e-02   4.498 8.81e-06 ***
## I(el^2)     -7.145e-07  9.465e-07  -0.755    0.451    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.145 on 437 degrees of freedom
## Multiple R-squared:  0.3063,	Adjusted R-squared:  0.3015 
## F-statistic: 64.31 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 22.5"
## [1] "N = 419"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-645.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-646.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -17.2474  -3.8530   0.6184   5.2353  14.8394 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.860e+00  2.317e+00   4.256 2.58e-05 ***
## el           2.632e-02  3.070e-03   8.575  < 2e-16 ***
## sl           5.000e-02  3.095e-02   1.615    0.107    
## I(el^2)     -8.531e-06  9.751e-07  -8.749  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.716 on 415 degrees of freedom
## Multiple R-squared:  0.165,	Adjusted R-squared:  0.159 
## F-statistic: 27.34 on 3 and 415 DF,  p-value: 3.761e-16
## 
## [1] "Point -104.5 23"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-647.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-648.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.6091  -5.4393  -0.6929   5.4317  13.9853 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.083e+01  3.849e+00   5.413 1.05e-07 ***
## el           7.988e-03  4.000e-03   1.997 0.046478 *  
## sl           1.249e-01  2.996e-02   4.168 3.74e-05 ***
## I(el^2)     -3.744e-06  1.040e-06  -3.599 0.000359 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.717 on 411 degrees of freedom
## Multiple R-squared:  0.244,	Adjusted R-squared:  0.2385 
## F-statistic: 44.23 on 3 and 411 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 23.5"
## [1] "N = 433"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-649.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-650.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -17.6392  -4.5172  -0.5568   5.2309  14.6882 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.166e+01  7.846e+00   4.035 6.47e-05 ***
## el          -2.237e-02  7.300e-03  -3.064  0.00232 ** 
## sl           1.493e-01  2.971e-02   5.026 7.38e-07 ***
## I(el^2)      7.047e-06  1.675e-06   4.207 3.15e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.355 on 429 degrees of freedom
## Multiple R-squared:  0.2538,	Adjusted R-squared:  0.2485 
## F-statistic: 48.63 on 3 and 429 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 24"
## [1] "N = 229"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-651.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-652.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -13.0109  -1.6277  -0.2718   1.5387   7.6735 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.037e+01  1.051e+00   9.870   <2e-16 ***
## el          -1.396e-02  1.223e-03 -11.413   <2e-16 ***
## sl           4.956e-02  2.180e-02   2.273    0.024 *  
## I(el^2)      7.513e-06  4.092e-07  18.363   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.986 on 225 degrees of freedom
## Multiple R-squared:  0.7664,	Adjusted R-squared:  0.7633 
## F-statistic: 246.1 on 3 and 225 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 19"
## [1] "N = 275"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-653.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-654.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.2391  -2.7920  -0.2914   2.6230  14.7231 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.213e+01  5.233e-01  42.292  < 2e-16 ***
## el          -2.109e-03  1.924e-03  -1.096 0.274022    
## sl           9.758e-02  3.027e-02   3.224 0.001421 ** 
## I(el^2)      4.673e-06  1.264e-06   3.696 0.000265 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.479 on 271 degrees of freedom
## Multiple R-squared:  0.2309,	Adjusted R-squared:  0.2224 
## F-statistic: 27.13 on 3 and 271 DF,  p-value: 2.262e-15
## 
## [1] "Point -104 19.5"
## [1] "N = 442"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-655.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-656.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.7911  -3.7343  -0.5074   3.0223  14.1004 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.768e+01  9.732e-01  28.441  < 2e-16 ***
## el          -8.106e-03  1.545e-03  -5.248  2.4e-07 ***
## sl           8.022e-02  2.471e-02   3.247  0.00126 ** 
## I(el^2)      4.644e-06  5.373e-07   8.642  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.201 on 438 degrees of freedom
## Multiple R-squared:  0.3357,	Adjusted R-squared:  0.3312 
## F-statistic: 73.78 on 3 and 438 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 20"
## [1] "N = 458"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-657.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-658.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.9724  -3.2068  -0.1484   3.5778  13.4275 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.278e+01  2.594e+00   8.783  < 2e-16 ***
## el          -5.308e-03  3.341e-03  -1.589   0.1128    
## sl           1.044e-01  2.379e-02   4.390 1.41e-05 ***
## I(el^2)      3.521e-06  1.019e-06   3.456   0.0006 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.915 on 454 degrees of freedom
## Multiple R-squared:  0.2777,	Adjusted R-squared:  0.2729 
## F-statistic: 58.17 on 3 and 454 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 21"
## [1] "N = 467"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-659.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-660.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.8061 -1.3158 -0.0728  1.1771  5.3512 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.374e+01  9.707e-01  14.152  < 2e-16 ***
## el          -8.093e-03  1.324e-03  -6.113 2.07e-09 ***
## sl           8.914e-03  7.128e-03   1.251    0.212    
## I(el^2)      5.268e-06  4.554e-07  11.569  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.769 on 463 degrees of freedom
## Multiple R-squared:  0.6959,	Adjusted R-squared:  0.694 
## F-statistic: 353.2 on 3 and 463 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 21.5"
## [1] "N = 458"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-661.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-662.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.0893 -1.6522 -0.3964  1.5534  8.8122 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.395e+01  1.146e+00  12.174  < 2e-16 ***
## el          -6.552e-03  1.563e-03  -4.191 3.33e-05 ***
## sl           5.498e-03  1.132e-02   0.486    0.627    
## I(el^2)      3.945e-06  5.187e-07   7.606 1.65e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.697 on 454 degrees of freedom
## Multiple R-squared:  0.4921,	Adjusted R-squared:  0.4887 
## F-statistic: 146.6 on 3 and 454 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 22"
## [1] "N = 437"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-663.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-664.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.659  -4.614  -1.322   3.450  16.061 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 9.578e+00  3.173e+00   3.019  0.00269 **
## el          3.887e-03  4.006e-03   0.970  0.33241   
## sl          6.804e-02  2.405e-02   2.829  0.00489 **
## I(el^2)     3.408e-07  1.227e-06   0.278  0.78128   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.686 on 433 degrees of freedom
## Multiple R-squared:  0.1367,	Adjusted R-squared:  0.1308 
## F-statistic: 22.86 on 3 and 433 DF,  p-value: 9.332e-14
## 
## [1] "Point -104 22.5"
## [1] "N = 437"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-665.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-666.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.077  -3.536  -1.602   3.042  14.973 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.498e+01  3.596e+00   4.164 3.77e-05 ***
## el           3.065e-03  4.047e-03   0.758  0.44916    
## sl           8.404e-02  2.506e-02   3.353  0.00087 ***
## I(el^2)     -8.787e-07  1.129e-06  -0.778  0.43683    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.575 on 433 degrees of freedom
## Multiple R-squared:  0.02684,	Adjusted R-squared:  0.02009 
## F-statistic:  3.98 on 3 and 433 DF,  p-value: 0.008131
## 
## [1] "Point -104 23"
## [1] "N = 417"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-667.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-668.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.0938 -1.8130 -0.4121  1.3394 10.7554 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.201e+00  2.629e+00   1.218    0.224    
## el           1.628e-02  2.619e-03   6.214 1.26e-09 ***
## sl           1.180e-02  1.403e-02   0.841    0.401    
## I(el^2)     -5.049e-06  6.376e-07  -7.920 2.23e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.747 on 413 degrees of freedom
## Multiple R-squared:  0.379,	Adjusted R-squared:  0.3744 
## F-statistic:    84 on 3 and 413 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 23.5"
## [1] "N = 396"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-669.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-670.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.5762 -1.5372 -0.1419  1.2367  6.1731 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -2.832e+01  4.831e+00  -5.863 9.67e-09 ***
## el           3.580e-02  4.442e-03   8.061 9.24e-15 ***
## sl           2.248e-02  1.310e-02   1.716    0.087 .  
## I(el^2)     -7.806e-06  1.017e-06  -7.675 1.33e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.178 on 392 degrees of freedom
## Multiple R-squared:  0.1746,	Adjusted R-squared:  0.1683 
## F-statistic: 27.63 on 3 and 392 DF,  p-value: 3.127e-16
## 
## [1] "Point -103.5 18.5"
## [1] "N = 240"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-671.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-672.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.2794  -2.7343   0.2397   3.0059  13.5300 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.969e+01  6.391e-01  30.817  < 2e-16 ***
## el           2.461e-02  1.837e-03  13.396  < 2e-16 ***
## sl           3.470e-02  3.237e-02   1.072 0.284697    
## I(el^2)     -3.294e-06  9.141e-07  -3.603 0.000383 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.355 on 236 degrees of freedom
## Multiple R-squared:  0.864,	Adjusted R-squared:  0.8623 
## F-statistic: 499.9 on 3 and 236 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 19"
## [1] "N = 376"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-673.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-674.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.1899  -4.2218  -0.9604   3.7299  19.4519 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.778e+01  8.218e-01  21.635  < 2e-16 ***
## el          1.048e-02  1.976e-03   5.306 1.93e-07 ***
## sl          1.591e-01  3.424e-02   4.646 4.71e-06 ***
## I(el^2)     5.206e-07  9.744e-07   0.534    0.593    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.325 on 372 degrees of freedom
## Multiple R-squared:  0.5661,	Adjusted R-squared:  0.5626 
## F-statistic: 161.8 on 3 and 372 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 19.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-675.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-676.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.838  -2.737   0.629   2.749  12.542 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.932e+01  1.147e+00  16.848  < 2e-16 ***
## el          1.788e-03  1.589e-03   1.126 0.261024    
## sl          8.851e-02  2.338e-02   3.786 0.000176 ***
## I(el^2)     1.915e-06  5.236e-07   3.657 0.000288 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.378 on 407 degrees of freedom
## Multiple R-squared:  0.5063,	Adjusted R-squared:  0.5027 
## F-statistic: 139.1 on 3 and 407 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 20"
## [1] "N = 432"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-677.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-678.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.2801  -4.6499   0.1218   4.7259  13.2015 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 5.791e+00  4.951e+00   1.170 0.242750    
## el          7.211e-03  5.486e-03   1.315 0.189377    
## sl          1.071e-01  2.890e-02   3.706 0.000239 ***
## I(el^2)     1.066e-06  1.469e-06   0.726 0.468393    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.663 on 428 degrees of freedom
## Multiple R-squared:  0.3977,	Adjusted R-squared:  0.3935 
## F-statistic: 94.22 on 3 and 428 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 21"
## [1] "N = 418"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-679.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-680.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.9845 -0.8159  0.1912  0.8480  5.4570 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.983e+00  1.314e+00   1.509   0.1320    
## el           1.171e-02  1.657e-03   7.065 6.85e-12 ***
## sl           1.334e-02  6.658e-03   2.004   0.0457 *  
## I(el^2)     -2.367e-06  5.257e-07  -4.503 8.72e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.468 on 414 degrees of freedom
## Multiple R-squared:  0.4691,	Adjusted R-squared:  0.4652 
## F-statistic: 121.9 on 3 and 414 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 21.5"
## [1] "N = 427"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-681.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-682.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.7577 -1.5513 -0.1547  1.7468  6.8433 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.466e+01  1.607e+00   9.126  < 2e-16 ***
## el          -6.862e-03  1.934e-03  -3.549 0.000431 ***
## sl           3.973e-02  1.046e-02   3.798 0.000167 ***
## I(el^2)      3.176e-06  5.685e-07   5.587 4.14e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.203 on 423 degrees of freedom
## Multiple R-squared:  0.3568,	Adjusted R-squared:  0.3522 
## F-statistic: 78.22 on 3 and 423 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 22"
## [1] "N = 468"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-683.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-684.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.3805 -1.5259 -0.1342  1.3021  6.6614 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.473e+01  1.825e+00   8.072 5.97e-15 ***
## el          -1.011e-02  1.982e-03  -5.104 4.86e-07 ***
## sl           5.154e-02  8.702e-03   5.922 6.19e-09 ***
## I(el^2)      4.698e-06  5.353e-07   8.776  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.029 on 464 degrees of freedom
## Multiple R-squared:  0.6201,	Adjusted R-squared:  0.6176 
## F-statistic: 252.4 on 3 and 464 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 18"
## [1] "N = 113"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-685.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-686.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.9545 -1.8481 -0.1571  1.7977  5.9642 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.357e+01  6.733e-01  34.998  < 2e-16 ***
## el           1.848e-02  2.084e-03   8.867 1.62e-14 ***
## sl           3.057e-02  2.967e-02   1.030    0.305    
## I(el^2)     -1.013e-06  1.376e-06  -0.736    0.464    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.69 on 109 degrees of freedom
## Multiple R-squared:  0.876,	Adjusted R-squared:  0.8726 
## F-statistic: 256.6 on 3 and 109 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 18.5"
## [1] "N = 392"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-687.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-688.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -17.1013  -4.8299   0.4604   4.7852  20.9643 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 2.335e+01  1.275e+00  18.323  < 2e-16 ***
## el          1.105e-02  2.183e-03   5.063  6.4e-07 ***
## sl          1.160e-01  3.672e-02   3.158  0.00172 ** 
## I(el^2)     1.252e-07  9.014e-07   0.139  0.88963    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.649 on 388 degrees of freedom
## Multiple R-squared:  0.4965,	Adjusted R-squared:  0.4926 
## F-statistic: 127.5 on 3 and 388 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 19"
## [1] "N = 406"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-689.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-690.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.9258  -4.0778  -0.9385   3.4009  16.1577 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.411e+00  1.059e+00   4.166 3.80e-05 ***
## el           3.074e-02  2.179e-03  14.109  < 2e-16 ***
## sl           3.600e-02  3.154e-02   1.141    0.254    
## I(el^2)     -5.646e-06  8.809e-07  -6.409 4.11e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.522 on 402 degrees of freedom
## Multiple R-squared:  0.7695,	Adjusted R-squared:  0.7678 
## F-statistic: 447.4 on 3 and 402 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 19.5"
## [1] "N = 448"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-691.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-692.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.2326  -2.9472   0.2834   2.5487  14.3249 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.812e+00  1.072e+00   1.690   0.0917 .  
## el           3.078e-02  1.835e-03  16.780   <2e-16 ***
## sl           1.690e-02  2.371e-02   0.713   0.4764    
## I(el^2)     -8.832e-06  7.074e-07 -12.486   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.224 on 444 degrees of freedom
## Multiple R-squared:  0.593,	Adjusted R-squared:  0.5903 
## F-statistic: 215.6 on 3 and 444 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 20"
## [1] "N = 409"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-693.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-694.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.704  -4.460  -1.613   4.969  13.524 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.689e+01  9.088e+00   1.858   0.0638 .  
## el          -2.964e-03  1.035e-02  -0.286   0.7748    
## sl           2.150e-01  3.198e-02   6.724 6.03e-11 ***
## I(el^2)      2.141e-06  2.923e-06   0.732   0.4643    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.008 on 405 degrees of freedom
## Multiple R-squared:  0.1626,	Adjusted R-squared:  0.1564 
## F-statistic: 26.22 on 3 and 405 DF,  p-value: 1.61e-15
## 
## [1] "Point -103 21.5"
## [1] "N = 406"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-695.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-696.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.9525 -1.7611  0.1174  1.8113  5.9754 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.139e+01  4.147e+00   7.569 2.59e-13 ***
## el          -2.547e-02  4.270e-03  -5.967 5.32e-09 ***
## sl           6.802e-02  1.339e-02   5.078 5.84e-07 ***
## I(el^2)      8.353e-06  1.087e-06   7.685 1.19e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.521 on 402 degrees of freedom
## Multiple R-squared:  0.5078,	Adjusted R-squared:  0.5042 
## F-statistic: 138.3 on 3 and 402 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 22"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-697.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-698.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.8984 -1.4804 -0.3394  1.3855  6.5028 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.124e+01  4.328e+00   4.907 1.32e-06 ***
## el          -1.507e-02  4.246e-03  -3.549 0.000429 ***
## sl           4.935e-02  1.199e-02   4.115 4.64e-05 ***
## I(el^2)      5.448e-06  1.029e-06   5.294 1.92e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.115 on 430 degrees of freedom
## Multiple R-squared:  0.5432,	Adjusted R-squared:   0.54 
## F-statistic: 170.5 on 3 and 430 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102.5 18"
## [1] "N = 183"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-699.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-700.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.291 -2.759  0.499  2.536  9.395 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 2.091e+01  6.076e-01  34.410  < 2e-16 ***
## el          7.260e-03  1.939e-03   3.745 0.000243 ***
## sl          1.056e-01  3.603e-02   2.930 0.003832 ** 
## I(el^2)     6.744e-07  1.188e-06   0.568 0.570845    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.832 on 179 degrees of freedom
## Multiple R-squared:  0.5164,	Adjusted R-squared:  0.5083 
## F-statistic: 63.72 on 3 and 179 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102.5 18.5"
## [1] "N = 408"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-701.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-702.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.077  -4.480  -1.126   4.246  14.100 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.976e+01  1.145e+00  17.263  < 2e-16 ***
## el          2.258e-04  2.562e-03   0.088    0.930    
## sl          3.261e-02  3.241e-02   1.006    0.315    
## I(el^2)     6.365e-06  1.271e-06   5.006  8.3e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.908 on 404 degrees of freedom
## Multiple R-squared:  0.5047,	Adjusted R-squared:  0.5011 
## F-statistic: 137.2 on 3 and 404 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102.5 19"
## [1] "N = 438"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-703.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-704.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.5291  -3.0178  -0.4897   3.0087  13.8015 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.817e+00  5.899e-01  11.556  < 2e-16 ***
## el           1.616e-02  1.478e-03  10.939  < 2e-16 ***
## sl           1.227e-01  2.248e-02   5.460 8.03e-08 ***
## I(el^2)     -2.762e-06  6.362e-07  -4.342 1.76e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.258 on 434 degrees of freedom
## Multiple R-squared:  0.6819,	Adjusted R-squared:  0.6797 
## F-statistic: 310.1 on 3 and 434 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102.5 19.5"
## [1] "N = 438"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-705.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-706.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.2363 -2.5597 -0.5053  2.1999 14.2658 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.295e+00  6.825e-01  12.153  < 2e-16 ***
## el           1.261e-02  1.044e-03  12.074  < 2e-16 ***
## sl           2.916e-02  2.196e-02   1.328    0.185    
## I(el^2)     -1.929e-06  3.480e-07  -5.544 5.15e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.695 on 434 degrees of freedom
## Multiple R-squared:  0.6688,	Adjusted R-squared:  0.6665 
## F-statistic: 292.1 on 3 and 434 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102.5 20"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-707.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-708.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -8.194 -3.101 -1.147  2.644 16.291 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.331e+01  5.453e+00   2.442    0.015 *  
## el          -1.764e-03  5.944e-03  -0.297    0.767    
## sl           1.517e-01  2.835e-02   5.352  1.4e-07 ***
## I(el^2)      2.489e-06  1.617e-06   1.540    0.124    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.222 on 437 degrees of freedom
## Multiple R-squared:  0.2856,	Adjusted R-squared:  0.2807 
## F-statistic: 58.23 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102 18.5"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-709.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-710.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -8.612 -2.994 -1.088  1.810 41.490 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.412e+01  9.476e-01  14.902   <2e-16 ***
## el           6.297e-03  3.518e-03   1.790   0.0742 .  
## sl          -4.593e-02  2.941e-02  -1.562   0.1191    
## I(el^2)      1.263e-06  2.514e-06   0.503   0.6156    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.397 on 396 degrees of freedom
## Multiple R-squared:  0.1525,	Adjusted R-squared:  0.146 
## F-statistic: 23.74 on 3 and 396 DF,  p-value: 3.762e-14
## 
## [1] "Point -102 19"
## [1] "N = 440"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-711.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-712.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.3343  -2.4341  -0.0882   2.0164  11.8872 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.643e+00  4.685e-01  16.313  < 2e-16 ***
## el           1.413e-02  1.141e-03  12.381  < 2e-16 ***
## sl          -4.834e-02  1.758e-02  -2.749  0.00622 ** 
## I(el^2)     -1.346e-06  5.007e-07  -2.689  0.00745 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.264 on 436 degrees of freedom
## Multiple R-squared:  0.8112,	Adjusted R-squared:  0.8099 
## F-statistic: 624.5 on 3 and 436 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102 19.5"
## [1] "N = 450"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-713.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-714.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.0830 -3.9247  0.0011  3.0779 17.3983 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.097e+01  1.523e+00   7.202 2.54e-12 ***
## el           6.978e-03  1.805e-03   3.866 0.000127 ***
## sl           1.491e-01  3.199e-02   4.661 4.17e-06 ***
## I(el^2)     -5.130e-07  5.290e-07  -0.970 0.332685    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.02 on 446 degrees of freedom
## Multiple R-squared:  0.3419,	Adjusted R-squared:  0.3375 
## F-statistic: 77.24 on 3 and 446 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102 20"
## [1] "N = 433"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-715.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-716.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.4905 -2.6916 -0.7148  1.8545 19.9864 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -3.375e+00  6.369e+00  -0.530 0.596389    
## el           7.662e-03  6.057e-03   1.265 0.206571    
## sl           1.167e-01  3.041e-02   3.839 0.000142 ***
## I(el^2)      1.370e-06  1.408e-06   0.973 0.331202    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.818 on 429 degrees of freedom
## Multiple R-squared:  0.6081,	Adjusted R-squared:  0.6053 
## F-statistic: 221.9 on 3 and 429 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101.5 17.5"
## [1] "N = 152"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-717.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-718.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.4693 -1.6513 -0.3645  2.0657  6.4279 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.563e+01  4.515e-01  56.773   <2e-16 ***
## el          -3.290e-03  1.950e-03  -1.688   0.0936 .  
## sl           1.223e-03  2.854e-02   0.043   0.9659    
## I(el^2)      2.082e-06  1.508e-06   1.380   0.1696    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.848 on 148 degrees of freedom
## Multiple R-squared:  0.0269,	Adjusted R-squared:  0.007176 
## F-statistic: 1.364 on 3 and 148 DF,  p-value: 0.2562
## 
## [1] "Point -101.5 18"
## [1] "N = 376"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-719.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-720.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.555 -2.987 -0.620  2.136 32.670 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.939e+01  6.621e-01   44.40   <2e-16 ***
## el          -2.168e-02  1.524e-03  -14.22   <2e-16 ***
## sl           5.615e-03  2.550e-02    0.22    0.826    
## I(el^2)      9.705e-06  7.272e-07   13.35   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.886 on 372 degrees of freedom
## Multiple R-squared:  0.3729,	Adjusted R-squared:  0.3678 
## F-statistic: 73.73 on 3 and 372 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101.5 18.5"
## [1] "N = 426"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-721.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-722.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.284  -2.228  -0.565   0.920  41.135 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.749e+01  8.306e-01  21.054  < 2e-16 ***
## el          -7.033e-03  1.948e-03  -3.609 0.000344 ***
## sl          -1.633e-02  2.571e-02  -0.635 0.525635    
## I(el^2)      5.588e-06  9.373e-07   5.962 5.27e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.278 on 422 degrees of freedom
## Multiple R-squared:  0.1528,	Adjusted R-squared:  0.1468 
## F-statistic: 25.37 on 3 and 422 DF,  p-value: 4.157e-15
## 
## [1] "Point -101.5 19"
## [1] "N = 468"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-723.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-724.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -7.248 -3.349 -1.684  1.529 28.122 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.553e+01  1.006e+00  15.435  < 2e-16 ***
## el           4.357e-03  1.634e-03   2.667  0.00792 ** 
## sl          -8.259e-02  2.708e-02  -3.049  0.00242 ** 
## I(el^2)      1.048e-06  5.649e-07   1.855  0.06428 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.585 on 464 degrees of freedom
## Multiple R-squared:  0.4215,	Adjusted R-squared:  0.4178 
## F-statistic: 112.7 on 3 and 464 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101.5 19.5"
## [1] "N = 460"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-725.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-726.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.0459  -3.5163  -0.7136   2.7332  15.3648 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.652e+01  3.075e+00  11.877  < 2e-16 ***
## el          -2.254e-02  3.196e-03  -7.051 6.62e-12 ***
## sl           5.199e-02  2.745e-02   1.894   0.0588 .  
## I(el^2)      7.215e-06  8.322e-07   8.670  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.008 on 456 degrees of freedom
## Multiple R-squared:  0.2739,	Adjusted R-squared:  0.2692 
## F-statistic: 57.35 on 3 and 456 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101 17.5"
## [1] "N = 314"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-727.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-728.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.2739  -2.9040   0.4853   3.1038   8.9150 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.578e+01  5.274e-01  48.876  < 2e-16 ***
## el          -6.291e-03  1.247e-03  -5.044 7.79e-07 ***
## sl           3.509e-02  2.741e-02   1.280    0.201    
## I(el^2)      4.490e-06  5.139e-07   8.737  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.488 on 310 degrees of freedom
## Multiple R-squared:  0.3923,	Adjusted R-squared:  0.3865 
## F-statistic: 66.72 on 3 and 310 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101 18"
## [1] "N = 389"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-729.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-730.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -8.492 -4.590 -1.167  3.774 13.954 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.555e+01  1.132e+00  13.734  < 2e-16 ***
## el          -8.962e-03  2.044e-03  -4.385  1.5e-05 ***
## sl           5.168e-02  2.675e-02   1.932   0.0541 .  
## I(el^2)      7.419e-06  7.690e-07   9.649  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.5 on 385 degrees of freedom
## Multiple R-squared:  0.5885,	Adjusted R-squared:  0.5853 
## F-statistic: 183.6 on 3 and 385 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101 19"
## [1] "N = 456"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-731.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-732.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -14.624  -8.203  -3.745   5.303  60.873 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.870e+00  3.127e+00   0.598     0.55    
## el           4.828e-02  6.342e-03   7.613 1.57e-13 ***
## sl          -4.179e-03  6.609e-02  -0.063     0.95    
## I(el^2)     -2.020e-05  2.707e-06  -7.461 4.43e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.42 on 452 degrees of freedom
## Multiple R-squared:  0.1178,	Adjusted R-squared:  0.1119 
## F-statistic: 20.11 on 3 and 452 DF,  p-value: 2.985e-12
## 
## [1] "Point -101 19.5"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-733.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-734.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -26.440  -4.394  -0.087   3.635  37.399 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.090e+02  2.982e+00  36.539  < 2e-16 ***
## el          -9.927e-02  3.508e-03 -28.298  < 2e-16 ***
## sl           1.007e-01  3.473e-02   2.901  0.00392 ** 
## I(el^2)      2.616e-05  1.013e-06  25.821  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.029 on 411 degrees of freedom
## Multiple R-squared:  0.7033,	Adjusted R-squared:  0.7012 
## F-statistic: 324.8 on 3 and 411 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101 23"
## [1] "N = 430"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-735.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-736.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.6678 -0.8817  0.0417  0.7969  4.0253 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.166e+01  2.665e+00  -4.375 1.53e-05 ***
## el           2.052e-02  2.827e-03   7.257 1.89e-12 ***
## sl          -2.619e-02  9.528e-03  -2.749  0.00623 ** 
## I(el^2)     -4.614e-06  7.449e-07  -6.194 1.38e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.389 on 426 degrees of freedom
## Multiple R-squared:  0.2912,	Adjusted R-squared:  0.2862 
## F-statistic: 58.35 on 3 and 426 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101 23.5"
## [1] "N = 446"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-737.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-738.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.5982 -1.2913  0.1845  1.3419  5.5971 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.877e+01  2.646e+00  10.875  < 2e-16 ***
## el          -2.221e-02  2.695e-03  -8.239 1.99e-15 ***
## sl           3.968e-03  1.033e-02   0.384    0.701    
## I(el^2)      6.303e-06  6.845e-07   9.208  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.626 on 442 degrees of freedom
## Multiple R-squared:  0.3169,	Adjusted R-squared:  0.3123 
## F-statistic: 68.36 on 3 and 442 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 17"
## [1] "N = 164"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-739.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-740.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.9553  -3.1431  -0.1284   2.0589  13.3590 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.279e+01  6.452e-01  35.327  < 2e-16 ***
## el           2.499e-02  2.938e-03   8.509  1.2e-14 ***
## sl          -1.464e-01  4.589e-02  -3.190  0.00171 ** 
## I(el^2)     -6.969e-06  2.223e-06  -3.135  0.00205 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.608 on 160 degrees of freedom
## Multiple R-squared:  0.627,	Adjusted R-squared:   0.62 
## F-statistic: 89.67 on 3 and 160 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 17.5"
## [1] "N = 390"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-741.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-742.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -14.597  -5.923  -0.607   4.698  27.178 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.811e+01  1.083e+00  25.957  < 2e-16 ***
## el          -8.051e-03  1.935e-03  -4.162 3.90e-05 ***
## sl          -8.016e-03  4.313e-02  -0.186    0.853    
## I(el^2)      5.104e-06  6.983e-07   7.309 1.56e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.099 on 386 degrees of freedom
## Multiple R-squared:  0.2526,	Adjusted R-squared:  0.2468 
## F-statistic: 43.48 on 3 and 386 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 18"
## [1] "N = 399"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-743.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-744.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.891  -2.508  -0.182   2.097  17.563 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.179e+01  6.199e-01  19.015  < 2e-16 ***
## el          -7.838e-03  1.342e-03  -5.841 1.08e-08 ***
## sl           8.080e-03  2.073e-02   0.390    0.697    
## I(el^2)      7.875e-06  4.956e-07  15.889  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.752 on 395 degrees of freedom
## Multiple R-squared:  0.8181,	Adjusted R-squared:  0.8167 
## F-statistic: 592.1 on 3 and 395 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 18.5"
## [1] "N = 435"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-745.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-746.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -15.292  -1.703  -0.222   1.648  15.069 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.635e+00  6.155e-01  14.031  < 2e-16 ***
## el           2.379e-04  1.694e-03   0.140    0.888    
## sl          -1.541e-02  2.059e-02  -0.749    0.455    
## I(el^2)      5.736e-06  8.045e-07   7.129 4.28e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.632 on 431 degrees of freedom
## Multiple R-squared:  0.657,	Adjusted R-squared:  0.6546 
## F-statistic: 275.2 on 3 and 431 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 19"
## [1] "N = 420"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-747.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-748.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -14.149  -7.455  -3.625   3.584  62.381 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.365e+01  2.881e+00   4.738 2.97e-06 ***
## el           1.498e-02  5.124e-03   2.924  0.00364 ** 
## sl           1.169e-01  5.679e-02   2.059  0.04010 *  
## I(el^2)     -5.403e-06  1.954e-06  -2.765  0.00595 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.86 on 416 degrees of freedom
## Multiple R-squared:  0.03868,	Adjusted R-squared:  0.03175 
## F-statistic:  5.58 on 3 and 416 DF,  p-value: 0.0009253
## 
## [1] "Point -100.5 19.5"
## [1] "N = 386"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-749.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-750.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.521  -4.835  -1.181   3.936  46.432 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.747e+01  3.409e+00  19.791   <2e-16 ***
## el          -4.576e-02  3.733e-03 -12.259   <2e-16 ***
## sl           1.095e-01  4.436e-02   2.469    0.014 *  
## I(el^2)      1.040e-05  9.796e-07  10.614   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.812 on 382 degrees of freedom
## Multiple R-squared:  0.3823,	Adjusted R-squared:  0.3774 
## F-statistic:  78.8 on 3 and 382 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 20"
## [1] "N = 413"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-751.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-752.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.7148 -2.7589 -0.7904  1.6445 15.8551 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  2.367e+01  9.132e+00   2.592  0.00990 **
## el          -1.567e-02  7.806e-03  -2.008  0.04534 * 
## sl           5.082e-02  2.418e-02   2.102  0.03620 * 
## I(el^2)      4.938e-06  1.644e-06   3.005  0.00282 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.934 on 409 degrees of freedom
## Multiple R-squared:  0.3086,	Adjusted R-squared:  0.3035 
## F-statistic: 60.85 on 3 and 409 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 22"
## [1] "N = 467"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-753.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-754.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.7214 -1.9796 -0.6236  1.2706  8.9690 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.996e+01  2.413e+00  12.418  < 2e-16 ***
## el          -1.977e-02  3.073e-03  -6.435 3.10e-10 ***
## sl           1.001e-01  1.490e-02   6.719 5.40e-11 ***
## I(el^2)      4.929e-06  9.409e-07   5.238 2.47e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.74 on 463 degrees of freedom
## Multiple R-squared:  0.241,	Adjusted R-squared:  0.2361 
## F-statistic: 49.01 on 3 and 463 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 22.5"
## [1] "N = 426"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-755.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-756.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -6.858 -1.837 -0.182  1.543  7.825 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.567e+01  3.128e+00   8.207 2.78e-15 ***
## el          -1.652e-02  4.005e-03  -4.126 4.45e-05 ***
## sl           6.672e-02  1.444e-02   4.619 5.14e-06 ***
## I(el^2)      4.209e-06  1.259e-06   3.344    9e-04 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.446 on 422 degrees of freedom
## Multiple R-squared:  0.1525,	Adjusted R-squared:  0.1465 
## F-statistic: 25.32 on 3 and 422 DF,  p-value: 4.437e-15
## 
## [1] "Point -100.5 23"
## [1] "N = 458"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-757.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-758.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.4722 -0.9964 -0.2834  0.6886  8.5503 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -7.281e+00  2.021e+00  -3.604 0.000349 ***
## el           1.655e-02  2.423e-03   6.830 2.74e-11 ***
## sl           4.352e-03  8.773e-03   0.496 0.620051    
## I(el^2)     -3.594e-06  7.129e-07  -5.041 6.70e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.697 on 454 degrees of freedom
## Multiple R-squared:  0.3931,	Adjusted R-squared:  0.389 
## F-statistic:    98 on 3 and 454 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 23.5"
## [1] "N = 469"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-759.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-760.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.7742 -0.9343  0.2414  0.8155  7.3696 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.323e+00  1.882e+00   3.359 0.000845 ***
## el           1.741e-03  2.031e-03   0.857 0.391912    
## sl          -4.583e-03  9.063e-03  -0.506 0.613317    
## I(el^2)      3.491e-07  5.459e-07   0.639 0.522842    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.571 on 465 degrees of freedom
## Multiple R-squared:  0.238,	Adjusted R-squared:  0.2331 
## F-statistic: 48.42 on 3 and 465 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 17"
## [1] "N = 258"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-761.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-762.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.425  -4.015  -1.214   2.367  22.750 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.294e+01  6.891e-01  33.287  < 2e-16 ***
## el           8.377e-03  1.786e-03   4.690 4.46e-06 ***
## sl           2.138e-02  4.180e-02   0.512    0.609    
## I(el^2)     -1.578e-06  9.577e-07  -1.648    0.101    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.749 on 254 degrees of freedom
## Multiple R-squared:  0.2639,	Adjusted R-squared:  0.2552 
## F-statistic: 30.35 on 3 and 254 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 17.5"
## [1] "N = 420"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-763.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-764.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -17.3720  -6.3382   0.2323   4.6654  26.3465 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.250e+01  1.998e+00  21.272   <2e-16 ***
## el          -2.750e-02  2.846e-03  -9.662   <2e-16 ***
## sl          -3.273e-02  3.930e-02  -0.833    0.405    
## I(el^2)      1.088e-05  9.019e-07  12.068   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.06 on 416 degrees of freedom
## Multiple R-squared:  0.3492,	Adjusted R-squared:  0.3445 
## F-statistic: 74.41 on 3 and 416 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 18"
## [1] "N = 438"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-765.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-766.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.3934  -2.7506  -0.4721   2.3599  17.8303 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.288e+01  1.018e+00  22.473  < 2e-16 ***
## el          -2.630e-02  1.682e-03 -15.639  < 2e-16 ***
## sl           7.471e-02  2.015e-02   3.709 0.000235 ***
## I(el^2)      1.325e-05  6.211e-07  21.329  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.366 on 434 degrees of freedom
## Multiple R-squared:  0.6788,	Adjusted R-squared:  0.6766 
## F-statistic: 305.7 on 3 and 434 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 18.5"
## [1] "N = 437"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-767.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-768.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.7023  -3.3420  -0.5348   2.2143  25.3620 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.340e+01  1.425e+00   9.406  < 2e-16 ***
## el          -1.054e-02  2.233e-03  -4.721 3.17e-06 ***
## sl           7.097e-02  2.617e-02   2.712  0.00695 ** 
## I(el^2)      7.786e-06  7.827e-07   9.947  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.617 on 433 degrees of freedom
## Multiple R-squared:  0.5844,	Adjusted R-squared:  0.5815 
## F-statistic: 202.9 on 3 and 433 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 19"
## [1] "N = 436"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-769.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-770.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.4140  -4.6344  -0.6394   4.0408  21.8831 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.558e+00  2.206e+00   2.066   0.0394 *  
## el           1.440e-02  2.119e-03   6.793 3.64e-11 ***
## sl           1.518e-01  2.669e-02   5.688 2.37e-08 ***
## I(el^2)     -2.828e-06  4.917e-07  -5.752 1.67e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.82 on 432 degrees of freedom
## Multiple R-squared:  0.1801,	Adjusted R-squared:  0.1744 
## F-statistic: 31.62 on 3 and 432 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 20"
## [1] "N = 424"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-771.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-772.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.4957 -1.5903 -0.0824  1.5705  6.0958 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  5.962e+00  7.307e+00   0.816  0.41500   
## el           6.944e-04  5.845e-03   0.119  0.90550   
## sl          -4.161e-02  1.379e-02  -3.017  0.00271 **
## I(el^2)      1.207e-06  1.164e-06   1.037  0.30032   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.043 on 420 degrees of freedom
## Multiple R-squared:  0.3906,	Adjusted R-squared:  0.3862 
## F-statistic: 89.72 on 3 and 420 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 20.5"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-773.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-774.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.8525 -1.1173 -0.0856  0.8947  6.0708 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  1.414e+01  4.391e+00   3.220  0.00138 **
## el          -4.865e-03  4.043e-03  -1.203  0.22958   
## sl          -6.375e-03  9.759e-03  -0.653  0.51396   
## I(el^2)      1.872e-06  9.238e-07   2.027  0.04331 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.524 on 430 degrees of freedom
## Multiple R-squared:  0.2401,	Adjusted R-squared:  0.2348 
## F-statistic: 45.28 on 3 and 430 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 21"
## [1] "N = 445"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-775.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-776.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.6590 -1.9790 -0.6992  1.3273 13.8581 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.887e+01  3.535e+00   5.337 1.52e-07 ***
## el          -1.422e-02  3.474e-03  -4.093 5.05e-05 ***
## sl           1.305e-01  1.562e-02   8.355 8.57e-16 ***
## I(el^2)      5.338e-06  8.592e-07   6.213 1.20e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.501 on 441 degrees of freedom
## Multiple R-squared:  0.4116,	Adjusted R-squared:  0.4076 
## F-statistic: 102.8 on 3 and 441 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 21.5"
## [1] "N = 454"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-777.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-778.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.7877 -2.6193 -0.2163  2.3478 11.2259 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.772e+01  1.881e+00  25.376   <2e-16 ***
## el          -4.149e-02  2.518e-03 -16.475   <2e-16 ***
## sl          -2.058e-02  1.577e-02  -1.305    0.193    
## I(el^2)      1.281e-05  7.816e-07  16.393   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.483 on 450 degrees of freedom
## Multiple R-squared:  0.3939,	Adjusted R-squared:  0.3899 
## F-statistic: 97.49 on 3 and 450 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 23"
## [1] "N = 469"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-779.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-780.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.4671 -2.0890 -0.5559  1.5979  9.4599 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.241e+01  2.331e+00  13.899  < 2e-16 ***
## el          -2.916e-02  2.983e-03  -9.776  < 2e-16 ***
## sl           6.348e-02  1.625e-02   3.905 0.000108 ***
## I(el^2)      9.568e-06  9.054e-07  10.568  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.079 on 465 degrees of freedom
## Multiple R-squared:  0.2503,	Adjusted R-squared:  0.2455 
## F-statistic: 51.76 on 3 and 465 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 23.5"
## [1] "N = 445"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-781.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-782.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.4265 -1.0843 -0.0543  1.2346  5.8785 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.548e+01  1.662e+00  15.330  < 2e-16 ***
## el          -2.161e-02  1.673e-03 -12.919  < 2e-16 ***
## sl           4.709e-02  9.070e-03   5.192 3.19e-07 ***
## I(el^2)      7.524e-06  4.071e-07  18.484  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.91 on 441 degrees of freedom
## Multiple R-squared:  0.8176,	Adjusted R-squared:  0.8163 
## F-statistic: 658.8 on 3 and 441 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 24"
## [1] "N = 233"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-783.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-784.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.5885 -1.7927 -0.1855  1.4115 12.4115 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.559e+01  7.071e-01  22.046  < 2e-16 ***
## el          -1.320e-02  8.091e-04 -16.311  < 2e-16 ***
## sl           7.479e-02  2.042e-02   3.662  0.00031 ***
## I(el^2)      5.844e-06  2.600e-07  22.477  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.941 on 229 degrees of freedom
## Multiple R-squared:  0.7865,	Adjusted R-squared:  0.7837 
## F-statistic: 281.2 on 3 and 229 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 17"
## [1] "N = 351"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-785.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-786.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.6680  -4.3395  -0.7101   3.2824  26.5854 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 2.047e+01  6.435e-01  31.813  < 2e-16 ***
## el          3.393e-04  2.010e-03   0.169  0.86609    
## sl          1.070e-01  4.120e-02   2.596  0.00983 ** 
## I(el^2)     2.407e-06  1.065e-06   2.260  0.02444 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.275 on 347 degrees of freedom
## Multiple R-squared:  0.164,	Adjusted R-squared:  0.1568 
## F-statistic: 22.69 on 3 and 347 DF,  p-value: 1.938e-13
## 
## [1] "Point -99.5 17.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-787.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-788.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -17.2518  -4.3163  -0.2881   3.7271  20.0902 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.990e+01  1.708e+00  23.356   <2e-16 ***
## el          -3.298e-02  2.546e-03 -12.956   <2e-16 ***
## sl           2.724e-02  3.350e-02   0.813    0.417    
## I(el^2)      1.336e-05  9.145e-07  14.607   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.658 on 407 degrees of freedom
## Multiple R-squared:  0.3698,	Adjusted R-squared:  0.3651 
## F-statistic:  79.6 on 3 and 407 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 18"
## [1] "N = 444"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-789.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-790.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.2518 -2.4302 -0.5723  1.9082 15.4955 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.968e+01  1.395e+00  14.104  < 2e-16 ***
## el          -2.002e-02  2.397e-03  -8.351 8.86e-16 ***
## sl           1.006e-02  1.827e-02   0.550    0.582    
## I(el^2)      1.133e-05  9.588e-07  11.812  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.683 on 440 degrees of freedom
## Multiple R-squared:  0.5071,	Adjusted R-squared:  0.5037 
## F-statistic: 150.9 on 3 and 440 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 18.5"
## [1] "N = 437"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-791.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-792.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.0838  -2.2780  -0.0928   2.3191  11.1461 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.057e+01  1.557e+00   6.788 3.75e-11 ***
## el          -4.781e-03  2.244e-03  -2.130   0.0337 *  
## sl          -1.510e-02  1.609e-02  -0.939   0.3485    
## I(el^2)      5.021e-06  7.480e-07   6.713 5.98e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.303 on 433 degrees of freedom
## Multiple R-squared:  0.6439,	Adjusted R-squared:  0.6415 
## F-statistic:   261 on 3 and 433 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 19"
## [1] "N = 403"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-793.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-794.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.2664  -3.1936  -0.8782   2.1459  23.1967 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.022e+00  2.120e+00   1.425  0.15480    
## el           9.689e-03  2.004e-03   4.835 1.90e-06 ***
## sl           1.051e-01  2.379e-02   4.420 1.27e-05 ***
## I(el^2)     -1.335e-06  4.460e-07  -2.992  0.00294 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.933 on 399 degrees of freedom
## Multiple R-squared:  0.2841,	Adjusted R-squared:  0.2787 
## F-statistic: 52.77 on 3 and 399 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 20"
## [1] "N = 396"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-795.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-796.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.6866 -1.1921  0.0235  1.1543  6.5151 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 5.585e+00  4.680e+00   1.193   0.2334  
## el          3.858e-03  3.735e-03   1.033   0.3023  
## sl          2.720e-02  1.345e-02   2.022   0.0438 *
## I(el^2)     3.844e-08  7.398e-07   0.052   0.9586  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.789 on 392 degrees of freedom
## Multiple R-squared:  0.3155,	Adjusted R-squared:  0.3103 
## F-statistic: 60.24 on 3 and 392 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 20.5"
## [1] "N = 425"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-797.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-798.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.3301 -1.6634 -0.4665  1.0144 14.7795 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.520e+01  4.675e+00   7.529 3.14e-13 ***
## el          -2.670e-02  4.423e-03  -6.037 3.46e-09 ***
## sl           1.160e-01  1.341e-02   8.654  < 2e-16 ***
## I(el^2)      7.603e-06  1.049e-06   7.247 2.05e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.922 on 421 degrees of freedom
## Multiple R-squared:  0.3905,	Adjusted R-squared:  0.3862 
## F-statistic: 89.92 on 3 and 421 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 21"
## [1] "N = 466"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-799.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-800.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -20.058  -6.755  -3.422   3.456  40.220 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.496e+01  4.786e+00  15.662  < 2e-16 ***
## el          -5.900e-02  5.595e-03 -10.545  < 2e-16 ***
## sl           1.349e-01  4.267e-02   3.162  0.00167 ** 
## I(el^2)      1.472e-05  1.638e-06   8.985  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.01 on 462 degrees of freedom
## Multiple R-squared:  0.2797,	Adjusted R-squared:  0.275 
## F-statistic: 59.79 on 3 and 462 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 21.5"
## [1] "N = 458"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-801.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-802.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -33.436  -9.921  -5.075   3.409  61.369 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.809e+01  3.713e+00  21.030   <2e-16 ***
## el          -7.182e-02  5.758e-03 -12.473   <2e-16 ***
## sl           1.044e-01  6.759e-02   1.545    0.123    
## I(el^2)      2.270e-05  2.083e-06  10.895   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.06 on 454 degrees of freedom
## Multiple R-squared:  0.2785,	Adjusted R-squared:  0.2737 
## F-statistic: 58.42 on 3 and 454 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 22"
## [1] "N = 474"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-803.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-804.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.575  -6.706  -1.542   6.343  32.566 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.851e+01  1.710e+00  34.227  < 2e-16 ***
## el          -5.525e-02  4.557e-03 -12.122  < 2e-16 ***
## sl           3.495e-01  4.976e-02   7.024 7.60e-12 ***
## I(el^2)      1.570e-05  2.774e-06   5.659 2.66e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.651 on 470 degrees of freedom
## Multiple R-squared:  0.6425,	Adjusted R-squared:  0.6402 
## F-statistic: 281.5 on 3 and 470 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 22.5"
## [1] "N = 430"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-805.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-806.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -14.336  -5.166  -1.083   3.944  31.821 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.125e+01  1.404e+00  29.375  < 2e-16 ***
## el          -3.002e-02  3.598e-03  -8.345 1.00e-15 ***
## sl           3.329e-01  4.228e-02   7.875 2.86e-14 ***
## I(el^2)      7.748e-06  2.050e-06   3.779  0.00018 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.419 on 426 degrees of freedom
## Multiple R-squared:  0.4946,	Adjusted R-squared:  0.491 
## F-statistic:   139 on 3 and 426 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 23"
## [1] "N = 456"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-807.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-808.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.1840 -4.1988 -0.7856  2.7427 23.4854 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.787e+01  8.374e-01  45.229  < 2e-16 ***
## el          -2.901e-02  1.569e-03 -18.484  < 2e-16 ***
## sl           1.082e-01  2.503e-02   4.321 1.91e-05 ***
## I(el^2)      8.710e-06  6.831e-07  12.751  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.558 on 452 degrees of freedom
## Multiple R-squared:  0.5612,	Adjusted R-squared:  0.5583 
## F-statistic: 192.7 on 3 and 452 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 23.5"
## [1] "N = 455"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-809.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-810.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.4996 -2.4281 -0.6927  2.0462 13.5799 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.315e+01  7.196e-01  32.165  < 2e-16 ***
## el          -1.588e-02  9.804e-04 -16.192  < 2e-16 ***
## sl           5.326e-02  1.242e-02   4.288 2.21e-05 ***
## I(el^2)      5.973e-06  3.018e-07  19.790  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.217 on 451 degrees of freedom
## Multiple R-squared:  0.5616,	Adjusted R-squared:  0.5587 
## F-statistic: 192.6 on 3 and 451 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 24"
## [1] "N = 224"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-811.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-812.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.2407 -1.7906 -0.2058  1.4200  7.5409 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.224e+01  4.195e-01  53.015   <2e-16 ***
## el          -1.306e-02  8.512e-04 -15.343   <2e-16 ***
## sl           1.646e-02  1.664e-02   0.989    0.324    
## I(el^2)      5.276e-06  2.642e-07  19.969   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.545 on 220 degrees of freedom
## Multiple R-squared:  0.7494,	Adjusted R-squared:  0.746 
## F-statistic: 219.3 on 3 and 220 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99 17"
## [1] "N = 385"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-813.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-814.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -21.4144  -5.0796  -0.5253   4.9121  23.1927 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.606e+01  9.093e-01  17.665  < 2e-16 ***
## el           2.675e-02  2.392e-03  11.185  < 2e-16 ***
## sl          -9.089e-02  5.382e-02  -1.689    0.092 .  
## I(el^2)     -7.711e-06  9.882e-07  -7.803  5.9e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.25 on 381 degrees of freedom
## Multiple R-squared:   0.42,	Adjusted R-squared:  0.4155 
## F-statistic: 91.98 on 3 and 381 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99 17.5"
## [1] "N = 391"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-815.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-816.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.8419  -6.5099  -0.3162   4.8649  31.6061 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.470e+01  3.533e+00   9.820  < 2e-16 ***
## el          -2.858e-02  4.847e-03  -5.896 8.12e-09 ***
## sl           1.004e-01  4.647e-02   2.161   0.0313 *  
## I(el^2)      1.079e-05  1.643e-06   6.566 1.67e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.576 on 387 degrees of freedom
## Multiple R-squared:  0.1233,	Adjusted R-squared:  0.1165 
## F-statistic: 18.15 on 3 and 387 DF,  p-value: 4.898e-11
## 
## [1] "Point -99 18"
## [1] "N = 440"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-817.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-818.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.4846  -2.4276  -0.5139   1.9438  11.4475 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.993e+01  1.816e+00  10.970  < 2e-16 ***
## el          -2.325e-02  2.993e-03  -7.769 5.71e-14 ***
## sl          -6.187e-03  1.695e-02  -0.365    0.715    
## I(el^2)      1.195e-05  1.186e-06  10.074  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.361 on 436 degrees of freedom
## Multiple R-squared:  0.3878,	Adjusted R-squared:  0.3836 
## F-statistic: 92.05 on 3 and 436 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99 19"
## [1] "N = 398"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-819.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-820.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.860 -2.382  0.186  1.912 12.948 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.678e+01  1.339e+00  12.534  < 2e-16 ***
## el          -6.883e-03  1.244e-03  -5.531 5.81e-08 ***
## sl           7.737e-02  1.987e-02   3.894 0.000116 ***
## I(el^2)      2.274e-06  2.743e-07   8.289 1.81e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.32 on 394 degrees of freedom
## Multiple R-squared:  0.4767,	Adjusted R-squared:  0.4727 
## F-statistic: 119.6 on 3 and 394 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99 20.5"
## [1] "N = 455"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-821.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-822.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.350  -8.764  -4.781   4.499  58.672 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.339e+01  1.126e+01   3.855 0.000133 ***
## el          -2.179e-02  1.195e-02  -1.823 0.069013 .  
## sl           2.539e-01  5.556e-02   4.571 6.29e-06 ***
## I(el^2)      5.006e-06  3.186e-06   1.571 0.116803    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.34 on 451 degrees of freedom
## Multiple R-squared:  0.06373,	Adjusted R-squared:  0.0575 
## F-statistic: 10.23 on 3 and 451 DF,  p-value: 1.573e-06
## 
## [1] "Point -99 21"
## [1] "N = 443"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-823.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-824.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -40.708 -15.787  -2.666  16.848  68.287 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.073e+02  3.636e+00  29.520  < 2e-16 ***
## el          -6.451e-02  6.138e-03 -10.511  < 2e-16 ***
## sl           4.307e-02  8.015e-02   0.537    0.591    
## I(el^2)      1.372e-05  2.353e-06   5.830 1.07e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.1 on 439 degrees of freedom
## Multiple R-squared:  0.4803,	Adjusted R-squared:  0.4767 
## F-statistic: 135.2 on 3 and 439 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99 21.5"
## [1] "N = 444"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-825.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-826.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -39.176 -15.774  -1.617  14.541  42.687 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.120e+01  1.716e+00  35.670  < 2e-16 ***
## el          -5.253e-03  5.567e-03  -0.944    0.346    
## sl           3.794e-01  9.650e-02   3.932  9.8e-05 ***
## I(el^2)     -1.852e-06  2.743e-06  -0.675    0.500    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.12 on 440 degrees of freedom
## Multiple R-squared:  0.05897,	Adjusted R-squared:  0.05255 
## F-statistic: 9.191 on 3 and 440 DF,  p-value: 6.57e-06
## 
## [1] "Point -99 23.5"
## [1] "N = 451"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-827.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-828.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.3279 -2.2359 -0.3001  2.1190 14.7590 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.322e+01  4.278e-01  54.275  < 2e-16 ***
## el          -2.112e-02  1.572e-03 -13.438  < 2e-16 ***
## sl           7.278e-02  1.980e-02   3.676 0.000266 ***
## I(el^2)      1.037e-05  8.065e-07  12.863  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.596 on 447 degrees of freedom
## Multiple R-squared:  0.3136,	Adjusted R-squared:  0.309 
## F-statistic: 68.09 on 3 and 447 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 17"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-829.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-830.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.2805  -7.0339  -0.1431   6.0672  27.0626 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.078e+01  1.528e+00   7.057 7.66e-12 ***
## el           3.556e-02  2.788e-03  12.751  < 2e-16 ***
## sl          -2.839e-02  5.816e-02  -0.488    0.626    
## I(el^2)     -1.216e-05  1.043e-06 -11.656  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.994 on 396 degrees of freedom
## Multiple R-squared:  0.3193,	Adjusted R-squared:  0.3141 
## F-statistic: 61.91 on 3 and 396 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 17.5"
## [1] "N = 420"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-831.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-832.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -19.763  -7.722  -3.730   4.071  43.798 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.646e+01  9.170e+00   2.885 0.004116 ** 
## el          -2.573e-02  1.057e-02  -2.434 0.015364 *  
## sl           2.330e-01  6.101e-02   3.819 0.000154 ***
## I(el^2)      9.831e-06  2.978e-06   3.301 0.001048 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.72 on 416 degrees of freedom
## Multiple R-squared:  0.1344,	Adjusted R-squared:  0.1282 
## F-statistic: 21.53 on 3 and 416 DF,  p-value: 5.561e-13
## 
## [1] "Point -98.5 18"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-833.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-834.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -14.1977  -2.9193  -0.3169   2.7152  11.2269 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.861e+01  4.165e+00   9.269  < 2e-16 ***
## el          -3.353e-02  6.371e-03  -5.262 2.25e-07 ***
## sl          -5.518e-02  2.082e-02  -2.650 0.008336 ** 
## I(el^2)      9.084e-06  2.400e-06   3.785 0.000175 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.028 on 430 degrees of freedom
## Multiple R-squared:  0.2925,	Adjusted R-squared:  0.2876 
## F-statistic: 59.26 on 3 and 430 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 18.5"
## [1] "N = 408"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-835.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-836.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.3678 -2.4110 -0.0111  2.3554  9.0529 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.602e+01  2.496e+00  14.435  < 2e-16 ***
## el          -2.948e-02  3.384e-03  -8.712  < 2e-16 ***
## sl          -7.786e-02  1.736e-02  -4.485 9.53e-06 ***
## I(el^2)      9.941e-06  1.109e-06   8.966  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.31 on 404 degrees of freedom
## Multiple R-squared:  0.2015,	Adjusted R-squared:  0.1956 
## F-statistic: 33.98 on 3 and 404 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 19"
## [1] "N = 384"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-837.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-838.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.7577 -1.3868 -0.0388  1.7069 11.6868 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.945e+01  1.545e+00  12.584  < 2e-16 ***
## el          -5.828e-03  1.311e-03  -4.445 1.16e-05 ***
## sl           3.597e-02  1.777e-02   2.024   0.0437 *  
## I(el^2)      1.787e-06  2.737e-07   6.531 2.10e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.632 on 380 degrees of freedom
## Multiple R-squared:  0.3477,	Adjusted R-squared:  0.3426 
## F-statistic: 67.53 on 3 and 380 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 20"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-839.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-840.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -35.294  -6.459  -2.453   4.177  59.902 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.285e+02  1.925e+01   6.675 8.05e-11 ***
## el          -7.564e-02  1.672e-02  -4.523 7.98e-06 ***
## sl           6.428e-01  7.862e-02   8.175 3.70e-15 ***
## I(el^2)      1.234e-05  3.654e-06   3.378 0.000799 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.58 on 411 degrees of freedom
## Multiple R-squared:  0.3384,	Adjusted R-squared:  0.3335 
## F-statistic: 70.06 on 3 and 411 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 20.5"
## [1] "N = 465"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-841.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-842.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -56.012 -14.135  -1.803  13.836  72.279 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.717e+01  4.681e+00  18.623  < 2e-16 ***
## el          -3.757e-02  6.522e-03  -5.760 1.53e-08 ***
## sl           5.052e-01  8.552e-02   5.907 6.78e-09 ***
## I(el^2)      4.985e-06  2.192e-06   2.274   0.0234 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 21.18 on 461 degrees of freedom
## Multiple R-squared:  0.4219,	Adjusted R-squared:  0.4182 
## F-statistic: 112.2 on 3 and 461 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 21"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-843.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-844.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -59.872  -7.529  -1.082  10.720  40.683 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.321e+01  1.433e+00  51.077  < 2e-16 ***
## el           1.528e-02  4.674e-03   3.269  0.00117 ** 
## sl           6.570e-03  7.040e-02   0.093  0.92569    
## I(el^2)     -1.180e-05  2.213e-06  -5.333 1.57e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.74 on 430 degrees of freedom
## Multiple R-squared:  0.156,	Adjusted R-squared:  0.1501 
## F-statistic: 26.49 on 3 and 430 DF,  p-value: 9.686e-16
## 
## [1] "Point -98 16.5"
## [1] "N = 362"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-845.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-846.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.7513  -1.3823  -0.1709   1.4950  17.2610 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.941e+01  3.259e-01  59.549  < 2e-16 ***
## el          -2.283e-03  1.146e-03  -1.992   0.0472 *  
## sl          -1.632e-03  1.836e-02  -0.089   0.9292    
## I(el^2)      5.754e-06  7.542e-07   7.629 2.15e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.76 on 358 degrees of freedom
## Multiple R-squared:  0.424,	Adjusted R-squared:  0.4191 
## F-statistic: 87.83 on 3 and 358 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98 17"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-847.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-848.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.121  -4.784  -1.417   3.442  30.047 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.506e+01  1.456e+00  10.343  < 2e-16 ***
## el           9.831e-03  2.282e-03   4.308 2.07e-05 ***
## sl           1.324e-01  4.200e-02   3.153  0.00173 ** 
## I(el^2)     -3.419e-06  7.389e-07  -4.627 5.00e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.354 on 407 degrees of freedom
## Multiple R-squared:  0.08719,	Adjusted R-squared:  0.08046 
## F-statistic: 12.96 on 3 and 407 DF,  p-value: 4.227e-08
## 
## [1] "Point -98 17.5"
## [1] "N = 422"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-849.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-850.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -14.125  -4.003  -0.461   2.218  26.072 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.753e+01  4.977e+00   3.521 0.000477 ***
## el          -9.250e-03  5.468e-03  -1.692 0.091461 .  
## sl           1.051e-01  3.270e-02   3.215 0.001406 ** 
## I(el^2)      3.207e-06  1.486e-06   2.158 0.031504 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.315 on 418 degrees of freedom
## Multiple R-squared:  0.06484,	Adjusted R-squared:  0.05813 
## F-statistic: 9.661 on 3 and 418 DF,  p-value: 3.534e-06
## 
## [1] "Point -98 18"
## [1] "N = 426"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-851.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-852.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -8.318 -2.205 -0.290  2.152 11.022 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.207e+01  3.013e+00  10.642  < 2e-16 ***
## el          -2.506e-02  3.645e-03  -6.875 2.25e-11 ***
## sl          -7.072e-02  1.796e-02  -3.938 9.62e-05 ***
## I(el^2)      7.077e-06  1.085e-06   6.524 1.96e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.365 on 422 degrees of freedom
## Multiple R-squared:  0.1336,	Adjusted R-squared:  0.1274 
## F-statistic: 21.69 on 3 and 422 DF,  p-value: 4.417e-13
## 
## [1] "Point -98 18.5"
## [1] "N = 413"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-853.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-854.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.0815 -1.9577  0.2123  2.0266  6.9132 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.095e+01  3.576e+00  14.249  < 2e-16 ***
## el          -4.403e-02  4.435e-03  -9.929  < 2e-16 ***
## sl          -5.073e-02  1.763e-02  -2.878  0.00421 ** 
## I(el^2)      1.208e-05  1.333e-06   9.060  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.94 on 409 degrees of freedom
## Multiple R-squared:  0.3025,	Adjusted R-squared:  0.2973 
## F-statistic: 59.11 on 3 and 409 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98 19"
## [1] "N = 392"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-855.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-856.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.6208 -1.7435  0.2831  2.0390  5.8795 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.663e+01  3.004e+00  12.194   <2e-16 ***
## el          -2.345e-02  2.595e-03  -9.038   <2e-16 ***
## sl          -6.525e-03  2.232e-02  -0.292     0.77    
## I(el^2)      5.972e-06  5.673e-07  10.526   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.918 on 388 degrees of freedom
## Multiple R-squared:  0.3307,	Adjusted R-squared:  0.3255 
## F-statistic:  63.9 on 3 and 388 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98 20"
## [1] "N = 452"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-857.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-858.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -53.226 -17.076  -4.011  10.558 135.077 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.304e+02  6.553e+00  19.900  < 2e-16 ***
## el          -4.361e-03  8.583e-03  -0.508   0.6117    
## sl           3.926e-01  1.243e-01   3.158   0.0017 ** 
## I(el^2)     -1.432e-05  2.539e-06  -5.640 3.02e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 28.92 on 448 degrees of freedom
## Multiple R-squared:  0.7038,	Adjusted R-squared:  0.7018 
## F-statistic: 354.8 on 3 and 448 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98 20.5"
## [1] "N = 466"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-859.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-860.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -52.498 -11.570  -3.927  11.335  59.507 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.200e+01  1.921e+00  32.273   <2e-16 ***
## el           7.208e-02  5.384e-03  13.387   <2e-16 ***
## sl          -7.810e-03  9.007e-02  -0.087    0.931    
## I(el^2)     -3.452e-05  2.300e-06 -15.006   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.15 on 462 degrees of freedom
## Multiple R-squared:  0.3844,	Adjusted R-squared:  0.3804 
## F-statistic: 96.17 on 3 and 462 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 16"
## [1] "N = 228"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-861.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-862.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.7343 -1.8160 -0.5196  1.4836  7.8834 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.241e+01  3.931e-01  31.575   <2e-16 ***
## el           1.697e-02  1.181e-03  14.369   <2e-16 ***
## sl           4.128e-03  2.415e-02   0.171    0.864    
## I(el^2)     -5.714e-06  6.115e-07  -9.345   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.902 on 224 degrees of freedom
## Multiple R-squared:  0.6921,	Adjusted R-squared:  0.688 
## F-statistic: 167.8 on 3 and 224 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 16.5"
## [1] "N = 421"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-863.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-864.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.6079 -2.2133 -0.6294  1.5174 15.8343 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.642e+01  6.056e-01  27.121  < 2e-16 ***
## el           8.699e-03  1.011e-03   8.606  < 2e-16 ***
## sl          -3.025e-02  1.737e-02  -1.742   0.0823 .  
## I(el^2)     -1.893e-06  3.734e-07  -5.071 5.96e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.426 on 417 degrees of freedom
## Multiple R-squared:  0.3534,	Adjusted R-squared:  0.3487 
## F-statistic: 75.96 on 3 and 417 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 17"
## [1] "N = 414"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-865.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-866.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.1912  -4.0282  -0.4635   3.3039  14.4834 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.681e+01  1.987e+00   8.462 4.70e-16 ***
## el           3.372e-03  2.319e-03   1.454    0.147    
## sl           1.051e-01  2.620e-02   4.010 7.22e-05 ***
## I(el^2)     -1.146e-06  6.595e-07  -1.738    0.083 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.236 on 410 degrees of freedom
## Multiple R-squared:  0.04981,	Adjusted R-squared:  0.04286 
## F-statistic: 7.164 on 3 and 410 DF,  p-value: 0.0001066
## 
## [1] "Point -97.5 18"
## [1] "N = 412"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-867.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-868.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -8.381 -3.329 -1.240  1.750 37.464 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 6.176e+00  3.415e+00   1.809   0.0712 .
## el          1.899e-03  4.020e-03   0.472   0.6370  
## sl          3.622e-02  2.897e-02   1.250   0.2119  
## I(el^2)     3.247e-07  1.145e-06   0.284   0.7769  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.492 on 408 degrees of freedom
## Multiple R-squared:  0.06766,	Adjusted R-squared:  0.06081 
## F-statistic:  9.87 on 3 and 408 DF,  p-value: 2.692e-06
## 
## [1] "Point -97.5 18.5"
## [1] "N = 424"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-869.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-870.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -19.809  -5.773  -2.163   1.568  53.305 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.574e+01  9.155e+00   2.811 0.005167 ** 
## el          -2.831e-02  9.863e-03  -2.871 0.004304 ** 
## sl           3.338e-01  5.466e-02   6.106 2.32e-09 ***
## I(el^2)      9.952e-06  2.626e-06   3.789 0.000173 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.1 on 420 degrees of freedom
## Multiple R-squared:  0.2221,	Adjusted R-squared:  0.2166 
## F-statistic: 39.98 on 3 and 420 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 19"
## [1] "N = 408"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-871.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-872.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -31.050  -6.406  -0.317   3.838  36.772 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.009e+02  7.966e+00   25.22   <2e-16 ***
## el          -1.527e-01  6.232e-03  -24.50   <2e-16 ***
## sl           6.948e-01  4.856e-02   14.31   <2e-16 ***
## I(el^2)      2.999e-05  1.216e-06   24.66   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.901 on 404 degrees of freedom
## Multiple R-squared:  0.6816,	Adjusted R-squared:  0.6793 
## F-statistic: 288.3 on 3 and 404 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 20"
## [1] "N = 446"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-873.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-874.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -77.723 -20.957  -6.236  17.061 115.792 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.118e+02  3.294e+00   33.92   <2e-16 ***
## el           9.080e-02  7.219e-03   12.58   <2e-16 ***
## sl          -1.845e-01  1.525e-01   -1.21    0.227    
## I(el^2)     -4.685e-05  2.606e-06  -17.98   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 32.58 on 442 degrees of freedom
## Multiple R-squared:  0.6475,	Adjusted R-squared:  0.6452 
## F-statistic: 270.7 on 3 and 442 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 16"
## [1] "N = 331"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-875.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-876.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -13.7697  -4.9230  -0.6344   3.9733  29.8097 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.090e+01  9.789e-01  11.130   <2e-16 ***
## el           1.861e-02  2.137e-03   8.706   <2e-16 ***
## sl           9.848e-02  4.665e-02   2.111   0.0355 *  
## I(el^2)     -8.414e-06  9.219e-07  -9.127   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.029 on 327 degrees of freedom
## Multiple R-squared:  0.2506,	Adjusted R-squared:  0.2437 
## F-statistic: 36.44 on 3 and 327 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 16.5"
## [1] "N = 466"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-877.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-878.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -14.454  -4.751  -1.001   4.278  23.848 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.737e+01  2.698e+00  13.854  < 2e-16 ***
## el          -3.062e-02  3.190e-03  -9.598  < 2e-16 ***
## sl           2.324e-01  3.053e-02   7.613 1.52e-13 ***
## I(el^2)      8.865e-06  9.316e-07   9.515  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.342 on 462 degrees of freedom
## Multiple R-squared:  0.2698,	Adjusted R-squared:  0.2651 
## F-statistic: 56.91 on 3 and 462 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 17"
## [1] "N = 432"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-879.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-880.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.182  -3.564  -1.277   3.234  19.809 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.055e+01  6.435e+00   1.640   0.1018    
## el          -5.306e-03  6.419e-03  -0.827   0.4090    
## sl           1.751e-01  2.706e-02   6.474 2.63e-10 ***
## I(el^2)      3.421e-06  1.544e-06   2.215   0.0273 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.082 on 428 degrees of freedom
## Multiple R-squared:  0.4097,	Adjusted R-squared:  0.4056 
## F-statistic: 99.03 on 3 and 428 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 17.5"
## [1] "N = 416"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-881.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-882.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -27.940 -10.292  -5.439   4.528  70.788 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.188e+01  1.032e+01   2.121   0.0345 *  
## el          -1.100e-02  1.144e-02  -0.961   0.3371    
## sl           5.491e-01  8.416e-02   6.524 2.01e-10 ***
## I(el^2)      3.732e-06  3.142e-06   1.188   0.2357    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 17.67 on 412 degrees of freedom
## Multiple R-squared:  0.09732,	Adjusted R-squared:  0.09074 
## F-statistic: 14.81 on 3 and 412 DF,  p-value: 3.582e-09
## 
## [1] "Point -97 18"
## [1] "N = 417"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-883.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-884.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -98.93 -36.58 -16.41  20.67 221.57 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.062e+02  1.422e+01   7.463 5.06e-13 ***
## el          -8.857e-02  1.971e-02  -4.494 9.08e-06 ***
## sl           1.489e+00  2.348e-01   6.342 5.94e-10 ***
## I(el^2)      2.083e-05  6.397e-06   3.257  0.00122 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 54.8 on 413 degrees of freedom
## Multiple R-squared:  0.1645,	Adjusted R-squared:  0.1585 
## F-statistic: 27.11 on 3 and 413 DF,  p-value: 5.036e-16
## 
## [1] "Point -97 18.5"
## [1] "N = 414"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-885.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-886.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -77.08 -29.18  -3.10  21.15 156.46 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.841e+01  5.994e+00  13.081  < 2e-16 ***
## el          -2.726e-02  9.880e-03  -2.759  0.00606 ** 
## sl           1.301e+00  1.831e-01   7.104 5.38e-12 ***
## I(el^2)     -5.569e-09  3.528e-06  -0.002  0.99874    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 41.31 on 410 degrees of freedom
## Multiple R-squared:  0.2091,	Adjusted R-squared:  0.2033 
## F-statistic: 36.13 on 3 and 410 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 19"
## [1] "N = 428"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-887.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-888.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -44.45 -20.01  -3.67  19.44  90.51 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.232e+01  3.334e+00  15.693  < 2e-16 ***
## el           6.097e-03  4.722e-03   1.291 0.197347    
## sl           5.059e-01  1.118e-01   4.525 7.87e-06 ***
## I(el^2)     -4.472e-06  1.316e-06  -3.399 0.000741 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 24.6 on 424 degrees of freedom
## Multiple R-squared:  0.1491,	Adjusted R-squared:  0.1431 
## F-statistic: 24.77 on 3 and 424 DF,  p-value: 8.706e-15
## 
## [1] "Point -97 19.5"
## [1] "N = 425"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-889.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-890.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -60.229 -27.431  -6.982  12.441 155.078 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.275e+01  6.882e+00   6.211 1.26e-09 ***
## el           3.988e-02  9.408e-03   4.239 2.76e-05 ***
## sl           8.880e-01  1.659e-01   5.354 1.42e-07 ***
## I(el^2)     -1.670e-05  2.763e-06  -6.045 3.29e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 37.97 on 421 degrees of freedom
## Multiple R-squared:  0.2356,	Adjusted R-squared:  0.2302 
## F-statistic: 43.26 on 3 and 421 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 20"
## [1] "N = 393"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-891.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-892.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -80.140 -16.781  -3.162  15.099 120.873 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.172e+02  2.617e+00  44.773  < 2e-16 ***
## el           6.356e-02  7.506e-03   8.467 5.21e-16 ***
## sl          -1.697e-03  1.593e-01  -0.011    0.992    
## I(el^2)     -3.605e-05  3.125e-06 -11.536  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 30.23 on 389 degrees of freedom
## Multiple R-squared:  0.3767,	Adjusted R-squared:  0.3718 
## F-statistic: 78.35 on 3 and 389 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 16"
## [1] "N = 423"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-893.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-894.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -20.315  -7.514  -1.237   5.863  38.377 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.445e+01  1.124e+00  12.855  < 2e-16 ***
## el           1.946e-04  2.032e-03   0.096    0.924    
## sl           4.038e-01  5.044e-02   8.004 1.19e-14 ***
## I(el^2)     -1.037e-06  7.378e-07  -1.405    0.161    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.996 on 419 degrees of freedom
## Multiple R-squared:  0.1484,	Adjusted R-squared:  0.1423 
## F-statistic: 24.33 on 3 and 419 DF,  p-value: 1.562e-14
## 
## [1] "Point -96.5 16.5"
## [1] "N = 422"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-895.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-896.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.3588 -2.3550 -0.5286  1.9510 20.7046 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.212e+01  2.488e+00   8.890  < 2e-16 ***
## el          -2.171e-02  2.626e-03  -8.267 1.84e-15 ***
## sl           1.151e-01  1.730e-02   6.652 9.08e-11 ***
## I(el^2)      7.764e-06  6.716e-07  11.560  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.573 on 418 degrees of freedom
## Multiple R-squared:  0.6087,	Adjusted R-squared:  0.6059 
## F-statistic: 216.8 on 3 and 418 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 17"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-897.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-898.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -37.189 -10.453  -4.659   4.612  74.494 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.120e+01  1.448e+01   2.846 0.004640 ** 
## el          -4.239e-02  1.473e-02  -2.878 0.004196 ** 
## sl           6.404e-01  8.584e-02   7.460 4.83e-13 ***
## I(el^2)      1.412e-05  3.610e-06   3.913 0.000106 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.32 on 430 degrees of freedom
## Multiple R-squared:  0.2623,	Adjusted R-squared:  0.2571 
## F-statistic: 50.96 on 3 and 430 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 17.5"
## [1] "N = 404"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-899.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-900.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -96.158 -25.698  -2.929  25.838 108.830 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.647e+02  7.751e+00  34.156  < 2e-16 ***
## el          -2.067e-01  1.001e-02 -20.639  < 2e-16 ***
## sl           6.686e-01  1.803e-01   3.708 0.000238 ***
## I(el^2)      4.536e-05  3.130e-06  14.491  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 36.56 on 400 degrees of freedom
## Multiple R-squared:  0.6942,	Adjusted R-squared:  0.6919 
## F-statistic: 302.7 on 3 and 400 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 18"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-901.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-902.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -115.21  -41.81  -10.46   41.36  186.81 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.114e+02  4.992e+00  22.316  < 2e-16 ***
## el           5.625e-02  1.325e-02   4.245 2.72e-05 ***
## sl           4.358e-01  2.734e-01   1.594    0.112    
## I(el^2)     -3.500e-05  5.377e-06  -6.509 2.29e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 53.61 on 396 degrees of freedom
## Multiple R-squared:  0.1832,	Adjusted R-squared:  0.177 
## F-statistic:  29.6 on 3 and 396 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 19.5"
## [1] "N = 285"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-903.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-904.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -31.431 -15.215  -5.542   6.623  98.833 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 3.713e+01  2.606e+00  14.250   <2e-16 ***
## el          1.340e-02  8.368e-03   1.601   0.1105    
## sl          4.445e-01  1.439e-01   3.090   0.0022 ** 
## I(el^2)     7.785e-06  4.787e-06   1.626   0.1050    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 22.89 on 281 degrees of freedom
## Multiple R-squared:  0.3085,	Adjusted R-squared:  0.3012 
## F-statistic:  41.8 on 3 and 281 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 20"
## [1] "N = 173"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-905.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-906.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -68.617 -25.088   2.964  23.804  89.654 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.122e+02  4.726e+00  23.732   <2e-16 ***
## el          -8.007e-03  1.914e-02  -0.418    0.676    
## sl          -1.556e-01  3.107e-01  -0.501    0.617    
## I(el^2)     -7.036e-07  9.950e-06  -0.071    0.944    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 36.93 on 169 degrees of freedom
## Multiple R-squared:  0.02822,	Adjusted R-squared:  0.01097 
## F-statistic: 1.636 on 3 and 169 DF,  p-value: 0.183
## 
## [1] "Point -96 16"
## [1] "N = 342"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-907.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-908.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.218  -6.544  -2.185   2.977  40.980 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.004e+01  1.202e+00   8.353 1.74e-15 ***
## el           9.027e-03  2.390e-03   3.778 0.000187 ***
## sl           6.768e-02  5.336e-02   1.268 0.205551    
## I(el^2)     -1.942e-06  8.061e-07  -2.409 0.016536 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.287 on 338 degrees of freedom
## Multiple R-squared:  0.1492,	Adjusted R-squared:  0.1416 
## F-statistic: 19.75 on 3 and 338 DF,  p-value: 8.033e-12
## 
## [1] "Point -96 16.5"
## [1] "N = 414"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-909.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-910.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -20.427  -7.860  -3.288   4.633  50.751 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  5.266e+00  3.004e+00   1.753  0.08036 . 
## el           1.003e-02  4.101e-03   2.445  0.01489 * 
## sl           2.031e-01  6.263e-02   3.242  0.00128 **
## I(el^2)     -1.703e-06  1.327e-06  -1.283  0.20015   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.93 on 410 degrees of freedom
## Multiple R-squared:  0.09058,	Adjusted R-squared:  0.08393 
## F-statistic: 13.61 on 3 and 410 DF,  p-value: 1.755e-08
## 
## [1] "Point -96 17"
## [1] "N = 381"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-911.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-912.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -92.240 -33.106  -5.041  30.474 121.420 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.762e+01  1.154e+01   7.594 2.46e-13 ***
## el          -5.000e-02  1.518e-02  -3.294  0.00108 ** 
## sl           1.278e+00  2.284e-01   5.595 4.25e-08 ***
## I(el^2)      9.234e-06  4.998e-06   1.848  0.06545 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 44.04 on 377 degrees of freedom
## Multiple R-squared:  0.1519,	Adjusted R-squared:  0.1451 
## F-statistic: 22.51 on 3 and 377 DF,  p-value: 2.001e-13
## 
## [1] "Point -96 17.5"
## [1] "N = 408"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-913.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-914.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -94.40 -27.38  -2.11  24.66 107.84 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.228e+02  3.788e+00  32.418  < 2e-16 ***
## el           2.786e-02  9.652e-03   2.886  0.00411 ** 
## sl           5.912e-01  2.058e-01   2.873  0.00428 ** 
## I(el^2)     -2.325e-05  3.763e-06  -6.177  1.6e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 36.17 on 404 degrees of freedom
## Multiple R-squared:  0.263,	Adjusted R-squared:  0.2575 
## F-statistic: 48.05 on 3 and 404 DF,  p-value: < 2.2e-16
## 
## [1] "Point -95.5 16"
## [1] "N = 243"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-915.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-916.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -13.0835  -2.5522  -0.4819   2.2735  11.1336 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.825e+00  4.100e-01  19.085  < 2e-16 ***
## el          -2.547e-03  1.869e-03  -1.363    0.174    
## sl          -1.518e-02  2.660e-02  -0.571    0.569    
## I(el^2)      1.036e-05  1.322e-06   7.838  1.5e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.24 on 239 degrees of freedom
## Multiple R-squared:  0.6299,	Adjusted R-squared:  0.6253 
## F-statistic: 135.6 on 3 and 239 DF,  p-value: < 2.2e-16
## 
## [1] "Point -95.5 16.5"
## [1] "N = 443"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-917.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-918.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -31.003 -14.510  -4.612   6.595  76.268 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.125e+00  2.053e+00   2.009   0.0451 *  
## el           3.453e-02  6.194e-03   5.575 4.34e-08 ***
## sl           1.158e-01  1.078e-01   1.074   0.2835    
## I(el^2)     -5.872e-06  3.390e-06  -1.732   0.0840 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.66 on 439 degrees of freedom
## Multiple R-squared:  0.2996,	Adjusted R-squared:  0.2948 
## F-statistic: 62.58 on 3 and 439 DF,  p-value: < 2.2e-16
## 
## [1] "Point -95.5 17"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-919.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-920.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -72.812 -20.850   2.866  21.493  86.420 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.263e+01  4.495e+00  18.382   <2e-16 ***
## el          -1.054e-02  1.437e-02  -0.734   0.4636    
## sl          -4.049e-01  1.947e-01  -2.080   0.0382 *  
## I(el^2)      1.442e-05  8.429e-06   1.711   0.0879 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.96 on 407 degrees of freedom
## Multiple R-squared:  0.03047,	Adjusted R-squared:  0.02333 
## F-statistic: 4.264 on 3 and 407 DF,  p-value: 0.005559
## 
## [1] "Point -94.5 16.5"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-921.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-922.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -49.033  -8.118  -3.734   1.800 122.380 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.349e+01  1.800e+00   7.496 4.34e-13 ***
## el           1.018e-01  1.118e-02   9.098  < 2e-16 ***
## sl           1.628e-01  1.709e-01   0.952    0.341    
## I(el^2)     -5.197e-05  6.967e-06  -7.459 5.55e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 24.42 on 396 degrees of freedom
## Multiple R-squared:  0.4263,	Adjusted R-squared:  0.4219 
## F-statistic: 98.08 on 3 and 396 DF,  p-value: < 2.2e-16
## 
## [1] "Point -94.5 17"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-923.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-924.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -200.374  -39.584    3.368   51.651  184.033 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.350e+02  7.495e+00  31.350  < 2e-16 ***
## el          -2.499e-01  3.047e-02  -8.200 2.70e-15 ***
## sl           5.272e-01  4.295e-01   1.227     0.22    
## I(el^2)      8.556e-05  1.943e-05   4.403 1.34e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 73.43 on 437 degrees of freedom
## Multiple R-squared:  0.3646,	Adjusted R-squared:  0.3602 
## F-statistic: 83.57 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -94 16.5"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-925.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-926.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -44.600  -8.687  -2.796   3.895  90.768 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.091e+01  1.583e+00  13.212  < 2e-16 ***
## el          -9.385e-03  5.537e-03  -1.695 0.090822 .  
## sl           5.734e-01  8.666e-02   6.617 1.08e-10 ***
## I(el^2)      1.380e-05  3.818e-06   3.615 0.000335 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.4 on 437 degrees of freedom
## Multiple R-squared:  0.2521,	Adjusted R-squared:  0.247 
## F-statistic:  49.1 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -94 17"
## [1] "N = 449"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-927.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-928.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -118.497  -35.912   -1.571   37.004  156.492 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.424e+02  6.844e+00  50.026  < 2e-16 ***
## el          -5.047e-01  2.003e-02 -25.198  < 2e-16 ***
## sl           1.014e+00  2.420e-01   4.191 3.35e-05 ***
## I(el^2)      2.029e-04  1.320e-05  15.380  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 50.68 on 445 degrees of freedom
## Multiple R-squared:  0.7687,	Adjusted R-squared:  0.7671 
## F-statistic: 492.8 on 3 and 445 DF,  p-value: < 2.2e-16
## 
## [1] "Point -93.5 16"
## [1] "N = 330"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-929.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-930.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -14.456  -6.456  -2.474   3.702  32.033 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.857e+01  9.427e-01  30.306  < 2e-16 ***
## el          -2.471e-02  2.980e-03  -8.294 2.93e-15 ***
## sl           2.897e-01  6.132e-02   4.725 3.42e-06 ***
## I(el^2)      1.035e-05  1.859e-06   5.564 5.51e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.969 on 326 degrees of freedom
## Multiple R-squared:  0.2027,	Adjusted R-squared:  0.1954 
## F-statistic: 27.63 on 3 and 326 DF,  p-value: 6.031e-16
## 
## [1] "Point -93.5 16.5"
## [1] "N = 408"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-931.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-932.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -17.098  -6.185  -2.686   2.340  54.335 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.695e+01  3.735e+00   9.894  < 2e-16 ***
## el          -3.888e-02  7.920e-03  -4.909 1.33e-06 ***
## sl          -4.489e-02  6.178e-02  -0.727    0.468    
## I(el^2)      2.189e-05  4.014e-06   5.454 8.59e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.44 on 404 degrees of freedom
## Multiple R-squared:  0.07295,	Adjusted R-squared:  0.06606 
## F-statistic:  10.6 on 3 and 404 DF,  p-value: 1.013e-06
## 
## [1] "Point -93.5 17"
## [1] "N = 416"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-933.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-934.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -212.436  -38.208   -9.917   30.993  236.135 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.013e+02  1.108e+01  27.188  < 2e-16 ***
## el          -5.274e-01  2.730e-02 -19.316  < 2e-16 ***
## sl           1.670e+00  3.012e-01   5.544  5.3e-08 ***
## I(el^2)      2.507e-04  1.667e-05  15.041  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 62.76 on 412 degrees of freedom
## Multiple R-squared:  0.5482,	Adjusted R-squared:  0.5449 
## F-statistic: 166.6 on 3 and 412 DF,  p-value: < 2.2e-16
## 
## [1] "Point -93 15.5"
## [1] "N = 278"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-935.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-936.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -28.963 -10.760  -2.909   9.864  44.170 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.039e+01  1.462e+00  41.295  < 2e-16 ***
## el          -1.699e-03  4.515e-03  -0.376    0.707    
## sl           1.239e-01  1.115e-01   1.111    0.268    
## I(el^2)     -7.742e-06  1.946e-06  -3.978 8.89e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.26 on 274 degrees of freedom
## Multiple R-squared:  0.3583,	Adjusted R-squared:  0.3513 
## F-statistic:    51 on 3 and 274 DF,  p-value: < 2.2e-16
## 
## [1] "Point -93 16"
## [1] "N = 423"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-937.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-938.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -27.989 -11.683  -2.618  10.861  38.257 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.009e+01  2.585e+00  15.509  < 2e-16 ***
## el          -3.115e-02  5.752e-03  -5.416 1.03e-07 ***
## sl           4.755e-01  8.469e-02   5.615 3.59e-08 ***
## I(el^2)      1.085e-05  2.990e-06   3.629  0.00032 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.76 on 419 degrees of freedom
## Multiple R-squared:  0.1142,	Adjusted R-squared:  0.1079 
## F-statistic: 18.01 on 3 and 419 DF,  p-value: 5.173e-11
## 
## [1] "Point -93 17"
## [1] "N = 410"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-939.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-940.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -188.03  -62.12  -21.46   49.19  288.01 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.262e+02  1.843e+01   6.847 2.80e-11 ***
## el          -9.711e-02  3.284e-02  -2.957  0.00328 ** 
## sl           2.559e+00  4.282e-01   5.976 5.02e-09 ***
## I(el^2)      3.008e-05  1.306e-05   2.303  0.02176 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 86.4 on 406 degrees of freedom
## Multiple R-squared:  0.09925,	Adjusted R-squared:  0.09259 
## F-statistic: 14.91 on 3 and 406 DF,  p-value: 3.159e-09
## 
## [1] "Point -93 17.5"
## [1] "N = 430"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-941.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-942.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -197.910  -50.126    1.055   44.150  157.111 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.574e+02  4.982e+00  51.675   <2e-16 ***
## el           2.232e-02  2.484e-02   0.899   0.3694    
## sl           4.146e-01  4.296e-01   0.965   0.3350    
## I(el^2)     -4.508e-05  1.402e-05  -3.216   0.0014 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 67.5 on 426 degrees of freedom
## Multiple R-squared:  0.1255,	Adjusted R-squared:  0.1194 
## F-statistic: 20.38 on 3 and 426 DF,  p-value: 2.321e-12
## 
## [1] "Point -92.5 15"
## [1] "N = 315"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-943.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-944.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -83.565 -32.194  -8.307  19.714 209.664 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.829e+01  3.756e+00  23.507   <2e-16 ***
## el           1.109e-01  1.249e-02   8.879   <2e-16 ***
## sl          -7.621e-01  3.844e-01  -1.983   0.0483 *  
## I(el^2)     -4.807e-05  4.506e-06 -10.668   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 45.98 on 311 degrees of freedom
## Multiple R-squared:  0.2744,	Adjusted R-squared:  0.2674 
## F-statistic:  39.2 on 3 and 311 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92.5 15.5"
## [1] "N = 401"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-945.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-946.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -41.47 -18.45  -6.91  10.44 134.54 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.118e+01  3.269e+00  27.895  < 2e-16 ***
## el          -2.348e-02  6.822e-03  -3.441 0.000641 ***
## sl           2.134e-01  1.516e-01   1.408 0.160026    
## I(el^2)     -2.030e-06  2.558e-06  -0.793 0.428015    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 28.44 on 397 degrees of freedom
## Multiple R-squared:  0.3044,	Adjusted R-squared:  0.2992 
## F-statistic: 57.92 on 3 and 397 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92.5 16.5"
## [1] "N = 449"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-947.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-948.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -40.051 -11.381  -0.515   7.797  61.252 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -3.234e+01  4.559e+00  -7.092 5.22e-12 ***
## el           9.091e-02  8.163e-03  11.137  < 2e-16 ***
## sl           3.231e-01  1.164e-01   2.775  0.00575 ** 
## I(el^2)     -2.317e-05  2.826e-06  -8.199 2.61e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.6 on 445 degrees of freedom
## Multiple R-squared:  0.559,	Adjusted R-squared:  0.556 
## F-statistic:   188 on 3 and 445 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92.5 17"
## [1] "N = 417"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-949.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-950.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -130.699  -31.498   -5.382   22.869  183.982 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.621e+02  1.200e+01  13.512   <2e-16 ***
## el          -9.355e-03  1.798e-02  -0.520   0.6031    
## sl           4.079e-01  3.058e-01   1.334   0.1830    
## I(el^2)     -1.486e-05  6.538e-06  -2.272   0.0236 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 56.57 on 413 degrees of freedom
## Multiple R-squared:  0.2443,	Adjusted R-squared:  0.2388 
## F-statistic: 44.51 on 3 and 413 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92.5 17.5"
## [1] "N = 443"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-951.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-952.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -123.949  -31.103   -3.405   21.672  187.206 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.367e+02  3.785e+00  62.554  < 2e-16 ***
## el          -1.243e-01  1.888e-02  -6.584 1.31e-10 ***
## sl           4.637e-01  3.254e-01   1.425    0.155    
## I(el^2)      7.556e-05  1.247e-05   6.060 2.94e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 49.27 on 439 degrees of freedom
## Multiple R-squared:  0.1084,	Adjusted R-squared:  0.1023 
## F-statistic: 17.79 on 3 and 439 DF,  p-value: 6.532e-11
## 
## [1] "Point -92 15"
## [1] "N = 445"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-953.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-954.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -80.42 -42.26 -12.32  41.68 140.34 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.008e+02  4.739e+00  21.271  < 2e-16 ***
## el           5.324e-02  9.615e-03   5.537  5.3e-08 ***
## sl           2.403e-03  2.574e-01   0.009    0.993    
## I(el^2)     -2.917e-05  3.033e-06  -9.619  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 50.62 on 441 degrees of freedom
## Multiple R-squared:  0.391,	Adjusted R-squared:  0.3869 
## F-statistic: 94.39 on 3 and 441 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92 15.5"
## [1] "N = 395"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-955.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-956.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -38.965 -19.894  -6.513  13.091 136.651 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.188e+01  6.774e+00   9.135   <2e-16 ***
## el          -5.047e-03  9.066e-03  -0.557   0.5781    
## sl           3.361e-01  1.398e-01   2.404   0.0167 *  
## I(el^2)     -1.403e-06  2.325e-06  -0.603   0.5467    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27.67 on 391 degrees of freedom
## Multiple R-squared:  0.08182,	Adjusted R-squared:  0.07477 
## F-statistic: 11.61 on 3 and 391 DF,  p-value: 2.629e-07
## 
## [1] "Point -92 16"
## [1] "N = 391"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-957.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-958.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -61.292 -20.579  -4.111  11.631 125.166 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -3.622e+01  1.031e+01  -3.513 0.000495 ***
## el           1.316e-01  1.764e-02   7.462 5.69e-13 ***
## sl           6.839e-01  2.069e-01   3.305 0.001037 ** 
## I(el^2)     -4.117e-05  6.656e-06  -6.186 1.57e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34.45 on 387 degrees of freedom
## Multiple R-squared:  0.2438,	Adjusted R-squared:  0.2379 
## F-statistic: 41.59 on 3 and 387 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92 16.5"
## [1] "N = 419"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-959.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-960.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -109.812  -20.984   -0.285   22.137   77.515 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.058e+02  1.563e+01  13.166  < 2e-16 ***
## el          -1.374e-01  2.053e-02  -6.692 7.17e-11 ***
## sl           1.251e+00  1.843e-01   6.786 4.00e-11 ***
## I(el^2)      2.840e-05  6.551e-06   4.336 1.82e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34.09 on 415 degrees of freedom
## Multiple R-squared:  0.3708,	Adjusted R-squared:  0.3663 
## F-statistic: 81.53 on 3 and 415 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92 17"
## [1] "N = 378"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-961.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-962.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -84.241 -17.425  -0.615  16.180 121.793 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.845e+02  6.124e+00  30.132  < 2e-16 ***
## el          -4.555e-02  1.222e-02  -3.728 0.000223 ***
## sl           2.379e-01  2.081e-01   1.143 0.253604    
## I(el^2)     -1.569e-06  5.449e-06  -0.288 0.773605    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.69 on 374 degrees of freedom
## Multiple R-squared:  0.367,	Adjusted R-squared:  0.3619 
## F-statistic: 72.28 on 3 and 374 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 14.5"
## [1] "N = 390"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-963.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-964.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -86.62 -35.95  -7.49  34.61 114.51 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.695e+01  3.394e+00  22.671   <2e-16 ***
## el           8.616e-02  9.286e-03   9.278   <2e-16 ***
## sl           2.364e-01  2.731e-01   0.866    0.387    
## I(el^2)     -4.032e-05  3.196e-06 -12.615   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 42.62 on 386 degrees of freedom
## Multiple R-squared:  0.3895,	Adjusted R-squared:  0.3847 
## F-statistic: 82.09 on 3 and 386 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 15"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-965.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-966.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -112.507  -10.751   -2.921    8.510   96.437 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.466e+02  8.531e+00  40.631  < 2e-16 ***
## el          -2.423e-01  8.859e-03 -27.353  < 2e-16 ***
## sl           4.151e-01  1.055e-01   3.934 9.81e-05 ***
## I(el^2)      4.459e-05  2.210e-06  20.174  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 23.33 on 407 degrees of freedom
## Multiple R-squared:  0.8118,	Adjusted R-squared:  0.8104 
## F-statistic: 585.1 on 3 and 407 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 15.5"
## [1] "N = 397"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-967.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-968.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -140.075  -30.310   -5.515   16.749  257.856 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.841e+02  2.434e+01  11.669  < 2e-16 ***
## el          -1.795e-01  2.350e-02  -7.639 1.68e-13 ***
## sl           1.086e+00  2.526e-01   4.299 2.17e-05 ***
## I(el^2)      3.240e-05  5.481e-06   5.912 7.34e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 53.91 on 393 degrees of freedom
## Multiple R-squared:  0.2792,	Adjusted R-squared:  0.2737 
## F-statistic: 50.74 on 3 and 393 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 16"
## [1] "N = 395"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-969.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-970.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -125.329  -33.617   -3.068   26.613  238.858 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.729e+02  9.054e+00  19.095  < 2e-16 ***
## el          -3.789e-02  1.301e-02  -2.912  0.00379 ** 
## sl           1.086e+00  2.663e-01   4.079 5.48e-05 ***
## I(el^2)     -1.555e-06  4.025e-06  -0.386  0.69951    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 58.65 on 391 degrees of freedom
## Multiple R-squared:  0.2669,	Adjusted R-squared:  0.2613 
## F-statistic: 47.45 on 3 and 391 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 16.5"
## [1] "N = 425"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-971.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-972.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -79.174 -20.456  -2.093  20.824 109.038 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.105e+02  6.258e+00  17.664  < 2e-16 ***
## el           1.514e-01  1.375e-02  11.013  < 2e-16 ***
## sl           4.916e-01  1.626e-01   3.022  0.00266 ** 
## I(el^2)     -9.040e-05  6.515e-06 -13.875  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 32.11 on 421 degrees of freedom
## Multiple R-squared:  0.4276,	Adjusted R-squared:  0.4235 
## F-statistic: 104.8 on 3 and 421 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 17"
## [1] "N = 412"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-973.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-974.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -58.592 -10.487   0.532   8.867  66.791 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.444e+02  2.812e+00  51.346  < 2e-16 ***
## el           7.381e-02  9.158e-03   8.060 8.54e-15 ***
## sl           1.503e-01  1.113e-01   1.350    0.178    
## I(el^2)     -4.362e-05  6.507e-06  -6.704 6.75e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.28 on 408 degrees of freedom
## Multiple R-squared:  0.1766,	Adjusted R-squared:  0.1705 
## F-statistic: 29.16 on 3 and 408 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91 14.5"
## [1] "N = 424"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-975.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-976.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -62.58 -22.16  -4.31  11.57 141.03 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.094e+02  3.378e+00  32.396  < 2e-16 ***
## el          -3.752e-02  6.699e-03  -5.602 3.85e-08 ***
## sl           4.711e-01  1.737e-01   2.712  0.00696 ** 
## I(el^2)     -1.657e-08  2.489e-06  -0.007  0.99469    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 32.6 on 420 degrees of freedom
## Multiple R-squared:  0.477,	Adjusted R-squared:  0.4732 
## F-statistic: 127.7 on 3 and 420 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91 15"
## [1] "N = 429"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-977.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-978.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -67.206 -17.830  -6.938  14.760 113.580 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.255e+02  1.765e+01   7.114 4.81e-12 ***
## el          -5.312e-02  1.776e-02  -2.990  0.00295 ** 
## sl           4.241e-01  1.397e-01   3.036  0.00254 ** 
## I(el^2)      6.108e-06  4.418e-06   1.382  0.16755    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.98 on 425 degrees of freedom
## Multiple R-squared:  0.2018,	Adjusted R-squared:  0.1962 
## F-statistic: 35.82 on 3 and 425 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91 15.5"
## [1] "N = 403"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-979.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-980.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -130.87  -49.51  -11.46   38.23  198.10 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.253e+02  1.129e+01  28.804  < 2e-16 ***
## el          -1.305e-01  1.573e-02  -8.297 1.66e-15 ***
## sl           7.922e-01  2.907e-01   2.725  0.00671 ** 
## I(el^2)      8.182e-06  5.076e-06   1.612  0.10779    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 63.61 on 399 degrees of freedom
## Multiple R-squared:  0.6139,	Adjusted R-squared:  0.611 
## F-statistic: 211.5 on 3 and 399 DF,  p-value: < 2.2e-16
## 
## [1] "Point -90.5 14"
## [1] "N = 301"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-981.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-982.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -28.663  -8.806  -3.199   6.528  51.686 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.076e+01  1.388e+00  29.362  < 2e-16 ***
## el           2.967e-02  5.114e-03   5.802 1.68e-08 ***
## sl           2.434e-01  1.262e-01   1.929   0.0547 .  
## I(el^2)     -2.082e-05  3.114e-06  -6.687 1.13e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15 on 297 degrees of freedom
## Multiple R-squared:  0.1524,	Adjusted R-squared:  0.1438 
## F-statistic:  17.8 on 3 and 297 DF,  p-value: 1.19e-10
## 
## [1] "Point -90.5 14.5"
## [1] "N = 404"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-983.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-984.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -40.205 -12.777   0.547   8.802  79.273 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.039e+01  3.498e+00  22.980  < 2e-16 ***
## el          -5.865e-02  5.725e-03 -10.244  < 2e-16 ***
## sl           9.991e-02  9.610e-02   1.040    0.299    
## I(el^2)      1.826e-05  2.198e-06   8.309 1.52e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.06 on 400 degrees of freedom
## Multiple R-squared:  0.2562,	Adjusted R-squared:  0.2506 
## F-statistic: 45.93 on 3 and 400 DF,  p-value: < 2.2e-16
## 
## [1] "Point -90.5 15"
## [1] "N = 404"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-985.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-986.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -70.68 -34.17 -16.09  25.05 190.06 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -2.838e+01  1.740e+01  -1.631  0.10371    
## el           8.512e-02  2.648e-02   3.215  0.00141 ** 
## sl           9.904e-01  2.214e-01   4.473    1e-05 ***
## I(el^2)     -2.042e-05  9.465e-06  -2.157  0.03157 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 44.71 on 400 degrees of freedom
## Multiple R-squared:  0.1382,	Adjusted R-squared:  0.1318 
## F-statistic: 21.39 on 3 and 400 DF,  p-value: 7.224e-13
## 
## [1] "Point -90.5 15.5"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-987.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-988.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -201.502  -49.328   -4.783   54.950  218.076 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.195e+02  1.287e+01  24.833  < 2e-16 ***
## el          -8.988e-02  2.767e-02  -3.248  0.00125 ** 
## sl          -3.900e-02  3.818e-01  -0.102  0.91870    
## I(el^2)     -4.133e-06  1.194e-05  -0.346  0.72940    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 83.87 on 430 degrees of freedom
## Multiple R-squared:  0.3348,	Adjusted R-squared:  0.3302 
## F-statistic: 72.15 on 3 and 430 DF,  p-value: < 2.2e-16
## 
## [1] "Point -90 14"
## [1] "N = 362"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-989.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-990.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -28.7289  -7.6910   0.8046   8.7382  24.8656 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.029e+01  1.347e+00  22.495  < 2e-16 ***
## el           1.103e-02  4.055e-03   2.720  0.00685 ** 
## sl           1.312e-01  7.632e-02   1.719  0.08645 .  
## I(el^2)     -3.490e-06  2.525e-06  -1.382  0.16779    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.64 on 358 degrees of freedom
## Multiple R-squared:  0.09226,	Adjusted R-squared:  0.08466 
## F-statistic: 12.13 on 3 and 358 DF,  p-value: 1.407e-07
## 
## [1] "Point -90 14.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-991.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-992.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.124 -11.454  -3.271   8.351  37.370 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 5.040e+00  4.096e+00   1.230  0.21928   
## el          2.224e-02  7.141e-03   3.114  0.00197 **
## sl          1.444e-01  7.901e-02   1.828  0.06828 . 
## I(el^2)     4.422e-07  2.769e-06   0.160  0.87319   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.52 on 407 degrees of freedom
## Multiple R-squared:  0.3897,	Adjusted R-squared:  0.3852 
## F-statistic: 86.64 on 3 and 407 DF,  p-value: < 2.2e-16
## 
## [1] "Point -90 15"
## [1] "N = 414"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-993.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-994.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -95.16 -39.08 -18.79  32.74 141.23 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 3.334e+01  9.012e+00   3.700 0.000245 ***
## el          8.390e-04  1.614e-02   0.052 0.958576    
## sl          8.851e-01  2.570e-01   3.444 0.000633 ***
## I(el^2)     1.729e-05  6.190e-06   2.793 0.005462 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 51.77 on 410 degrees of freedom
## Multiple R-squared:  0.2665,	Adjusted R-squared:  0.2612 
## F-statistic: 49.66 on 3 and 410 DF,  p-value: < 2.2e-16
## 
## [1] "Point -90 15.5"
## [1] "N = 435"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-995.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-996.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -169.217  -48.514   -4.766   39.883  229.329 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.942e+02  8.544e+00  22.732  < 2e-16 ***
## el           1.213e-01  1.894e-02   6.406 3.92e-10 ***
## sl          -7.557e-01  3.080e-01  -2.454   0.0145 *  
## I(el^2)     -6.494e-05  8.232e-06  -7.889 2.53e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 68.53 on 431 degrees of freedom
## Multiple R-squared:  0.1668,	Adjusted R-squared:  0.161 
## F-statistic: 28.76 on 3 and 431 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89.5 13.5"
## [1] "N = 209"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-997.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-998.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -21.1964  -5.4260  -0.1134   4.8136  20.1076 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.142e+01  1.225e+00  41.986  < 2e-16 ***
## el          -3.278e-02  4.165e-03  -7.869 2.03e-13 ***
## sl           3.436e-01  7.858e-02   4.373 1.95e-05 ***
## I(el^2)      2.124e-05  3.176e-06   6.687 2.12e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.968 on 205 degrees of freedom
## Multiple R-squared:  0.2595,	Adjusted R-squared:  0.2487 
## F-statistic: 23.95 on 3 and 205 DF,  p-value: 2.498e-13
## 
## [1] "Point -89.5 14"
## [1] "N = 385"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-999.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1000.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -25.979  -7.408   1.026   5.753  29.911 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.794e+01  2.459e+00  19.499  < 2e-16 ***
## el          -5.323e-02  6.780e-03  -7.851 4.24e-14 ***
## sl           9.503e-02  6.851e-02   1.387    0.166    
## I(el^2)      3.930e-05  4.148e-06   9.475  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.5 on 381 degrees of freedom
## Multiple R-squared:  0.226,	Adjusted R-squared:  0.2199 
## F-statistic: 37.09 on 3 and 381 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89.5 14.5"
## [1] "N = 402"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1001.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1002.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -29.635 -11.924  -1.974   9.864  45.800 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.051e+01  4.188e+00   2.511   0.0124 *  
## el          -1.464e-03  8.308e-03  -0.176   0.8602    
## sl           3.633e-01  7.726e-02   4.703 3.55e-06 ***
## I(el^2)      1.998e-05  3.769e-06   5.303 1.90e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.98 on 398 degrees of freedom
## Multiple R-squared:  0.5634,	Adjusted R-squared:  0.5601 
## F-statistic: 171.2 on 3 and 398 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89.5 15"
## [1] "N = 429"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1003.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1004.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -96.261 -36.893   2.034  29.825 100.277 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.939e+01  4.905e+00  20.261  < 2e-16 ***
## el          -1.128e-01  1.063e-02 -10.613  < 2e-16 ***
## sl           7.671e-01  2.033e-01   3.773 0.000184 ***
## I(el^2)      5.796e-05  4.633e-06  12.510  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 41.48 on 425 degrees of freedom
## Multiple R-squared:  0.2802,	Adjusted R-squared:  0.2751 
## F-statistic: 55.14 on 3 and 425 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89.5 15.5"
## [1] "N = 420"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1005.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1006.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -151.77  -36.18   13.00   30.14  121.09 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.829e+02  4.666e+00  39.194   <2e-16 ***
## el           2.032e-02  1.548e-02   1.313   0.1898    
## sl          -5.127e-01  2.644e-01  -1.939   0.0532 .  
## I(el^2)     -9.894e-06  7.298e-06  -1.356   0.1759    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 50.68 on 416 degrees of freedom
## Multiple R-squared:  0.01109,	Adjusted R-squared:  0.003963 
## F-statistic: 1.556 on 3 and 416 DF,  p-value: 0.1996
## 
## [1] "Point -89 13.5"
## [1] "N = 285"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1007.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1008.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -19.1539  -2.9203  -0.1219   2.5447  30.4567 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.625e+01  6.284e-01   89.52  < 2e-16 ***
## el          -3.882e-02  2.589e-03  -14.99  < 2e-16 ***
## sl           1.334e-01  5.053e-02    2.64  0.00875 ** 
## I(el^2)      2.454e-05  2.197e-06   11.17  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.507 on 281 degrees of freedom
## Multiple R-squared:  0.4901,	Adjusted R-squared:  0.4847 
## F-statistic: 90.04 on 3 and 281 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89 14.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1009.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1010.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -33.751  -8.472  -1.805   7.889  37.201 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 3.668e+01  3.535e+00  10.377   <2e-16 ***
## el          1.216e-02  6.022e-03   2.020   0.0441 *  
## sl          1.309e-01  6.270e-02   2.087   0.0375 *  
## I(el^2)     3.853e-06  2.407e-06   1.601   0.1101    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.51 on 407 degrees of freedom
## Multiple R-squared:  0.3863,	Adjusted R-squared:  0.3818 
## F-statistic: 85.41 on 3 and 407 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89 15"
## [1] "N = 418"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1011.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1012.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -110.511  -15.719   -0.129   14.948   80.986 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.938e+02  4.532e+00  42.752   <2e-16 ***
## el          -2.414e-01  1.217e-02 -19.831   <2e-16 ***
## sl           2.566e-01  1.590e-01   1.614    0.107    
## I(el^2)      1.139e-04  7.652e-06  14.885   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.34 on 414 degrees of freedom
## Multiple R-squared:  0.5884,	Adjusted R-squared:  0.5854 
## F-statistic: 197.3 on 3 and 414 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89 16.5"
## [1] "N = 427"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1013.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1014.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -60.051 -11.963  -1.136  12.549  70.724 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.115e+02  2.341e+00  90.349  < 2e-16 ***
## el          -2.194e-01  1.279e-02 -17.157  < 2e-16 ***
## sl           4.116e-01  1.390e-01   2.961  0.00323 ** 
## I(el^2)      3.096e-04  1.522e-05  20.341  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 21.22 on 423 degrees of freedom
## Multiple R-squared:  0.5269,	Adjusted R-squared:  0.5236 
## F-statistic: 157.1 on 3 and 423 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88.5 14"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1015.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1016.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.0684  -3.2185  -0.0298   2.6012  19.1794 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.102e+01  6.551e-01  77.880  < 2e-16 ***
## el          -1.451e-02  1.809e-03  -8.021  9.7e-15 ***
## sl           8.006e-02  2.579e-02   3.104  0.00203 ** 
## I(el^2)      9.141e-06  9.443e-07   9.680  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.697 on 437 degrees of freedom
## Multiple R-squared:  0.2194,	Adjusted R-squared:  0.2141 
## F-statistic: 40.95 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88.5 14.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1017.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1018.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -37.705 -10.563  -2.824   9.827  47.647 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.805e+01  4.938e+00  19.855  < 2e-16 ***
## el          -6.427e-02  8.040e-03  -7.994 1.36e-14 ***
## sl           6.441e-02  8.638e-02   0.746    0.456    
## I(el^2)      2.498e-05  3.131e-06   7.979 1.52e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.66 on 407 degrees of freedom
## Multiple R-squared:  0.1388,	Adjusted R-squared:  0.1324 
## F-statistic: 21.86 on 3 and 407 DF,  p-value: 3.803e-13
## 
## [1] "Point -88.5 15"
## [1] "N = 435"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1019.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1020.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -64.630 -16.456  -2.599  16.868  98.744 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.291e+02  4.464e+00  28.921  < 2e-16 ***
## el          -7.565e-02  9.675e-03  -7.819 4.13e-14 ***
## sl           5.040e-01  1.320e-01   3.818 0.000155 ***
## I(el^2)      2.608e-05  4.905e-06   5.316 1.70e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 24.2 on 431 degrees of freedom
## Multiple R-squared:  0.1949,	Adjusted R-squared:  0.1893 
## F-statistic: 34.79 on 3 and 431 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88.5 15.5"
## [1] "N = 372"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1021.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1022.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -95.786 -24.726  -0.421  23.278 145.642 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.379e+02  3.785e+00  62.872   <2e-16 ***
## el          -2.946e-01  1.632e-02 -18.054   <2e-16 ***
## sl           5.754e-01  2.594e-01   2.218   0.0271 *  
## I(el^2)      1.554e-04  1.236e-05  12.567   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 38.81 on 368 degrees of freedom
## Multiple R-squared:  0.6142,	Adjusted R-squared:  0.6111 
## F-statistic: 195.3 on 3 and 368 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88 14"
## [1] "N = 432"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1023.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1024.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -14.334  -4.499  -0.548   2.649  33.854 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.084e+01  1.217e+00  49.977   <2e-16 ***
## el          -4.505e-02  2.716e-03 -16.589   <2e-16 ***
## sl           6.331e-02  3.994e-02   1.585    0.114    
## I(el^2)      2.469e-05  1.286e-06  19.193   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.009 on 428 degrees of freedom
## Multiple R-squared:  0.5135,	Adjusted R-squared:  0.5101 
## F-statistic: 150.6 on 3 and 428 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88 14.5"
## [1] "N = 442"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1025.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1026.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -76.198 -18.089  -3.018  11.610 108.373 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.867e+02  1.009e+01  18.498  < 2e-16 ***
## el          -1.755e-01  1.816e-02  -9.667  < 2e-16 ***
## sl           4.616e-01  1.720e-01   2.684  0.00755 ** 
## I(el^2)      5.722e-05  7.254e-06   7.888 2.47e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.17 on 438 degrees of freedom
## Multiple R-squared:  0.2779,	Adjusted R-squared:  0.273 
## F-statistic: 56.19 on 3 and 438 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88 15"
## [1] "N = 484"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1027.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1028.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -52.570 -18.303  -3.733  15.672  81.580 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.359e+02  3.573e+00  38.042   <2e-16 ***
## el          -1.049e-02  9.463e-03  -1.109   0.2680    
## sl          -4.503e-02  1.469e-01  -0.306   0.7594    
## I(el^2)     -1.158e-05  5.214e-06  -2.221   0.0268 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.3 on 480 degrees of freedom
## Multiple R-squared:  0.1678,	Adjusted R-squared:  0.1626 
## F-statistic: 32.25 on 3 and 480 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87.5 14"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1029.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1030.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -30.765  -5.333  -1.947   4.400  29.765 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.013e+01  1.575e+00  38.185   <2e-16 ***
## el          -5.812e-02  3.219e-03 -18.058   <2e-16 ***
## sl           5.308e-02  4.749e-02   1.118    0.264    
## I(el^2)      3.750e-05  1.585e-06  23.662   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.733 on 411 degrees of freedom
## Multiple R-squared:  0.6783,	Adjusted R-squared:  0.676 
## F-statistic: 288.9 on 3 and 411 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87.5 14.5"
## [1] "N = 412"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1031.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1032.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -46.979 -16.526  -1.219  11.573 114.699 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.638e+01  8.593e+00  11.216  < 2e-16 ***
## el          -6.516e-02  1.585e-02  -4.112 4.74e-05 ***
## sl           7.025e-01  1.285e-01   5.467 8.00e-08 ***
## I(el^2)      2.673e-05  6.781e-06   3.942 9.52e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 24.42 on 408 degrees of freedom
## Multiple R-squared:  0.1004,	Adjusted R-squared:  0.09381 
## F-statistic: 15.18 on 3 and 408 DF,  p-value: 2.195e-09
## 
## [1] "Point -87.5 15"
## [1] "N = 443"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1033.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1034.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -53.454 -20.462  -3.603  14.454 100.696 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.237e+02  5.504e+00  22.468  < 2e-16 ***
## el          -5.444e-02  1.272e-02  -4.278 2.32e-05 ***
## sl           1.770e-01  1.578e-01   1.122    0.262    
## I(el^2)      2.828e-05  6.370e-06   4.440 1.14e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.13 on 439 degrees of freedom
## Multiple R-squared:  0.04325,	Adjusted R-squared:  0.03671 
## F-statistic: 6.615 on 3 and 439 DF,  p-value: 0.0002226
## 
## [1] "Point -87.5 15.5"
## [1] "N = 412"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1035.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1036.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -149.338  -30.008    7.469   38.818  186.408 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.859e+02  5.234e+00  54.625   <2e-16 ***
## el          -3.694e-01  2.563e-02 -14.412   <2e-16 ***
## sl           9.284e-01  4.063e-01   2.285   0.0228 *  
## I(el^2)      1.677e-04  1.766e-05   9.499   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 63.32 on 408 degrees of freedom
## Multiple R-squared:  0.577,	Adjusted R-squared:  0.5739 
## F-statistic: 185.5 on 3 and 408 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87 13.5"
## [1] "N = 413"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1037.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1038.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -30.543  -8.498  -2.952   5.127  59.547 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.581e+01  1.547e+00  48.999   <2e-16 ***
## el          -1.087e-01  5.388e-03 -20.168   <2e-16 ***
## sl           4.014e-02  7.899e-02   0.508    0.612    
## I(el^2)      7.638e-05  3.826e-06  19.963   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.21 on 409 degrees of freedom
## Multiple R-squared:  0.5237,	Adjusted R-squared:  0.5202 
## F-statistic: 149.9 on 3 and 409 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87 14"
## [1] "N = 397"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1039.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1040.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -33.279 -12.406  -2.106   8.870  50.712 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 2.117e+01  5.361e+00   3.949  9.3e-05 ***
## el          2.578e-02  1.068e-02   2.414  0.01624 *  
## sl          3.471e-01  9.211e-02   3.768  0.00019 ***
## I(el^2)     3.054e-06  5.152e-06   0.593  0.55370    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.45 on 393 degrees of freedom
## Multiple R-squared:  0.3387,	Adjusted R-squared:  0.3337 
## F-statistic: 67.11 on 3 and 393 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87 15"
## [1] "N = 423"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1041.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1042.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -42.93 -14.26  -2.66  13.82  59.39 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.096e+01  6.121e+00  14.860  < 2e-16 ***
## el          -3.602e-02  1.212e-02  -2.973  0.00312 ** 
## sl           1.907e-01  1.116e-01   1.709  0.08824 .  
## I(el^2)      3.864e-05  5.447e-06   7.095 5.56e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.65 on 419 degrees of freedom
## Multiple R-squared:  0.4614,	Adjusted R-squared:  0.4576 
## F-statistic: 119.7 on 3 and 419 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87 15.5"
## [1] "N = 384"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1043.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1044.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -118.442  -36.489    5.292   37.169  122.349 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.939e+02  5.751e+00  51.101  < 2e-16 ***
## el          -3.917e-01  1.638e-02 -23.911  < 2e-16 ***
## sl           1.014e+00  2.565e-01   3.954 9.15e-05 ***
## I(el^2)      1.842e-04  9.081e-06  20.282  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 50.03 on 380 degrees of freedom
## Multiple R-squared:  0.6372,	Adjusted R-squared:  0.6343 
## F-statistic: 222.4 on 3 and 380 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 13"
## [1] "N = 385"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1045.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1046.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -33.222  -6.621  -0.448   6.484  46.770 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.592e+01  1.231e+00  61.688  < 2e-16 ***
## el          -1.082e-01  5.196e-03 -20.826  < 2e-16 ***
## sl           2.568e-01  8.140e-02   3.155  0.00173 ** 
## I(el^2)      9.065e-05  4.081e-06  22.212  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.24 on 381 degrees of freedom
## Multiple R-squared:  0.5648,	Adjusted R-squared:  0.5613 
## F-statistic: 164.8 on 3 and 381 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 13.5"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1047.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1048.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -43.814 -13.925  -3.907  12.258  49.039 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.471e+01  6.075e+00  10.651  < 2e-16 ***
## el          -8.067e-02  1.343e-02  -6.009 4.13e-09 ***
## sl           9.334e-02  1.012e-01   0.922    0.357    
## I(el^2)      8.032e-05  7.189e-06  11.172  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.24 on 411 degrees of freedom
## Multiple R-squared:  0.5663,	Adjusted R-squared:  0.5632 
## F-statistic: 178.9 on 3 and 411 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 14"
## [1] "N = 426"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1049.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1050.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -55.034 -15.167   0.217  15.362  52.020 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 3.706e+01  7.658e+00   4.839 1.83e-06 ***
## el          7.073e-03  1.706e-02   0.415 0.678665    
## sl          3.871e-01  1.155e-01   3.351 0.000876 ***
## I(el^2)     3.028e-05  8.730e-06   3.469 0.000577 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.97 on 422 degrees of freedom
## Multiple R-squared:  0.4977,	Adjusted R-squared:  0.4941 
## F-statistic: 139.4 on 3 and 422 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 15"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1051.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1052.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -55.391 -15.199  -5.941  12.605  85.573 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 6.908e+01  8.540e+00   8.089 5.98e-15 ***
## el          1.594e-02  1.815e-02   0.879 0.380140    
## sl          2.172e-01  1.085e-01   2.002 0.045872 *  
## I(el^2)     3.200e-05  9.179e-06   3.486 0.000539 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 22.54 on 437 degrees of freedom
## Multiple R-squared:  0.5113,	Adjusted R-squared:  0.5079 
## F-statistic: 152.4 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 15.5"
## [1] "N = 396"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1053.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1054.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -99.675 -34.597  -1.022  31.946 135.166 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.307e+02  5.127e+00  44.999   <2e-16 ***
## el          -1.981e-01  1.471e-02 -13.463   <2e-16 ***
## sl           3.718e-01  2.154e-01   1.726   0.0851 .  
## I(el^2)      9.800e-05  8.393e-06  11.675   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 43.59 on 392 degrees of freedom
## Multiple R-squared:  0.3476,	Adjusted R-squared:  0.3426 
## F-statistic: 69.61 on 3 and 392 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 16"
## [1] "N = 106"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1055.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1056.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -73.17 -21.67  -1.91  14.45 129.49 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.765e+02  6.563e+00  42.134  < 2e-16 ***
## el          -1.322e-01  2.278e-02  -5.802 7.41e-08 ***
## sl           5.535e-01  3.833e-01   1.444 0.151785    
## I(el^2)      4.570e-05  1.157e-05   3.952 0.000143 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.11 on 102 degrees of freedom
## Multiple R-squared:  0.3622,	Adjusted R-squared:  0.3434 
## F-statistic: 19.31 on 3 and 102 DF,  p-value: 5.433e-10
## 
## [1] "Point -86 12.5"
## [1] "N = 401"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1057.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1058.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -30.780 -11.205  -0.887   7.762  72.495 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 5.595e+01  1.488e+00  37.603   <2e-16 ***
## el          9.060e-03  8.633e-03   1.050   0.2946    
## sl          2.628e-01  1.201e-01   2.188   0.0292 *  
## I(el^2)     1.246e-05  8.639e-06   1.443   0.1499    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.41 on 397 degrees of freedom
## Multiple R-squared:  0.1868,	Adjusted R-squared:  0.1807 
## F-statistic:  30.4 on 3 and 397 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86 13"
## [1] "N = 409"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1059.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1060.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -56.753 -28.252  -7.506  23.560  86.823 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.221e+02  1.233e+01   9.901  < 2e-16 ***
## el          -1.698e-01  3.325e-02  -5.106 5.07e-07 ***
## sl           4.823e-01  1.872e-01   2.576   0.0103 *  
## I(el^2)      1.358e-04  2.073e-05   6.550 1.75e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.29 on 405 degrees of freedom
## Multiple R-squared:  0.2171,	Adjusted R-squared:  0.2113 
## F-statistic: 37.43 on 3 and 405 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86 13.5"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1061.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1062.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -61.534 -20.941  -2.735  24.117  67.934 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.321e+02  1.239e+01  10.660  < 2e-16 ***
## el          -1.180e-01  3.029e-02  -3.894 0.000115 ***
## sl           1.493e-01  1.720e-01   0.868 0.385818    
## I(el^2)      9.218e-05  1.774e-05   5.197 3.19e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.17 on 411 degrees of freedom
## Multiple R-squared:  0.1407,	Adjusted R-squared:  0.1344 
## F-statistic: 22.43 on 3 and 411 DF,  p-value: 1.807e-13
## 
## [1] "Point -86 14"
## [1] "N = 438"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1063.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1064.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -53.952 -15.184   0.456  14.871  63.584 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.234e+01  5.748e+00  16.064  < 2e-16 ***
## el          -4.038e-02  1.529e-02  -2.640  0.00858 ** 
## sl           6.318e-01  1.171e-01   5.395 1.13e-07 ***
## I(el^2)      4.971e-05  9.244e-06   5.377 1.24e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 21.32 on 434 degrees of freedom
## Multiple R-squared:  0.3254,	Adjusted R-squared:  0.3207 
## F-statistic: 69.77 on 3 and 434 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86 15"
## [1] "N = 436"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1065.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1066.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -73.510 -36.469  -9.773  19.572 209.102 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.386e+01  1.463e+01   5.047 6.61e-07 ***
## el           1.221e-01  3.572e-02   3.417 0.000692 ***
## sl          -5.661e-01  2.830e-01  -2.001 0.046067 *  
## I(el^2)     -1.766e-05  1.854e-05  -0.953 0.341348    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 50.11 on 432 degrees of freedom
## Multiple R-squared:  0.2178,	Adjusted R-squared:  0.2123 
## F-statistic: 40.09 on 3 and 432 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86 15.5"
## [1] "N = 446"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1067.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1068.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -100.075  -31.761   -1.468   30.765  163.649 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.407e+02  5.204e+00  46.261  < 2e-16 ***
## el          -1.536e-01  2.027e-02  -7.578 2.07e-13 ***
## sl           2.638e-01  2.446e-01   1.078    0.281    
## I(el^2)      8.012e-05  1.649e-05   4.857 1.65e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 45.43 on 442 degrees of freedom
## Multiple R-squared:  0.2167,	Adjusted R-squared:  0.2114 
## F-statistic: 40.76 on 3 and 442 DF,  p-value: < 2.2e-16
## 
## [1] "Point -85.5 15"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1069.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1070.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -103.693  -30.416    6.443   32.448  152.769 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 94.5582209  7.8029526  12.118  < 2e-16 ***
## el           0.2171728  0.0213780  10.159  < 2e-16 ***
## sl          -0.1028969  0.2349462  -0.438    0.662    
## I(el^2)     -0.0000853  0.0000131  -6.512 2.04e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 43.58 on 437 degrees of freedom
## Multiple R-squared:  0.3374,	Adjusted R-squared:  0.3329 
## F-statistic: 74.18 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -85.5 15.5"
## [1] "N = 433"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1071.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1072.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -60.67 -24.15  -0.11  18.25 119.04 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.121e+02  3.674e+00  57.734   <2e-16 ***
## el           2.637e-02  1.305e-02   2.020    0.044 *  
## sl          -1.619e-01  1.801e-01  -0.899    0.369    
## I(el^2)     -3.946e-06  9.802e-06  -0.403    0.687    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.54 on 429 degrees of freedom
## Multiple R-squared:  0.04833,	Adjusted R-squared:  0.04168 
## F-statistic: 7.263 on 3 and 429 DF,  p-value: 9.221e-05
## 
## [1] "Point -84.5 9.5"
## [1] "N = 143"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1073.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1074.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -115.188  -27.106    2.864   33.191  116.011 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.996e+02  8.216e+00  36.466  < 2e-16 ***
## el          -1.746e-01  2.673e-02  -6.532 1.14e-09 ***
## sl          -3.330e-01  5.119e-01  -0.651    0.516    
## I(el^2)      6.115e-05  1.505e-05   4.062 8.09e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 49.66 on 139 degrees of freedom
## Multiple R-squared:  0.448,	Adjusted R-squared:  0.436 
## F-statistic:  37.6 on 3 and 139 DF,  p-value: < 2.2e-16
## 
## [1] "Point -84.5 10"
## [1] "N = 356"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1075.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1076.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -120.54  -55.55  -18.57   34.47  283.08 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.068e+02  1.014e+01  20.405  < 2e-16 ***
## el          -1.282e-02  2.266e-02  -0.566  0.57195    
## sl           1.355e+00  4.545e-01   2.982  0.00307 ** 
## I(el^2)      1.472e-05  1.078e-05   1.365  0.17312    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 78.87 on 352 degrees of freedom
## Multiple R-squared:  0.05532,	Adjusted R-squared:  0.04727 
## F-statistic: 6.872 on 3 and 352 DF,  p-value: 0.0001648
## 
## [1] "Point -84.5 10.5"
## [1] "N = 382"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1077.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1078.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -149.89  -52.83  -10.38   56.25  174.93 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.083e+02  6.659e+00  46.297   <2e-16 ***
## el           3.002e-02  2.116e-02   1.419   0.1568    
## sl           4.982e-01  4.756e-01   1.047   0.2956    
## I(el^2)     -3.075e-05  1.205e-05  -2.551   0.0111 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 71.69 on 378 degrees of freedom
## Multiple R-squared:  0.03201,	Adjusted R-squared:  0.02433 
## F-statistic: 4.166 on 3 and 378 DF,  p-value: 0.006384
## 
## [1] "Point -84 9"
## [1] "N = 60"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1079.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1080.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -46.244 -14.770  -5.174   7.001 236.639 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.064e+02  8.250e+00  49.258  < 2e-16 ***
## el          -3.313e-01  5.360e-02  -6.182 7.68e-08 ***
## sl          -3.576e-01  6.526e-01  -0.548 0.585874    
## I(el^2)      1.792e-04  5.015e-05   3.573 0.000734 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 38.04 on 56 degrees of freedom
## Multiple R-squared:  0.7453,	Adjusted R-squared:  0.7316 
## F-statistic: 54.62 on 3 and 56 DF,  p-value: < 2.2e-16
## 
## [1] "Point -84 9.5"
## [1] "N = 270"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1081.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1082.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -134.08  -56.42  -10.99   31.48  338.70 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.181e+02  1.257e+01  25.317  < 2e-16 ***
## el          -1.004e-01  2.017e-02  -4.978 1.16e-06 ***
## sl           5.519e-01  5.238e-01   1.054    0.293    
## I(el^2)      3.018e-05  7.025e-06   4.296 2.44e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 83.64 on 266 degrees of freedom
## Multiple R-squared:  0.0915,	Adjusted R-squared:  0.08125 
## F-statistic:  8.93 on 3 and 266 DF,  p-value: 1.172e-05
## 
## [1] "Point -84 10"
## [1] "N = 407"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1083.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1084.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -235.39  -64.56   -0.65   63.30  355.67 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.305e+02  1.222e+01  35.228  < 2e-16 ***
## el          -2.798e-01  2.110e-02 -13.260  < 2e-16 ***
## sl           1.156e+00  4.168e-01   2.775  0.00578 ** 
## I(el^2)      8.554e-05  7.812e-06  10.950  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 86.43 on 403 degrees of freedom
## Multiple R-squared:  0.3378,	Adjusted R-squared:  0.3329 
## F-statistic: 68.54 on 3 and 403 DF,  p-value: < 2.2e-16
## 
## [1] "Point -83.5 9"
## [1] "N = 278"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1085.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1086.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -215.91  -59.96  -15.32   57.28  223.09 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.199e+02  9.710e+00  43.246   <2e-16 ***
## el          -2.305e-01  2.056e-02 -11.211   <2e-16 ***
## sl           5.151e-01  5.330e-01   0.966    0.335    
## I(el^2)      6.778e-05  7.575e-06   8.949   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 79.08 on 274 degrees of freedom
## Multiple R-squared:  0.3569,	Adjusted R-squared:  0.3499 
## F-statistic:  50.7 on 3 and 274 DF,  p-value: < 2.2e-16
## 
## [1] "Point -83.5 9.5"
## [1] "N = 370"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1087.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1088.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -131.38  -38.88  -12.21   26.81  238.30 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.099e+02  1.073e+01  19.554   <2e-16 ***
## el           1.615e-01  1.501e-02  10.755   <2e-16 ***
## sl           2.841e-01  2.938e-01   0.967    0.334    
## I(el^2)     -4.861e-05  4.516e-06 -10.764   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 59.23 on 366 degrees of freedom
## Multiple R-squared:  0.2596,	Adjusted R-squared:  0.2536 
## F-statistic: 42.78 on 3 and 366 DF,  p-value: < 2.2e-16
## 
## [1] "Point -83.5 10"
## [1] "N = 378"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1089.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1090.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -190.770  -49.061   -8.382   45.894  268.733 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.227e+02  6.860e+00  61.614  < 2e-16 ***
## el          -1.111e-01  1.572e-02  -7.067 7.78e-12 ***
## sl           3.256e-01  4.109e-01   0.793    0.429    
## I(el^2)      2.734e-05  6.221e-06   4.394 1.45e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 74.83 on 374 degrees of freedom
## Multiple R-squared:  0.2377,	Adjusted R-squared:  0.2316 
## F-statistic: 38.88 on 3 and 374 DF,  p-value: < 2.2e-16
## 
## [1] "Point -83 8.5"
## [1] "N = 243"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1091.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1092.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -108.73  -52.67   -7.97   37.17  198.94 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.730e+02  8.122e+00  45.931   <2e-16 ***
## el          -5.104e-02  3.498e-02  -1.459    0.146    
## sl           7.310e-01  5.792e-01   1.262    0.208    
## I(el^2)      1.361e-05  2.765e-05   0.492    0.623    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 66.31 on 239 degrees of freedom
## Multiple R-squared:  0.04437,	Adjusted R-squared:  0.03238 
## F-statistic: 3.699 on 3 and 239 DF,  p-value: 0.01245
## 
## [1] "Point -83 9"
## [1] "N = 388"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1093.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1094.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -127.320  -25.741    0.475   27.004  198.563 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.838e+02  7.129e+00  53.829  < 2e-16 ***
## el          -8.016e-02  1.222e-02  -6.559 1.75e-10 ***
## sl          -3.965e-01  2.631e-01  -1.507 0.132655    
## I(el^2)      1.551e-05  4.356e-06   3.560 0.000417 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 52.71 on 384 degrees of freedom
## Multiple R-squared:  0.2913,	Adjusted R-squared:  0.2858 
## F-statistic: 52.61 on 3 and 384 DF,  p-value: < 2.2e-16
## 
## [1] "Point -83 9.5"
## [1] "N = 365"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1095.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1096.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -57.242 -20.696  -8.759   9.759 130.916 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.548e+02  3.929e+00  64.839   <2e-16 ***
## el           9.931e-02  7.611e-03  13.049   <2e-16 ***
## sl          -8.176e-02  1.697e-01  -0.482     0.63    
## I(el^2)     -3.325e-05  2.777e-06 -11.975   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.18 on 361 degrees of freedom
## Multiple R-squared:  0.3629,	Adjusted R-squared:  0.3576 
## F-statistic: 68.53 on 3 and 361 DF,  p-value: < 2.2e-16
## 
## [1] "Point -82.5 8.5"
## [1] "N = 322"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1097.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1098.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -59.695 -22.691  -1.079  18.794 132.176 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.232e+02  2.915e+00 110.889  < 2e-16 ***
## el           3.507e-02  7.975e-03   4.397  1.5e-05 ***
## sl           4.590e-01  2.389e-01   1.921   0.0556 .  
## I(el^2)     -3.331e-05  3.445e-06  -9.667  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 30.18 on 318 degrees of freedom
## Multiple R-squared:  0.4214,	Adjusted R-squared:  0.416 
## F-statistic: 77.21 on 3 and 318 DF,  p-value: < 2.2e-16
## 
## [1] "Point -82.5 9"
## [1] "N = 352"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1099.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1100.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -83.45 -19.00   0.45  20.76  66.82 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.216e+02  3.819e+00  84.194  < 2e-16 ***
## el           2.910e-02  6.817e-03   4.269 2.54e-05 ***
## sl          -1.340e-01  1.654e-01  -0.810    0.419    
## I(el^2)     -2.501e-05  2.637e-06  -9.481  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 28.92 on 348 degrees of freedom
## Multiple R-squared:  0.4756,	Adjusted R-squared:  0.4711 
## F-statistic: 105.2 on 3 and 348 DF,  p-value: < 2.2e-16
## 
## [1] "Point -82.5 9.5"
## [1] "N = 198"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1101.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1102.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -47.22 -18.89  -2.62  14.45  69.14 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.602e+02  3.258e+00  79.859  < 2e-16 ***
## el           1.039e-01  1.206e-02   8.616 2.41e-15 ***
## sl           2.836e-01  2.256e-01   1.257     0.21    
## I(el^2)     -5.205e-05  6.404e-06  -8.127 5.07e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.63 on 194 degrees of freedom
## Multiple R-squared:  0.402,	Adjusted R-squared:  0.3927 
## F-statistic: 43.47 on 3 and 194 DF,  p-value: < 2.2e-16
## 
## [1] "Point -82 8.5"
## [1] "N = 363"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1103.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1104.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -63.988 -14.413  -1.367  11.618 104.038 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.663e+02  2.945e+00 124.368  < 2e-16 ***
## el          -1.789e-02  1.106e-02  -1.618   0.1066    
## sl           4.796e-01  1.993e-01   2.406   0.0166 *  
## I(el^2)     -2.782e-05  6.764e-06  -4.113 4.86e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.37 on 359 degrees of freedom
## Multiple R-squared:  0.4186,	Adjusted R-squared:  0.4137 
## F-statistic: 86.15 on 3 and 359 DF,  p-value: < 2.2e-16
## 
## [1] "Point -82 9"
## [1] "N = 240"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1105.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1106.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -52.74 -13.75   4.92  15.50  38.38 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.560e+02  2.572e+00 138.447  < 2e-16 ***
## el           9.970e-03  8.831e-03   1.129     0.26    
## sl           3.143e-02  1.785e-01   0.176     0.86    
## I(el^2)     -3.484e-05  4.961e-06  -7.023  2.3e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.97 on 236 degrees of freedom
## Multiple R-squared:  0.5439,	Adjusted R-squared:  0.5381 
## F-statistic: 93.82 on 3 and 236 DF,  p-value: < 2.2e-16
## 
## [1] "Point -81.5 8.5"
## [1] "N = 393"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1107.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1108.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -62.741 -17.560  -2.868  15.864 110.905 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.006e+02  3.061e+00 130.866  < 2e-16 ***
## el          -1.333e-01  9.875e-03 -13.503  < 2e-16 ***
## sl           9.401e-02  1.701e-01   0.553    0.581    
## I(el^2)      3.136e-05  5.942e-06   5.278 2.18e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 26.01 on 389 degrees of freedom
## Multiple R-squared:  0.6772,	Adjusted R-squared:  0.6747 
## F-statistic: 272.1 on 3 and 389 DF,  p-value: < 2.2e-16
## 
## [1] "Point -81.5 9"
## [1] "N = 153"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1109.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1110.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -28.701  -7.029   1.900   6.790  25.280 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.806e+02  1.646e+00 231.304   <2e-16 ***
## el          -8.241e-02  6.627e-03 -12.436   <2e-16 ***
## sl           5.079e-02  1.253e-01   0.405   0.6859    
## I(el^2)      7.308e-06  4.300e-06   1.700   0.0913 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.63 on 149 degrees of freedom
## Multiple R-squared:  0.8997,	Adjusted R-squared:  0.8976 
## F-statistic: 445.3 on 3 and 149 DF,  p-value: < 2.2e-16
## 
## [1] "Point -81 7"
## [1] "N = 30"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1111.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1112.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -59.648 -14.139   9.602  21.467  42.651 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.522e+02  1.057e+01  42.790  < 2e-16 ***
## el          -3.225e-01  4.954e-02  -6.509 6.70e-07 ***
## sl          -3.044e-02  6.258e-01  -0.049    0.962    
## I(el^2)      1.679e-04  3.609e-05   4.652 8.42e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 30.45 on 26 degrees of freedom
## Multiple R-squared:  0.7966,	Adjusted R-squared:  0.7731 
## F-statistic: 33.94 on 3 and 26 DF,  p-value: 3.847e-09
## 
## [1] "Point -81 8.5"
## [1] "N = 390"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1113.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1114.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -83.12 -21.23   0.74  21.88  85.56 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.022e+02  3.627e+00  83.337  < 2e-16 ***
## el           5.835e-02  1.655e-02   3.525 0.000474 ***
## sl           3.683e-01  2.416e-01   1.525 0.128193    
## I(el^2)     -6.605e-05  1.173e-05  -5.631 3.46e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 35.06 on 386 degrees of freedom
## Multiple R-squared:  0.1325,	Adjusted R-squared:  0.1258 
## F-statistic: 19.65 on 3 and 386 DF,  p-value: 7.066e-12
## 
## [1] "Point -80.5 7"
## [1] "N = 54"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1115.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1116.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -28.210 -15.209   0.984   7.619  62.768 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.007e+02  5.341e+00  56.296   <2e-16 ***
## el           3.287e-02  2.728e-02   1.205    0.234    
## sl           2.516e-01  3.311e-01   0.760    0.451    
## I(el^2)     -2.735e-05  2.079e-05  -1.315    0.194    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.29 on 50 degrees of freedom
## Multiple R-squared:  0.0857,	Adjusted R-squared:  0.03084 
## F-statistic: 1.562 on 3 and 50 DF,  p-value: 0.2102
## 
## [1] "Point -78.5 7.5"
## [1] "N = 42"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1117.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1118.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -140.987  -19.479    3.982   33.097  103.059 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.658e+02  2.596e+01  21.796   <2e-16 ***
## el          -3.289e-01  9.610e-02  -3.423   0.0015 ** 
## sl          -3.180e-01  1.113e+00  -0.286   0.7767    
## I(el^2)      1.413e-04  6.961e-05   2.031   0.0493 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 55.18 on 38 degrees of freedom
## Multiple R-squared:  0.5488,	Adjusted R-squared:  0.5132 
## F-statistic: 15.41 on 3 and 38 DF,  p-value: 1.026e-06
## 
## [1] "Point -76.5 6"
## [1] "N = 183"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1119.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1120.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -274.508  -37.727   -1.757   41.313  114.566 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.445e+02  6.079e+00  89.571  < 2e-16 ***
## el          -1.821e-01  1.537e-02 -11.847  < 2e-16 ***
## sl           9.321e-01  2.701e-01   3.451 0.000698 ***
## I(el^2)      2.235e-05  5.722e-06   3.905 0.000133 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 52.04 on 179 degrees of freedom
## Multiple R-squared:  0.7997,	Adjusted R-squared:  0.7963 
## F-statistic: 238.2 on 3 and 179 DF,  p-value: < 2.2e-16
## 
## [1] "Point -76.5 6.5"
## [1] "N = 374"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1121.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1122.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -90.908 -39.740  -8.335  29.626 162.468 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.728e+02  4.669e+00 101.269  < 2e-16 ***
## el          -1.731e-01  1.111e-02 -15.573  < 2e-16 ***
## sl           6.185e-01  2.314e-01   2.673  0.00785 ** 
## I(el^2)      3.155e-05  4.301e-06   7.336  1.4e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 50.58 on 370 degrees of freedom
## Multiple R-squared:  0.6816,	Adjusted R-squared:  0.679 
## F-statistic:   264 on 3 and 370 DF,  p-value: < 2.2e-16
## 
## [1] "Point -76.5 7"
## [1] "N = 401"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1123.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1124.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -76.070 -22.375  -1.883  16.767 100.183 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.409e+02  2.726e+00 161.737   <2e-16 ***
## el          -1.962e-01  6.992e-03 -28.064   <2e-16 ***
## sl          -2.499e-02  1.446e-01  -0.173    0.863    
## I(el^2)      4.985e-05  2.971e-06  16.778   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.67 on 397 degrees of freedom
## Multiple R-squared:  0.8517,	Adjusted R-squared:  0.8506 
## F-statistic:   760 on 3 and 397 DF,  p-value: < 2.2e-16
## 
## [1] "Point -76.5 7.5"
## [1] "N = 403"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1125.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1126.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -143.572  -41.377   -1.974   41.962  153.931 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.678e+02  5.452e+00  67.460  < 2e-16 ***
## el          -1.957e-01  1.958e-02  -9.997  < 2e-16 ***
## sl           2.922e-01  4.207e-01   0.695    0.488    
## I(el^2)      6.960e-05  9.221e-06   7.548 3.03e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 62.56 on 399 degrees of freedom
## Multiple R-squared:  0.3368,	Adjusted R-squared:  0.3318 
## F-statistic: 67.55 on 3 and 399 DF,  p-value: < 2.2e-16
## 
## [1] "Point -76.5 18"
## [1] "N = 128"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1127.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1128.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -123.686  -46.505    4.967   36.547  146.829 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.999e+02  1.166e+01  17.147   <2e-16 ***
## el          -2.812e-03  6.220e-02  -0.045   0.9640    
## sl           1.464e+00  6.597e-01   2.219   0.0283 *  
## I(el^2)      9.524e-05  5.158e-05   1.847   0.0672 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 65.85 on 124 degrees of freedom
## Multiple R-squared:  0.3442,	Adjusted R-squared:  0.3283 
## F-statistic: 21.69 on 3 and 124 DF,  p-value: 2.328e-11
## 
## [1] "Point -76 6"
## [1] "N = 186"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1129.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1130.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -167.689  -40.444   -3.757   18.169  250.988 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.753e+02  2.661e+01  14.102  < 2e-16 ***
## el          -1.680e-01  3.307e-02  -5.080 9.28e-07 ***
## sl           5.468e-01  4.313e-01   1.268    0.206    
## I(el^2)      4.329e-05  1.005e-05   4.307 2.70e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 81.2 on 182 degrees of freedom
## Multiple R-squared:  0.1466,	Adjusted R-squared:  0.1325 
## F-statistic: 10.42 on 3 and 182 DF,  p-value: 2.32e-06
## 
## [1] "Point -76 6.5"
## [1] "N = 371"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1131.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1132.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -143.180  -38.098   -1.739   27.353  197.803 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.696e+02  1.688e+01  15.972   <2e-16 ***
## el          -4.317e-02  2.172e-02  -1.987   0.0476 *  
## sl          -4.807e-01  2.588e-01  -1.857   0.0641 .  
## I(el^2)      1.310e-05  6.224e-06   2.104   0.0360 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 61.22 on 367 degrees of freedom
## Multiple R-squared:  0.02399,	Adjusted R-squared:  0.01602 
## F-statistic: 3.007 on 3 and 367 DF,  p-value: 0.0303
## 
## [1] "Point -76 7"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1133.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1134.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -128.540  -22.955    3.283   23.301  113.084 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.990e+02  1.092e+01  27.379  < 2e-16 ***
## el          -6.718e-02  1.361e-02  -4.935 1.17e-06 ***
## sl          -2.108e-01  1.806e-01  -1.167    0.244    
## I(el^2)      1.986e-05  3.836e-06   5.177 3.55e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 41.02 on 407 degrees of freedom
## Multiple R-squared:  0.07746,	Adjusted R-squared:  0.07066 
## F-statistic: 11.39 on 3 and 407 DF,  p-value: 3.458e-07
## 
## [1] "Point -76 7.5"
## [1] "N = 398"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1135.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1136.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -69.761 -21.632  -1.414  21.650  82.355 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.126e+02  3.555e+00  59.819  < 2e-16 ***
## el           6.868e-02  7.514e-03   9.140  < 2e-16 ***
## sl          -1.347e-01  1.514e-01  -0.890    0.374    
## I(el^2)     -1.663e-05  2.570e-06  -6.469 2.93e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.76 on 394 degrees of freedom
## Multiple R-squared:  0.3272,	Adjusted R-squared:  0.3221 
## F-statistic: 63.87 on 3 and 394 DF,  p-value: < 2.2e-16
## 
## [1] "Point -76 18"
## [1] "N = 38"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1137.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1138.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -44.725 -22.517  -8.022   8.939  89.264 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.343e+02  1.276e+01  18.362   <2e-16 ***
## el           1.081e-01  7.694e-02   1.405    0.169    
## sl          -6.914e-02  8.518e-01  -0.081    0.936    
## I(el^2)     -1.591e-05  7.377e-05  -0.216    0.831    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 35.98 on 34 degrees of freedom
## Multiple R-squared:  0.4197,	Adjusted R-squared:  0.3685 
## F-statistic: 8.197 on 3 and 34 DF,  p-value: 0.0003064
## 
## [1] "Point -75.5 6"
## [1] "N = 208"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1139.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1140.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -92.55 -33.18 -14.02  17.25 201.32 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.054e+02  2.526e+01   8.131 4.07e-14 ***
## el           4.872e-02  3.110e-02   1.567   0.1187    
## sl           1.205e-02  3.702e-01   0.033   0.9741    
## I(el^2)     -2.020e-05  9.441e-06  -2.140   0.0335 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 57.71 on 204 degrees of freedom
## Multiple R-squared:  0.04576,	Adjusted R-squared:  0.03173 
## F-statistic: 3.261 on 3 and 204 DF,  p-value: 0.0225
## 
## [1] "Point -75.5 6.5"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1141.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1142.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -63.493 -30.323  -4.421  26.345 130.372 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.156e+02  1.354e+01   8.536 2.72e-16 ***
## el           7.704e-02  1.503e-02   5.127 4.53e-07 ***
## sl          -1.715e-01  1.828e-01  -0.938  0.34869    
## I(el^2)     -1.426e-05  4.095e-06  -3.482  0.00055 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 37.64 on 411 degrees of freedom
## Multiple R-squared:  0.1797,	Adjusted R-squared:  0.1738 
## F-statistic: 30.02 on 3 and 411 DF,  p-value: < 2.2e-16
## 
## [1] "Point -75.5 7"
## [1] "N = 397"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1143.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1144.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -159.532  -27.024    4.609   36.449  136.043 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.300e+02  1.676e+01  19.686  < 2e-16 ***
## el          -5.455e-02  2.004e-02  -2.723  0.00677 ** 
## sl           1.248e-02  2.346e-01   0.053  0.95760    
## I(el^2)      6.852e-06  5.661e-06   1.210  0.22684    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 55.32 on 393 degrees of freedom
## Multiple R-squared:  0.1435,	Adjusted R-squared:  0.137 
## F-statistic: 21.95 on 3 and 393 DF,  p-value: 3.656e-13
## 
## [1] "Point -75.5 7.5"
## [1] "N = 365"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1145.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1146.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -132.900  -43.298   -9.585   47.283  180.133 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.197e+02  6.180e+00  35.554   <2e-16 ***
## el           1.779e-01  1.687e-02  10.545   <2e-16 ***
## sl          -2.654e-01  2.862e-01  -0.927    0.354    
## I(el^2)     -6.565e-05  7.553e-06  -8.692   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 58.52 on 361 degrees of freedom
## Multiple R-squared:  0.3018,	Adjusted R-squared:  0.296 
## F-statistic: 52.02 on 3 and 361 DF,  p-value: < 2.2e-16
## 
## [1] "Point -75 6"
## [1] "N = 224"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1147.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1148.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -77.485 -32.022  -7.708  30.545 153.408 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.336e+02  9.338e+00  14.306   <2e-16 ***
## el           2.869e-01  1.796e-02  15.975   <2e-16 ***
## sl           2.800e-01  3.513e-01   0.797    0.426    
## I(el^2)     -1.086e-04  6.933e-06 -15.664   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 43.34 on 220 degrees of freedom
## Multiple R-squared:  0.5772,	Adjusted R-squared:  0.5714 
## F-statistic: 100.1 on 3 and 220 DF,  p-value: < 2.2e-16
## 
## [1] "Point -75 6.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1149.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1150.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -126.945  -23.265   -8.615   12.945  156.015 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.131e+02  1.155e+01   9.791   <2e-16 ***
## el           2.490e-01  1.710e-02  14.559   <2e-16 ***
## sl          -2.964e-02  2.494e-01  -0.119    0.905    
## I(el^2)     -9.003e-05  5.889e-06 -15.288   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 40.32 on 407 degrees of freedom
## Multiple R-squared:  0.3692,	Adjusted R-squared:  0.3645 
## F-statistic:  79.4 on 3 and 407 DF,  p-value: < 2.2e-16
## 
## [1] "Point -75 7"
## [1] "N = 381"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1151.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1152.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -100.980  -30.999   -1.648   27.186  113.780 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.282e+02  9.148e+00  46.813   <2e-16 ***
## el          -2.184e-01  1.667e-02 -13.104   <2e-16 ***
## sl           6.411e-01  2.545e-01   2.519   0.0122 *  
## I(el^2)      6.340e-05  7.163e-06   8.851   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 44.11 on 377 degrees of freedom
## Multiple R-squared:  0.5248,	Adjusted R-squared:  0.5211 
## F-statistic: 138.8 on 3 and 377 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74.5 7"
## [1] "N = 389"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1153.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1154.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -123.423  -27.853   -0.517   20.995  135.460 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.325e+02  7.462e+00  44.555   <2e-16 ***
## el          -6.972e-02  2.879e-02  -2.422   0.0159 *  
## sl           7.347e-01  3.420e-01   2.148   0.0323 *  
## I(el^2)     -1.398e-05  2.504e-05  -0.558   0.5770    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 46.87 on 385 degrees of freedom
## Multiple R-squared:  0.1895,	Adjusted R-squared:  0.1831 
## F-statistic:    30 on 3 and 385 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74.5 7.5"
## [1] "N = 425"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1155.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1156.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -187.991  -26.252    1.277   29.961  113.269 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.716e+02  6.756e+00  69.811  < 2e-16 ***
## el          -2.729e-01  2.009e-02 -13.584  < 2e-16 ***
## sl           7.979e-02  2.835e-01   0.281    0.778    
## I(el^2)      7.582e-05  1.404e-05   5.401 1.11e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 47.79 on 421 degrees of freedom
## Multiple R-squared:  0.643,	Adjusted R-squared:  0.6405 
## F-statistic: 252.8 on 3 and 421 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74.5 8"
## [1] "N = 409"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1157.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1158.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -160.08  -42.41   11.79   44.70  132.57 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.516e+02  6.444e+00  70.082  < 2e-16 ***
## el          -2.301e-01  2.451e-02  -9.390  < 2e-16 ***
## sl           1.941e+00  3.641e-01   5.329 1.64e-07 ***
## I(el^2)      1.777e-05  1.772e-05   1.003    0.317    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 65.11 on 405 degrees of freedom
## Multiple R-squared:  0.6651,	Adjusted R-squared:  0.6626 
## F-statistic: 268.1 on 3 and 405 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74 6"
## [1] "N = 219"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1159.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1160.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -49.226 -14.399  -4.408  18.132  61.532 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.304e+02  3.305e+00  69.721  < 2e-16 ***
## el           6.041e-02  1.013e-02   5.965 9.96e-09 ***
## sl           4.505e-01  2.136e-01   2.109   0.0361 *  
## I(el^2)     -2.585e-05  4.099e-06  -6.308 1.59e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.11 on 215 degrees of freedom
## Multiple R-squared:  0.2382,	Adjusted R-squared:  0.2276 
## F-statistic: 22.41 on 3 and 215 DF,  p-value: 1.155e-12
## 
## [1] "Point -74 8"
## [1] "N = 422"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1161.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1162.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -99.067 -21.431   0.708  14.644 306.398 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.830e+02  3.672e+00  49.847  < 2e-16 ***
## el           1.280e-01  1.945e-02   6.583 1.38e-10 ***
## sl           7.658e-01  3.073e-01   2.492   0.0131 *  
## I(el^2)     -8.834e-05  1.438e-05  -6.143 1.89e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 43.57 on 418 degrees of freedom
## Multiple R-squared:  0.1794,	Adjusted R-squared:  0.1735 
## F-statistic: 30.47 on 3 and 418 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74 10.5"
## [1] "N = 405"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1163.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1164.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -72.638 -12.137   1.722  16.617  58.639 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.331e+02  1.721e+00  77.369  < 2e-16 ***
## el           1.474e-01  3.876e-03  38.032  < 2e-16 ***
## sl          -3.926e-01  1.323e-01  -2.967  0.00318 ** 
## I(el^2)     -2.728e-05  8.790e-07 -31.034  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 22.53 on 401 degrees of freedom
## Multiple R-squared:  0.8936,	Adjusted R-squared:  0.8929 
## F-statistic:  1123 on 3 and 401 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74 11"
## [1] "N = 345"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1165.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1166.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -119.001  -29.283   -5.355   18.105  225.775 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.442e+02  5.213e+00  27.666  < 2e-16 ***
## el           1.272e-01  9.409e-03  13.518  < 2e-16 ***
## sl           9.180e-01  3.016e-01   3.044  0.00252 ** 
## I(el^2)     -2.545e-05  2.088e-06 -12.185  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 56.79 on 341 degrees of freedom
## Multiple R-squared:  0.5798,	Adjusted R-squared:  0.5762 
## F-statistic: 156.9 on 3 and 341 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74 11.5"
## [1] "N = 87"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1167.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1168.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -148.709  -43.849   -1.236   43.183  192.224 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.778e+02  1.711e+01  10.388  < 2e-16 ***
## el           1.555e-01  5.716e-02   2.721  0.00792 ** 
## sl           5.357e-01  9.518e-01   0.563  0.57503    
## I(el^2)     -5.058e-05  3.011e-05  -1.680  0.09681 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 88.44 on 83 degrees of freedom
## Multiple R-squared:  0.2032,	Adjusted R-squared:  0.1744 
## F-statistic: 7.056 on 3 and 83 DF,  p-value: 0.0002781
## 
## [1] "Point -74 18"
## [1] "N = 87"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1169.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1170.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -37.008 -11.649  -0.117   9.130  55.138 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.027e+02  3.350e+00  30.649   <2e-16 ***
## el           3.980e-01  1.668e-02  23.860   <2e-16 ***
## sl          -2.971e-01  2.241e-01  -1.326    0.189    
## I(el^2)     -2.102e-04  1.158e-05 -18.149   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 17.24 on 83 degrees of freedom
## Multiple R-squared:  0.9368,	Adjusted R-squared:  0.9345 
## F-statistic: 409.8 on 3 and 83 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73.5 6"
## [1] "N = 202"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1171.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1172.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -67.170 -19.515  -0.702  16.820  96.155 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.774e+02  9.817e+00  18.073   <2e-16 ***
## el           1.736e-01  1.262e-02  13.763   <2e-16 ***
## sl          -5.333e-01  2.214e-01  -2.409   0.0169 *  
## I(el^2)     -6.073e-05  4.049e-06 -14.999   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 28.1 on 198 degrees of freedom
## Multiple R-squared:  0.5476,	Adjusted R-squared:  0.5407 
## F-statistic: 79.87 on 3 and 198 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73.5 6.5"
## [1] "N = 392"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1173.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1174.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -143.100  -34.253    7.711   36.359  123.692 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.960e+02  8.440e+00  35.075  < 2e-16 ***
## el          -7.749e-02  1.439e-02  -5.385 1.26e-07 ***
## sl          -6.414e-01  3.201e-01  -2.004   0.0458 *  
## I(el^2)      2.478e-05  6.029e-06   4.110 4.83e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 56.16 on 388 degrees of freedom
## Multiple R-squared:  0.1144,	Adjusted R-squared:  0.1076 
## F-statistic: 16.71 on 3 and 388 DF,  p-value: 3.138e-10
## 
## [1] "Point -73.5 7"
## [1] "N = 374"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1175.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1176.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -123.900  -12.956    0.615   16.423   97.714 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.113e+02  2.954e+00 105.374   <2e-16 ***
## el          -2.419e-01  7.762e-03 -31.169   <2e-16 ***
## sl           3.865e-01  2.006e-01   1.927   0.0547 .  
## I(el^2)      7.662e-05  3.569e-06  21.467   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.79 on 370 degrees of freedom
## Multiple R-squared:  0.8112,	Adjusted R-squared:  0.8096 
## F-statistic: 529.8 on 3 and 370 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73.5 8"
## [1] "N = 446"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1177.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1178.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -80.265 -17.214  -3.253  18.234 114.825 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.082e+02  2.465e+00  84.456  < 2e-16 ***
## el          -1.064e-01  8.348e-03 -12.749  < 2e-16 ***
## sl           2.942e-01  2.134e-01   1.379    0.169    
## I(el^2)      2.937e-05  3.580e-06   8.204 2.57e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.83 on 442 degrees of freedom
## Multiple R-squared:  0.5274,	Adjusted R-squared:  0.5242 
## F-statistic: 164.4 on 3 and 442 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73.5 8.5"
## [1] "N = 439"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1179.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1180.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -123.878  -29.236   -9.786   20.519  163.516 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.746e+02  3.965e+00  44.023  < 2e-16 ***
## el          -4.257e-02  1.275e-02  -3.338 0.000916 ***
## sl           1.169e+00  2.485e-01   4.702 3.47e-06 ***
## I(el^2)     -9.432e-08  6.621e-06  -0.014 0.988640    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 46.51 on 435 degrees of freedom
## Multiple R-squared:  0.1542,	Adjusted R-squared:  0.1483 
## F-statistic: 26.43 on 3 and 435 DF,  p-value: 1.011e-15
## 
## [1] "Point -73.5 9"
## [1] "N = 406"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1181.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1182.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -105.921  -36.476   -0.865   33.684  201.315 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.437e+02  4.185e+00  58.235  < 2e-16 ***
## el          -3.900e-02  1.377e-02  -2.832  0.00486 ** 
## sl           4.242e-01  2.727e-01   1.556  0.12058    
## I(el^2)      6.438e-06  6.994e-06   0.921  0.35785    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 49.95 on 402 degrees of freedom
## Multiple R-squared:  0.07011,	Adjusted R-squared:  0.06317 
## F-statistic:  10.1 on 3 and 402 DF,  p-value: 1.978e-06
## 
## [1] "Point -73.5 10.5"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1183.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1184.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -56.678 -22.895  -0.598  19.657  77.346 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.416e+01  2.590e+00  32.488   <2e-16 ***
## el           1.853e-01  4.287e-03  43.221   <2e-16 ***
## sl          -2.512e-01  1.500e-01  -1.674   0.0949 .  
## I(el^2)     -3.405e-05  9.318e-07 -36.543   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 26.74 on 396 degrees of freedom
## Multiple R-squared:  0.9058,	Adjusted R-squared:  0.905 
## F-statistic:  1269 on 3 and 396 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73.5 11"
## [1] "N = 374"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1185.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1186.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -139.962  -16.849    1.334   33.279  164.227 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.058e+02  6.535e+00  31.488  < 2e-16 ***
## el           6.819e-02  8.788e-03   7.759 8.41e-14 ***
## sl           2.028e-01  2.914e-01   0.696    0.487    
## I(el^2)     -1.311e-05  1.916e-06  -6.844 3.20e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 57.06 on 370 degrees of freedom
## Multiple R-squared:  0.2301,	Adjusted R-squared:  0.2238 
## F-statistic: 36.86 on 3 and 370 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 6"
## [1] "N = 100"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1187.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1188.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -69.995 -20.217   0.001  16.568  88.028 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.638e+02  1.568e+01  10.447   <2e-16 ***
## el           2.247e-01  1.735e-02  12.952   <2e-16 ***
## sl          -7.569e-01  4.298e-01  -1.761   0.0814 .  
## I(el^2)     -8.100e-05  5.132e-06 -15.783   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 32.14 on 96 degrees of freedom
## Multiple R-squared:  0.7604,	Adjusted R-squared:  0.7529 
## F-statistic: 101.5 on 3 and 96 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 6.5"
## [1] "N = 184"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1189.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1190.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -114.786  -67.443    0.464   57.635  183.189 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.701e+01  2.784e+01   2.766 0.006271 ** 
## el           1.262e-01  3.494e-02   3.612 0.000393 ***
## sl          -9.126e-01  5.965e-01  -1.530 0.127780    
## I(el^2)     -2.261e-05  1.202e-05  -1.881 0.061586 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 75.55 on 180 degrees of freedom
## Multiple R-squared:  0.165,	Adjusted R-squared:  0.151 
## F-statistic: 11.85 on 3 and 180 DF,  p-value: 4.028e-07
## 
## [1] "Point -73 7"
## [1] "N = 216"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1191.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1192.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -107.605  -28.642   -0.702   31.108   95.414 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.846e+02  1.000e+01  18.457  < 2e-16 ***
## el          -7.102e-02  1.594e-02  -4.454 1.36e-05 ***
## sl           7.576e-01  2.831e-01   2.676 0.008029 ** 
## I(el^2)      2.000e-05  5.958e-06   3.357 0.000934 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 40.86 on 212 degrees of freedom
## Multiple R-squared:  0.1278,	Adjusted R-squared:  0.1155 
## F-statistic: 10.36 on 3 and 212 DF,  p-value: 2.17e-06
## 
## [1] "Point -73 7.5"
## [1] "N = 206"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1193.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1194.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -117.87  -17.54    3.72   27.30   82.09 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.269e+02  6.230e+00  36.415  < 2e-16 ***
## el          -5.701e-02  1.175e-02  -4.852 2.44e-06 ***
## sl           1.047e+00  2.615e-01   4.004 8.75e-05 ***
## I(el^2)      2.927e-06  3.870e-06   0.756     0.45    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 38.78 on 202 degrees of freedom
## Multiple R-squared:  0.5164,	Adjusted R-squared:  0.5092 
## F-statistic: 71.89 on 3 and 202 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 8"
## [1] "N = 215"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1195.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1196.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -119.248  -22.581    2.197   24.499  105.934 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.292e+02  9.356e+00  24.502  < 2e-16 ***
## el          -1.038e-01  1.383e-02  -7.503 1.72e-12 ***
## sl           9.276e-01  2.871e-01   3.230  0.00143 ** 
## I(el^2)      2.137e-05  4.400e-06   4.858 2.31e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 44.77 on 211 degrees of freedom
## Multiple R-squared:  0.3476,	Adjusted R-squared:  0.3383 
## F-statistic: 37.47 on 3 and 211 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 8.5"
## [1] "N = 218"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1197.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1198.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -224.697  -31.176    3.263   36.479  154.281 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.677e+02  1.407e+01  26.142  < 2e-16 ***
## el          -2.445e-01  3.053e-02  -8.008 7.37e-14 ***
## sl           8.014e-01  4.007e-01   2.000 0.046786 *  
## I(el^2)      5.379e-05  1.404e-05   3.830 0.000168 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 61.02 on 214 degrees of freedom
## Multiple R-squared:  0.5666,	Adjusted R-squared:  0.5606 
## F-statistic: 93.27 on 3 and 214 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 9"
## [1] "N = 214"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1199.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1200.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -102.68  -35.72   -5.92   30.91  139.87 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.250e+02  9.665e+00  43.974  < 2e-16 ***
## el          -2.562e-01  2.126e-02 -12.051  < 2e-16 ***
## sl          -2.418e-01  2.979e-01  -0.812    0.418    
## I(el^2)      8.372e-05  1.013e-05   8.264 1.59e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 47.21 on 210 degrees of freedom
## Multiple R-squared:  0.573,	Adjusted R-squared:  0.5669 
## F-statistic: 93.95 on 3 and 210 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 9.5"
## [1] "N = 202"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1201.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1202.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -90.184 -39.005  -8.655  27.310 179.507 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.235e+02  8.066e+00  27.708  < 2e-16 ***
## el           1.353e-01  2.042e-02   6.624 3.22e-10 ***
## sl           1.504e-01  3.692e-01   0.407    0.684    
## I(el^2)     -5.020e-05  8.698e-06  -5.772 2.99e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 54.59 on 198 degrees of freedom
## Multiple R-squared:  0.2559,	Adjusted R-squared:  0.2447 
## F-statistic:  22.7 on 3 and 198 DF,  p-value: 1.125e-12
## 
## [1] "Point -73 10"
## [1] "N = 232"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1203.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1204.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -76.49 -29.29 -15.32  16.22 217.49 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.405e+02  5.651e+00  24.865  < 2e-16 ***
## el           1.609e-01  1.887e-02   8.525 2.13e-15 ***
## sl          -8.450e-01  4.081e-01  -2.071 0.039525 *  
## I(el^2)     -3.104e-05  7.983e-06  -3.888 0.000132 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 50.21 on 228 degrees of freedom
## Multiple R-squared:  0.6001,	Adjusted R-squared:  0.5948 
## F-statistic:   114 on 3 and 228 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 10.5"
## [1] "N = 228"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1205.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1206.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -82.127 -29.193  -5.451  15.658 211.839 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.012e+02  5.820e+00  17.381  < 2e-16 ***
## el           1.049e-01  1.987e-02   5.282 3.03e-07 ***
## sl          -2.263e-01  3.972e-01  -0.570    0.569    
## I(el^2)      4.176e-06  8.402e-06   0.497    0.620    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 45.09 on 224 degrees of freedom
## Multiple R-squared:  0.6739,	Adjusted R-squared:  0.6695 
## F-statistic: 154.3 on 3 and 224 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 11"
## [1] "N = 202"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1207.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1208.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -91.024 -44.722  -7.232  32.761 136.654 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.297e+02  5.661e+00  22.910  < 2e-16 ***
## el           8.371e-02  1.582e-02   5.292  3.2e-07 ***
## sl           8.809e-03  4.872e-01   0.018    0.986    
## I(el^2)     -6.839e-06  4.589e-06  -1.490    0.138    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 50.49 on 198 degrees of freedom
## Multiple R-squared:  0.5443,	Adjusted R-squared:  0.5374 
## F-statistic: 78.83 on 3 and 198 DF,  p-value: < 2.2e-16
## 
## [1] "Variable prec_12_cl"
## [1] "Point -106 23.5"
## [1] "N = 424"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1209.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1210.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -21.840  -6.599  -0.349   5.263  33.015 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.325e+01  9.221e-01  36.059   <2e-16 ***
## el           5.552e-02  2.711e-03  20.479   <2e-16 ***
## sl          -6.746e-02  5.376e-02  -1.255     0.21    
## I(el^2)     -1.333e-05  1.078e-06 -12.360   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.644 on 420 degrees of freedom
## Multiple R-squared:  0.8065,	Adjusted R-squared:  0.8051 
## F-statistic: 583.5 on 3 and 420 DF,  p-value: < 2.2e-16
## 
## [1] "Point -106 24"
## [1] "N = 213"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1211.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1212.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.488  -7.458  -1.816   6.069  27.755 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.805e+01  2.145e+00  31.720   <2e-16 ***
## el           2.042e-03  4.594e-03   0.445    0.657    
## sl          -9.022e-02  7.199e-02  -1.253    0.212    
## I(el^2)      2.431e-06  1.583e-06   1.536    0.126    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.9 on 209 degrees of freedom
## Multiple R-squared:  0.2605,	Adjusted R-squared:  0.2499 
## F-statistic: 24.54 on 3 and 209 DF,  p-value: 1.204e-13
## 
## [1] "Point -105.5 20.5"
## [1] "N = 217"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1213.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1214.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.3213 -1.3062  0.1451  1.4320  5.0720 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.405e+01  3.434e-01  70.031  < 2e-16 ***
## el           1.187e-02  1.005e-03  11.809  < 2e-16 ***
## sl          -6.611e-02  1.635e-02  -4.044 7.34e-05 ***
## I(el^2)     -3.791e-06  5.427e-07  -6.985 3.58e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.292 on 213 degrees of freedom
## Multiple R-squared:  0.6072,	Adjusted R-squared:  0.6017 
## F-statistic: 109.7 on 3 and 213 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105.5 23"
## [1] "N = 386"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1215.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1216.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -24.4502  -6.6813   0.7807   6.9176  22.8464 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.765e+01  9.290e-01  40.534  < 2e-16 ***
## el           3.255e-02  2.901e-03  11.220  < 2e-16 ***
## sl           6.398e-02  4.634e-02   1.381  0.16817    
## I(el^2)     -4.355e-06  1.388e-06  -3.138  0.00183 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.947 on 382 degrees of freedom
## Multiple R-squared:  0.7677,	Adjusted R-squared:  0.7658 
## F-statistic: 420.7 on 3 and 382 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105.5 23.5"
## [1] "N = 439"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1217.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1218.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -31.972 -10.397   0.225   8.969  36.009 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.322e+01  3.314e+00  16.060  < 2e-16 ***
## el           2.237e-02  4.580e-03   4.884 1.46e-06 ***
## sl           5.262e-02  5.661e-02   0.930  0.35313    
## I(el^2)     -4.423e-06  1.426e-06  -3.102  0.00205 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.4 on 435 degrees of freedom
## Multiple R-squared:  0.1734,	Adjusted R-squared:  0.1677 
## F-statistic: 30.43 on 3 and 435 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105.5 24"
## [1] "N = 226"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1219.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1220.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -35.600 -14.577   0.353  11.717  39.712 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.677e+01  4.999e+00  13.356  < 2e-16 ***
## el          -1.740e-02  7.436e-03  -2.340 0.020150 *  
## sl           2.909e-01  9.932e-02   2.929 0.003755 ** 
## I(el^2)      7.879e-06  2.354e-06   3.348 0.000957 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 17.66 on 222 degrees of freedom
## Multiple R-squared:  0.1127,	Adjusted R-squared:  0.1007 
## F-statistic: 9.402 on 3 and 222 DF,  p-value: 7.1e-06
## 
## [1] "Point -105 20"
## [1] "N = 431"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1221.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1222.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.8272 -2.0769 -0.1356  1.9162 12.7665 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.776e+01  3.584e-01  77.464  < 2e-16 ***
## el           4.805e-03  9.867e-04   4.870 1.57e-06 ***
## sl           1.991e-02  1.844e-02   1.080    0.281    
## I(el^2)     -2.675e-06  4.520e-07  -5.919 6.67e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.221 on 427 degrees of freedom
## Multiple R-squared:  0.09247,	Adjusted R-squared:  0.08609 
## F-statistic:  14.5 on 3 and 427 DF,  p-value: 5.175e-09
## 
## [1] "Point -105 20.5"
## [1] "N = 458"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1223.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1224.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -13.7329  -3.3131  -0.0994   3.6023   9.1118 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.450e+01  5.931e-01  41.302   <2e-16 ***
## el           2.496e-03  1.227e-03   2.035   0.0425 *  
## sl          -1.440e-03  1.894e-02  -0.076   0.9394    
## I(el^2)     -4.461e-07  5.366e-07  -0.831   0.4062    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.258 on 454 degrees of freedom
## Multiple R-squared:  0.05067,	Adjusted R-squared:  0.0444 
## F-statistic: 8.077 on 3 and 454 DF,  p-value: 2.98e-05
## 
## [1] "Point -105 21"
## [1] "N = 367"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1225.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1226.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.4421 -3.0266 -0.1096  2.6692 10.4999 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.646e+01  5.107e-01  51.815  < 2e-16 ***
## el          -1.889e-02  1.208e-03 -15.633  < 2e-16 ***
## sl           5.404e-02  1.906e-02   2.836  0.00482 ** 
## I(el^2)      9.664e-06  6.576e-07  14.696  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.854 on 363 degrees of freedom
## Multiple R-squared:  0.4024,	Adjusted R-squared:  0.3975 
## F-statistic: 81.49 on 3 and 363 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105 21.5"
## [1] "N = 374"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1227.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1228.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -13.5145  -3.0498   0.6688   2.7604  14.9560 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.711e+01  4.665e-01  58.108  < 2e-16 ***
## el          -7.025e-03  1.624e-03  -4.327 1.95e-05 ***
## sl           1.247e-01  2.519e-02   4.951 1.13e-06 ***
## I(el^2)      1.567e-06  1.128e-06   1.389    0.166    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.487 on 370 degrees of freedom
## Multiple R-squared:   0.22,	Adjusted R-squared:  0.2137 
## F-statistic:  34.8 on 3 and 370 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105 22"
## [1] "N = 437"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1229.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1230.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.3905  -1.4054   0.5752   1.7443  10.6919 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.722e+01  2.665e-01 102.104  < 2e-16 ***
## el           4.935e-03  9.777e-04   5.048 6.59e-07 ***
## sl          -6.315e-03  1.784e-02  -0.354    0.723    
## I(el^2)      2.530e-06  4.691e-07   5.393 1.14e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.241 on 433 degrees of freedom
## Multiple R-squared:  0.7306,	Adjusted R-squared:  0.7288 
## F-statistic: 391.5 on 3 and 433 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105 22.5"
## [1] "N = 427"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1231.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1232.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.120  -6.896  -1.306   5.065  26.105 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.953e+01  1.132e+00  26.086  < 2e-16 ***
## el           1.169e-02  2.670e-03   4.379  1.5e-05 ***
## sl          -1.010e-01  4.533e-02  -2.227   0.0264 *  
## I(el^2)      3.098e-07  1.190e-06   0.260   0.7946    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.596 on 423 degrees of freedom
## Multiple R-squared:  0.443,	Adjusted R-squared:  0.439 
## F-statistic: 112.1 on 3 and 423 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105 23"
## [1] "N = 417"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1233.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1234.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -29.7989 -10.9822   0.3863   9.3196  31.0433 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.496e+01  3.423e+00   7.292 1.57e-12 ***
## el           3.750e-02  4.537e-03   8.266 1.92e-15 ***
## sl           1.202e-01  6.178e-02   1.945   0.0524 .  
## I(el^2)     -1.059e-05  1.514e-06  -6.996 1.07e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.44 on 413 degrees of freedom
## Multiple R-squared:  0.1765,	Adjusted R-squared:  0.1705 
## F-statistic: 29.51 on 3 and 413 DF,  p-value: < 2.2e-16
## 
## [1] "Point -105 23.5"
## [1] "N = 427"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1235.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1236.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -34.58 -13.13   0.10   9.64  53.60 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.976e+01  1.158e+01   6.890 2.03e-11 ***
## el          -2.044e-02  1.173e-02  -1.743  0.08214 .  
## sl           2.049e-01  7.557e-02   2.712  0.00696 ** 
## I(el^2)      4.470e-06  2.949e-06   1.516  0.13034    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 17.44 on 423 degrees of freedom
## Multiple R-squared:  0.03934,	Adjusted R-squared:  0.03253 
## F-statistic: 5.774 on 3 and 423 DF,  p-value: 0.0007078
## 
## [1] "Point -104.5 19.5"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1237.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1238.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.3469  -2.4625   0.0262   2.9788  13.2694 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.980e+01  4.800e-01  62.086  < 2e-16 ***
## el          -7.269e-03  1.190e-03  -6.108 2.41e-09 ***
## sl          -5.416e-02  2.301e-02  -2.354    0.019 *  
## I(el^2)      2.567e-06  5.477e-07   4.687 3.82e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.968 on 396 degrees of freedom
## Multiple R-squared:  0.1754,	Adjusted R-squared:  0.1691 
## F-statistic: 28.07 on 3 and 396 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 20"
## [1] "N = 439"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1239.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1240.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.2111 -1.8490 -0.3943  1.7329 10.3155 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.371e+01  6.387e-01  68.430  < 2e-16 ***
## el          -3.382e-02  1.225e-03 -27.612  < 2e-16 ***
## sl           6.486e-02  1.426e-02   4.548 7.02e-06 ***
## I(el^2)      1.183e-05  5.155e-07  22.957  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.898 on 435 degrees of freedom
## Multiple R-squared:  0.7201,	Adjusted R-squared:  0.7182 
## F-statistic: 373.1 on 3 and 435 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 20.5"
## [1] "N = 433"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1241.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1242.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.8477 -2.4269 -0.6599  1.9476 13.0868 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.315e+01  2.136e+00   6.156 1.71e-09 ***
## el          3.683e-03  2.887e-03   1.276  0.20268    
## sl          5.450e-02  1.647e-02   3.309  0.00102 ** 
## I(el^2)     9.116e-07  9.659e-07   0.944  0.34584    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.68 on 429 degrees of freedom
## Multiple R-squared:  0.304,	Adjusted R-squared:  0.2991 
## F-statistic: 62.46 on 3 and 429 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 21"
## [1] "N = 424"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1243.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1244.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.3889 -1.7603 -0.1924  1.6747  7.8347 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.056e+01  8.962e-01  22.935   <2e-16 ***
## el          -1.222e-02  1.387e-03  -8.813   <2e-16 ***
## sl          -5.118e-03  1.061e-02  -0.482     0.63    
## I(el^2)      7.096e-06  5.310e-07  13.364   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.534 on 420 degrees of freedom
## Multiple R-squared:  0.5822,	Adjusted R-squared:  0.5792 
## F-statistic: 195.1 on 3 and 420 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 21.5"
## [1] "N = 442"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1245.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1246.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.143  -4.364  -1.216   3.755  19.561 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.991e+01  1.251e+00  23.913  < 2e-16 ***
## el          -1.316e-02  2.076e-03  -6.337 5.82e-10 ***
## sl           1.161e-03  2.599e-02   0.045    0.964    
## I(el^2)      5.779e-06  8.600e-07   6.720 5.67e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.639 on 438 degrees of freedom
## Multiple R-squared:  0.09453,	Adjusted R-squared:  0.08832 
## F-statistic: 15.24 on 3 and 438 DF,  p-value: 1.889e-09
## 
## [1] "Point -104.5 22"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1247.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1248.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -13.8921  -4.2283  -0.0944   3.3548  14.5543 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.355e+01  1.120e+00  21.026  < 2e-16 ***
## el           5.587e-03  2.025e-03   2.758  0.00606 ** 
## sl           8.024e-02  2.569e-02   3.124  0.00190 ** 
## I(el^2)     -6.975e-07  8.642e-07  -0.807  0.42001    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.611 on 437 degrees of freedom
## Multiple R-squared:  0.1447,	Adjusted R-squared:  0.1389 
## F-statistic: 24.65 on 3 and 437 DF,  p-value: 9.426e-15
## 
## [1] "Point -104.5 22.5"
## [1] "N = 419"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1249.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1250.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.699  -6.869  -1.647   7.158  17.813 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.929e+00  2.723e+00   2.177    0.030 *  
## el           2.696e-02  3.607e-03   7.473 4.69e-13 ***
## sl           3.981e-02  3.637e-02   1.094    0.274    
## I(el^2)     -6.189e-06  1.146e-06  -5.401 1.12e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.891 on 415 degrees of freedom
## Multiple R-squared:  0.276,	Adjusted R-squared:  0.2708 
## F-statistic: 52.75 on 3 and 415 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 23"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1251.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1252.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.115  -9.370  -3.141   8.457  31.148 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.285e+01  6.828e+00   1.883   0.0605 .  
## el           1.507e-02  7.096e-03   2.123   0.0343 *  
## sl           2.583e-01  5.315e-02   4.859 1.68e-06 ***
## I(el^2)     -3.210e-06  1.846e-06  -1.739   0.0828 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.92 on 411 degrees of freedom
## Multiple R-squared:  0.06339,	Adjusted R-squared:  0.05656 
## F-statistic: 9.273 on 3 and 411 DF,  p-value: 6.033e-06
## 
## [1] "Point -104.5 23.5"
## [1] "N = 433"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1253.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1254.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -37.290 -10.825  -1.674  10.031  38.200 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.093e+02  1.714e+01   6.380 4.60e-10 ***
## el          -1.009e-01  1.594e-02  -6.331 6.16e-10 ***
## sl           2.750e-01  6.489e-02   4.238 2.76e-05 ***
## I(el^2)      2.828e-05  3.659e-06   7.730 7.70e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.88 on 429 degrees of freedom
## Multiple R-squared:  0.3477,	Adjusted R-squared:  0.3431 
## F-statistic: 76.21 on 3 and 429 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104.5 24"
## [1] "N = 229"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1255.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1256.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -17.3478  -4.0043  -0.3458   3.7247  20.2592 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.845e+01  2.482e+00   7.436 2.16e-12 ***
## el          -2.490e-02  2.888e-03  -8.621 1.20e-15 ***
## sl           1.045e-01  5.148e-02   2.030   0.0436 *  
## I(el^2)      1.303e-05  9.660e-07  13.489  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.051 on 225 degrees of freedom
## Multiple R-squared:  0.6325,	Adjusted R-squared:  0.6276 
## F-statistic: 129.1 on 3 and 225 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 19"
## [1] "N = 275"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1257.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1258.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.0390 -1.9275 -0.2274  1.7560  8.4420 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.724e+01  3.046e-01  56.593  < 2e-16 ***
## el           4.523e-03  1.120e-03   4.038 7.03e-05 ***
## sl          -2.795e-02  1.762e-02  -1.586    0.114    
## I(el^2)     -1.346e-08  7.360e-07  -0.018    0.985    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.607 on 271 degrees of freedom
## Multiple R-squared:  0.2946,	Adjusted R-squared:  0.2868 
## F-statistic: 37.72 on 3 and 271 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 19.5"
## [1] "N = 442"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1259.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1260.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.1530 -2.3072 -0.9724  2.0482 11.5541 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.073e+01  6.389e-01  32.437  < 2e-16 ***
## el          -2.405e-03  1.014e-03  -2.372   0.0181 *  
## sl           6.084e-02  1.622e-02   3.750   0.0002 ***
## I(el^2)      1.554e-06  3.528e-07   4.404 1.34e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.414 on 438 degrees of freedom
## Multiple R-squared:  0.1733,	Adjusted R-squared:  0.1676 
## F-statistic:  30.6 on 3 and 438 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 20"
## [1] "N = 458"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1261.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1262.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.9448 -1.5164 -0.3057  1.0870 11.0213 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.421e+01  1.243e+00  11.433  < 2e-16 ***
## el          3.319e-03  1.601e-03   2.073   0.0387 *  
## sl          4.818e-02  1.140e-02   4.226 2.87e-05 ***
## I(el^2)     4.348e-07  4.882e-07   0.891   0.3736    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.356 on 454 degrees of freedom
## Multiple R-squared:  0.4604,	Adjusted R-squared:  0.4568 
## F-statistic: 129.1 on 3 and 454 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 21"
## [1] "N = 467"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1263.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1264.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -6.286 -1.631 -0.350  1.641  7.946 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.922e+01  1.426e+00  13.479  < 2e-16 ***
## el          -8.848e-03  1.945e-03  -4.549 6.88e-06 ***
## sl          -4.243e-02  1.047e-02  -4.052 5.97e-05 ***
## I(el^2)      4.964e-06  6.690e-07   7.420 5.66e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.599 on 463 degrees of freedom
## Multiple R-squared:  0.416,	Adjusted R-squared:  0.4122 
## F-statistic: 109.9 on 3 and 463 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 21.5"
## [1] "N = 458"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1265.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1266.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.1054 -2.8655 -0.5056  2.1284 13.7534 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.091e+01  1.667e+00  12.538  < 2e-16 ***
## el          -7.912e-03  2.274e-03  -3.480 0.000551 ***
## sl          -8.294e-02  1.646e-02  -5.039 6.77e-07 ***
## I(el^2)      5.216e-06  7.546e-07   6.913 1.62e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.924 on 454 degrees of freedom
## Multiple R-squared:  0.5333,	Adjusted R-squared:  0.5302 
## F-statistic: 172.9 on 3 and 454 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 22"
## [1] "N = 437"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1267.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1268.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.148  -3.557  -0.277   3.301  21.259 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.769e+01  3.250e+00   5.445  8.7e-08 ***
## el          1.360e-03  4.103e-03   0.331    0.741    
## sl          1.816e-02  2.463e-02   0.737    0.461    
## I(el^2)     9.744e-07  1.256e-06   0.776    0.438    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.823 on 433 degrees of freedom
## Multiple R-squared:  0.1056,	Adjusted R-squared:  0.09944 
## F-statistic: 17.05 on 3 and 433 DF,  p-value: 1.754e-10
## 
## [1] "Point -104 22.5"
## [1] "N = 437"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1269.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1270.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.276  -1.729   0.021   2.279  12.593 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.585e+01  2.563e+00   6.184 1.45e-09 ***
## el          1.534e-03  2.884e-03   0.532    0.595    
## sl          2.356e-02  1.786e-02   1.319    0.188    
## I(el^2)     1.261e-06  8.046e-07   1.568    0.118    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.973 on 433 degrees of freedom
## Multiple R-squared:  0.3009,	Adjusted R-squared:  0.2961 
## F-statistic: 62.13 on 3 and 433 DF,  p-value: < 2.2e-16
## 
## [1] "Point -104 23"
## [1] "N = 417"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1271.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1272.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.9913 -2.1536 -0.2179  2.0422  9.8349 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.571e+01  3.273e+00   4.800 2.22e-06 ***
## el           3.941e-03  3.261e-03   1.209    0.227    
## sl           5.505e-03  1.747e-02   0.315    0.753    
## I(el^2)     -3.986e-07  7.936e-07  -0.502    0.616    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.42 on 413 degrees of freedom
## Multiple R-squared:  0.07933,	Adjusted R-squared:  0.07264 
## F-statistic: 11.86 on 3 and 413 DF,  p-value: 1.819e-07
## 
## [1] "Point -104 23.5"
## [1] "N = 396"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1273.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1274.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.7055 -2.0701 -0.2668  2.2011  7.5780 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.147e+01  5.778e+00   8.907  < 2e-16 ***
## el          -3.811e-02  5.313e-03  -7.173 3.70e-12 ***
## sl           6.632e-02  1.567e-02   4.231 2.90e-05 ***
## I(el^2)      9.970e-06  1.217e-06   8.195 3.59e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.605 on 392 degrees of freedom
## Multiple R-squared:  0.3828,	Adjusted R-squared:  0.3781 
## F-statistic: 81.03 on 3 and 392 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 18.5"
## [1] "N = 240"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1275.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1276.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.4906 -2.2047 -0.2182  1.8182  6.5468 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.450e+01  3.847e-01  37.696  < 2e-16 ***
## el           8.109e-04  1.106e-03   0.733  0.46415    
## sl          -5.538e-02  1.949e-02  -2.842  0.00487 ** 
## I(el^2)      9.680e-07  5.503e-07   1.759  0.07988 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.622 on 236 degrees of freedom
## Multiple R-squared:  0.233,	Adjusted R-squared:  0.2232 
## F-statistic:  23.9 on 3 and 236 DF,  p-value: 1.533e-13
## 
## [1] "Point -103.5 19"
## [1] "N = 376"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1277.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1278.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.6659 -1.5311  0.0361  1.8780  6.4553 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.559e+01  3.250e-01  47.980  < 2e-16 ***
## el          -2.750e-04  7.812e-04  -0.352   0.7250    
## sl          -2.270e-02  1.354e-02  -1.676   0.0945 .  
## I(el^2)      1.536e-06  3.853e-07   3.986 8.08e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.501 on 372 degrees of freedom
## Multiple R-squared:  0.259,	Adjusted R-squared:  0.253 
## F-statistic: 43.34 on 3 and 372 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 19.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1279.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1280.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.7900 -1.9707 -0.1027  1.5113 11.8092 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.369e+01  7.447e-01  18.389  < 2e-16 ***
## el           4.109e-03  1.032e-03   3.982 8.09e-05 ***
## sl           5.351e-02  1.518e-02   3.524 0.000473 ***
## I(el^2)     -1.980e-07  3.401e-07  -0.582 0.560773    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.843 on 407 degrees of freedom
## Multiple R-squared:  0.3612,	Adjusted R-squared:  0.3565 
## F-statistic: 76.71 on 3 and 407 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 20"
## [1] "N = 432"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1281.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1282.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.5305  -4.1926   0.2266   3.0311  14.2035 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.626e+01  4.017e+00   6.537 1.79e-10 ***
## el          -1.356e-02  4.452e-03  -3.046 0.002466 ** 
## sl           1.363e-01  2.345e-02   5.811 1.21e-08 ***
## I(el^2)      4.657e-06  1.192e-06   3.907 0.000109 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.595 on 428 degrees of freedom
## Multiple R-squared:  0.1975,	Adjusted R-squared:  0.1919 
## F-statistic: 35.11 on 3 and 428 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 21"
## [1] "N = 418"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1283.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1284.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.6598 -1.8419 -0.1732  1.6563 15.2926 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.054e+01  2.625e+00   7.826 4.25e-14 ***
## el          -1.332e-02  3.311e-03  -4.024 6.81e-05 ***
## sl          -2.467e-02  1.330e-02  -1.855   0.0643 .  
## I(el^2)      6.172e-06  1.050e-06   5.878 8.56e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.933 on 414 degrees of freedom
## Multiple R-squared:  0.3305,	Adjusted R-squared:  0.3257 
## F-statistic: 68.13 on 3 and 414 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 21.5"
## [1] "N = 427"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1285.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1286.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.7439 -1.6652  0.0377  1.4542  5.9850 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.845e+00  1.715e+00   3.408 0.000718 ***
## el           4.007e-03  2.064e-03   1.941 0.052882 .  
## sl          -3.624e-02  1.117e-02  -3.246 0.001265 ** 
## I(el^2)      1.907e-06  6.069e-07   3.142 0.001795 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.352 on 423 degrees of freedom
## Multiple R-squared:  0.7691,	Adjusted R-squared:  0.7675 
## F-statistic: 469.8 on 3 and 423 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103.5 22"
## [1] "N = 468"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1287.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1288.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.5551 -1.7416 -0.1694  1.7743  8.8170 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 2.041e+00  2.222e+00   0.919  0.35876   
## el          7.649e-03  2.412e-03   3.171  0.00162 **
## sl          1.867e-02  1.059e-02   1.762  0.07867 . 
## I(el^2)     5.951e-07  6.516e-07   0.913  0.36150   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.47 on 464 degrees of freedom
## Multiple R-squared:  0.6491,	Adjusted R-squared:  0.6468 
## F-statistic: 286.1 on 3 and 464 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 18"
## [1] "N = 113"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1289.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1290.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.5269 -1.2883 -0.1690  0.6947  3.4687 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.500e+00  3.867e-01  24.565  < 2e-16 ***
## el          -2.653e-03  1.197e-03  -2.217   0.0287 *  
## sl           1.258e-02  1.704e-02   0.738   0.4619    
## I(el^2)      4.516e-06  7.906e-07   5.713 9.77e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.545 on 109 degrees of freedom
## Multiple R-squared:  0.5719,	Adjusted R-squared:  0.5601 
## F-statistic: 48.53 on 3 and 109 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 18.5"
## [1] "N = 392"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1291.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1292.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.9134 -1.9147 -0.4927  1.3739  8.3755 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.651e+00  4.796e-01  15.955  < 2e-16 ***
## el           7.052e-03  8.213e-04   8.586  < 2e-16 ***
## sl           1.909e-03  1.382e-02   0.138 0.890196    
## I(el^2)     -1.277e-06  3.392e-07  -3.765 0.000192 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.502 on 388 degrees of freedom
## Multiple R-squared:  0.4705,	Adjusted R-squared:  0.4664 
## F-statistic: 114.9 on 3 and 388 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 19"
## [1] "N = 406"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1293.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1294.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.5938 -1.5851 -0.3645  1.4138  8.4985 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.781e+00  4.793e-01   3.715 0.000232 ***
## el           1.871e-02  9.864e-04  18.971  < 2e-16 ***
## sl          -8.583e-04  1.428e-02  -0.060 0.952098    
## I(el^2)     -5.199e-06  3.988e-07 -13.038  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.5 on 402 degrees of freedom
## Multiple R-squared:  0.7195,	Adjusted R-squared:  0.7174 
## F-statistic: 343.8 on 3 and 402 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 19.5"
## [1] "N = 448"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1295.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1296.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.0885 -1.9236 -0.1724  1.7497 13.1094 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -5.129e+00  8.126e-01  -6.311 6.72e-10 ***
## el           3.141e-02  1.391e-03  22.581  < 2e-16 ***
## sl           3.122e-02  1.798e-02   1.737   0.0832 .  
## I(el^2)     -9.855e-06  5.363e-07 -18.378  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.202 on 444 degrees of freedom
## Multiple R-squared:  0.6635,	Adjusted R-squared:  0.6612 
## F-statistic: 291.8 on 3 and 444 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 20"
## [1] "N = 409"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1297.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1298.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.8308 -2.8780 -0.4672  2.7638 15.3287 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.144e+01  6.265e+00   5.018 7.82e-07 ***
## el          -1.956e-02  7.137e-03  -2.740  0.00642 ** 
## sl           2.060e-01  2.205e-02   9.342  < 2e-16 ***
## I(el^2)      5.012e-06  2.015e-06   2.487  0.01329 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.142 on 405 degrees of freedom
## Multiple R-squared:  0.2004,	Adjusted R-squared:  0.1945 
## F-statistic: 33.83 on 3 and 405 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 21.5"
## [1] "N = 406"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1299.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1300.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.0917 -2.0093  0.0808  2.1000  7.1318 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -2.408e+00  4.981e+00  -0.483   0.6291  
## el           9.586e-03  5.127e-03   1.870   0.0623 .
## sl           1.256e-02  1.608e-02   0.781   0.4352  
## I(el^2)     -2.568e-08  1.305e-06  -0.020   0.9843  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.027 on 402 degrees of freedom
## Multiple R-squared:  0.4802,	Adjusted R-squared:  0.4763 
## F-statistic: 123.8 on 3 and 402 DF,  p-value: < 2.2e-16
## 
## [1] "Point -103 22"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1301.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1302.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.398 -1.503  0.076  1.222  7.801 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -2.771e+01  3.947e+00  -7.021 8.65e-12 ***
## el           3.914e-02  3.872e-03  10.109  < 2e-16 ***
## sl          -2.816e-02  1.093e-02  -2.575   0.0104 *  
## I(el^2)     -8.024e-06  9.384e-07  -8.550  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.929 on 430 degrees of freedom
## Multiple R-squared:  0.4941,	Adjusted R-squared:  0.4906 
## F-statistic:   140 on 3 and 430 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102.5 18"
## [1] "N = 183"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1303.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1304.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.786 -1.262  0.008  1.234  6.056 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.427e+01  3.041e-01  46.904  < 2e-16 ***
## el          -8.388e-03  9.704e-04  -8.645 2.98e-15 ***
## sl          -1.041e-01  1.803e-02  -5.773 3.38e-08 ***
## I(el^2)      5.594e-06  5.944e-07   9.410  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.918 on 179 degrees of freedom
## Multiple R-squared:  0.4922,	Adjusted R-squared:  0.4836 
## F-statistic: 57.82 on 3 and 179 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102.5 18.5"
## [1] "N = 408"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1305.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1306.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.1774 -0.8241 -0.1773  0.5156  4.7065 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.983e+00  2.545e-01  35.301  < 2e-16 ***
## el           9.244e-04  5.695e-04   1.623   0.1053    
## sl          -1.207e-02  7.204e-03  -1.675   0.0946 .  
## I(el^2)      1.240e-06  2.826e-07   4.388 1.46e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.313 on 404 degrees of freedom
## Multiple R-squared:  0.5689,	Adjusted R-squared:  0.5657 
## F-statistic: 177.7 on 3 and 404 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102.5 19"
## [1] "N = 438"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1307.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1308.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.2323 -0.8476 -0.2144  0.5981  7.0054 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.934e+00  2.043e-01  29.044  < 2e-16 ***
## el           5.813e-03  5.117e-04  11.359  < 2e-16 ***
## sl           3.425e-02  7.784e-03   4.399 1.37e-05 ***
## I(el^2)     -1.448e-06  2.203e-07  -6.571 1.44e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.475 on 434 degrees of freedom
## Multiple R-squared:  0.5758,	Adjusted R-squared:  0.5729 
## F-statistic: 196.4 on 3 and 434 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102.5 19.5"
## [1] "N = 438"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1309.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1310.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.3777 -2.2298 -0.5942  1.2616 16.7216 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.231e+00  6.840e-01   4.724 3.13e-06 ***
## el           1.256e-02  1.046e-03  12.001  < 2e-16 ***
## sl           7.231e-02  2.200e-02   3.287   0.0011 ** 
## I(el^2)     -3.471e-06  3.487e-07  -9.952  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.703 on 434 degrees of freedom
## Multiple R-squared:  0.3521,	Adjusted R-squared:  0.3476 
## F-statistic:  78.6 on 3 and 434 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102.5 20"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1311.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1312.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.1085 -2.7477 -0.8984  1.6584 13.8939 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.168e+01  4.705e+00  -2.483   0.0134 *  
## el           2.443e-02  5.128e-03   4.764 2.59e-06 ***
## sl           2.245e-01  2.446e-02   9.180  < 2e-16 ***
## I(el^2)     -6.097e-06  1.395e-06  -4.371 1.55e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.642 on 437 degrees of freedom
## Multiple R-squared:  0.2117,	Adjusted R-squared:  0.2063 
## F-statistic: 39.12 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102 18.5"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1313.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1314.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -5.208 -1.621 -0.285  1.245 13.298 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.736e+00  3.930e-01  24.769  < 2e-16 ***
## el          -4.451e-03  1.459e-03  -3.050  0.00244 ** 
## sl          -1.558e-02  1.220e-02  -1.277  0.20235    
## I(el^2)      4.050e-06  1.043e-06   3.885  0.00012 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.239 on 396 degrees of freedom
## Multiple R-squared:  0.05615,	Adjusted R-squared:  0.049 
## F-statistic: 7.852 on 3 and 396 DF,  p-value: 4.213e-05
## 
## [1] "Point -102 19"
## [1] "N = 440"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1315.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1316.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.7946 -1.5436 -0.0236  1.1480  8.3643 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.119e+00  3.233e-01  18.924  < 2e-16 ***
## el           4.062e-03  7.876e-04   5.158 3.80e-07 ***
## sl          -5.766e-02  1.214e-02  -4.751 2.75e-06 ***
## I(el^2)     -2.102e-07  3.456e-07  -0.608    0.543    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.253 on 436 degrees of freedom
## Multiple R-squared:  0.4784,	Adjusted R-squared:  0.4748 
## F-statistic: 133.3 on 3 and 436 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102 19.5"
## [1] "N = 450"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1317.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1318.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.6924 -1.4800 -0.3483  0.9521 10.8542 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.135e+00  6.706e-01   4.675 3.90e-06 ***
## el           7.514e-03  7.945e-04   9.458  < 2e-16 ***
## sl           1.517e-02  1.408e-02   1.077    0.282    
## I(el^2)     -1.442e-06  2.329e-07  -6.193 1.34e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.21 on 446 degrees of freedom
## Multiple R-squared:  0.4047,	Adjusted R-squared:  0.4007 
## F-statistic: 101.1 on 3 and 446 DF,  p-value: < 2.2e-16
## 
## [1] "Point -102 20"
## [1] "N = 433"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1319.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1320.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.9214 -1.4340 -0.4813  0.6545 16.3875 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.420e+01  4.531e+00   3.133  0.00185 ** 
## el          -3.954e-03  4.310e-03  -0.917  0.35945    
## sl           1.160e-01  2.163e-02   5.360 1.36e-07 ***
## I(el^2)      1.170e-06  1.002e-06   1.168  0.24353    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.717 on 429 degrees of freedom
## Multiple R-squared:  0.1139,	Adjusted R-squared:  0.1077 
## F-statistic: 18.37 on 3 and 429 DF,  p-value: 3.123e-11
## 
## [1] "Point -101.5 17.5"
## [1] "N = 152"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1321.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1322.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.6191 -1.7066 -0.2908  0.9374  6.3905 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.619e+00  3.485e-01  21.861   <2e-16 ***
## el          -1.561e-03  1.505e-03  -1.038    0.301    
## sl           4.391e-02  2.203e-02   1.994    0.048 *  
## I(el^2)      1.726e-06  1.164e-06   1.483    0.140    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.198 on 148 degrees of freedom
## Multiple R-squared:  0.05617,	Adjusted R-squared:  0.03704 
## F-statistic: 2.936 on 3 and 148 DF,  p-value: 0.03536
## 
## [1] "Point -101.5 18"
## [1] "N = 376"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1323.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1324.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.4400 -1.4211 -0.1877  1.0858 10.5847 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.373e+01  3.399e-01  40.394   <2e-16 ***
## el          -9.667e-03  7.823e-04 -12.357   <2e-16 ***
## sl           3.190e-03  1.309e-02   0.244    0.808    
## I(el^2)      3.458e-06  3.733e-07   9.264   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.509 on 372 degrees of freedom
## Multiple R-squared:  0.3982,	Adjusted R-squared:  0.3934 
## F-statistic: 82.06 on 3 and 372 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101.5 18.5"
## [1] "N = 426"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1325.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1326.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.7218 -1.3539 -0.3765  0.6374 14.6418 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.322e+00  3.853e-01  21.599  < 2e-16 ***
## el          -4.636e-03  9.038e-04  -5.130 4.43e-07 ***
## sl           1.325e-02  1.193e-02   1.111    0.267    
## I(el^2)      2.312e-06  4.348e-07   5.319 1.70e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.448 on 422 degrees of freedom
## Multiple R-squared:  0.06327,	Adjusted R-squared:  0.05661 
## F-statistic: 9.501 on 3 and 422 DF,  p-value: 4.375e-06
## 
## [1] "Point -101.5 19"
## [1] "N = 468"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1327.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1328.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.2377 -1.0925 -0.4656  0.7628  6.3840 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.207e+00  2.821e-01  18.457  < 2e-16 ***
## el           6.582e-04  4.582e-04   1.436  0.15154    
## sl          -2.058e-02  7.596e-03  -2.710  0.00698 ** 
## I(el^2)      1.316e-06  1.584e-07   8.309 1.07e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.566 on 464 degrees of freedom
## Multiple R-squared:  0.7705,	Adjusted R-squared:  0.769 
## F-statistic: 519.3 on 3 and 464 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101.5 19.5"
## [1] "N = 460"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1329.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1330.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.5910 -1.3274 -0.1678  1.0530  9.9140 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.418e+00  1.293e+00   3.418 0.000688 ***
## el           3.582e-03  1.344e-03   2.666 0.007960 ** 
## sl          -2.342e-02  1.154e-02  -2.030 0.042957 *  
## I(el^2)      1.284e-07  3.498e-07   0.367 0.713845    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.105 on 456 degrees of freedom
## Multiple R-squared:  0.434,	Adjusted R-squared:  0.4303 
## F-statistic: 116.6 on 3 and 456 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101 17.5"
## [1] "N = 314"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1331.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1332.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.1428 -0.8590  0.0105  0.9905  5.3850 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.866e+00  1.697e-01  34.578  < 2e-16 ***
## el           3.178e-03  4.013e-04   7.921 4.27e-14 ***
## sl          -8.559e-03  8.819e-03  -0.970    0.333    
## I(el^2)      1.964e-08  1.653e-07   0.119    0.906    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.444 on 310 degrees of freedom
## Multiple R-squared:  0.7248,	Adjusted R-squared:  0.7221 
## F-statistic: 272.1 on 3 and 310 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101 18"
## [1] "N = 389"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1333.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1334.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.6431 -1.5292 -0.4583  1.6536  7.4127 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 3.311e+00  3.998e-01   8.282 2.03e-15 ***
## el          1.636e-03  7.214e-04   2.268 0.023874 *  
## sl          7.038e-03  9.443e-03   0.745 0.456516    
## I(el^2)     9.197e-07  2.715e-07   3.388 0.000777 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.941 on 385 degrees of freedom
## Multiple R-squared:  0.6015,	Adjusted R-squared:  0.5984 
## F-statistic: 193.7 on 3 and 385 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101 19"
## [1] "N = 456"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1335.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1336.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -5.806 -2.782 -1.406  1.177 26.589 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -2.217e+00  1.217e+00  -1.821   0.0692 .  
## el           1.937e-02  2.469e-03   7.848 3.09e-14 ***
## sl           2.124e-02  2.573e-02   0.826   0.4094    
## I(el^2)     -6.854e-06  1.054e-06  -6.505 2.07e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.834 on 452 degrees of freedom
## Multiple R-squared:  0.1817,	Adjusted R-squared:  0.1763 
## F-statistic: 33.45 on 3 and 452 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101 19.5"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1337.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1338.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.7459  -2.1219   0.1796   2.0198  17.3928 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.698e+01  1.591e+00  23.240   <2e-16 ***
## el          -3.222e-02  1.872e-03 -17.215   <2e-16 ***
## sl           3.590e-02  1.853e-02   1.937   0.0534 .  
## I(el^2)      9.260e-06  5.406e-07  17.131   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.751 on 411 degrees of freedom
## Multiple R-squared:  0.4297,	Adjusted R-squared:  0.4256 
## F-statistic: 103.2 on 3 and 411 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101 23"
## [1] "N = 430"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1339.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1340.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.3885 -1.7271  0.4206  2.1076  5.2545 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.740e+01  5.174e+00   3.363  0.00084 ***
## el          -4.175e-03  5.489e-03  -0.761  0.44730    
## sl           1.332e-02  1.850e-02   0.720  0.47188    
## I(el^2)      2.274e-06  1.446e-06   1.572  0.11671    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.696 on 426 degrees of freedom
## Multiple R-squared:  0.1656,	Adjusted R-squared:  0.1597 
## F-statistic: 28.17 on 3 and 426 DF,  p-value: < 2.2e-16
## 
## [1] "Point -101 23.5"
## [1] "N = 446"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1341.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1342.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.8244 -2.6281 -0.1847  2.6158  7.2910 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.343e+01  4.958e+00  10.778  < 2e-16 ***
## el          -3.931e-02  5.051e-03  -7.784 5.05e-14 ***
## sl          -4.074e-02  1.936e-02  -2.104   0.0359 *  
## I(el^2)      1.051e-05  1.283e-06   8.198 2.68e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.047 on 442 degrees of freedom
## Multiple R-squared:  0.157,	Adjusted R-squared:  0.1513 
## F-statistic: 27.44 on 3 and 442 DF,  p-value: 2.713e-16
## 
## [1] "Point -100.5 17"
## [1] "N = 164"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1343.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1344.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.1943 -1.5235 -0.3607  1.6647  4.0422 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.562e+00  2.870e-01  22.866  < 2e-16 ***
## el           5.347e-03  1.307e-03   4.092 6.77e-05 ***
## sl          -6.848e-02  2.041e-02  -3.355 0.000992 ***
## I(el^2)      1.009e-06  9.888e-07   1.021 0.308904    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.05 on 160 degrees of freedom
## Multiple R-squared:  0.5376,	Adjusted R-squared:  0.5289 
## F-statistic:    62 on 3 and 160 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 17.5"
## [1] "N = 390"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1345.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1346.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.4757 -1.8488 -0.0919  1.3984  7.7194 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.760e+00  3.409e-01  22.760   <2e-16 ***
## el           1.141e-03  6.092e-04   1.873   0.0619 .  
## sl          -3.509e-02  1.358e-02  -2.584   0.0101 *  
## I(el^2)      3.737e-07  2.199e-07   1.699   0.0901 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.55 on 386 degrees of freedom
## Multiple R-squared:  0.2375,	Adjusted R-squared:  0.2316 
## F-statistic: 40.08 on 3 and 386 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 18"
## [1] "N = 399"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1347.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1348.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.4115 -0.8854 -0.0145  0.8682  5.6640 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.416e+00  2.614e-01   9.242  < 2e-16 ***
## el           1.388e-03  5.658e-04   2.452   0.0146 *  
## sl          -1.499e-02  8.741e-03  -1.716   0.0870 .  
## I(el^2)      1.260e-06  2.090e-07   6.027 3.83e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.582 on 395 degrees of freedom
## Multiple R-squared:  0.7553,	Adjusted R-squared:  0.7535 
## F-statistic: 406.5 on 3 and 395 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 18.5"
## [1] "N = 435"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1349.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1350.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.8754 -0.6652 -0.0272  0.3991  5.4653 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.861e+00  1.786e-01  16.019  < 2e-16 ***
## el          -3.197e-04  4.916e-04  -0.650    0.516    
## sl           4.033e-03  5.975e-03   0.675    0.500    
## I(el^2)      1.570e-06  2.334e-07   6.727 5.52e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.054 on 431 degrees of freedom
## Multiple R-squared:  0.5851,	Adjusted R-squared:  0.5822 
## F-statistic: 202.6 on 3 and 431 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 19"
## [1] "N = 420"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1351.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1352.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -6.325 -3.001 -1.310  1.446 27.159 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.276e+00  1.185e+00   3.608 0.000346 ***
## el           4.205e-03  2.108e-03   1.995 0.046681 *  
## sl           5.473e-02  2.336e-02   2.343 0.019622 *  
## I(el^2)     -7.635e-07  8.040e-07  -0.950 0.342843    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.879 on 416 degrees of freedom
## Multiple R-squared:  0.07963,	Adjusted R-squared:  0.073 
## F-statistic:    12 on 3 and 416 DF,  p-value: 1.507e-07
## 
## [1] "Point -100.5 19.5"
## [1] "N = 386"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1353.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1354.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.9866  -2.0571  -0.2918   2.0690  19.3097 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.635e+01  1.482e+00  17.784   <2e-16 ***
## el          -1.653e-02  1.622e-03 -10.187   <2e-16 ***
## sl           1.294e-02  1.928e-02   0.671    0.503    
## I(el^2)      4.644e-06  4.258e-07  10.907   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.83 on 382 degrees of freedom
## Multiple R-squared:  0.2539,	Adjusted R-squared:  0.2481 
## F-statistic: 43.34 on 3 and 382 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 20"
## [1] "N = 413"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1355.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1356.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.8534 -1.7719 -0.5556  1.6322  7.8028 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  1.722e+01  6.508e+00   2.646  0.00845 **
## el          -9.320e-03  5.562e-03  -1.676  0.09459 . 
## sl           2.084e-02  1.723e-02   1.210  0.22709   
## I(el^2)      2.783e-06  1.171e-06   2.376  0.01796 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.803 on 409 degrees of freedom
## Multiple R-squared:  0.1777,	Adjusted R-squared:  0.1716 
## F-statistic: 29.45 on 3 and 409 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 22"
## [1] "N = 467"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1357.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1358.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.7246 -1.1367 -0.1376  1.0737  6.0862 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.625e+01  1.563e+00  16.798  < 2e-16 ***
## el          -1.545e-02  1.990e-03  -7.763 5.37e-14 ***
## sl           2.655e-02  9.650e-03   2.751  0.00617 ** 
## I(el^2)      3.834e-06  6.094e-07   6.291 7.30e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.775 on 463 degrees of freedom
## Multiple R-squared:  0.3053,	Adjusted R-squared:  0.3008 
## F-statistic: 67.83 on 3 and 463 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 22.5"
## [1] "N = 426"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1359.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1360.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.7407 -1.4138 -0.0634  1.8154  6.4942 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.979e+00  3.272e+00   0.910 0.363109    
## el           1.484e-02  4.189e-03   3.543 0.000439 ***
## sl           2.279e-02  1.511e-02   1.509 0.132167    
## I(el^2)     -4.563e-06  1.317e-06  -3.466 0.000583 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.558 on 422 degrees of freedom
## Multiple R-squared:  0.03521,	Adjusted R-squared:  0.02835 
## F-statistic: 5.134 on 3 and 422 DF,  p-value: 0.001697
## 
## [1] "Point -100.5 23"
## [1] "N = 458"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1361.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1362.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.8431 -1.1226 -0.1811  1.0318  6.3753 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.462e-01  2.259e+00   0.109  0.91326    
## el           1.505e-02  2.709e-03   5.555 4.74e-08 ***
## sl          -5.978e-02  9.808e-03  -6.095 2.34e-09 ***
## I(el^2)     -2.525e-06  7.970e-07  -3.169  0.00163 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.897 on 454 degrees of freedom
## Multiple R-squared:  0.4832,	Adjusted R-squared:  0.4798 
## F-statistic: 141.5 on 3 and 454 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100.5 23.5"
## [1] "N = 469"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1363.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1364.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.4656 -1.7497  0.1163  1.2440  4.9616 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.484e+01  2.497e+00   9.948  < 2e-16 ***
## el          -9.435e-03  2.695e-03  -3.501 0.000508 ***
## sl           3.059e-03  1.202e-02   0.254 0.799286    
## I(el^2)      2.742e-06  7.242e-07   3.786 0.000173 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.085 on 465 degrees of freedom
## Multiple R-squared:  0.04538,	Adjusted R-squared:  0.03922 
## F-statistic: 7.368 on 3 and 465 DF,  p-value: 7.835e-05
## 
## [1] "Point -100 17"
## [1] "N = 258"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1365.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1366.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.1775 -1.1672 -0.2830  0.7089  6.8003 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.559e+00  2.303e-01  19.799  < 2e-16 ***
## el           5.405e-03  5.967e-04   9.057  < 2e-16 ***
## sl          -3.798e-03  1.396e-02  -0.272    0.786    
## I(el^2)     -1.550e-06  3.200e-07  -4.844 2.21e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.921 on 254 degrees of freedom
## Multiple R-squared:  0.4313,	Adjusted R-squared:  0.4246 
## F-statistic: 64.21 on 3 and 254 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 17.5"
## [1] "N = 420"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1367.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1368.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.8275 -1.9976 -0.4958  1.3930  9.0984 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.123e+01  6.795e-01  16.525  < 2e-16 ***
## el          -7.029e-03  9.679e-04  -7.262 1.89e-12 ***
## sl          -4.077e-03  1.336e-02  -0.305     0.76    
## I(el^2)      2.636e-06  3.067e-07   8.595  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.741 on 416 degrees of freedom
## Multiple R-squared:  0.1868,	Adjusted R-squared:  0.1809 
## F-statistic: 31.86 on 3 and 416 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 18"
## [1] "N = 438"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1369.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1370.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.5371 -0.7639 -0.1532  0.6191  4.7721 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.956e+00  2.567e-01  19.302   <2e-16 ***
## el          -4.209e-03  4.241e-04  -9.924   <2e-16 ***
## sl           1.018e-02  5.081e-03   2.003   0.0458 *  
## I(el^2)      2.376e-06  1.566e-07  15.170   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.101 on 434 degrees of freedom
## Multiple R-squared:  0.5951,	Adjusted R-squared:  0.5923 
## F-statistic: 212.6 on 3 and 434 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 18.5"
## [1] "N = 437"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1371.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1372.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.3990 -0.9859  0.0043  0.6659  5.1579 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.403e+00  3.725e-01  11.820  < 2e-16 ***
## el          -4.765e-03  5.838e-04  -8.163 3.61e-15 ***
## sl           1.090e-02  6.841e-03   1.593    0.112    
## I(el^2)      3.289e-06  2.046e-07  16.073  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.468 on 433 degrees of freedom
## Multiple R-squared:  0.7611,	Adjusted R-squared:  0.7594 
## F-statistic: 459.7 on 3 and 433 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 19"
## [1] "N = 436"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1373.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1374.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -5.945 -1.756 -0.002  1.478 10.770 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 1.018e+00  1.062e+00   0.959  0.33814   
## el          3.331e-03  1.020e-03   3.265  0.00118 **
## sl          1.193e-02  1.285e-02   0.929  0.35342   
## I(el^2)     5.217e-07  2.367e-07   2.204  0.02807 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.802 on 432 degrees of freedom
## Multiple R-squared:  0.6539,	Adjusted R-squared:  0.6515 
## F-statistic: 272.1 on 3 and 432 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 20"
## [1] "N = 424"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1375.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1376.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.4683 -2.0433  0.0152  2.0264  6.3548 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.244e+01  9.957e+00   5.266 2.23e-07 ***
## el          -3.808e-02  7.965e-03  -4.781 2.42e-06 ***
## sl          -4.717e-02  1.879e-02  -2.510   0.0124 *  
## I(el^2)      8.546e-06  1.586e-06   5.388 1.19e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.785 on 420 degrees of freedom
## Multiple R-squared:  0.1911,	Adjusted R-squared:  0.1853 
## F-statistic: 33.07 on 3 and 420 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 20.5"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1377.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1378.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.6946 -0.6797 -0.1532  0.5977  6.7784 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.399e+01  3.849e+00  -3.635 0.000312 ***
## el           1.755e-02  3.544e-03   4.952 1.06e-06 ***
## sl          -4.148e-03  8.555e-03  -0.485 0.628041    
## I(el^2)     -3.390e-06  8.098e-07  -4.187 3.43e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.336 on 430 degrees of freedom
## Multiple R-squared:  0.2395,	Adjusted R-squared:  0.2342 
## F-statistic: 45.14 on 3 and 430 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 21"
## [1] "N = 445"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1379.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1380.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.9498 -1.2100 -0.4911  0.4994 11.3096 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.701e+00  2.356e+00   3.268  0.00117 ** 
## el          -5.902e-03  2.315e-03  -2.549  0.01114 *  
## sl           9.367e-02  1.041e-02   9.000  < 2e-16 ***
## I(el^2)      2.866e-06  5.726e-07   5.005  8.1e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.333 on 441 degrees of freedom
## Multiple R-squared:  0.4599,	Adjusted R-squared:  0.4562 
## F-statistic: 125.2 on 3 and 441 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 21.5"
## [1] "N = 454"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1381.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1382.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.4197 -1.7325 -0.3774  1.6969  8.5498 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.228e+01  1.280e+00  17.408   <2e-16 ***
## el          -1.542e-02  1.714e-03  -8.999   <2e-16 ***
## sl          -8.927e-03  1.073e-02  -0.832    0.406    
## I(el^2)      4.765e-06  5.320e-07   8.957   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.37 on 450 degrees of freedom
## Multiple R-squared:  0.1634,	Adjusted R-squared:  0.1578 
## F-statistic: 29.29 on 3 and 450 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 23"
## [1] "N = 469"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1383.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1384.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.5831 -1.0390  0.1318  1.0609  5.4980 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.640e+01  1.323e+00  19.955   <2e-16 ***
## el          -1.861e-02  1.693e-03 -10.991   <2e-16 ***
## sl           1.241e-03  9.224e-03   0.135    0.893    
## I(el^2)      7.230e-06  5.138e-07  14.071   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.748 on 465 degrees of freedom
## Multiple R-squared:  0.5588,	Adjusted R-squared:  0.556 
## F-statistic: 196.3 on 3 and 465 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 23.5"
## [1] "N = 445"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1385.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1386.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.5272 -0.8691  0.0776  1.0307  4.8969 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.352e+00  1.280e+00   4.965 9.85e-07 ***
## el           3.450e-03  1.288e-03   2.680 0.007645 ** 
## sl          -1.793e-02  6.983e-03  -2.567 0.010572 *  
## I(el^2)      1.213e-06  3.134e-07   3.870 0.000125 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.47 on 441 degrees of freedom
## Multiple R-squared:  0.8239,	Adjusted R-squared:  0.8227 
## F-statistic: 687.9 on 3 and 441 DF,  p-value: < 2.2e-16
## 
## [1] "Point -100 24"
## [1] "N = 233"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1387.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1388.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.2860 -0.7205  0.0353  0.6224  8.1975 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.680e+01  2.963e-01  56.704   <2e-16 ***
## el          -7.047e-03  3.391e-04 -20.784   <2e-16 ***
## sl          -2.526e-02  8.558e-03  -2.951   0.0035 ** 
## I(el^2)      3.649e-06  1.090e-07  33.497   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.232 on 229 degrees of freedom
## Multiple R-squared:  0.895,	Adjusted R-squared:  0.8936 
## F-statistic: 650.4 on 3 and 229 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 17"
## [1] "N = 351"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1389.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1390.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.6717 -0.9093  0.0178  0.8807  6.7230 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.572e+00  1.458e-01  24.500  < 2e-16 ***
## el           2.620e-03  4.556e-04   5.751 1.95e-08 ***
## sl           3.784e-02  9.336e-03   4.053 6.24e-05 ***
## I(el^2)     -2.780e-07  2.413e-07  -1.152     0.25    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.422 on 347 degrees of freedom
## Multiple R-squared:  0.4531,	Adjusted R-squared:  0.4484 
## F-statistic: 95.83 on 3 and 347 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 17.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1391.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1392.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.6775 -1.4693  0.0085  1.2613  4.3955 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.046e+00  4.275e-01  18.821  < 2e-16 ***
## el          -4.264e-03  6.372e-04  -6.691 7.34e-11 ***
## sl          -3.226e-03  8.384e-03  -0.385    0.701    
## I(el^2)      1.823e-06  2.289e-07   7.964 1.68e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.666 on 407 degrees of freedom
## Multiple R-squared:  0.1678,	Adjusted R-squared:  0.1617 
## F-statistic: 27.36 on 3 and 407 DF,  p-value: 3.859e-16
## 
## [1] "Point -99.5 18"
## [1] "N = 444"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1393.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1394.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.3413 -0.5108 -0.1800  0.6609  2.1245 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.079e+00  3.225e-01  18.850   <2e-16 ***
## el          -5.490e-03  5.543e-04  -9.905   <2e-16 ***
## sl          -7.319e-03  4.224e-03  -1.733   0.0839 .  
## I(el^2)      2.726e-06  2.217e-07  12.298   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.8514 on 440 degrees of freedom
## Multiple R-squared:  0.3963,	Adjusted R-squared:  0.3922 
## F-statistic: 96.28 on 3 and 440 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 18.5"
## [1] "N = 437"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1395.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1396.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.3920 -0.7456  0.0747  0.7984  3.8788 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.319e+00  5.514e-01  13.273  < 2e-16 ***
## el          -6.670e-03  7.947e-04  -8.393 6.81e-16 ***
## sl          -2.428e-02  5.696e-03  -4.262 2.49e-05 ***
## I(el^2)      3.226e-06  2.649e-07  12.180  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.17 on 433 degrees of freedom
## Multiple R-squared:  0.5891,	Adjusted R-squared:  0.5863 
## F-statistic: 206.9 on 3 and 433 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 19"
## [1] "N = 403"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1397.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1398.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.5212 -1.6291 -0.6884  0.8622 14.3002 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.070e+00  1.179e+00   2.604  0.00955 ** 
## el          -1.329e-04  1.114e-03  -0.119  0.90512    
## sl           8.333e-03  1.322e-02   0.630  0.52897    
## I(el^2)      1.030e-06  2.480e-07   4.152 4.04e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.743 on 399 degrees of freedom
## Multiple R-squared:  0.5876,	Adjusted R-squared:  0.5845 
## F-statistic: 189.5 on 3 and 399 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 20"
## [1] "N = 396"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1399.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1400.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.2487 -1.3720 -0.0417  1.1622  6.6324 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -6.279e+00  5.445e+00  -1.153   0.2495  
## el           9.162e-03  4.346e-03   2.108   0.0356 *
## sl          -2.524e-02  1.565e-02  -1.613   0.1076  
## I(el^2)     -1.016e-06  8.608e-07  -1.180   0.2386  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.081 on 392 degrees of freedom
## Multiple R-squared:  0.2221,	Adjusted R-squared:  0.2161 
## F-statistic: 37.31 on 3 and 392 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 20.5"
## [1] "N = 425"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1401.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1402.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.8071 -1.6188 -0.5623  1.1278 11.7222 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -2.136e+00  3.824e+00  -0.559    0.577    
## el           5.582e-03  3.617e-03   1.543    0.124    
## sl           8.638e-02  1.096e-02   7.879 2.85e-14 ***
## I(el^2)     -2.905e-07  8.581e-07  -0.339    0.735    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.39 on 421 degrees of freedom
## Multiple R-squared:  0.293,	Adjusted R-squared:  0.288 
## F-statistic: 58.16 on 3 and 421 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 21"
## [1] "N = 466"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1403.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1404.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -14.050  -5.363  -2.481   2.804  31.906 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.994e+01  3.569e+00  11.190  < 2e-16 ***
## el          -3.079e-02  4.173e-03  -7.378 7.53e-13 ***
## sl           9.970e-02  3.183e-02   3.133  0.00184 ** 
## I(el^2)      7.978e-06  1.222e-06   6.530 1.74e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.21 on 462 degrees of freedom
## Multiple R-squared:  0.1522,	Adjusted R-squared:  0.1466 
## F-statistic: 27.64 on 3 and 462 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 21.5"
## [1] "N = 458"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1405.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1406.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.495  -7.431  -3.635   2.476  45.860 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.061e+01  2.781e+00  18.196   <2e-16 ***
## el          -4.595e-02  4.313e-03 -10.653   <2e-16 ***
## sl           5.221e-02  5.062e-02   1.031    0.303    
## I(el^2)      1.479e-05  1.560e-06   9.479   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.03 on 454 degrees of freedom
## Multiple R-squared:  0.2137,	Adjusted R-squared:  0.2085 
## F-statistic: 41.12 on 3 and 454 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 22"
## [1] "N = 474"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1407.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1408.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -16.328  -5.004  -1.044   4.208  28.918 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.629e+01  1.245e+00  29.144  < 2e-16 ***
## el          -2.462e-02  3.320e-03  -7.415 5.71e-13 ***
## sl           2.513e-01  3.624e-02   6.934 1.36e-11 ***
## I(el^2)      3.245e-06  2.021e-06   1.606    0.109    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.03 on 470 degrees of freedom
## Multiple R-squared:  0.578,	Adjusted R-squared:  0.5753 
## F-statistic: 214.6 on 3 and 470 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 22.5"
## [1] "N = 430"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1409.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1410.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.7984 -3.1224 -0.0316  3.1808  9.7213 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.880e+01  7.523e-01  38.281  < 2e-16 ***
## el          -1.221e-02  1.928e-03  -6.331 6.17e-10 ***
## sl           1.793e-01  2.265e-02   7.916 2.15e-14 ***
## I(el^2)      2.185e-07  1.098e-06   0.199    0.842    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.975 on 426 degrees of freedom
## Multiple R-squared:  0.6165,	Adjusted R-squared:  0.6138 
## F-statistic: 228.3 on 3 and 426 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 23"
## [1] "N = 456"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1411.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1412.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.0908 -2.8174 -0.5101  2.3739 10.1930 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.901e+01  5.832e-01  49.739  < 2e-16 ***
## el          -1.842e-02  1.093e-03 -16.852  < 2e-16 ***
## sl           6.836e-02  1.744e-02   3.921 0.000102 ***
## I(el^2)      5.968e-06  4.758e-07  12.543  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.871 on 452 degrees of freedom
## Multiple R-squared:  0.4725,	Adjusted R-squared:  0.469 
## F-statistic:   135 on 3 and 452 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 23.5"
## [1] "N = 455"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1413.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1414.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.3375 -1.9975  0.0185  1.8175  7.6709 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.922e+01  5.824e-01  50.168  < 2e-16 ***
## el          -2.102e-02  7.935e-04 -26.485  < 2e-16 ***
## sl           3.784e-02  1.005e-02   3.764 0.000189 ***
## I(el^2)      7.056e-06  2.443e-07  28.883  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.604 on 451 degrees of freedom
## Multiple R-squared:  0.6617,	Adjusted R-squared:  0.6594 
## F-statistic:   294 on 3 and 451 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99.5 24"
## [1] "N = 224"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1415.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1416.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.0060  -1.2711   0.0582   1.5196   7.2622 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.501e+01  4.875e-01  51.296   <2e-16 ***
## el          -1.522e-02  9.891e-04 -15.390   <2e-16 ***
## sl           1.919e-02  1.933e-02   0.993    0.322    
## I(el^2)      5.343e-06  3.070e-07  17.403   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.958 on 220 degrees of freedom
## Multiple R-squared:  0.6285,	Adjusted R-squared:  0.6235 
## F-statistic: 124.1 on 3 and 220 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99 17"
## [1] "N = 385"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1417.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1418.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.9111 -2.2378  0.0627  1.9631 16.7564 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.863e+00  3.710e-01   7.716 1.06e-13 ***
## el           1.155e-02  9.758e-04  11.841  < 2e-16 ***
## sl          -2.121e-02  2.196e-02  -0.966    0.335    
## I(el^2)     -2.175e-06  4.032e-07  -5.394 1.21e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.366 on 381 degrees of freedom
## Multiple R-squared:  0.6305,	Adjusted R-squared:  0.6276 
## F-statistic: 216.7 on 3 and 381 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99 17.5"
## [1] "N = 391"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1419.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1420.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.7346 -2.6954 -0.6581  1.4057 15.5438 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.357e+00  1.552e+00   5.386 1.25e-07 ***
## el          -6.916e-03  2.129e-03  -3.249 0.001260 ** 
## sl           7.507e-02  2.041e-02   3.679 0.000267 ***
## I(el^2)      3.437e-06  7.215e-07   4.763 2.70e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.766 on 387 degrees of freedom
## Multiple R-squared:  0.1823,	Adjusted R-squared:  0.176 
## F-statistic: 28.76 on 3 and 387 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99 18"
## [1] "N = 440"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1421.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1422.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.2676 -1.0338 -0.2238  0.7725  3.9667 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.489e+00  6.961e-01  10.758   <2e-16 ***
## el          -1.037e-02  1.147e-03  -9.038   <2e-16 ***
## sl          -3.713e-03  6.497e-03  -0.571    0.568    
## I(el^2)      5.120e-06  4.546e-07  11.263   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.288 on 436 degrees of freedom
## Multiple R-squared:  0.3962,	Adjusted R-squared:  0.392 
## F-statistic: 95.35 on 3 and 436 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99 19"
## [1] "N = 398"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1423.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1424.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.7898 -0.5901 -0.1218  0.7637  4.0549 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 2.694e+00  4.352e-01   6.191  1.5e-09 ***
## el          3.951e-04  4.044e-04   0.977    0.329    
## sl          5.632e-04  6.458e-03   0.087    0.931    
## I(el^2)     5.831e-07  8.915e-08   6.541  1.9e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.079 on 394 degrees of freedom
## Multiple R-squared:  0.8242,	Adjusted R-squared:  0.8228 
## F-statistic: 615.6 on 3 and 394 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99 20.5"
## [1] "N = 455"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1425.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1426.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -14.630  -5.107  -2.991   1.874  35.535 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.530e+01  6.968e+00   2.196   0.0286 *  
## el           7.010e-04  7.398e-03   0.095   0.9246    
## sl           1.650e-01  3.439e-02   4.798 2.18e-06 ***
## I(el^2)     -1.196e-06  1.972e-06  -0.607   0.5443    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.877 on 451 degrees of freedom
## Multiple R-squared:  0.07563,	Adjusted R-squared:  0.06948 
## F-statistic:  12.3 on 3 and 451 DF,  p-value: 9.512e-08
## 
## [1] "Point -99 21"
## [1] "N = 443"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1427.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1428.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -28.93 -10.87  -2.98  11.45  37.43 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.647e+01  2.527e+00  26.305  < 2e-16 ***
## el          -3.953e-02  4.265e-03  -9.269  < 2e-16 ***
## sl           3.070e-02  5.570e-02   0.551    0.582    
## I(el^2)      8.513e-06  1.635e-06   5.207 2.96e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.97 on 439 degrees of freedom
## Multiple R-squared:  0.4119,	Adjusted R-squared:  0.4079 
## F-statistic: 102.5 on 3 and 439 DF,  p-value: < 2.2e-16
## 
## [1] "Point -99 21.5"
## [1] "N = 444"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1429.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1430.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -25.458  -8.692   1.527   7.053  32.552 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.109e+01  1.051e+00  48.612  < 2e-16 ***
## el          -2.023e-02  3.410e-03  -5.932 6.06e-09 ***
## sl           1.211e-01  5.910e-02   2.050 0.040985 *  
## I(el^2)      6.271e-06  1.680e-06   3.733 0.000214 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.71 on 440 degrees of freedom
## Multiple R-squared:  0.1424,	Adjusted R-squared:  0.1365 
## F-statistic: 24.35 on 3 and 440 DF,  p-value: 1.358e-14
## 
## [1] "Point -99 23.5"
## [1] "N = 451"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1431.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1432.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.0149 -3.2426 -0.4309  2.9306 15.1730 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.235e+01  5.309e-01  42.097  < 2e-16 ***
## el          -6.696e-03  1.951e-03  -3.433 0.000653 ***
## sl           2.156e-02  2.457e-02   0.878 0.380590    
## I(el^2)      2.104e-06  1.001e-06   2.102 0.036133 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.463 on 447 degrees of freedom
## Multiple R-squared:  0.07969,	Adjusted R-squared:  0.07351 
## F-statistic:  12.9 on 3 and 447 DF,  p-value: 4.255e-08
## 
## [1] "Point -98.5 17"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1433.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1434.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.0304  -1.6985  -0.0812   1.4284  21.7245 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.508e+00  6.548e-01   5.357 1.44e-07 ***
## el           1.483e-02  1.195e-03  12.412  < 2e-16 ***
## sl          -1.550e-02  2.492e-02  -0.622    0.534    
## I(el^2)     -4.192e-06  4.470e-07  -9.377  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.282 on 396 degrees of freedom
## Multiple R-squared:  0.4167,	Adjusted R-squared:  0.4122 
## F-statistic: 94.28 on 3 and 396 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 17.5"
## [1] "N = 420"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1435.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1436.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.188 -4.120 -2.162  1.579 28.896 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 1.311e-01  4.887e+00   0.027  0.97862   
## el          3.755e-04  5.634e-03   0.067  0.94689   
## sl          1.061e-01  3.251e-02   3.263  0.00119 **
## I(el^2)     1.766e-06  1.587e-06   1.112  0.26662   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.245 on 416 degrees of freedom
## Multiple R-squared:  0.1717,	Adjusted R-squared:  0.1658 
## F-statistic: 28.75 on 3 and 416 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 18"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1437.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1438.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.2782 -0.6761 -0.1904  0.6349  2.6688 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.573e+00  1.110e+00   5.018 7.65e-07 ***
## el          -4.981e-03  1.699e-03  -2.932 0.003543 ** 
## sl          -2.238e-02  5.550e-03  -4.032 6.54e-05 ***
## I(el^2)      2.155e-06  6.398e-07   3.369 0.000823 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.074 on 430 degrees of freedom
## Multiple R-squared:  0.07595,	Adjusted R-squared:  0.0695 
## F-statistic: 11.78 on 3 and 430 DF,  p-value: 1.978e-07
## 
## [1] "Point -98.5 18.5"
## [1] "N = 408"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1439.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1440.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.6232 -1.0028 -0.0265  1.0637  4.6723 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.211e+00  1.057e+00  -1.146  0.25262    
## el           4.169e-03  1.433e-03   2.909  0.00382 ** 
## sl          -3.619e-02  7.354e-03  -4.922 1.25e-06 ***
## I(el^2)     -1.798e-07  4.696e-07  -0.383  0.70201    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.402 on 404 degrees of freedom
## Multiple R-squared:  0.4207,	Adjusted R-squared:  0.4164 
## F-statistic: 97.81 on 3 and 404 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 19"
## [1] "N = 384"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1441.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1442.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.6540 -0.6652 -0.1066  0.8176  4.2544 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 2.665e+00  7.200e-01   3.701 0.000247 ***
## el          6.589e-04  6.109e-04   1.079 0.281485    
## sl          1.323e-02  8.279e-03   1.598 0.110919    
## I(el^2)     4.880e-07  1.275e-07   3.827 0.000152 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.226 on 380 degrees of freedom
## Multiple R-squared:  0.6839,	Adjusted R-squared:  0.6814 
## F-statistic:   274 on 3 and 380 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 20"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1443.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1444.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.019  -3.858  -1.200   2.098  34.457 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.051e+02  1.164e+01   9.031  < 2e-16 ***
## el          -6.824e-02  1.012e-02  -6.747 5.15e-11 ***
## sl           3.895e-01  4.756e-02   8.191 3.31e-15 ***
## I(el^2)      1.166e-05  2.210e-06   5.274 2.16e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.61 on 411 degrees of freedom
## Multiple R-squared:  0.4292,	Adjusted R-squared:  0.425 
## F-statistic:   103 on 3 and 411 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 20.5"
## [1] "N = 465"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1445.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1446.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -34.033  -8.720  -0.912   7.518  52.401 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.676e+01  2.999e+00  18.927  < 2e-16 ***
## el          -2.316e-02  4.178e-03  -5.543 5.02e-08 ***
## sl           2.671e-01  5.479e-02   4.875 1.50e-06 ***
## I(el^2)      2.181e-06  1.404e-06   1.553    0.121    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.57 on 461 degrees of freedom
## Multiple R-squared:  0.461,	Adjusted R-squared:  0.4575 
## F-statistic: 131.4 on 3 and 461 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98.5 21"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1447.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1448.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -37.547  -4.013  -0.340   4.565  23.956 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.669e+01  9.039e-01  62.718   <2e-16 ***
## el          -5.530e-03  2.948e-03  -1.876   0.0614 .  
## sl          -5.220e-03  4.440e-02  -0.118   0.9065    
## I(el^2)     -2.707e-06  1.396e-06  -1.940   0.0530 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.667 on 430 degrees of freedom
## Multiple R-squared:  0.3592,	Adjusted R-squared:  0.3548 
## F-statistic: 80.36 on 3 and 430 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98 16.5"
## [1] "N = 362"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1449.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1450.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.4632 -1.8180 -0.3274  1.7865  6.7447 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.745e+00  3.173e-01  21.259  < 2e-16 ***
## el           3.272e-04  1.116e-03   0.293   0.7695    
## sl          -3.569e-02  1.787e-02  -1.997   0.0466 *  
## I(el^2)      2.912e-06  7.342e-07   3.966 8.83e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.686 on 358 degrees of freedom
## Multiple R-squared:  0.2606,	Adjusted R-squared:  0.2544 
## F-statistic: 42.05 on 3 and 358 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98 17"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1451.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1452.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.4286 -2.5289 -0.6676  1.6832 16.9733 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.925e+00  6.856e-01  11.559  < 2e-16 ***
## el           1.769e-03  1.074e-03   1.647  0.10038    
## sl           6.380e-02  1.977e-02   3.227  0.00135 ** 
## I(el^2)     -4.687e-07  3.479e-07  -1.347  0.17864    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.462 on 407 degrees of freedom
## Multiple R-squared:  0.04552,	Adjusted R-squared:  0.03848 
## F-statistic:  6.47 on 3 and 407 DF,  p-value: 0.0002755
## 
## [1] "Point -98 17.5"
## [1] "N = 422"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1453.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1454.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.3971 -1.9162 -0.5042  1.0525 11.7122 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.904e+00  2.190e+00   3.609 0.000344 ***
## el          -7.226e-03  2.406e-03  -3.003 0.002831 ** 
## sl           5.461e-02  1.439e-02   3.796 0.000169 ***
## I(el^2)      2.909e-06  6.540e-07   4.448 1.11e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.779 on 418 degrees of freedom
## Multiple R-squared:  0.2612,	Adjusted R-squared:  0.2559 
## F-statistic: 49.25 on 3 and 418 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98 18"
## [1] "N = 426"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1455.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1456.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.31260 -0.77407 -0.01222  0.68417  2.83225 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.196e+01  8.794e-01  13.606  < 2e-16 ***
## el          -1.199e-02  1.064e-03 -11.273  < 2e-16 ***
## sl          -1.440e-02  5.242e-03  -2.747  0.00626 ** 
## I(el^2)      4.001e-06  3.166e-07  12.639  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.982 on 422 degrees of freedom
## Multiple R-squared:  0.3733,	Adjusted R-squared:  0.3688 
## F-statistic: 83.77 on 3 and 422 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98 18.5"
## [1] "N = 413"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1457.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1458.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.5219 -1.0808 -0.2086  0.7604  4.0023 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.544e+00  1.457e+00   5.864 9.34e-09 ***
## el          -5.384e-03  1.807e-03  -2.980  0.00306 ** 
## sl          -8.136e-03  7.183e-03  -1.133  0.25797    
## I(el^2)      1.641e-06  5.434e-07   3.019  0.00269 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.198 on 409 degrees of freedom
## Multiple R-squared:  0.02525,	Adjusted R-squared:  0.0181 
## F-statistic: 3.532 on 3 and 409 DF,  p-value: 0.01494
## 
## [1] "Point -98 19"
## [1] "N = 392"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1459.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1460.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.9324 -0.8423  0.0193  0.8891  4.2048 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.861e+01  1.469e+00  12.674   <2e-16 ***
## el          -1.495e-02  1.269e-03 -11.784   <2e-16 ***
## sl          -1.127e-02  1.092e-02  -1.033    0.302    
## I(el^2)      4.061e-06  2.774e-07  14.638   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.427 on 388 degrees of freedom
## Multiple R-squared:  0.5701,	Adjusted R-squared:  0.5668 
## F-statistic: 171.5 on 3 and 388 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98 20"
## [1] "N = 452"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1461.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1462.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -42.181 -10.806  -2.213   6.913  84.017 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.023e+02  4.213e+00  24.287  < 2e-16 ***
## el          -2.865e-02  5.518e-03  -5.192 3.15e-07 ***
## sl           1.682e-01  7.992e-02   2.105   0.0359 *  
## I(el^2)     -2.498e-06  1.632e-06  -1.530   0.1267    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.59 on 448 degrees of freedom
## Multiple R-squared:  0.7291,	Adjusted R-squared:  0.7273 
## F-statistic:   402 on 3 and 448 DF,  p-value: < 2.2e-16
## 
## [1] "Point -98 20.5"
## [1] "N = 466"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1463.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1464.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -29.611  -9.678  -2.439   8.557  34.687 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.461e+01  1.311e+00  41.648  < 2e-16 ***
## el           2.519e-02  3.675e-03   6.855  2.3e-11 ***
## sl          -1.155e-01  6.147e-02  -1.880   0.0608 .  
## I(el^2)     -1.574e-05  1.570e-06 -10.026  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.39 on 462 degrees of freedom
## Multiple R-squared:  0.3379,	Adjusted R-squared:  0.3336 
## F-statistic: 78.58 on 3 and 462 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 16"
## [1] "N = 228"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1465.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1466.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.2436 -1.1940 -0.0686  1.0244  4.7547 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.978e+00  2.152e-01   9.189  < 2e-16 ***
## el           2.738e-03  6.465e-04   4.235 3.34e-05 ***
## sl          -2.707e-02  1.322e-02  -2.047   0.0418 *  
## I(el^2)      5.826e-07  3.348e-07   1.740   0.0832 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.589 on 224 degrees of freedom
## Multiple R-squared:  0.5907,	Adjusted R-squared:  0.5852 
## F-statistic: 107.7 on 3 and 224 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 16.5"
## [1] "N = 421"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1467.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1468.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.4732 -1.8670 -0.1197  1.8555  7.7443 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.770e+00  4.668e-01   3.791 0.000172 ***
## el           4.427e-03  7.791e-04   5.681 2.51e-08 ***
## sl          -1.934e-04  1.339e-02  -0.014 0.988483    
## I(el^2)      3.975e-07  2.878e-07   1.381 0.167973    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.641 on 417 degrees of freedom
## Multiple R-squared:  0.6434,	Adjusted R-squared:  0.6408 
## F-statistic: 250.8 on 3 and 417 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 17"
## [1] "N = 414"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1469.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1470.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.3585 -2.5002 -0.5264  1.7215 16.9994 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.517e+00  1.287e+00   4.287 2.26e-05 ***
## el          -2.032e-04  1.502e-03  -0.135   0.8924    
## sl           9.718e-02  1.697e-02   5.728 1.97e-08 ***
## I(el^2)      7.432e-07  4.271e-07   1.740   0.0826 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.391 on 410 degrees of freedom
## Multiple R-squared:  0.1965,	Adjusted R-squared:  0.1906 
## F-statistic: 33.42 on 3 and 410 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 18"
## [1] "N = 412"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1471.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1472.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.9757 -1.8999 -0.8486  0.3349 28.5926 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.321e+01  2.384e+00   5.541 5.39e-08 ***
## el          -1.307e-02  2.806e-03  -4.659 4.32e-06 ***
## sl           5.112e-02  2.022e-02   2.528   0.0119 *  
## I(el^2)      4.439e-06  7.993e-07   5.554 5.05e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.834 on 408 degrees of freedom
## Multiple R-squared:  0.1557,	Adjusted R-squared:  0.1495 
## F-statistic: 25.08 on 3 and 408 DF,  p-value: 6.508e-15
## 
## [1] "Point -97.5 18.5"
## [1] "N = 424"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1473.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1474.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -14.547  -4.258  -1.339   0.638  37.948 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.428e+01  6.783e+00   3.580 0.000384 ***
## el          -2.913e-02  7.308e-03  -3.986 7.92e-05 ***
## sl           2.833e-01  4.050e-02   6.996 1.05e-11 ***
## I(el^2)      9.233e-06  1.946e-06   4.745 2.87e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.222 on 420 degrees of freedom
## Multiple R-squared:  0.2338,	Adjusted R-squared:  0.2283 
## F-statistic: 42.71 on 3 and 420 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 19"
## [1] "N = 408"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1475.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1476.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -21.157  -5.035  -0.439   2.463  42.938 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.410e+02  7.209e+00   19.56   <2e-16 ***
## el          -1.116e-01  5.640e-03  -19.79   <2e-16 ***
## sl           6.450e-01  4.395e-02   14.68   <2e-16 ***
## I(el^2)      2.223e-05  1.101e-06   20.20   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.961 on 404 degrees of freedom
## Multiple R-squared:  0.6256,	Adjusted R-squared:  0.6228 
## F-statistic:   225 on 3 and 404 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97.5 20"
## [1] "N = 446"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1477.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1478.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -47.518 -16.999  -5.791  13.984  74.749 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.853e+01  2.415e+00  36.665  < 2e-16 ***
## el           3.265e-02  5.291e-03   6.171 1.54e-09 ***
## sl          -2.862e-02  1.117e-01  -0.256    0.798    
## I(el^2)     -2.385e-05  1.910e-06 -12.486  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 23.88 on 442 degrees of freedom
## Multiple R-squared:  0.6595,	Adjusted R-squared:  0.6572 
## F-statistic: 285.4 on 3 and 442 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 16"
## [1] "N = 331"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1479.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1480.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.5510 -1.5972 -0.2374  0.9378 16.1838 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.433e+00  3.119e-01   4.595  6.2e-06 ***
## el           4.094e-03  6.809e-04   6.012  4.9e-09 ***
## sl           1.644e-02  1.486e-02   1.106 0.269433    
## I(el^2)     -1.018e-06  2.937e-07  -3.466 0.000599 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.24 on 327 degrees of freedom
## Multiple R-squared:  0.2954,	Adjusted R-squared:  0.289 
## F-statistic:  45.7 on 3 and 327 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 16.5"
## [1] "N = 466"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1481.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1482.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.8660 -2.2559 -0.8286  1.6134 17.0826 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.840e+00  1.446e+00   5.422 9.53e-08 ***
## el          -9.673e-03  1.710e-03  -5.656 2.71e-08 ***
## sl           8.029e-02  1.637e-02   4.906 1.29e-06 ***
## I(el^2)      4.548e-06  4.994e-07   9.107  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.4 on 462 degrees of freedom
## Multiple R-squared:  0.509,	Adjusted R-squared:  0.5058 
## F-statistic: 159.6 on 3 and 462 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 17"
## [1] "N = 432"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1483.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1484.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.9797 -2.0257 -0.4076  1.4491 16.2826 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.661e+01  4.174e+00   3.979 8.12e-05 ***
## el          -1.933e-02  4.163e-03  -4.642 4.60e-06 ***
## sl           9.647e-02  1.755e-02   5.498 6.63e-08 ***
## I(el^2)      7.152e-06  1.001e-06   7.142 3.97e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.296 on 428 degrees of freedom
## Multiple R-squared:  0.625,	Adjusted R-squared:  0.6224 
## F-statistic: 237.8 on 3 and 428 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 17.5"
## [1] "N = 416"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1485.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1486.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.649  -7.462  -3.662   2.711  59.587 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.835e+01  7.694e+00   3.684 0.000260 ***
## el          -2.947e-02  8.535e-03  -3.453 0.000613 ***
## sl           3.939e-01  6.277e-02   6.275 8.85e-10 ***
## I(el^2)      9.202e-06  2.344e-06   3.926 0.000101 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.18 on 412 degrees of freedom
## Multiple R-squared:  0.124,	Adjusted R-squared:  0.1176 
## F-statistic: 19.43 on 3 and 412 DF,  p-value: 8.371e-12
## 
## [1] "Point -97 18"
## [1] "N = 417"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1487.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1488.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -67.51 -25.86 -13.18  16.33 162.00 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.849e+01  1.044e+01   6.563 1.59e-10 ***
## el          -5.622e-02  1.446e-02  -3.888 0.000118 ***
## sl           1.033e+00  1.723e-01   5.997 4.40e-09 ***
## I(el^2)      1.316e-05  4.693e-06   2.803 0.005299 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 40.21 on 413 degrees of freedom
## Multiple R-squared:  0.1397,	Adjusted R-squared:  0.1335 
## F-statistic: 22.36 on 3 and 413 DF,  p-value: 1.964e-13
## 
## [1] "Point -97 18.5"
## [1] "N = 414"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1489.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1490.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -53.781 -20.675  -3.922  15.030 118.318 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.848e+01  4.357e+00  11.127  < 2e-16 ***
## el          -1.139e-02  7.181e-03  -1.587    0.113    
## sl           8.783e-01  1.331e-01   6.599 1.28e-10 ***
## I(el^2)     -1.986e-06  2.564e-06  -0.774    0.439    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 30.03 on 410 degrees of freedom
## Multiple R-squared:  0.1682,	Adjusted R-squared:  0.1621 
## F-statistic: 27.64 on 3 and 410 DF,  p-value: 2.698e-16
## 
## [1] "Point -97 19"
## [1] "N = 428"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1491.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1492.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -35.923 -14.511  -1.246  13.328  63.527 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.522e+01  2.380e+00  14.800  < 2e-16 ***
## el           4.252e-03  3.371e-03   1.261  0.20789    
## sl           4.053e-01  7.981e-02   5.079  5.7e-07 ***
## I(el^2)     -2.872e-06  9.394e-07  -3.057  0.00238 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 17.56 on 424 degrees of freedom
## Multiple R-squared:  0.1325,	Adjusted R-squared:  0.1263 
## F-statistic: 21.58 on 3 and 424 DF,  p-value: 5.03e-13
## 
## [1] "Point -97 19.5"
## [1] "N = 425"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1493.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1494.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -42.455 -19.456  -3.422  10.969 104.524 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.585e+01  4.709e+00   5.489 6.99e-08 ***
## el           3.158e-02  6.437e-03   4.906 1.33e-06 ***
## sl           6.951e-01  1.135e-01   6.125 2.08e-09 ***
## I(el^2)     -1.266e-05  1.890e-06  -6.696 6.86e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.98 on 421 degrees of freedom
## Multiple R-squared:  0.261,	Adjusted R-squared:  0.2557 
## F-statistic: 49.56 on 3 and 421 DF,  p-value: < 2.2e-16
## 
## [1] "Point -97 20"
## [1] "N = 393"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1495.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1496.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -46.988 -15.231  -1.191  10.027  69.986 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.930e+01  1.796e+00  49.707  < 2e-16 ***
## el           2.677e-02  5.153e-03   5.195 3.31e-07 ***
## sl           1.547e-01  1.094e-01   1.414    0.158    
## I(el^2)     -2.059e-05  2.146e-06  -9.595  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.75 on 389 degrees of freedom
## Multiple R-squared:  0.4572,	Adjusted R-squared:  0.453 
## F-statistic: 109.2 on 3 and 389 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 16"
## [1] "N = 423"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1497.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1498.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.3584 -1.7164 -0.4432  1.0394 22.3919 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 2.366e+00  3.424e-01   6.911 1.80e-11 ***
## el          9.244e-04  6.190e-04   1.493    0.136    
## sl          1.126e-01  1.537e-02   7.330 1.19e-12 ***
## I(el^2)     1.762e-07  2.247e-07   0.784    0.433    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.045 on 419 degrees of freedom
## Multiple R-squared:  0.3053,	Adjusted R-squared:  0.3004 
## F-statistic: 61.39 on 3 and 419 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 16.5"
## [1] "N = 422"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1499.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1500.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.2620 -1.1193 -0.2682  0.7175  8.0126 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.686e+00  1.251e+00   4.546 7.17e-06 ***
## el          -6.021e-03  1.320e-03  -4.561 6.69e-06 ***
## sl           4.675e-02  8.693e-03   5.377 1.26e-07 ***
## I(el^2)      2.749e-06  3.376e-07   8.143 4.49e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.796 on 418 degrees of freedom
## Multiple R-squared:  0.6059,	Adjusted R-squared:  0.6031 
## F-statistic: 214.2 on 3 and 418 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 17"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1501.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1502.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -25.523  -7.007  -2.779   2.555  67.475 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 33.1733309 10.4288519   3.181 0.001574 ** 
## el          -0.0388679  0.0106079  -3.664 0.000279 ***
## sl           0.3832155  0.0618325   6.198 1.34e-09 ***
## I(el^2)      0.0000130  0.0000026   4.999 8.41e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.2 on 430 degrees of freedom
## Multiple R-squared:  0.2947,	Adjusted R-squared:  0.2898 
## F-statistic: 59.89 on 3 and 430 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 17.5"
## [1] "N = 404"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1503.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1504.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -63.174 -21.359  -4.618  18.656 110.306 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.773e+02  5.871e+00  30.208  < 2e-16 ***
## el          -1.451e-01  7.584e-03 -19.136  < 2e-16 ***
## sl           4.656e-01  1.366e-01   3.409 0.000717 ***
## I(el^2)      3.353e-05  2.371e-06  14.144  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27.69 on 400 degrees of freedom
## Multiple R-squared:  0.6274,	Adjusted R-squared:  0.6246 
## F-statistic: 224.5 on 3 and 400 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 18"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1505.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1506.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -79.58 -30.98  -6.92  31.02 162.72 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.341e+01  3.563e+00  20.603  < 2e-16 ***
## el           4.062e-02  9.457e-03   4.295 2.20e-05 ***
## sl           2.683e-01  1.951e-01   1.375     0.17    
## I(el^2)     -2.341e-05  3.837e-06  -6.100 2.53e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 38.26 on 396 degrees of freedom
## Multiple R-squared:  0.1471,	Adjusted R-squared:  0.1406 
## F-statistic: 22.76 on 3 and 396 DF,  p-value: 1.296e-13
## 
## [1] "Point -96.5 19.5"
## [1] "N = 285"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1507.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1508.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -18.619  -8.065  -2.988   2.566  61.421 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.707e+01  1.560e+00  10.937  < 2e-16 ***
## el           2.979e-02  5.011e-03   5.946 8.13e-09 ***
## sl           2.409e-01  8.615e-02   2.796  0.00552 ** 
## I(el^2)     -1.327e-06  2.867e-06  -0.463  0.64377    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.71 on 281 degrees of freedom
## Multiple R-squared:  0.5402,	Adjusted R-squared:  0.5353 
## F-statistic: 110.1 on 3 and 281 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96.5 20"
## [1] "N = 173"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1509.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1510.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -59.946 -17.454   2.185  24.272  51.134 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.608e+01  3.729e+00  20.405   <2e-16 ***
## el          -1.175e-02  1.510e-02  -0.778    0.438    
## sl          -1.168e-01  2.451e-01  -0.476    0.634    
## I(el^2)      5.257e-06  7.850e-06   0.670    0.504    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.13 on 169 degrees of freedom
## Multiple R-squared:  0.01176,	Adjusted R-squared:  -0.005787 
## F-statistic: 0.6701 on 3 and 169 DF,  p-value: 0.5715
## 
## [1] "Point -96 16"
## [1] "N = 342"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1511.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1512.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.0734 -2.0011 -0.4717  1.0883 22.6153 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.402e+00  3.889e-01   3.605 0.000359 ***
## el           3.873e-03  7.735e-04   5.008 8.89e-07 ***
## sl           2.783e-02  1.727e-02   1.612 0.107966    
## I(el^2)     -3.081e-07  2.609e-07  -1.181 0.238480    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.006 on 338 degrees of freedom
## Multiple R-squared:  0.4389,	Adjusted R-squared:  0.4339 
## F-statistic: 88.12 on 3 and 338 DF,  p-value: < 2.2e-16
## 
## [1] "Point -96 16.5"
## [1] "N = 414"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1513.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1514.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.067  -5.559  -3.130   1.384  45.465 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -3.042e+00  2.159e+00  -1.409  0.15967    
## el           1.174e-02  2.948e-03   3.980 8.14e-05 ***
## sl           9.871e-02  4.503e-02   2.192  0.02891 *  
## I(el^2)     -2.540e-06  9.541e-07  -2.663  0.00806 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.293 on 410 degrees of freedom
## Multiple R-squared:  0.1027,	Adjusted R-squared:  0.09613 
## F-statistic: 15.64 on 3 and 410 DF,  p-value: 1.194e-09
## 
## [1] "Point -96 17"
## [1] "N = 381"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1515.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1516.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -67.643 -24.955  -4.868  21.648  98.200 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.080e+01  8.737e+00   5.815 1.30e-08 ***
## el          -2.500e-02  1.150e-02  -2.175   0.0303 *  
## sl           9.404e-01  1.729e-01   5.438 9.72e-08 ***
## I(el^2)      3.563e-06  3.785e-06   0.942   0.3470    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.35 on 377 degrees of freedom
## Multiple R-squared:  0.1244,	Adjusted R-squared:  0.1175 
## F-statistic: 17.86 on 3 and 377 DF,  p-value: 7.386e-11
## 
## [1] "Point -96 17.5"
## [1] "N = 408"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1517.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1518.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -70.168 -18.140  -0.048  16.241  76.199 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.823e+01  2.581e+00  34.190  < 2e-16 ***
## el           1.077e-02  6.576e-03   1.638   0.1023    
## sl           3.350e-01  1.402e-01   2.390   0.0173 *  
## I(el^2)     -1.187e-05  2.564e-06  -4.628 4.98e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 24.64 on 404 degrees of freedom
## Multiple R-squared:  0.2153,	Adjusted R-squared:  0.2095 
## F-statistic: 36.95 on 3 and 404 DF,  p-value: < 2.2e-16
## 
## [1] "Point -95.5 16"
## [1] "N = 243"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1519.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1520.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.9307 -0.8406 -0.3748  0.6969  5.2327 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.876e+00  1.802e-01  10.409  < 2e-16 ***
## el          -3.107e-03  8.212e-04  -3.784 0.000196 ***
## sl          -1.183e-02  1.169e-02  -1.012 0.312540    
## I(el^2)      5.733e-06  5.810e-07   9.868  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.424 on 239 degrees of freedom
## Multiple R-squared:  0.5992,	Adjusted R-squared:  0.5941 
## F-statistic: 119.1 on 3 and 239 DF,  p-value: < 2.2e-16
## 
## [1] "Point -95.5 16.5"
## [1] "N = 443"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1521.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1522.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -26.650  -9.291  -3.149   4.824  54.991 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.031e+00  1.431e+00  -0.721    0.472    
## el           1.981e-02  4.317e-03   4.588 5.86e-06 ***
## sl           3.888e-02  7.514e-02   0.517    0.605    
## I(el^2)     -6.451e-07  2.363e-06  -0.273    0.785    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.7 on 439 degrees of freedom
## Multiple R-squared:  0.3087,	Adjusted R-squared:  0.304 
## F-statistic: 65.34 on 3 and 439 DF,  p-value: < 2.2e-16
## 
## [1] "Point -95.5 17"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1523.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1524.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -59.26 -18.42   2.26  17.47  74.41 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.916e+01  3.479e+00  19.878  < 2e-16 ***
## el          -4.678e-02  1.112e-02  -4.205 3.21e-05 ***
## sl          -3.614e-01  1.507e-01  -2.399   0.0169 *  
## I(el^2)      3.379e-05  6.523e-06   5.180 3.50e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 26.28 on 407 degrees of freedom
## Multiple R-squared:  0.09336,	Adjusted R-squared:  0.08668 
## F-statistic: 13.97 on 3 and 407 DF,  p-value: 1.097e-08
## 
## [1] "Point -94.5 16.5"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1525.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1526.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -37.441  -6.703  -3.321  -0.096 105.698 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.817e+00  1.501e+00   2.543   0.0114 *  
## el           7.356e-02  9.328e-03   7.886 3.08e-14 ***
## sl           6.327e-02  1.425e-01   0.444   0.6574    
## I(el^2)     -3.607e-05  5.811e-06  -6.208 1.36e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.37 on 396 degrees of freedom
## Multiple R-squared:  0.3637,	Adjusted R-squared:  0.3589 
## F-statistic: 75.45 on 3 and 396 DF,  p-value: < 2.2e-16
## 
## [1] "Point -94.5 17"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1527.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1528.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -202.941  -38.152    5.457   56.190  158.242 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.272e+02  7.194e+00  31.575  < 2e-16 ***
## el          -2.983e-01  2.925e-02 -10.200  < 2e-16 ***
## sl           4.812e-01  4.123e-01   1.167    0.244    
## I(el^2)      1.134e-04  1.865e-05   6.080 2.62e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 70.48 on 437 degrees of freedom
## Multiple R-squared:  0.4308,	Adjusted R-squared:  0.4269 
## F-statistic: 110.2 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -94 16.5"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1529.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1530.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -36.421  -5.654  -1.596   2.707  69.212 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.228e+00  1.157e+00   2.789  0.00552 ** 
## el          -3.278e-04  4.050e-03  -0.081  0.93553    
## sl           3.177e-01  6.337e-02   5.012 7.82e-07 ***
## I(el^2)      1.103e-05  2.792e-06   3.950 9.12e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12 on 437 degrees of freedom
## Multiple R-squared:  0.3685,	Adjusted R-squared:  0.3641 
## F-statistic: 84.99 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -94 17"
## [1] "N = 449"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1531.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1532.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -111.788  -33.110    0.236   32.256  142.777 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.901e+02  6.454e+00   44.95  < 2e-16 ***
## el          -4.536e-01  1.889e-02  -24.01  < 2e-16 ***
## sl           8.355e-01  2.282e-01    3.66 0.000282 ***
## I(el^2)      1.863e-04  1.244e-05   14.97  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 47.79 on 445 degrees of freedom
## Multiple R-squared:  0.7434,	Adjusted R-squared:  0.7417 
## F-statistic: 429.7 on 3 and 445 DF,  p-value: < 2.2e-16
## 
## [1] "Point -93.5 16"
## [1] "N = 330"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1533.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1534.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.4978 -1.4207 -0.5864  1.3426 10.6804 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.329e+00  2.513e-01  17.224  < 2e-16 ***
## el          -4.786e-03  7.944e-04  -6.025 4.57e-09 ***
## sl           6.445e-02  1.635e-02   3.943 9.87e-05 ***
## I(el^2)      4.249e-06  4.957e-07   8.571 4.19e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.391 on 326 degrees of freedom
## Multiple R-squared:  0.3284,	Adjusted R-squared:  0.3222 
## F-statistic: 53.13 on 3 and 326 DF,  p-value: < 2.2e-16
## 
## [1] "Point -93.5 16.5"
## [1] "N = 408"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1535.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1536.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -8.670 -5.116 -2.826  2.548 45.050 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  1.837e+00  2.843e+00   0.646   0.5186   
## el           9.903e-03  6.030e-03   1.642   0.1013   
## sl          -1.240e-01  4.703e-02  -2.636   0.0087 **
## I(el^2)     -4.011e-08  3.056e-06  -0.013   0.9895   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.947 on 404 degrees of freedom
## Multiple R-squared:  0.08874,	Adjusted R-squared:  0.08197 
## F-statistic: 13.11 on 3 and 404 DF,  p-value: 3.455e-08
## 
## [1] "Point -93.5 17"
## [1] "N = 416"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1537.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1538.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -166.266  -31.534   -8.695   25.774  194.806 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.168e+02  9.062e+00  23.930  < 2e-16 ***
## el          -3.911e-01  2.233e-02 -17.516  < 2e-16 ***
## sl           1.363e+00  2.463e-01   5.535 5.54e-08 ***
## I(el^2)      1.882e-04  1.363e-05  13.813  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 51.32 on 412 degrees of freedom
## Multiple R-squared:  0.4969,	Adjusted R-squared:  0.4933 
## F-statistic: 135.7 on 3 and 412 DF,  p-value: < 2.2e-16
## 
## [1] "Point -93 15.5"
## [1] "N = 278"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1539.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1540.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.9697 -1.8069 -0.4294  0.9934 14.5606 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.190e+00  3.880e-01  15.955   <2e-16 ***
## el           1.751e-02  1.198e-03  14.614   <2e-16 ***
## sl           1.387e-02  2.959e-02   0.469     0.64    
## I(el^2)     -7.658e-06  5.163e-07 -14.834   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.784 on 274 degrees of freedom
## Multiple R-squared:  0.5578,	Adjusted R-squared:  0.5529 
## F-statistic: 115.2 on 3 and 274 DF,  p-value: < 2.2e-16
## 
## [1] "Point -93 16"
## [1] "N = 423"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1541.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1542.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.7757 -3.8331 -0.8479  2.9525 16.2509 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.622e+00  8.481e-01   5.450 8.61e-08 ***
## el           3.749e-03  1.887e-03   1.986  0.04763 *  
## sl           7.467e-02  2.779e-02   2.687  0.00749 ** 
## I(el^2)     -7.523e-07  9.809e-07  -0.767  0.44357    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.842 on 419 degrees of freedom
## Multiple R-squared:  0.08648,	Adjusted R-squared:  0.07994 
## F-statistic: 13.22 on 3 and 419 DF,  p-value: 2.903e-08
## 
## [1] "Point -93 17"
## [1] "N = 410"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1543.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1544.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -150.17  -49.01  -16.98   36.38  231.89 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.765e+01  1.471e+01   5.957 5.58e-09 ***
## el          -7.037e-02  2.622e-02  -2.684  0.00757 ** 
## sl           2.044e+00  3.419e-01   5.979 4.92e-09 ***
## I(el^2)      2.097e-05  1.043e-05   2.011  0.04495 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 68.99 on 406 degrees of freedom
## Multiple R-squared:  0.0979,	Adjusted R-squared:  0.09123 
## F-statistic: 14.69 on 3 and 406 DF,  p-value: 4.254e-09
## 
## [1] "Point -93 17.5"
## [1] "N = 430"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1545.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1546.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -164.516  -43.622    0.364   38.269  119.353 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.035e+02  4.247e+00  47.910   <2e-16 ***
## el           1.728e-03  2.118e-02   0.082   0.9350    
## sl           4.515e-01  3.662e-01   1.233   0.2183    
## I(el^2)     -3.031e-05  1.195e-05  -2.536   0.0116 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 57.55 on 426 degrees of freedom
## Multiple R-squared:  0.1356,	Adjusted R-squared:  0.1295 
## F-statistic: 22.27 on 3 and 426 DF,  p-value: 2.065e-13
## 
## [1] "Point -92.5 15"
## [1] "N = 315"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1547.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1548.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -26.355  -7.013  -3.079   4.546  66.743 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.156e+01  1.019e+00  11.345   <2e-16 ***
## el           4.485e-02  3.389e-03  13.232   <2e-16 ***
## sl          -1.768e-01  1.043e-01  -1.695   0.0911 .  
## I(el^2)     -1.612e-05  1.223e-06 -13.186   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.48 on 311 degrees of freedom
## Multiple R-squared:  0.4475,	Adjusted R-squared:  0.4421 
## F-statistic: 83.96 on 3 and 311 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92.5 15.5"
## [1] "N = 401"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1549.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1550.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.854  -6.247  -2.414   3.795  30.028 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.035e+01  9.638e-01  10.736  < 2e-16 ***
## el           1.664e-02  2.012e-03   8.270 2.04e-15 ***
## sl           1.065e-01  4.470e-02   2.382   0.0177 *  
## I(el^2)     -7.394e-06  7.543e-07  -9.802  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.385 on 397 degrees of freedom
## Multiple R-squared:  0.2427,	Adjusted R-squared:  0.237 
## F-statistic: 42.41 on 3 and 397 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92.5 16.5"
## [1] "N = 449"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1551.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1552.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -29.059  -9.153  -1.290   7.097  48.375 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -3.678e+01  3.527e+00 -10.427  < 2e-16 ***
## el           7.655e-02  6.315e-03  12.122  < 2e-16 ***
## sl           2.884e-01  9.009e-02   3.201  0.00147 ** 
## I(el^2)     -2.045e-05  2.186e-06  -9.355  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.39 on 445 degrees of freedom
## Multiple R-squared:  0.5564,	Adjusted R-squared:  0.5534 
## F-statistic:   186 on 3 and 445 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92.5 17"
## [1] "N = 417"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1553.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1554.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -104.705  -23.920   -4.211   21.621  138.978 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.383e+02  9.171e+00  15.078  < 2e-16 ***
## el          -3.965e-02  1.374e-02  -2.885  0.00412 ** 
## sl           3.452e-01  2.338e-01   1.476  0.14060    
## I(el^2)     -1.639e-06  4.998e-06  -0.328  0.74307    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 43.24 on 413 degrees of freedom
## Multiple R-squared:  0.2987,	Adjusted R-squared:  0.2936 
## F-statistic: 58.64 on 3 and 413 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92.5 17.5"
## [1] "N = 443"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1555.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1556.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -107.711  -27.694   -2.329   26.468  153.264 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.831e+02  3.241e+00  56.479  < 2e-16 ***
## el          -1.005e-01  1.617e-02  -6.212 1.21e-09 ***
## sl           6.816e-01  2.787e-01   2.446   0.0149 *  
## I(el^2)      5.033e-05  1.068e-05   4.713 3.28e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 42.19 on 439 degrees of freedom
## Multiple R-squared:  0.123,	Adjusted R-squared:  0.117 
## F-statistic: 20.52 on 3 and 439 DF,  p-value: 1.855e-12
## 
## [1] "Point -92 15"
## [1] "N = 445"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1557.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1558.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.087 -12.942  -4.507  10.175  51.624 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.866e+01  1.502e+00  12.419   <2e-16 ***
## el           2.793e-02  3.049e-03   9.161   <2e-16 ***
## sl           4.580e-02  8.161e-02   0.561    0.575    
## I(el^2)     -1.123e-05  9.615e-07 -11.676   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.05 on 441 degrees of freedom
## Multiple R-squared:  0.3052,	Adjusted R-squared:  0.3005 
## F-statistic: 64.58 on 3 and 441 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92 15.5"
## [1] "N = 395"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1559.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1560.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -15.490  -9.048  -4.746   6.787  47.396 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.889e+01  3.067e+00   6.159 1.82e-09 ***
## el           2.088e-03  4.105e-03   0.509   0.6112    
## sl           1.587e-01  6.329e-02   2.507   0.0126 *  
## I(el^2)     -1.196e-06  1.052e-06  -1.136   0.2565    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.53 on 391 degrees of freedom
## Multiple R-squared:  0.04216,	Adjusted R-squared:  0.03481 
## F-statistic: 5.737 on 3 and 391 DF,  p-value: 0.000754
## 
## [1] "Point -92 16"
## [1] "N = 391"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1561.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1562.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -51.007 -16.926  -3.732   8.313 116.436 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -7.914e+01  9.363e+00  -8.452 5.91e-16 ***
## el           1.702e-01  1.602e-02  10.621  < 2e-16 ***
## sl           2.819e-01  1.879e-01   1.500    0.134    
## I(el^2)     -5.383e-05  6.046e-06  -8.903  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.29 on 387 degrees of freedom
## Multiple R-squared:  0.3319,	Adjusted R-squared:  0.3267 
## F-statistic: 64.08 on 3 and 387 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92 16.5"
## [1] "N = 419"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1563.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1564.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -78.926 -16.057   1.207  18.432  68.427 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.108e+02  1.237e+01   8.954  < 2e-16 ***
## el          -5.052e-02  1.626e-02  -3.108  0.00201 ** 
## sl           9.677e-01  1.460e-01   6.630 1.05e-10 ***
## I(el^2)      4.803e-06  5.187e-06   0.926  0.35499    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 26.99 on 415 degrees of freedom
## Multiple R-squared:  0.3159,	Adjusted R-squared:  0.311 
## F-statistic: 63.89 on 3 and 415 DF,  p-value: < 2.2e-16
## 
## [1] "Point -92 17"
## [1] "N = 378"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1565.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1566.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -66.083 -13.651  -0.681   9.269  88.952 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.563e+02  4.313e+00  36.240  < 2e-16 ***
## el          -8.075e-02  8.607e-03  -9.383  < 2e-16 ***
## sl           2.362e-01  1.465e-01   1.612    0.108    
## I(el^2)      1.692e-05  3.838e-06   4.410 1.35e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 23.73 on 374 degrees of freedom
## Multiple R-squared:  0.5039,	Adjusted R-squared:  0.4999 
## F-statistic: 126.6 on 3 and 374 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 14.5"
## [1] "N = 390"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1567.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1568.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -26.374  -7.811  -2.963   5.964  46.423 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.420e+01  1.104e+00   12.87   <2e-16 ***
## el           3.397e-02  3.019e-03   11.25   <2e-16 ***
## sl           1.287e-01  8.878e-02    1.45    0.148    
## I(el^2)     -1.408e-05  1.039e-06  -13.55   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.86 on 386 degrees of freedom
## Multiple R-squared:  0.3586,	Adjusted R-squared:  0.3536 
## F-statistic: 71.94 on 3 and 386 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 15"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1569.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1570.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -45.816  -6.395  -1.006   4.474  56.311 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.183e+02  4.287e+00  27.599  < 2e-16 ***
## el          -8.751e-02  4.451e-03 -19.659  < 2e-16 ***
## sl           2.527e-01  5.301e-02   4.767 2.61e-06 ***
## I(el^2)      1.659e-05  1.110e-06  14.934  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.72 on 407 degrees of freedom
## Multiple R-squared:  0.6671,	Adjusted R-squared:  0.6647 
## F-statistic: 271.9 on 3 and 407 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 15.5"
## [1] "N = 397"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1571.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1572.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -114.328  -21.966   -5.573   10.341  225.617 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.292e+02  1.949e+01  11.760  < 2e-16 ***
## el          -1.652e-01  1.882e-02  -8.780  < 2e-16 ***
## sl           8.472e-01  2.023e-01   4.188 3.48e-05 ***
## I(el^2)      3.033e-05  4.389e-06   6.912 1.94e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 43.17 on 393 degrees of freedom
## Multiple R-squared:  0.3151,	Adjusted R-squared:  0.3099 
## F-statistic: 60.28 on 3 and 393 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 16"
## [1] "N = 395"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1573.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1574.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -102.495  -30.014   -4.466   24.016  196.125 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.154e+02  7.809e+00  14.772  < 2e-16 ***
## el          -1.617e-03  1.122e-02  -0.144  0.88548    
## sl           6.347e-01  2.297e-01   2.764  0.00599 ** 
## I(el^2)     -1.138e-05  3.471e-06  -3.277  0.00114 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 50.58 on 391 degrees of freedom
## Multiple R-squared:  0.2788,	Adjusted R-squared:  0.2733 
## F-statistic: 50.39 on 3 and 391 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 16.5"
## [1] "N = 425"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1575.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1576.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -51.177 -13.688  -2.992  10.643 106.368 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.563e+01  4.381e+00  14.982   <2e-16 ***
## el           1.061e-01  9.623e-03  11.028   <2e-16 ***
## sl           3.423e-01  1.139e-01   3.006   0.0028 ** 
## I(el^2)     -5.694e-05  4.561e-06 -12.485   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 22.48 on 421 degrees of freedom
## Multiple R-squared:  0.3196,	Adjusted R-squared:  0.3148 
## F-statistic: 65.92 on 3 and 421 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91.5 17"
## [1] "N = 412"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1577.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1578.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -30.781 -13.198   0.026  10.933  52.399 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.275e+02  2.264e+00  56.296  < 2e-16 ***
## el          -4.202e-02  7.375e-03  -5.697 2.34e-08 ***
## sl           1.431e-01  8.963e-02   1.596    0.111    
## I(el^2)      2.071e-05  5.240e-06   3.951 9.16e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.52 on 408 degrees of freedom
## Multiple R-squared:  0.1339,	Adjusted R-squared:  0.1275 
## F-statistic: 21.02 on 3 and 408 DF,  p-value: 1.107e-12
## 
## [1] "Point -91 14.5"
## [1] "N = 424"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1579.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1580.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -19.399  -7.606  -3.361   2.571  59.446 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.670e+01  1.330e+00  12.563  < 2e-16 ***
## el           4.733e-03  2.637e-03   1.795   0.0734 .  
## sl           1.650e-01  6.837e-02   2.414   0.0162 *  
## I(el^2)     -4.320e-06  9.795e-07  -4.410 1.31e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.83 on 420 degrees of freedom
## Multiple R-squared:  0.1594,	Adjusted R-squared:  0.1534 
## F-statistic: 26.55 on 3 and 420 DF,  p-value: 9.636e-16
## 
## [1] "Point -91 15"
## [1] "N = 429"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1581.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1582.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -44.360 -10.311  -5.342   6.819  84.847 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.170e+01  1.105e+01   7.397 7.52e-13 ***
## el          -5.472e-02  1.112e-02  -4.922 1.23e-06 ***
## sl           2.900e-01  8.743e-02   3.316 0.000990 ***
## I(el^2)      9.435e-06  2.765e-06   3.412 0.000706 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.77 on 425 degrees of freedom
## Multiple R-squared:  0.2084,	Adjusted R-squared:  0.2028 
## F-statistic: 37.29 on 3 and 425 DF,  p-value: < 2.2e-16
## 
## [1] "Point -91 15.5"
## [1] "N = 403"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1583.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1584.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -106.40  -41.26  -12.22   28.22  190.16 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.402e+02  9.406e+00  25.537  < 2e-16 ***
## el          -1.191e-01  1.310e-02  -9.091  < 2e-16 ***
## sl           6.926e-01  2.421e-01   2.861  0.00444 ** 
## I(el^2)      1.062e-05  4.227e-06   2.512  0.01241 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 52.97 on 399 degrees of freedom
## Multiple R-squared:  0.6102,	Adjusted R-squared:  0.6072 
## F-statistic: 208.2 on 3 and 399 DF,  p-value: < 2.2e-16
## 
## [1] "Point -90.5 14"
## [1] "N = 301"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1585.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1586.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.3461 -2.0450 -0.2863  1.3577 10.3215 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.093e+00  2.749e-01  22.163  < 2e-16 ***
## el           8.159e-03  1.013e-03   8.055 1.94e-14 ***
## sl           9.346e-02  2.499e-02   3.740 0.000221 ***
## I(el^2)     -3.045e-06  6.168e-07  -4.937 1.33e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.971 on 297 degrees of freedom
## Multiple R-squared:  0.4815,	Adjusted R-squared:  0.4762 
## F-statistic: 91.92 on 3 and 297 DF,  p-value: < 2.2e-16
## 
## [1] "Point -90.5 14.5"
## [1] "N = 404"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1587.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1588.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.2665  -4.4102  -0.1577   3.1453  22.5705 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.366e+01  1.126e+00  12.135  < 2e-16 ***
## el          -1.085e-02  1.842e-03  -5.888 8.28e-09 ***
## sl           9.678e-02  3.092e-02   3.130  0.00188 ** 
## I(el^2)      5.112e-06  7.074e-07   7.227 2.52e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.81 on 400 degrees of freedom
## Multiple R-squared:  0.1611,	Adjusted R-squared:  0.1548 
## F-statistic:  25.6 on 3 and 400 DF,  p-value: 3.614e-15
## 
## [1] "Point -90.5 15"
## [1] "N = 404"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1589.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1590.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -39.30 -21.63  -9.40  15.23 127.79 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.975e+01  1.142e+01  -1.729   0.0846 .  
## el           4.464e-02  1.738e-02   2.568   0.0106 *  
## sl           5.965e-01  1.453e-01   4.105 4.91e-05 ***
## I(el^2)     -1.105e-05  6.213e-06  -1.778   0.0761 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.35 on 400 degrees of freedom
## Multiple R-squared:  0.09725,	Adjusted R-squared:  0.09048 
## F-statistic: 14.36 on 3 and 400 DF,  p-value: 6.617e-09
## 
## [1] "Point -90.5 15.5"
## [1] "N = 434"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1591.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1592.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -142.72  -39.85   -6.17   42.34  157.56 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.389e+02  9.892e+00  24.154  < 2e-16 ***
## el          -9.316e-02  2.127e-02  -4.379  1.5e-05 ***
## sl          -8.037e-03  2.936e-01  -0.027    0.978    
## I(el^2)      2.682e-06  9.181e-06   0.292    0.770    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 64.48 on 430 degrees of freedom
## Multiple R-squared:  0.3976,	Adjusted R-squared:  0.3933 
## F-statistic: 94.58 on 3 and 430 DF,  p-value: < 2.2e-16
## 
## [1] "Point -90 14"
## [1] "N = 362"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1593.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1594.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.9012 -1.6166 -0.2144  1.5375 12.0599 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.464e+00  3.275e-01  22.787  < 2e-16 ***
## el          -1.972e-03  9.862e-04  -1.999   0.0463 *  
## sl           3.169e-02  1.856e-02   1.707   0.0886 .  
## I(el^2)      3.446e-06  6.142e-07   5.610 4.06e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.831 on 358 degrees of freedom
## Multiple R-squared:  0.2865,	Adjusted R-squared:  0.2805 
## F-statistic: 47.92 on 3 and 358 DF,  p-value: < 2.2e-16
## 
## [1] "Point -90 14.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1595.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1596.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.378  -4.106  -1.313   2.030  25.228 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -1.538e+00  1.914e+00  -0.804  0.42215   
## el           4.235e-03  3.336e-03   1.269  0.20503   
## sl           1.165e-01  3.691e-02   3.157  0.00171 **
## I(el^2)      4.142e-06  1.294e-06   3.202  0.00147 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.315 on 407 degrees of freedom
## Multiple R-squared:  0.5439,	Adjusted R-squared:  0.5406 
## F-statistic: 161.8 on 3 and 407 DF,  p-value: < 2.2e-16
## 
## [1] "Point -90 15"
## [1] "N = 414"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1597.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1598.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -65.34 -26.01 -15.30  23.14 112.34 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.454e+01  6.325e+00   2.298 0.022040 *  
## el          8.854e-04  1.133e-02   0.078 0.937751    
## sl          6.982e-01  1.804e-01   3.870 0.000126 ***
## I(el^2)     1.085e-05  4.344e-06   2.498 0.012881 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 36.34 on 410 degrees of freedom
## Multiple R-squared:  0.2437,	Adjusted R-squared:  0.2381 
## F-statistic: 44.03 on 3 and 410 DF,  p-value: < 2.2e-16
## 
## [1] "Point -90 15.5"
## [1] "N = 435"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1599.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1600.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -129.699  -34.368    1.415   32.140  155.209 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.505e+02  6.583e+00  22.865  < 2e-16 ***
## el           6.707e-02  1.459e-02   4.596 5.67e-06 ***
## sl          -4.128e-01  2.373e-01  -1.739   0.0827 .  
## I(el^2)     -4.439e-05  6.343e-06  -6.999 9.93e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 52.8 on 431 degrees of freedom
## Multiple R-squared:  0.2125,	Adjusted R-squared:  0.207 
## F-statistic: 38.76 on 3 and 431 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89.5 13.5"
## [1] "N = 209"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1601.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1602.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.5687 -1.5649  0.2567  1.6330  7.6416 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.590e+00  3.720e-01  17.715  < 2e-16 ***
## el           4.545e-03  1.265e-03   3.593  0.00041 ***
## sl          -3.239e-02  2.387e-02  -1.357  0.17617    
## I(el^2)     -1.628e-06  9.647e-07  -1.688  0.09298 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.42 on 205 degrees of freedom
## Multiple R-squared:  0.1356,	Adjusted R-squared:  0.1229 
## F-statistic: 10.72 on 3 and 205 DF,  p-value: 1.422e-06
## 
## [1] "Point -89.5 14"
## [1] "N = 385"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1603.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1604.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.7054 -2.2443 -0.4042  1.5793 18.4412 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.343e+01  8.726e-01  15.386  < 2e-16 ***
## el          -1.632e-02  2.406e-03  -6.781 4.58e-11 ***
## sl           2.387e-02  2.431e-02   0.982    0.327    
## I(el^2)      1.063e-05  1.472e-06   7.218 2.88e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.727 on 381 degrees of freedom
## Multiple R-squared:  0.1213,	Adjusted R-squared:  0.1144 
## F-statistic: 17.54 on 3 and 381 DF,  p-value: 1.1e-10
## 
## [1] "Point -89.5 14.5"
## [1] "N = 402"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1605.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1606.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.997  -8.635  -3.081   5.914  40.713 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -4.966e+00  3.570e+00  -1.391   0.1650    
## el           1.376e-02  7.083e-03   1.943   0.0527 .  
## sl           3.147e-01  6.587e-02   4.777  2.5e-06 ***
## I(el^2)      3.060e-06  3.213e-06   0.953   0.3414    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.92 on 398 degrees of freedom
## Multiple R-squared:  0.325,	Adjusted R-squared:  0.3199 
## F-statistic: 63.88 on 3 and 398 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89.5 15"
## [1] "N = 429"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1607.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1608.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -64.303 -28.325  -0.943  25.886  74.678 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.017e+01  3.994e+00  17.567  < 2e-16 ***
## el          -8.495e-02  8.657e-03  -9.813  < 2e-16 ***
## sl           6.495e-01  1.655e-01   3.923 0.000102 ***
## I(el^2)      4.062e-05  3.773e-06  10.768  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.77 on 425 degrees of freedom
## Multiple R-squared:  0.218,	Adjusted R-squared:  0.2125 
## F-statistic:  39.5 on 3 and 425 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89.5 15.5"
## [1] "N = 420"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1609.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1610.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -131.59  -29.47   12.20   24.47   94.96 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.378e+02  3.826e+00  36.020  < 2e-16 ***
## el           2.224e-02  1.269e-02   1.753  0.08036 .  
## sl          -3.380e-01  2.168e-01  -1.559  0.11979    
## I(el^2)     -1.570e-05  5.984e-06  -2.624  0.00901 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 41.55 on 416 degrees of freedom
## Multiple R-squared:  0.02846,	Adjusted R-squared:  0.02146 
## F-statistic: 4.063 on 3 and 416 DF,  p-value: 0.007291
## 
## [1] "Point -89 13.5"
## [1] "N = 285"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1611.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1612.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.2866 -1.0196 -0.2804  1.0560  8.8844 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.207e+00  1.997e-01  21.071  < 2e-16 ***
## el           9.883e-03  8.228e-04  12.012  < 2e-16 ***
## sl           1.373e-02  1.606e-02   0.855    0.393    
## I(el^2)     -5.765e-06  6.982e-07  -8.258 5.88e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.75 on 281 degrees of freedom
## Multiple R-squared:  0.4577,	Adjusted R-squared:  0.4519 
## F-statistic: 79.06 on 3 and 281 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89 14.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1613.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1614.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -24.848 -11.555  -0.288   9.719  35.989 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.169e+01  4.037e+00   5.372 1.31e-07 ***
## el           1.231e-02  6.877e-03   1.790   0.0743 .  
## sl          -1.698e-03  7.160e-02  -0.024   0.9811    
## I(el^2)     -1.156e-06  2.748e-06  -0.420   0.6743    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.29 on 407 degrees of freedom
## Multiple R-squared:  0.0796,	Adjusted R-squared:  0.07282 
## F-statistic: 11.73 on 3 and 407 DF,  p-value: 2.181e-07
## 
## [1] "Point -89 15"
## [1] "N = 418"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1615.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1616.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -104.044  -14.225   -0.453   13.447   65.081 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.525e+02  4.008e+00   38.04   <2e-16 ***
## el          -1.848e-01  1.077e-02  -17.17   <2e-16 ***
## sl           1.842e-01  1.406e-01    1.31    0.191    
## I(el^2)      8.125e-05  6.766e-06   12.01   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.95 on 414 degrees of freedom
## Multiple R-squared:  0.5577,	Adjusted R-squared:  0.5545 
## F-statistic:   174 on 3 and 414 DF,  p-value: < 2.2e-16
## 
## [1] "Point -89 16.5"
## [1] "N = 427"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1617.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1618.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -33.274  -8.897  -0.947   8.850  35.607 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.475e+02  1.389e+00 106.194   <2e-16 ***
## el          -1.275e-01  7.584e-03 -16.811   <2e-16 ***
## sl          -5.265e-02  8.244e-02  -0.639    0.523    
## I(el^2)      1.726e-04  9.029e-06  19.114   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.59 on 423 degrees of freedom
## Multiple R-squared:  0.4829,	Adjusted R-squared:  0.4792 
## F-statistic: 131.7 on 3 and 423 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88.5 14"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1619.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1620.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -14.477  -4.741  -0.887   2.683  51.066 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 5.442e+00  1.067e+00   5.102 5.03e-07 ***
## el          8.898e-03  2.946e-03   3.020  0.00268 ** 
## sl          1.023e-02  4.200e-02   0.244  0.80764    
## I(el^2)     1.350e-06  1.538e-06   0.878  0.38037    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.648 on 437 degrees of freedom
## Multiple R-squared:  0.3522,	Adjusted R-squared:  0.3478 
## F-statistic:  79.2 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88.5 14.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1621.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1622.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -41.475 -11.154  -1.753  11.254  42.815 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.985e+01  5.284e+00  11.327  < 2e-16 ***
## el          -3.309e-02  8.602e-03  -3.847 0.000139 ***
## sl          -8.435e-02  9.242e-02  -0.913 0.361941    
## I(el^2)      1.185e-05  3.350e-06   3.538 0.000450 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.75 on 407 degrees of freedom
## Multiple R-squared:  0.03987,	Adjusted R-squared:  0.03279 
## F-statistic: 5.633 on 3 and 407 DF,  p-value: 0.0008629
## 
## [1] "Point -88.5 15"
## [1] "N = 435"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1623.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1624.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -59.047 -15.145  -0.931  14.840  71.735 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.738e+01  3.835e+00  25.394  < 2e-16 ***
## el          -4.982e-02  8.311e-03  -5.995  4.3e-09 ***
## sl           3.695e-01  1.134e-01   3.259  0.00121 ** 
## I(el^2)      1.328e-05  4.213e-06   3.151  0.00174 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.79 on 431 degrees of freedom
## Multiple R-squared:  0.1841,	Adjusted R-squared:  0.1784 
## F-statistic: 32.41 on 3 and 431 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88.5 15.5"
## [1] "N = 372"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1625.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1626.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -75.149 -18.258   1.513  17.239 114.591 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.925e+02  2.983e+00  64.530   <2e-16 ***
## el          -2.341e-01  1.286e-02 -18.200   <2e-16 ***
## sl           3.580e-01  2.045e-01   1.751   0.0808 .  
## I(el^2)      1.218e-04  9.746e-06  12.497   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 30.59 on 368 degrees of freedom
## Multiple R-squared:  0.6292,	Adjusted R-squared:  0.6262 
## F-statistic: 208.2 on 3 and 368 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88 14"
## [1] "N = 432"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1627.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1628.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -16.361  -4.656  -1.346   2.423  43.214 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  2.982e+00  1.381e+00   2.160  0.03131 * 
## el           5.552e-03  3.080e-03   1.803  0.07212 . 
## sl          -3.580e-02  4.529e-02  -0.790  0.42976   
## I(el^2)      4.495e-06  1.459e-06   3.082  0.00219 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.948 on 428 degrees of freedom
## Multiple R-squared:  0.4906,	Adjusted R-squared:  0.487 
## F-statistic: 137.4 on 3 and 428 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88 14.5"
## [1] "N = 442"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1629.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1630.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -67.896 -16.238  -2.761  10.262 103.466 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.505e+02  9.045e+00  16.637  < 2e-16 ***
## el          -1.568e-01  1.627e-02  -9.634  < 2e-16 ***
## sl           4.458e-01  1.541e-01   2.893  0.00401 ** 
## I(el^2)      4.826e-05  6.500e-06   7.424 5.98e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.72 on 438 degrees of freedom
## Multiple R-squared:  0.3278,	Adjusted R-squared:  0.3232 
## F-statistic: 71.21 on 3 and 438 DF,  p-value: < 2.2e-16
## 
## [1] "Point -88 15"
## [1] "N = 484"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1631.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1632.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -58.238 -18.016  -2.126  15.065  77.686 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.002e+02  3.302e+00  30.337  < 2e-16 ***
## el          -8.598e-04  8.745e-03  -0.098 0.921723    
## sl           1.900e-03  1.358e-01   0.014 0.988843    
## I(el^2)     -1.627e-05  4.819e-06  -3.377 0.000792 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27.07 on 480 degrees of freedom
## Multiple R-squared:  0.1782,	Adjusted R-squared:  0.1731 
## F-statistic:  34.7 on 3 and 480 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87.5 14"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1633.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1634.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -26.926  -6.050  -1.349   4.287  27.647 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.786e+00  1.632e+00   3.546 0.000435 ***
## el          -9.707e-04  3.335e-03  -0.291 0.771177    
## sl          -1.039e-01  4.921e-02  -2.112 0.035299 *  
## I(el^2)      1.197e-05  1.642e-06   7.290 1.61e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.049 on 411 degrees of freedom
## Multiple R-squared:  0.6199,	Adjusted R-squared:  0.6172 
## F-statistic: 223.5 on 3 and 411 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87.5 14.5"
## [1] "N = 412"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1635.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1636.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -47.246 -14.538  -1.946   9.766 109.098 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.902e+01  8.137e+00   7.252 2.08e-12 ***
## el          -3.297e-02  1.501e-02  -2.197   0.0286 *  
## sl           6.657e-01  1.217e-01   5.470 7.86e-08 ***
## I(el^2)      9.427e-06  6.422e-06   1.468   0.1429    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 23.13 on 408 degrees of freedom
## Multiple R-squared:  0.08767,	Adjusted R-squared:  0.08096 
## F-statistic: 13.07 on 3 and 408 DF,  p-value: 3.639e-08
## 
## [1] "Point -87.5 15"
## [1] "N = 443"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1637.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1638.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -51.502 -19.056  -1.671  13.428  86.460 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.711e+01  5.048e+00  15.278   <2e-16 ***
## el          -8.126e-03  1.167e-02  -0.696    0.487    
## sl           1.833e-01  1.447e-01   1.267    0.206    
## I(el^2)      4.443e-06  5.841e-06   0.761    0.447    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 28.55 on 439 degrees of freedom
## Multiple R-squared:  0.004763,	Adjusted R-squared:  -0.002039 
## F-statistic: 0.7003 on 3 and 439 DF,  p-value: 0.5523
## 
## [1] "Point -87.5 15.5"
## [1] "N = 412"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1639.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1640.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -150.736  -43.576    2.749   44.875  188.185 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.538e+02  5.900e+00  43.023  < 2e-16 ***
## el          -3.425e-01  2.889e-02 -11.856  < 2e-16 ***
## sl           1.057e+00  4.580e-01   2.308   0.0215 *  
## I(el^2)      1.509e-04  1.990e-05   7.581 2.33e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 71.38 on 408 degrees of freedom
## Multiple R-squared:  0.4861,	Adjusted R-squared:  0.4823 
## F-statistic: 128.6 on 3 and 408 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87 13.5"
## [1] "N = 413"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1641.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1642.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.125  -3.622  -0.576   2.860  52.087 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.990e+00  8.525e-01  11.718  < 2e-16 ***
## el          -2.542e-02  2.969e-03  -8.561 2.29e-16 ***
## sl          -1.768e-02  4.352e-02  -0.406    0.685    
## I(el^2)      3.164e-05  2.108e-06  15.008  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.279 on 409 degrees of freedom
## Multiple R-squared:  0.5762,	Adjusted R-squared:  0.5731 
## F-statistic: 185.3 on 3 and 409 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87 14"
## [1] "N = 397"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1643.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1644.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -33.701 -11.051  -1.697  10.654  44.473 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.841e+01  5.024e+00  -3.664 0.000282 ***
## el           5.990e-02  1.001e-02   5.983 4.93e-09 ***
## sl           2.751e-01  8.633e-02   3.186 0.001558 ** 
## I(el^2)     -1.555e-05  4.829e-06  -3.221 0.001384 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.42 on 393 degrees of freedom
## Multiple R-squared:  0.3221,	Adjusted R-squared:  0.3169 
## F-statistic: 62.24 on 3 and 393 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87 15"
## [1] "N = 423"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1645.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1646.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -39.583 -14.280  -0.759  12.427  58.575 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.231e+01  5.941e+00  12.172  < 2e-16 ***
## el          -4.923e-02  1.176e-02  -4.186 3.46e-05 ***
## sl           2.555e-01  1.083e-01   2.359   0.0188 *  
## I(el^2)      3.557e-05  5.286e-06   6.729 5.64e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.04 on 419 degrees of freedom
## Multiple R-squared:  0.2819,	Adjusted R-squared:  0.2768 
## F-statistic: 54.83 on 3 and 419 DF,  p-value: < 2.2e-16
## 
## [1] "Point -87 15.5"
## [1] "N = 384"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1647.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1648.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -123.91  -38.98   10.08   42.71  140.33 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.926e+02  6.479e+00   45.16  < 2e-16 ***
## el          -4.312e-01  1.846e-02  -23.36  < 2e-16 ***
## sl           1.089e+00  2.890e-01    3.77 0.000189 ***
## I(el^2)      1.903e-04  1.023e-05   18.60  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 56.36 on 380 degrees of freedom
## Multiple R-squared:  0.6521,	Adjusted R-squared:  0.6493 
## F-statistic: 237.4 on 3 and 380 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 13"
## [1] "N = 385"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1649.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1650.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -35.113  -3.156  -0.265   2.567  36.212 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.121e+01  7.615e-01  14.721   <2e-16 ***
## el          -4.918e-02  3.215e-03 -15.298   <2e-16 ***
## sl           6.373e-02  5.036e-02   1.265    0.206    
## I(el^2)      5.934e-05  2.525e-06  23.500   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.954 on 381 degrees of freedom
## Multiple R-squared:  0.7349,	Adjusted R-squared:  0.7328 
## F-statistic:   352 on 3 and 381 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 13.5"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1651.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1652.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -46.079 -12.746  -2.906  12.437  40.288 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.890e+00  5.629e+00   1.224    0.222    
## el          -1.326e-02  1.244e-02  -1.066    0.287    
## sl           7.954e-02  9.376e-02   0.848    0.397    
## I(el^2)      4.067e-05  6.661e-06   6.106 2.36e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.9 on 411 degrees of freedom
## Multiple R-squared:  0.5247,	Adjusted R-squared:  0.5212 
## F-statistic: 151.2 on 3 and 411 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 14"
## [1] "N = 426"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1653.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1654.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -51.185 -17.405   0.522  14.789  46.669 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.406e+01  7.892e+00   1.781   0.0756 .  
## el          1.364e-02  1.758e-02   0.776   0.4383    
## sl          4.248e-01  1.190e-01   3.569   0.0004 ***
## I(el^2)     1.898e-05  8.996e-06   2.110   0.0354 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.58 on 422 degrees of freedom
## Multiple R-squared:  0.3721,	Adjusted R-squared:  0.3676 
## F-statistic: 83.35 on 3 and 422 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 15"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1655.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1656.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -51.10 -12.60  -3.28  11.89  84.49 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.579e+01  6.825e+00   9.639  < 2e-16 ***
## el          -2.698e-02  1.451e-02  -1.860  0.06353 .  
## sl           2.315e-01  8.669e-02   2.670  0.00787 ** 
## I(el^2)      3.618e-05  7.336e-06   4.932 1.16e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.01 on 437 degrees of freedom
## Multiple R-squared:  0.3597,	Adjusted R-squared:  0.3553 
## F-statistic: 81.83 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 15.5"
## [1] "N = 396"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1657.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1658.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -91.261 -26.300   0.409  29.700 116.013 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.153e+02  4.646e+00  46.334   <2e-16 ***
## el          -2.365e-01  1.333e-02 -17.742   <2e-16 ***
## sl           4.336e-01  1.952e-01   2.222   0.0269 *  
## I(el^2)      1.060e-04  7.607e-06  13.932   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 39.51 on 392 degrees of freedom
## Multiple R-squared:  0.5218,	Adjusted R-squared:  0.5181 
## F-statistic: 142.6 on 3 and 392 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86.5 16"
## [1] "N = 106"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1659.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1660.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -50.181 -13.838  -0.597   9.960 137.304 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.734e+02  5.133e+00  53.258   <2e-16 ***
## el          -2.471e-01  1.782e-02 -13.871   <2e-16 ***
## sl           5.544e-01  2.998e-01   1.849   0.0673 .  
## I(el^2)      8.992e-05  9.046e-06   9.940   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.9 on 102 degrees of freedom
## Multiple R-squared:  0.7698,	Adjusted R-squared:  0.763 
## F-statistic: 113.7 on 3 and 102 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86 12.5"
## [1] "N = 401"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1661.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1662.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -35.371 -10.213  -3.513   9.368  81.976 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.079e+01  1.791e+00   6.023  3.9e-09 ***
## el          2.665e-02  1.039e-02   2.564   0.0107 *  
## sl          2.548e-01  1.446e-01   1.762   0.0788 .  
## I(el^2)     6.747e-06  1.040e-05   0.649   0.5169    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.55 on 397 degrees of freedom
## Multiple R-squared:  0.2572,	Adjusted R-squared:  0.2516 
## F-statistic: 45.82 on 3 and 397 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86 13"
## [1] "N = 409"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1663.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1664.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -62.541 -28.093  -6.171  25.847  75.619 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.191e+01  1.145e+01   6.278 8.83e-10 ***
## el          -1.432e-01  3.088e-02  -4.638 4.75e-06 ***
## sl           4.802e-01  1.739e-01   2.762  0.00601 ** 
## I(el^2)      1.171e-04  1.926e-05   6.081 2.76e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 30.92 on 405 degrees of freedom
## Multiple R-squared:  0.2116,	Adjusted R-squared:  0.2058 
## F-statistic: 36.24 on 3 and 405 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86 13.5"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1665.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1666.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -57.48 -17.87   1.32  19.98  54.69 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.037e+01  1.043e+01   6.747 5.13e-11 ***
## el          -6.141e-02  2.549e-02  -2.409   0.0164 *  
## sl           2.101e-01  1.447e-01   1.451   0.1474    
## I(el^2)      5.882e-05  1.493e-05   3.941 9.53e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 26.23 on 411 degrees of freedom
## Multiple R-squared:  0.1571,	Adjusted R-squared:  0.1509 
## F-statistic: 25.53 on 3 and 411 DF,  p-value: 3.632e-15
## 
## [1] "Point -86 14"
## [1] "N = 438"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1667.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1668.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -51.05 -13.96   2.53  14.77  51.95 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.060e+01  5.394e+00  14.943  < 2e-16 ***
## el          -5.691e-02  1.435e-02  -3.966 8.56e-05 ***
## sl           5.111e-01  1.099e-01   4.650 4.41e-06 ***
## I(el^2)      4.970e-05  8.674e-06   5.729 1.89e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20 on 434 degrees of freedom
## Multiple R-squared:  0.211,	Adjusted R-squared:  0.2056 
## F-statistic: 38.69 on 3 and 434 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86 15"
## [1] "N = 436"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1669.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1670.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -51.862 -21.498  -4.295  13.801 107.419 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.202e+01  8.739e+00   7.097 5.27e-12 ***
## el           6.137e-02  2.133e-02   2.878  0.00421 ** 
## sl          -3.449e-01  1.690e-01  -2.041  0.04186 *  
## I(el^2)     -5.061e-06  1.107e-05  -0.457  0.64773    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.93 on 432 degrees of freedom
## Multiple R-squared:  0.2066,	Adjusted R-squared:  0.2011 
## F-statistic:  37.5 on 3 and 432 DF,  p-value: < 2.2e-16
## 
## [1] "Point -86 15.5"
## [1] "N = 446"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1671.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1672.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -63.065 -21.559  -1.316  18.316  85.357 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.154e+02  3.584e+00  60.092   <2e-16 ***
## el          -2.239e-01  1.396e-02 -16.040   <2e-16 ***
## sl           2.670e-01  1.685e-01   1.584    0.114    
## I(el^2)      1.136e-04  1.136e-05   9.999   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.29 on 442 degrees of freedom
## Multiple R-squared:  0.574,	Adjusted R-squared:  0.5711 
## F-statistic: 198.5 on 3 and 442 DF,  p-value: < 2.2e-16
## 
## [1] "Point -85.5 15"
## [1] "N = 441"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1673.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1674.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -64.650 -14.375   3.435  18.814  73.322 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.406e+01  4.375e+00  21.498  < 2e-16 ***
## el           7.250e-02  1.199e-02   6.048 3.14e-09 ***
## sl          -9.560e-03  1.317e-01  -0.073 0.942179    
## I(el^2)     -2.499e-05  7.344e-06  -3.403 0.000729 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 24.44 on 437 degrees of freedom
## Multiple R-squared:  0.1918,	Adjusted R-squared:  0.1863 
## F-statistic: 34.57 on 3 and 437 DF,  p-value: < 2.2e-16
## 
## [1] "Point -85.5 15.5"
## [1] "N = 433"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1675.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1676.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -54.237 -13.726   0.155  12.980  87.670 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.769e+02  2.517e+00  70.286  < 2e-16 ***
## el          -8.552e-02  8.942e-03  -9.564  < 2e-16 ***
## sl          -8.441e-02  1.234e-01  -0.684    0.494    
## I(el^2)      4.980e-05  6.715e-06   7.416  6.5e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 21.61 on 429 degrees of freedom
## Multiple R-squared:  0.2521,	Adjusted R-squared:  0.2468 
## F-statistic: 48.19 on 3 and 429 DF,  p-value: < 2.2e-16
## 
## [1] "Point -84.5 9.5"
## [1] "N = 143"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1677.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1678.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -58.795 -20.241  -4.364  19.979  93.413 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.001e+02  5.197e+00  19.255  < 2e-16 ***
## el          -8.510e-02  1.691e-02  -5.034 1.47e-06 ***
## sl          -3.221e-01  3.238e-01  -0.995 0.321619    
## I(el^2)      3.575e-05  9.522e-06   3.754 0.000255 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.41 on 139 degrees of freedom
## Multiple R-squared:  0.2793,	Adjusted R-squared:  0.2638 
## F-statistic: 17.96 on 3 and 139 DF,  p-value: 6.595e-10
## 
## [1] "Point -84.5 10"
## [1] "N = 356"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1679.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1680.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -153.26  -62.62  -39.58   20.30  402.29 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 6.883e+01  1.352e+01   5.092 5.78e-07 ***
## el          1.103e-02  3.022e-02   0.365   0.7154    
## sl          1.081e+00  6.061e-01   1.783   0.0755 .  
## I(el^2)     1.768e-05  1.438e-05   1.230   0.2197    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 105.2 on 352 degrees of freedom
## Multiple R-squared:  0.08414,	Adjusted R-squared:  0.07634 
## F-statistic: 10.78 on 3 and 352 DF,  p-value: 8.576e-07
## 
## [1] "Point -84.5 10.5"
## [1] "N = 382"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1681.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1682.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -191.78  -71.74  -18.44   76.52  251.83 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.972e+02  9.493e+00  31.304   <2e-16 ***
## el          -8.225e-02  3.017e-02  -2.727   0.0067 ** 
## sl           6.647e-01  6.781e-01   0.980   0.3276    
## I(el^2)      7.983e-06  1.718e-05   0.465   0.6425    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 102.2 on 378 degrees of freedom
## Multiple R-squared:  0.1002,	Adjusted R-squared:  0.09306 
## F-statistic: 14.03 on 3 and 378 DF,  p-value: 1.084e-08
## 
## [1] "Point -84 9"
## [1] "N = 60"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1683.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1684.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -35.756 -10.083   1.768   7.069  90.415 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.466e+02  3.964e+00  36.977  < 2e-16 ***
## el          -1.450e-01  2.576e-02  -5.631 6.01e-07 ***
## sl           7.437e-02  3.136e-01   0.237   0.8134    
## I(el^2)      6.225e-05  2.410e-05   2.583   0.0124 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.28 on 56 degrees of freedom
## Multiple R-squared:  0.7665,	Adjusted R-squared:  0.754 
## F-statistic: 61.28 on 3 and 56 DF,  p-value: < 2.2e-16
## 
## [1] "Point -84 9.5"
## [1] "N = 270"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1685.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1686.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -101.69  -48.53  -14.95   33.31  322.18 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.035e+02  1.115e+01   9.276   <2e-16 ***
## el          1.645e-02  1.790e-02   0.919    0.359    
## sl          5.500e-02  4.649e-01   0.118    0.906    
## I(el^2)     4.242e-07  6.234e-06   0.068    0.946    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 74.24 on 266 degrees of freedom
## Multiple R-squared:  0.03936,	Adjusted R-squared:  0.02853 
## F-statistic: 3.633 on 3 and 266 DF,  p-value: 0.01345
## 
## [1] "Point -84 10"
## [1] "N = 407"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1687.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1688.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -315.17  -97.97    5.61  100.65  318.93 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.282e+02  1.761e+01  24.313   <2e-16 ***
## el          -3.788e-01  3.042e-02 -12.452   <2e-16 ***
## sl           1.351e+00  6.007e-01   2.249    0.025 *  
## I(el^2)      1.121e-04  1.126e-05   9.958   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 124.6 on 403 degrees of freedom
## Multiple R-squared:  0.326,	Adjusted R-squared:  0.321 
## F-statistic: 64.98 on 3 and 403 DF,  p-value: < 2.2e-16
## 
## [1] "Point -83.5 9"
## [1] "N = 278"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1689.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1690.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -148.550  -30.414   -7.221   19.098  118.274 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.525e+02  5.257e+00  29.014   <2e-16 ***
## el          -1.380e-01  1.113e-02 -12.398   <2e-16 ***
## sl           2.714e-01  2.886e-01   0.941    0.348    
## I(el^2)      4.887e-05  4.101e-06  11.915   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 42.82 on 274 degrees of freedom
## Multiple R-squared:  0.3641,	Adjusted R-squared:  0.3571 
## F-statistic: 52.29 on 3 and 274 DF,  p-value: < 2.2e-16
## 
## [1] "Point -83.5 9.5"
## [1] "N = 370"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1691.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1692.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -155.05  -64.71  -23.39   65.71  257.46 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.974e+01  1.557e+01   3.836 0.000147 ***
## el           1.577e-01  2.178e-02   7.239 2.68e-12 ***
## sl           1.133e+00  4.263e-01   2.659 0.008186 ** 
## I(el^2)     -4.861e-05  6.553e-06  -7.417 8.42e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 85.94 on 366 degrees of freedom
## Multiple R-squared:  0.1648,	Adjusted R-squared:  0.158 
## F-statistic: 24.07 on 3 and 366 DF,  p-value: 3.067e-14
## 
## [1] "Point -83.5 10"
## [1] "N = 378"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1693.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1694.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -165.964  -30.877   -5.837   36.282  224.416 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.671e+02  5.439e+00  85.868  < 2e-16 ***
## el          -1.872e-01  1.246e-02 -15.020  < 2e-16 ***
## sl           5.267e-01  3.258e-01   1.617    0.107    
## I(el^2)      3.049e-05  4.933e-06   6.181 1.66e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 59.34 on 374 degrees of freedom
## Multiple R-squared:  0.7154,	Adjusted R-squared:  0.7131 
## F-statistic: 313.3 on 3 and 374 DF,  p-value: < 2.2e-16
## 
## [1] "Point -83 8.5"
## [1] "N = 243"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1695.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1696.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -56.658 -22.492  -2.048  19.183 107.287 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.617e+02  4.169e+00  38.790  < 2e-16 ***
## el          -9.766e-02  1.796e-02  -5.439 1.32e-07 ***
## sl           1.591e-01  2.973e-01   0.535   0.5930    
## I(el^2)      2.413e-05  1.419e-05   1.700   0.0904 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34.04 on 239 degrees of freedom
## Multiple R-squared:  0.4229,	Adjusted R-squared:  0.4157 
## F-statistic: 58.39 on 3 and 239 DF,  p-value: < 2.2e-16
## 
## [1] "Point -83 9"
## [1] "N = 388"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1697.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1698.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -75.737 -26.645  -1.793  22.286 127.594 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.414e+02  5.168e+00  27.359  < 2e-16 ***
## el          -8.273e-02  8.860e-03  -9.338  < 2e-16 ***
## sl           6.900e-01  1.907e-01   3.618 0.000337 ***
## I(el^2)      3.124e-05  3.158e-06   9.894  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 38.21 on 384 degrees of freedom
## Multiple R-squared:  0.2144,	Adjusted R-squared:  0.2082 
## F-statistic: 34.92 on 3 and 384 DF,  p-value: < 2.2e-16
## 
## [1] "Point -83 9.5"
## [1] "N = 365"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1699.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1700.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -182.731  -29.912   -7.656   25.461  169.873 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.210e+02  6.705e+00  47.870   <2e-16 ***
## el          -1.833e-01  1.299e-02 -14.114   <2e-16 ***
## sl          -4.577e-03  2.895e-01  -0.016    0.987    
## I(el^2)      4.657e-05  4.738e-06   9.829   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 56.62 on 361 degrees of freedom
## Multiple R-squared:  0.5656,	Adjusted R-squared:  0.562 
## F-statistic: 156.7 on 3 and 361 DF,  p-value: < 2.2e-16
## 
## [1] "Point -82.5 8.5"
## [1] "N = 322"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1701.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1702.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -99.557 -20.630  -6.928  10.939 187.128 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.066e+02  3.879e+00  27.473  < 2e-16 ***
## el           2.585e-02  1.061e-02   2.436 0.015401 *  
## sl           1.233e+00  3.179e-01   3.880 0.000127 ***
## I(el^2)     -9.337e-06  4.584e-06  -2.037 0.042520 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 40.16 on 318 degrees of freedom
## Multiple R-squared:  0.1284,	Adjusted R-squared:  0.1201 
## F-statistic: 15.61 on 3 and 318 DF,  p-value: 1.7e-09
## 
## [1] "Point -82.5 9"
## [1] "N = 352"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1703.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1704.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -126.965  -26.866    3.117   23.396  193.737 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.636e+02  6.017e+00  43.816  < 2e-16 ***
## el          -1.998e-01  1.074e-02 -18.603  < 2e-16 ***
## sl           1.529e+00  2.605e-01   5.868 1.03e-08 ***
## I(el^2)      5.541e-05  4.155e-06  13.337  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 45.55 on 348 degrees of freedom
## Multiple R-squared:  0.5924,	Adjusted R-squared:  0.5889 
## F-statistic: 168.6 on 3 and 348 DF,  p-value: < 2.2e-16
## 
## [1] "Point -82.5 9.5"
## [1] "N = 198"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1705.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1706.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -68.875 -17.632  -1.269  18.648  66.370 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.032e+02  3.054e+00  99.292  < 2e-16 ***
## el          -1.471e-01  1.130e-02 -13.021  < 2e-16 ***
## sl          -3.556e-01  2.115e-01  -1.682   0.0943 .  
## I(el^2)      2.925e-05  6.003e-06   4.872 2.29e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 24.02 on 194 degrees of freedom
## Multiple R-squared:  0.8424,	Adjusted R-squared:  0.8399 
## F-statistic: 345.6 on 3 and 194 DF,  p-value: < 2.2e-16
## 
## [1] "Point -82 8.5"
## [1] "N = 363"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1707.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1708.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -115.26  -51.91  -26.33   23.86  259.45 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.489e+02  7.460e+00  19.958  < 2e-16 ***
## el           2.763e-02  2.801e-02   0.986 0.324712    
## sl           1.801e+00  5.049e-01   3.568 0.000408 ***
## I(el^2)     -1.304e-05  1.713e-05  -0.761 0.447051    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 74.4 on 359 degrees of freedom
## Multiple R-squared:  0.06914,	Adjusted R-squared:  0.06136 
## F-statistic: 8.888 on 3 and 359 DF,  p-value: 1.072e-05
## 
## [1] "Point -82 9"
## [1] "N = 240"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1709.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1710.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -145.843  -39.605   -6.435   38.047  115.144 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.383e+02  6.352e+00  53.261  < 2e-16 ***
## el          -1.925e-01  2.181e-02  -8.827 2.46e-16 ***
## sl          -6.678e-01  4.410e-01  -1.514    0.131    
## I(el^2)      6.567e-05  1.225e-05   5.360 1.98e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 51.8 on 236 degrees of freedom
## Multiple R-squared:  0.5245,	Adjusted R-squared:  0.5185 
## F-statistic: 86.78 on 3 and 236 DF,  p-value: < 2.2e-16
## 
## [1] "Point -81.5 8.5"
## [1] "N = 393"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1711.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1712.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -143.53  -44.71  -19.69   39.02  228.06 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.765e+02  7.819e+00  22.572  < 2e-16 ***
## el          -1.292e-01  2.522e-02  -5.122 4.77e-07 ***
## sl           1.075e+00  4.343e-01   2.476   0.0137 *  
## I(el^2)      8.079e-05  1.518e-05   5.324 1.72e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 66.43 on 389 degrees of freedom
## Multiple R-squared:  0.07501,	Adjusted R-squared:  0.06787 
## F-statistic: 10.51 on 3 and 389 DF,  p-value: 1.155e-06
## 
## [1] "Point -81.5 9"
## [1] "N = 153"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1713.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1714.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -117.17  -45.24   10.45   48.03  102.52 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.570e+02  8.487e+00  42.061  < 2e-16 ***
## el          -3.873e-01  3.418e-02 -11.330  < 2e-16 ***
## sl          -1.070e-01  6.464e-01  -0.165    0.869    
## I(el^2)      1.902e-04  2.218e-05   8.577 1.17e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 54.8 on 149 degrees of freedom
## Multiple R-squared:  0.6386,	Adjusted R-squared:  0.6313 
## F-statistic: 87.75 on 3 and 149 DF,  p-value: < 2.2e-16
## 
## [1] "Point -81 7"
## [1] "N = 30"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1715.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1716.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.9162  -3.1251  -0.0289   2.3430  13.0769 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.069e+02  2.222e+00  48.129  < 2e-16 ***
## el          -7.789e-02  1.042e-02  -7.478 6.13e-08 ***
## sl           1.709e-02  1.316e-01   0.130    0.898    
## I(el^2)      5.168e-05  7.587e-06   6.812 3.14e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.402 on 26 degrees of freedom
## Multiple R-squared:  0.762,	Adjusted R-squared:  0.7345 
## F-statistic: 27.75 on 3 and 26 DF,  p-value: 2.905e-08
## 
## [1] "Point -81 8.5"
## [1] "N = 390"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1717.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1718.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -87.76 -46.92 -17.68  22.12 228.13 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.159e+02  6.517e+00  17.779  < 2e-16 ***
## el          -6.791e-02  2.975e-02  -2.283 0.022985 *  
## sl           1.632e+00  4.341e-01   3.761 0.000196 ***
## I(el^2)      5.956e-05  2.108e-05   2.825 0.004966 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 63 on 386 degrees of freedom
## Multiple R-squared:  0.06794,	Adjusted R-squared:  0.0607 
## F-statistic: 9.379 on 3 and 386 DF,  p-value: 5.362e-06
## 
## [1] "Point -80.5 7"
## [1] "N = 54"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1719.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1720.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -14.9472  -3.6844  -0.2253   4.8376  14.6345 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.684e+01  1.826e+00  53.026  < 2e-16 ***
## el          -6.337e-02  9.326e-03  -6.795 1.26e-08 ***
## sl          -6.392e-02  1.132e-01  -0.565    0.575    
## I(el^2)      4.873e-05  7.109e-06   6.855 1.01e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.937 on 50 degrees of freedom
## Multiple R-squared:  0.6065,	Adjusted R-squared:  0.5829 
## F-statistic: 25.69 on 3 and 50 DF,  p-value: 3.372e-10
## 
## [1] "Point -78.5 7.5"
## [1] "N = 42"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1721.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1722.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -83.029 -17.823  -1.101  19.412  68.603 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.207e+02  1.632e+01  13.518 4.27e-16 ***
## el          -1.278e-01  6.043e-02  -2.116    0.041 *  
## sl          -1.096e-01  7.001e-01  -0.157    0.876    
## I(el^2)      6.958e-05  4.377e-05   1.590    0.120    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34.7 on 38 degrees of freedom
## Multiple R-squared:  0.2153,	Adjusted R-squared:  0.1533 
## F-statistic: 3.475 on 3 and 38 DF,  p-value: 0.02523
## 
## [1] "Point -76.5 6"
## [1] "N = 183"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1723.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1724.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -353.62  -32.03   -2.01   36.28  103.55 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.286e+02  6.155e+00  85.887  < 2e-16 ***
## el          -2.717e-01  1.556e-02 -17.458  < 2e-16 ***
## sl           8.343e-01  2.735e-01   3.050  0.00263 ** 
## I(el^2)      4.619e-05  5.793e-06   7.972 1.77e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 52.68 on 179 degrees of freedom
## Multiple R-squared:  0.8648,	Adjusted R-squared:  0.8625 
## F-statistic: 381.5 on 3 and 179 DF,  p-value: < 2.2e-16
## 
## [1] "Point -76.5 6.5"
## [1] "N = 374"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1725.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1726.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -115.81  -29.74  -10.30   26.75  151.50 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.604e+02  4.578e+00 100.556   <2e-16 ***
## el          -2.836e-01  1.090e-02 -26.020   <2e-16 ***
## sl           4.997e-01  2.269e-01   2.202   0.0283 *  
## I(el^2)      6.272e-05  4.217e-06  14.872   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 49.6 on 370 degrees of freedom
## Multiple R-squared:  0.8258,	Adjusted R-squared:  0.8244 
## F-statistic: 584.6 on 3 and 370 DF,  p-value: < 2.2e-16
## 
## [1] "Point -76.5 7"
## [1] "N = 401"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1727.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1728.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -82.195 -23.771  -0.963  18.119  97.554 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.720e+02  3.194e+00  116.44   <2e-16 ***
## el          -2.431e-01  8.194e-03  -29.67   <2e-16 ***
## sl           1.525e-01  1.694e-01    0.90    0.369    
## I(el^2)      6.120e-05  3.482e-06   17.58   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34.77 on 397 degrees of freedom
## Multiple R-squared:  0.8633,	Adjusted R-squared:  0.8623 
## F-statistic: 835.8 on 3 and 397 DF,  p-value: < 2.2e-16
## 
## [1] "Point -76.5 7.5"
## [1] "N = 403"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1729.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1730.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -128.840  -35.349   -7.632   26.949  117.388 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.690e+02  4.417e+00  60.912   <2e-16 ***
## el          -2.007e-01  1.586e-02 -12.656   <2e-16 ***
## sl           2.735e-01  3.408e-01   0.802    0.423    
## I(el^2)      6.815e-05  7.470e-06   9.122   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 50.68 on 399 degrees of freedom
## Multiple R-squared:  0.469,	Adjusted R-squared:  0.465 
## F-statistic: 117.5 on 3 and 399 DF,  p-value: < 2.2e-16
## 
## [1] "Point -76.5 18"
## [1] "N = 128"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1731.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1732.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -109.21  -36.57   -6.55   30.43  150.82 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.445e+02  1.112e+01  12.992   <2e-16 ***
## el          -4.618e-02  5.934e-02  -0.778   0.4380    
## sl           1.574e+00  6.294e-01   2.501   0.0137 *  
## I(el^2)      3.981e-05  4.921e-05   0.809   0.4201    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 62.83 on 124 degrees of freedom
## Multiple R-squared:  0.06806,	Adjusted R-squared:  0.04552 
## F-statistic: 3.019 on 3 and 124 DF,  p-value: 0.03242
## 
## [1] "Point -76 6"
## [1] "N = 186"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1733.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1734.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -172.065  -42.112   -5.013   21.783  259.731 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.831e+02  2.702e+01  10.476  < 2e-16 ***
## el          -1.578e-01  3.358e-02  -4.700 5.12e-06 ***
## sl           5.675e-01  4.379e-01   1.296 0.196640    
## I(el^2)      3.931e-05  1.020e-05   3.852 0.000162 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 82.44 on 182 degrees of freedom
## Multiple R-squared:  0.138,	Adjusted R-squared:  0.1238 
## F-statistic:  9.71 on 3 and 182 DF,  p-value: 5.628e-06
## 
## [1] "Point -76 6.5"
## [1] "N = 371"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1735.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1736.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -121.066  -33.264   -2.413   24.505  194.534 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.994e+02  1.540e+01  12.946  < 2e-16 ***
## el          -5.596e-02  1.982e-02  -2.824  0.00500 ** 
## sl          -5.607e-01  2.361e-01  -2.375  0.01806 *  
## I(el^2)      1.475e-05  5.678e-06   2.598  0.00975 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 55.85 on 367 degrees of freedom
## Multiple R-squared:  0.04756,	Adjusted R-squared:  0.03978 
## F-statistic: 6.109 on 3 and 367 DF,  p-value: 0.0004594
## 
## [1] "Point -76 7"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1737.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1738.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -94.498 -16.780   2.919  17.711 114.453 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.058e+02  8.801e+00  23.382  < 2e-16 ***
## el          -7.780e-02  1.097e-02  -7.092 5.87e-12 ***
## sl          -1.166e-01  1.455e-01  -0.801    0.423    
## I(el^2)      2.090e-05  3.091e-06   6.763 4.72e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.06 on 407 degrees of freedom
## Multiple R-squared:  0.1244,	Adjusted R-squared:  0.118 
## F-statistic: 19.28 on 3 and 407 DF,  p-value: 1.045e-11
## 
## [1] "Point -76 7.5"
## [1] "N = 398"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1739.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1740.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -56.474 -13.448  -0.064  13.598  53.281 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.310e+02  2.445e+00  53.575  < 2e-16 ***
## el           2.548e-02  5.168e-03   4.931 1.21e-06 ***
## sl          -1.614e-01  1.041e-01  -1.550  0.12194    
## I(el^2)     -5.882e-06  1.768e-06  -3.327  0.00096 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.47 on 394 degrees of freedom
## Multiple R-squared:  0.1203,	Adjusted R-squared:  0.1136 
## F-statistic: 17.96 on 3 and 394 DF,  p-value: 6.053e-11
## 
## [1] "Point -76 18"
## [1] "N = 38"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1741.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1742.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -39.413 -19.893  -8.983   7.251  90.544 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.492e+02  1.200e+01  12.440  3.3e-14 ***
## el           2.655e-02  7.232e-02   0.367    0.716    
## sl           1.558e-01  8.007e-01   0.195    0.847    
## I(el^2)     -1.666e-05  6.934e-05  -0.240    0.812    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.82 on 34 degrees of freedom
## Multiple R-squared:  0.02228,	Adjusted R-squared:  -0.06398 
## F-statistic: 0.2583 on 3 and 34 DF,  p-value: 0.8549
## 
## [1] "Point -75.5 6"
## [1] "N = 208"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1743.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1744.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -66.54 -25.36 -10.58  11.70 141.95 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.058e+02  1.788e+01   5.914 1.39e-08 ***
## el           3.827e-02  2.201e-02   1.739   0.0836 .  
## sl           5.417e-02  2.620e-01   0.207   0.8364    
## I(el^2)     -1.420e-05  6.683e-06  -2.125   0.0348 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 40.85 on 204 degrees of freedom
## Multiple R-squared:  0.03179,	Adjusted R-squared:  0.01755 
## F-statistic: 2.233 on 3 and 204 DF,  p-value: 0.08554
## 
## [1] "Point -75.5 6.5"
## [1] "N = 415"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1745.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1746.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -35.468 -16.440  -1.161  12.225  97.270 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.607e+01  7.260e+00   7.724 8.71e-14 ***
## el           3.506e-02  8.057e-03   4.352 1.71e-05 ***
## sl          -2.183e-02  9.803e-02  -0.223   0.8239    
## I(el^2)     -3.954e-06  2.196e-06  -1.801   0.0725 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.18 on 411 degrees of freedom
## Multiple R-squared:  0.3005,	Adjusted R-squared:  0.2954 
## F-statistic: 58.85 on 3 and 411 DF,  p-value: < 2.2e-16
## 
## [1] "Point -75.5 7"
## [1] "N = 397"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1747.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1748.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -97.679 -17.115  -0.778  21.745  88.799 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.815e+02  1.027e+01  17.676  < 2e-16 ***
## el          -4.954e-02  1.227e-02  -4.037 6.51e-05 ***
## sl           2.295e-01  1.437e-01   1.597  0.11113    
## I(el^2)      1.056e-05  3.467e-06   3.045  0.00248 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.89 on 393 degrees of freedom
## Multiple R-squared:  0.103,	Adjusted R-squared:  0.09611 
## F-statistic: 15.04 on 3 and 393 DF,  p-value: 2.773e-09
## 
## [1] "Point -75.5 7.5"
## [1] "N = 365"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1749.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1750.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -87.704 -27.287  -6.516  25.076 130.814 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.366e+02  3.935e+00  34.715  < 2e-16 ***
## el           7.668e-02  1.074e-02   7.141 5.15e-12 ***
## sl          -2.079e-01  1.822e-01  -1.141    0.255    
## I(el^2)     -3.007e-05  4.809e-06  -6.253 1.14e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 37.26 on 361 degrees of freedom
## Multiple R-squared:  0.1447,	Adjusted R-squared:  0.1376 
## F-statistic: 20.36 on 3 and 361 DF,  p-value: 3.286e-12
## 
## [1] "Point -75 6"
## [1] "N = 224"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1751.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1752.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -58.181 -26.207  -7.553  23.148 105.957 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.449e+01  7.211e+00   13.10   <2e-16 ***
## el           1.431e-01  1.387e-02   10.32   <2e-16 ***
## sl           3.960e-01  2.713e-01    1.46    0.146    
## I(el^2)     -5.712e-05  5.353e-06  -10.67   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.47 on 220 degrees of freedom
## Multiple R-squared:  0.3944,	Adjusted R-squared:  0.3862 
## F-statistic: 47.76 on 3 and 220 DF,  p-value: < 2.2e-16
## 
## [1] "Point -75 6.5"
## [1] "N = 411"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1753.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1754.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -72.851 -21.630  -9.777  11.132 145.450 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.235e+01  1.015e+01   8.110 6.03e-15 ***
## el           9.312e-02  1.504e-02   6.191 1.46e-09 ***
## sl           1.202e-02  2.193e-01   0.055    0.956    
## I(el^2)     -3.475e-05  5.178e-06  -6.710 6.56e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 35.46 on 407 degrees of freedom
## Multiple R-squared:  0.1067,	Adjusted R-squared:  0.1001 
## F-statistic: 16.21 on 3 and 407 DF,  p-value: 5.722e-10
## 
## [1] "Point -75 7"
## [1] "N = 381"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1755.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1756.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -73.916 -22.390   1.873  16.272  90.983 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.565e+02  6.366e+00  40.289  < 2e-16 ***
## el          -1.871e-01  1.160e-02 -16.135  < 2e-16 ***
## sl           5.473e-01  1.771e-01   3.091  0.00215 ** 
## I(el^2)      5.855e-05  4.984e-06  11.748  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 30.69 on 377 degrees of freedom
## Multiple R-squared:  0.5759,	Adjusted R-squared:  0.5725 
## F-statistic: 170.6 on 3 and 377 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74.5 7"
## [1] "N = 389"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1757.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1758.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -57.75 -23.48 -10.43  13.86 130.55 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.179e+01  6.068e+00  15.127   <2e-16 ***
## el           2.065e-01  2.341e-02   8.820   <2e-16 ***
## sl           6.313e-01  2.781e-01   2.270   0.0238 *  
## I(el^2)     -1.863e-04  2.036e-05  -9.148   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 38.11 on 385 degrees of freedom
## Multiple R-squared:  0.2063,	Adjusted R-squared:  0.2001 
## F-statistic: 33.35 on 3 and 385 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74.5 7.5"
## [1] "N = 425"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1759.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1760.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -140.473  -22.155   -0.219   27.947   95.787 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.386e+02  5.565e+00  42.881  < 2e-16 ***
## el          -6.335e-02  1.655e-02  -3.828 0.000149 ***
## sl          -1.787e-01  2.335e-01  -0.765 0.444586    
## I(el^2)     -1.779e-05  1.156e-05  -1.538 0.124775    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 39.37 on 421 degrees of freedom
## Multiple R-squared:  0.4103,	Adjusted R-squared:  0.4061 
## F-statistic: 97.62 on 3 and 421 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74.5 8"
## [1] "N = 409"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1761.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1762.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -79.620 -30.851   3.559  28.560 111.066 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.852e+02  3.896e+00  47.550  < 2e-16 ***
## el           4.663e-02  1.482e-02   3.147  0.00177 ** 
## sl           1.501e-01  2.202e-01   0.682  0.49571    
## I(el^2)     -8.331e-05  1.071e-05  -7.777 6.23e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 39.36 on 405 degrees of freedom
## Multiple R-squared:  0.3968,	Adjusted R-squared:  0.3923 
## F-statistic:  88.8 on 3 and 405 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74 6"
## [1] "N = 219"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1763.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1764.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -90.395 -28.829  -5.872  20.425 151.363 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.964e+02  5.270e+00  37.264  < 2e-16 ***
## el           7.005e-03  1.615e-02   0.434  0.66491    
## sl           6.275e-01  3.407e-01   1.842  0.06684 .  
## I(el^2)     -1.942e-05  6.536e-06  -2.971  0.00331 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 40.04 on 215 degrees of freedom
## Multiple R-squared:  0.2728,	Adjusted R-squared:  0.2626 
## F-statistic: 26.88 on 3 and 215 DF,  p-value: 8.379e-15
## 
## [1] "Point -74 8"
## [1] "N = 422"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1765.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1766.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -70.130 -22.379   3.854  16.755 126.120 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.571e+01  2.469e+00  22.567   <2e-16 ***
## el           1.513e-01  1.307e-02  11.575   <2e-16 ***
## sl           4.807e-01  2.066e-01   2.327   0.0205 *  
## I(el^2)     -9.333e-05  9.668e-06  -9.653   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.29 on 418 degrees of freedom
## Multiple R-squared:  0.4127,	Adjusted R-squared:  0.4085 
## F-statistic: 97.91 on 3 and 418 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74 10.5"
## [1] "N = 405"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1767.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1768.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -16.4390  -4.3983   0.7527   4.5743  13.5684 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.411e+01  4.614e-01  52.249   <2e-16 ***
## el           4.301e-02  1.039e-03  41.392   <2e-16 ***
## sl          -6.849e-02  3.547e-02  -1.931   0.0542 .  
## I(el^2)     -5.660e-06  2.357e-07 -24.019   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.039 on 401 degrees of freedom
## Multiple R-squared:  0.9522,	Adjusted R-squared:  0.9518 
## F-statistic:  2662 on 3 and 401 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74 11"
## [1] "N = 345"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1769.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1770.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -27.305 -13.749  -3.687   4.136  85.082 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.292e+01  1.946e+00  16.919  < 2e-16 ***
## el           2.922e-02  3.512e-03   8.322 2.11e-15 ***
## sl           3.989e-01  1.126e-01   3.544 0.000449 ***
## I(el^2)     -3.677e-06  7.795e-07  -4.718 3.48e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 21.2 on 341 degrees of freedom
## Multiple R-squared:  0.5831,	Adjusted R-squared:  0.5794 
## F-statistic:   159 on 3 and 341 DF,  p-value: < 2.2e-16
## 
## [1] "Point -74 11.5"
## [1] "N = 87"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1771.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1772.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -47.709 -17.406  -0.822  16.761  59.096 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.890e+01  5.660e+00  10.407   <2e-16 ***
## el           1.634e-02  1.890e-02   0.865    0.390    
## sl           2.014e-01  3.148e-01   0.640    0.524    
## I(el^2)     -5.015e-06  9.959e-06  -0.504    0.616    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29.25 on 83 degrees of freedom
## Multiple R-squared:  0.04329,	Adjusted R-squared:  0.008709 
## F-statistic: 1.252 on 3 and 83 DF,  p-value: 0.2963
## 
## [1] "Point -74 18"
## [1] "N = 87"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1773.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1774.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -41.154 -11.849  -2.447   9.376  50.266 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.015e+02  3.451e+00  29.423   <2e-16 ***
## el          -5.860e-02  1.718e-02  -3.410    0.001 ** 
## sl          -1.290e-01  2.309e-01  -0.559    0.578    
## I(el^2)      1.310e-05  1.193e-05   1.098    0.275    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 17.76 on 83 degrees of freedom
## Multiple R-squared:  0.5458,	Adjusted R-squared:  0.5294 
## F-statistic: 33.24 on 3 and 83 DF,  p-value: 3.27e-14
## 
## [1] "Point -73.5 6"
## [1] "N = 202"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1775.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1776.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -73.80 -16.73  -2.27  18.55  73.13 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.518e+02  9.306e+00  16.311  < 2e-16 ***
## el           4.540e-02  1.196e-02   3.796 0.000195 ***
## sl          -4.272e-02  2.099e-01  -0.204 0.838918    
## I(el^2)     -2.166e-05  3.839e-06  -5.644  5.7e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 26.64 on 198 degrees of freedom
## Multiple R-squared:  0.2602,	Adjusted R-squared:  0.249 
## F-statistic: 23.21 on 3 and 198 DF,  p-value: 6.458e-13
## 
## [1] "Point -73.5 6.5"
## [1] "N = 392"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1777.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1778.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -94.154 -32.163  -1.099  35.126 107.224 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.443e+02  6.660e+00  21.675  < 2e-16 ***
## el          -2.949e-02  1.136e-02  -2.597  0.00976 ** 
## sl           3.135e-01  2.526e-01   1.241  0.21527    
## I(el^2)      8.765e-06  4.758e-06   1.842  0.06620 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 44.31 on 388 degrees of freedom
## Multiple R-squared:  0.02829,	Adjusted R-squared:  0.02077 
## F-statistic: 3.765 on 3 and 388 DF,  p-value: 0.01096
## 
## [1] "Point -73.5 7"
## [1] "N = 374"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1779.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1780.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -50.43 -11.91   1.19  11.01  53.32 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.017e+02  1.796e+00  56.604  < 2e-16 ***
## el          -4.348e-02  4.720e-03  -9.213  < 2e-16 ***
## sl           4.481e-01  1.220e-01   3.674 0.000274 ***
## I(el^2)      1.484e-05  2.170e-06   6.837 3.35e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.11 on 370 degrees of freedom
## Multiple R-squared:  0.2136,	Adjusted R-squared:  0.2072 
## F-statistic:  33.5 on 3 and 370 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73.5 8"
## [1] "N = 446"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1781.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1782.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -55.988 -18.018   2.113  13.703  71.921 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.098e+01  1.877e+00  37.825  < 2e-16 ***
## el          -2.314e-02  6.356e-03  -3.640 0.000305 ***
## sl           5.284e-01  1.625e-01   3.253 0.001231 ** 
## I(el^2)      3.033e-06  2.726e-06   1.113 0.266367    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.76 on 442 degrees of freedom
## Multiple R-squared:  0.1295,	Adjusted R-squared:  0.1236 
## F-statistic: 21.92 on 3 and 442 DF,  p-value: 2.993e-13
## 
## [1] "Point -73.5 8.5"
## [1] "N = 439"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1783.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1784.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -89.137 -14.117  -6.519  11.185  99.982 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.666e+01  2.575e+00  10.353  < 2e-16 ***
## el           1.016e-01  8.281e-03  12.273  < 2e-16 ***
## sl           7.415e-01  1.614e-01   4.594 5.69e-06 ***
## I(el^2)     -4.929e-05  4.300e-06 -11.464  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 30.21 on 435 degrees of freedom
## Multiple R-squared:  0.4312,	Adjusted R-squared:  0.4273 
## F-statistic: 109.9 on 3 and 435 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73.5 9"
## [1] "N = 406"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1785.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1786.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -49.019 -14.878  -1.074   9.583 119.846 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.005e+01  1.944e+00  25.743   <2e-16 ***
## el           9.895e-02  6.398e-03  15.467   <2e-16 ***
## sl           3.079e-01  1.267e-01   2.431   0.0155 *  
## I(el^2)     -3.934e-05  3.249e-06 -12.109   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 23.21 on 402 degrees of freedom
## Multiple R-squared:  0.5756,	Adjusted R-squared:  0.5725 
## F-statistic: 181.8 on 3 and 402 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73.5 10.5"
## [1] "N = 400"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1787.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1788.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -18.098  -5.069  -0.058   4.967  19.264 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.803e+01  6.404e-01   28.16  < 2e-16 ***
## el           4.840e-02  1.060e-03   45.67  < 2e-16 ***
## sl          -1.142e-01  3.709e-02   -3.08  0.00222 ** 
## I(el^2)     -6.561e-06  2.303e-07  -28.49  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.609 on 396 degrees of freedom
## Multiple R-squared:  0.9538,	Adjusted R-squared:  0.9535 
## F-statistic:  2725 on 3 and 396 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73.5 11"
## [1] "N = 374"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1789.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1790.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -45.765  -7.363   0.962   8.776  56.459 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 6.154e+01  2.032e+00  30.281  < 2e-16 ***
## el          7.605e-03  2.733e-03   2.783  0.00567 ** 
## sl          9.577e-02  9.062e-02   1.057  0.29123    
## I(el^2)     5.740e-07  5.959e-07   0.963  0.33602    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 17.74 on 370 degrees of freedom
## Multiple R-squared:  0.3885,	Adjusted R-squared:  0.3835 
## F-statistic: 78.35 on 3 and 370 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 6"
## [1] "N = 100"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1791.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1792.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -73.03 -17.77   5.78  18.57  67.22 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.478e+01  1.356e+01   6.252 1.12e-08 ***
## el           1.287e-01  1.501e-02   8.577 1.68e-13 ***
## sl          -7.080e-01  3.717e-01  -1.905   0.0598 .  
## I(el^2)     -4.220e-05  4.439e-06  -9.508 1.69e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27.8 on 96 degrees of freedom
## Multiple R-squared:  0.5221,	Adjusted R-squared:  0.5072 
## F-statistic: 34.96 on 3 and 96 DF,  p-value: 2.332e-15
## 
## [1] "Point -73 6.5"
## [1] "N = 184"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1793.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1794.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -65.883 -39.215  -9.075  30.725 121.789 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  3.533e+01  1.691e+01   2.089  0.03812 * 
## el           5.809e-02  2.122e-02   2.737  0.00682 **
## sl          -5.147e-01  3.623e-01  -1.421  0.15716   
## I(el^2)     -5.080e-06  7.302e-06  -0.696  0.48756   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 45.89 on 180 degrees of freedom
## Multiple R-squared:  0.187,	Adjusted R-squared:  0.1734 
## F-statistic:  13.8 on 3 and 180 DF,  p-value: 3.86e-08
## 
## [1] "Point -73 7"
## [1] "N = 216"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1795.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1796.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -51.331 -14.834   0.181  16.001  40.162 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.633e+01  4.841e+00  17.833  < 2e-16 ***
## el          -2.767e-02  7.717e-03  -3.586 0.000417 ***
## sl           3.121e-01  1.370e-01   2.278 0.023724 *  
## I(el^2)      8.342e-06  2.883e-06   2.893 0.004212 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.78 on 212 degrees of freedom
## Multiple R-squared:  0.07843,	Adjusted R-squared:  0.06539 
## F-statistic: 6.014 on 3 and 212 DF,  p-value: 0.0005972
## 
## [1] "Point -73 7.5"
## [1] "N = 206"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1797.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1798.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -61.153 -10.800   2.118  11.355  49.127 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.022e+02  3.281e+00  31.137  < 2e-16 ***
## el          -2.076e-02  6.188e-03  -3.355 0.000949 ***
## sl           5.767e-01  1.377e-01   4.188  4.2e-05 ***
## I(el^2)     -1.617e-06  2.038e-06  -0.794 0.428265    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.42 on 202 degrees of freedom
## Multiple R-squared:  0.5106,	Adjusted R-squared:  0.5034 
## F-statistic: 70.26 on 3 and 202 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 8"
## [1] "N = 215"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1799.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1800.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -74.323 -19.241  -0.768  18.282  67.895 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.203e+02  5.751e+00  20.924  < 2e-16 ***
## el          -6.936e-02  8.503e-03  -8.158 3.04e-14 ***
## sl           7.222e-01  1.765e-01   4.092 6.08e-05 ***
## I(el^2)      1.348e-05  2.704e-06   4.983 1.30e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27.52 on 211 degrees of freedom
## Multiple R-squared:  0.4126,	Adjusted R-squared:  0.4043 
## F-statistic: 49.41 on 3 and 211 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 8.5"
## [1] "N = 218"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1801.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1802.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -113.069  -16.489    1.845   20.639   66.845 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.761e+02  7.072e+00  24.896  < 2e-16 ***
## el          -7.092e-02  1.535e-02  -4.619 6.65e-06 ***
## sl           4.543e-01  2.015e-01   2.255   0.0251 *  
## I(el^2)      7.338e-07  7.061e-06   0.104   0.9173    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 30.68 on 214 degrees of freedom
## Multiple R-squared:  0.5685,	Adjusted R-squared:  0.5624 
## F-statistic: 93.98 on 3 and 214 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 9"
## [1] "N = 214"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1803.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1804.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -58.581 -15.405  -0.517  16.517  45.256 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.787e+02  4.635e+00  38.551  < 2e-16 ***
## el          -6.068e-02  1.020e-02  -5.950 1.11e-08 ***
## sl          -2.839e-01  1.429e-01  -1.987 0.048183 *  
## I(el^2)      1.667e-05  4.859e-06   3.432 0.000722 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 22.64 on 210 degrees of freedom
## Multiple R-squared:  0.3554,	Adjusted R-squared:  0.3462 
## F-statistic:  38.6 on 3 and 210 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 9.5"
## [1] "N = 202"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1805.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1806.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -41.46 -21.82  -4.20  17.94 102.78 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.523e+01  3.926e+00  19.161  < 2e-16 ***
## el           7.223e-02  9.938e-03   7.268 8.22e-12 ***
## sl           1.902e-01  1.797e-01   1.059    0.291    
## I(el^2)     -2.635e-05  4.233e-06  -6.225 2.82e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 26.57 on 198 degrees of freedom
## Multiple R-squared:  0.3223,	Adjusted R-squared:  0.3121 
## F-statistic: 31.39 on 3 and 198 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 10"
## [1] "N = 232"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1807.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1808.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -37.346 -15.861  -5.606   9.882  96.660 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.134e+01  2.585e+00  15.993  < 2e-16 ***
## el           5.922e-02  8.632e-03   6.861 6.38e-11 ***
## sl          -2.940e-01  1.867e-01  -1.575  0.11673    
## I(el^2)     -1.056e-05  3.652e-06  -2.891  0.00422 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 22.97 on 228 degrees of freedom
## Multiple R-squared:  0.5192,	Adjusted R-squared:  0.5129 
## F-statistic: 82.07 on 3 and 228 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 10.5"
## [1] "N = 228"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1809.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1810.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -25.348  -8.960  -3.765   3.838  75.249 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 2.875e+01  2.010e+00  14.306  < 2e-16 ***
## el          1.234e-02  6.860e-03   1.798 0.073471 .  
## sl          2.203e-03  1.372e-01   0.016 0.987198    
## I(el^2)     1.123e-05  2.901e-06   3.872 0.000142 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.57 on 224 degrees of freedom
## Multiple R-squared:  0.6392,	Adjusted R-squared:  0.6344 
## F-statistic: 132.3 on 3 and 224 DF,  p-value: < 2.2e-16
## 
## [1] "Point -73 11"
## [1] "N = 202"
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1811.png) 

```
## [1] "Elevation + slope 2-nd order"
## 
## Call:
## lm(formula = y ~ el + sl + I(el^2))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -26.831 -12.469  -0.267   8.068  41.032 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 3.559e+01  1.589e+00  22.395  < 2e-16 ***
## el          1.545e-02  4.440e-03   3.479 0.000619 ***
## sl          1.270e-01  1.367e-01   0.929 0.354009    
## I(el^2)     1.648e-06  1.288e-06   1.279 0.202296    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.17 on 198 degrees of freedom
## Multiple R-squared:  0.6131,	Adjusted R-squared:  0.6073 
## F-statistic: 104.6 on 3 and 198 DF,  p-value: < 2.2e-16
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1812.png) 


SDM
========================================================


```r
library(rgdal)
```

```
## Warning: package 'rgdal' was built under R version 3.1.2
```

```
## Loading required package: sp
```

```
## Warning: package 'sp' was built under R version 3.1.3
```

```
## rgdal: version: 0.9-1, (SVN revision 518)
## Geospatial Data Abstraction Library extensions to R successfully loaded
## Loaded GDAL runtime: GDAL 1.11.1, released 2014/09/24
## Path to GDAL shared files: C:/Users/Viacheslav/Documents/R/win-library/3.1/rgdal/gdal
## GDAL does not use iconv for recoding strings.
## Loaded PROJ.4 runtime: Rel. 4.8.0, 6 March 2012, [PJ_VERSION: 480]
## Path to PROJ.4 shared files: C:/Users/Viacheslav/Documents/R/win-library/3.1/rgdal/proj
```

```r
library(maptools)
```

```
## Warning: package 'maptools' was built under R version 3.1.3
```

```
## Checking rgeos availability: TRUE
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
my_factor.as.numeric <- function (f) { as.numeric(levels(f))[f] }
```

Cargar variables


```r
my_path <- 'C:\\Users\\Viacheslav\\Google Drive\\Projects_actual\\geo_BioclimInterpolation_Occidente\\Bioclim_a100\\'

b <- stack(paste(my_path,"biovars_primnev_modis_a300.tif",sep=""))
b1 <- stack(paste(my_path,"biovars_primavera_modis_a100.tif",sep=""))
b2 <- stack(paste(my_path,"biovars_nevado_modis_a100.tif",sep=""))

names(b) <- c(c(paste('b',1:19,sep='')),'bosque')
names(b1) <- c(c(paste('b',1:19,sep='')),'bosque')
names(b2) <- c(c(paste('b',1:19,sep='')),'bosque')

# variable 18 presenta artefactos
b <- dropLayer(b,18)
b1 <- dropLayer(b1,18)
b2 <- dropLayer(b2,18)

b
```

```
## class       : RasterStack 
## dimensions  : 710, 818, 580780, 19  (nrow, ncol, ncell, nlayers)
## resolution  : 0.002826, 0.002826  (x, y)
## extent      : -104.4701, -102.1584, 19.1279, 21.13436  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
## names       :            b1,            b2,            b3,            b4,            b5,            b6,            b7,            b8,            b9,           b10,           b11,           b12,           b13,           b14,           b15, ... 
## min values  :     4.4052229,    10.9750853,    58.2798920,   134.2431641,    13.6920795,    -4.4778304,    15.8833466,     5.4771938,     3.8136790,     6.4285603,     2.0272667,   582.9362793,   126.9471054,     0.5586119,    83.5632095, ... 
## max values  :      29.09713,      18.95131,      73.55495,     304.98874,      40.61411,      18.31081,      29.00008,      30.18755,      29.25659,      31.52076,      26.68643,    1673.64124,     427.13290,      15.02741,     127.91476, ...
```

```r
summary(b)
```

```
## Warning in .local(object, ...): summary is an estimate based on a sample of 1e+05 cells (17.22% of all cells)
```

```
##                b1       b2       b3       b4       b5        b6       b7
## Min.     5.010379 10.99543 58.27989 135.9685 14.17943 -3.800337 15.90026
## 1st Qu. 17.821900 14.65207 63.42052 189.0876 29.70666  4.885640 22.35699
## Median  19.508052 15.25959 64.33964 228.3731 31.38538  6.979039 23.85974
## 3rd Qu. 21.243605 16.23388 65.55941 253.8704 32.96555  9.051965 25.48901
## Max.    29.097128 18.95131 73.48251 304.9503 40.57313 18.310806 29.00008
## NA's     0.000000  0.00000  0.00000   0.0000  0.00000  0.000000  0.00000
##                b8        b9       b10      b11       b12      b13
## Min.     6.041408  4.409182  6.984944  2.70340  586.7786 129.5736
## 1st Qu. 19.754200 17.099059 20.419888 14.51491  828.2810 199.8797
## Median  21.391521 18.822578 22.013971 16.31789  899.8633 223.3840
## 3rd Qu. 22.870901 20.432270 23.566324 18.35339 1006.1586 250.7695
## Max.    30.136705 29.216553 31.485538 26.68643 1671.4352 424.9762
## NA's     0.000000  0.000000  0.000000  0.00000    0.0000   0.0000
##                b14       b15       b16       b17       b19 bosque
## Min.     0.5637149  83.69952  344.1025  3.845378  15.92943  -1999
## 1st Qu.  3.7251334 104.31909  531.6743 17.139454  34.15264   3422
## Median   4.9581990 110.73201  586.1349 20.646889  39.51943   4498
## 3rd Qu.  5.9768168 114.66469  654.5441 23.919662  48.54155   6097
## Max.    14.9223719 127.86947 1146.4808 52.440823 101.12070  10000
## NA's     0.0000000   0.00000    0.0000  0.000000   0.00000      0
```

```r
mask_reclass_table <- matrix(c(-Inf, Inf, 1), ncol=3, byrow=TRUE)
b_mask <- reclassify(subset(b,1),mask_reclass_table)
```


```r
plot(b,c(1:9), nc=3)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
plot(b,c(10:18), nc=3)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-2.png) 


```r
par(mfrow=c(1, 3))
plot(b,c(19))
#plot(b,c(20))
plot(b_mask)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Cargando puntos de presencia


```r
my_specie <- 'Abies flinckii'
my_specie_file <- 'Abies_flinckii'
my_method <- 'MAXENT'

obs_points <- read.csv("Abies_flinckii_para_modelo.csv")

obs_points
```

```
##            Specie        X       Y
## 1  Abies_flinckii -102.270 19.3630
## 2  Abies_flinckii -104.293 19.5933
## 3  Abies_flinckii -104.267 19.5735
## 4  Abies_flinckii -104.151 19.5599
## 5  Abies_flinckii -104.214 19.5786
## 6  Abies_flinckii -104.254 19.5664
## 7  Abies_flinckii -104.184 19.5946
## 8  Abies_flinckii -104.229 19.5145
## 9  Abies_flinckii -104.226 19.5335
## 10 Abies_flinckii -104.203 19.5814
## 11 Abies_flinckii -104.227 19.5614
## 12 Abies_flinckii -104.256 19.5756
## 13 Abies_flinckii -104.250 19.5841
## 14 Abies_flinckii -104.186 19.5481
## 15 Abies_flinckii -104.237 19.5578
## 16 Abies_flinckii -104.221 19.5499
## 17 Abies_flinckii -104.216 19.5674
## 18 Abies_flinckii -103.635 19.6176
## 19 Abies_flinckii -103.949 19.4674
## 20 Abies_flinckii -103.561 19.5888
## 21 Abies_flinckii -103.563 19.5570
## 22 Abies_flinckii -103.565 19.5519
## 23 Abies_flinckii -103.714 19.5473
## 24 Abies_flinckii -103.604 19.6349
## 25 Abies_flinckii -103.570 19.5972
## 26 Abies_flinckii -103.629 19.6223
## 27 Abies_flinckii -103.584 19.6172
## 28 Abies_flinckii -103.631 19.6231
## 29 Abies_flinckii -103.634 19.6270
## 30 Abies_flinckii -103.594 19.6218
## 31 Abies_flinckii -103.602 19.6207
## 32 Abies_flinckii -103.623 19.6483
## 33 Abies_flinckii -103.570 19.6070
## 34 Abies_flinckii -103.615 19.6433
## 35 Abies_flinckii -103.602 19.6353
## 36 Abies_flinckii -103.626 19.6088
## 37 Abies_flinckii -103.641 19.6330
## 38 Abies_flinckii -103.634 19.6558
```

```r
obs_points_xy <- data.frame(cbind(obs_points[,2],obs_points[,3]))
colnames(obs_points_xy) <- c('x','y')

data(wrld_simpl)
plot(wrld_simpl, xlim=c(-110,-90), ylim=c(10,30), axes=TRUE, col="lightyellow")

points(obs_points_xy$x, obs_points_xy$y, col="red", cex=0.75)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

Generando puntos aleatorios (fondo)


```r
set.seed(0)
random_bg <- randomPoints(b_mask, 1000)

plot(wrld_simpl, xlim=c(-110,-90), ylim=c(10,30), axes=TRUE, col="lightyellow")
points(random_bg, cex=0.2, col="blue")
points(obs_points_xy$x, obs_points_xy$y, col="red", cex=0.75)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
pb <- c(rep(1, nrow(obs_points_xy)), rep(0, nrow(random_bg)))

all_points_xy <- data.frame(cbind(pb, rbind(obs_points_xy, random_bg)))
#dim(all_points_xy)
```

Muestreo de variables


```r
all_vals <- extract(b, all_points_xy[,2:3])

all_vals_xy <- data.frame(cbind(all_points_xy, all_vals))
all_vals_xy <- na.omit(all_vals_xy)

pairs(all_vals_xy[all_vals_xy$pb==1,4:22], cex=0.1, fig=TRUE)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

Separación de "trainset" y "testset"


```r
train_test <- sample(nrow(all_vals_xy), round(0.75 * nrow(all_vals_xy)))
traindata <- all_vals_xy[train_test,]
paste('Número de puntos de entrenamiento:',nrow(traindata[traindata$pb==1,]),sep=' ')
```

```
## [1] "Número de puntos de entrenamiento: 27"
```

```r
#traindata[traindata$pb==1,]
testdata <- all_vals_xy[-train_test,]
paste('Número de puntos de control:',nrow(traindata[testdata$pb==1,]),sep=' ')
```

```
## [1] "Número de puntos de control: 33"
```

```r
#testdata[testdata$pb==1,]
```

Modelación


```r
jar <- paste(system.file(package='dismo'), '/java/maxent.jar', sep='')
if (file.exists(jar)) {
  #bc_model <- maxent(b, obs_points_xy, factors='bosque')
  bc_model <- maxent(b, obs_points_xy)  
  
  str(bc_model)  
  plot(bc_model)
  response(bc_model)
  
} else {
  print('maxent.jar no disponible')
}
```

```
## Loading required package: rJava
```

```
## Warning: package 'rJava' was built under R version 3.1.2
```

```
## Formal class 'MaxEnt' [package "dismo"] with 7 slots
##   ..@ lambdas   : chr [1:66] "b1, 0.0, 5.63145065307617, 28.9682922363281" "b10, 0.0, 7.56752443313599, 31.4578266143799" "b11, 0.0, 3.37821912765503, 26.4789752960205" "b12, 0.0, 588.780883789062, 1646.80590820312" ...
##   ..@ results   : num [1:82, 1] 38 3.677 4.075 500 0.994 ...
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr [1:82] "X.Training.samples" "Regularized.training.gain" "Unregularized.training.gain" "Iterations" ...
##   .. .. ..$ : NULL
##   ..@ path      : chr "C:/Users/VIACHE~1/AppData/Local/Temp/R_raster_Viacheslav/maxent/10947034534"
##   ..@ html      : chr "C:/Users/VIACHE~1/AppData/Local/Temp/R_raster_Viacheslav/maxent/10947034534/maxent.html"
##   ..@ presence  :'data.frame':	38 obs. of  19 variables:
##   .. ..$ b1    : num [1:38] 14.6 16.7 18.5 12.8 18.2 ...
##   .. ..$ b2    : num [1:38] 14.3 13.2 13.3 12 13.3 ...
##   .. ..$ b3    : num [1:38] 65.8 65.9 66.7 62.7 66.4 ...
##   .. ..$ b4    : num [1:38] 187 156 152 180 154 ...
##   .. ..$ b5    : num [1:38] 25.3 26.5 28.1 22.3 27.8 ...
##   .. ..$ b6    : num [1:38] 3.51 6.37 8.12 3.16 7.87 ...
##   .. ..$ b7    : num [1:38] 21.8 20.1 20 19.1 20 ...
##   .. ..$ b8    : num [1:38] 15.7 17.7 19.4 13.9 19.2 ...
##   .. ..$ b9    : num [1:38] 14 15.9 17.6 12 17.4 ...
##   .. ..$ b10   : num [1:38] 16.6 18.5 20.2 14.8 19.9 ...
##   .. ..$ b11   : num [1:38] 12 14.6 16.4 10.4 16.1 ...
##   .. ..$ b12   : num [1:38] 1170 1300 1370 1241 1303 ...
##   .. ..$ b13   : num [1:38] 255 287 315 250 294 ...
##   .. ..$ b14   : num [1:38] 3.47 5.43 4.97 8.45 5.48 ...
##   .. ..$ b15   : num [1:38] 105.5 103.5 108.2 96.8 106 ...
##   .. ..$ b16   : num [1:38] 728 816 893 736 835 ...
##   .. ..$ b17   : num [1:38] 21.3 24.9 24.8 30.1 25 ...
##   .. ..$ b19   : num [1:38] 34.6 72.9 68.7 75.8 67.9 ...
##   .. ..$ bosque: num [1:38] 6661 8471 8363 8084 8280 ...
##   ..@ absence   :'data.frame':	10000 obs. of  19 variables:
##   .. ..$ b1    : num [1:10000] 22.9 16.3 21.3 25.4 21.1 ...
##   .. ..$ b2    : num [1:10000] 14.4 14.2 16.3 13.9 16.9 ...
##   .. ..$ b3    : num [1:10000] 66.3 63.6 63.8 71.6 63.7 ...
##   .. ..$ b4    : num [1:10000] 162 216 248 146 252 ...
##   .. ..$ b5    : num [1:10000] 34 27.8 33.7 34.6 34.2 ...
##   .. ..$ b6    : num [1:10000] 12.37 5.49 8.18 15.18 7.66 ...
##   .. ..$ b7    : num [1:10000] 21.6 22.3 25.5 19.4 26.5 ...
##   .. ..$ b8    : num [1:10000] 23 18.2 23.4 26.4 23.1 ...
##   .. ..$ b9    : num [1:10000] 23.2 15.7 20.3 24.4 20.6 ...
##   .. ..$ b10   : num [1:10000] 25.1 18.8 23.7 27.1 23.8 ...
##   .. ..$ b11   : num [1:10000] 20.9 13.5 17.8 23.5 17.7 ...
##   .. ..$ b12   : num [1:10000] 1080 1013 885 922 923 ...
##   .. ..$ b13   : num [1:10000] 293 242 216 223 242 ...
##   .. ..$ b14   : num [1:10000] 2.15 6.91 6.24 2.78 3.02 ...
##   .. ..$ b15   : num [1:10000] 118 102 108 115 119 ...
##   .. ..$ b16   : num [1:10000] 724 621 563 640 645 ...
##   .. ..$ b17   : num [1:10000] 7.29 29.69 22.53 11.96 16.5 ...
##   .. ..$ b19   : num [1:10000] 21.2 59.3 47.3 43 31.1 ...
##   .. ..$ bosque: num [1:10000] 4277 4168 3247 6191 3704 ...
##   ..@ hasabsence: logi TRUE
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-2.png) 

Predicción


```r
  pb1 <- predict(b1, bc_model, progress='window')
```

```
## Loading required package: tcltk
```

```r
  pb1
```

```
## class       : RasterLayer 
## dimensions  : 278, 253, 70334  (nrow, ncol, ncell)
## resolution  : 0.002413, 0.002413  (x, y)
## extent      : -103.8431, -103.2326, 20.25888, 20.92969  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
## data source : in memory
## names       : layer 
## values      : 4.167482e-11, 0.2714106  (min, max)
```

```r
  plot(pb1, main=paste('Modelo',my_method,'Primavera:',my_specie,sep=' '))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
  pb2 <- predict(b2, bc_model, progress='window')
  pb2
```

```
## class       : RasterLayer 
## dimensions  : 223, 203, 45269  (nrow, ncol, ncell)
## resolution  : 0.002413, 0.002413  (x, y)
## extent      : -103.8647, -103.3749, 19.32139, 19.85949  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
## data source : in memory
## names       : layer 
## values      : 3.865973e-07, 0.8419801  (min, max)
```

```r
  plot(pb2, main=paste('Modelo',my_method,'Nevado:',my_specie,sep=' '))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-2.png) 



Evaluación


```r
e <- evaluate(testdata[testdata$pb==1,], testdata[testdata$pb==0,], bc_model)
e
```

```
## class          : ModelEvaluation 
## n presences    : 11 
## n absences     : 249 
## AUC            : 0.9978094 
## cor            : 0.8993896 
## max TPR+TNR at : 0.06604085
```

```r
#str(e)

par(mfrow=c(1, 3))

plot(e, 'ROC')
density(e)
boxplot(e, col=c('lightblue','coral'), notch=TRUE)
```

```
## Warning in bxp(structure(list(stats = structure(c(5.81242387553971e-10, :
## some notches went outside hinges ('box'): maybe set notch=FALSE
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

Evaluación parte 2


```r
test_presence1 <- extract(pb1,testdata[testdata$pb==1,2:3])
test_absence1 <- extract(pb1,testdata[testdata$pb==0,2:3])
test_presence2 <- extract(pb2,testdata[testdata$pb==1,2:3])
test_absence2 <- extract(pb2,testdata[testdata$pb==0,2:3])

test_pb1 = c(rep('presence', length(test_presence1)), rep('absence', length(test_absence1)))
test_points1 <- data.frame(cbind(test_pb1, c(test_presence1,test_absence1)))
colnames(test_points1) <- c('obs','prob')
test_points1$obs <- as.factor(test_points1$obs)
test_points1$prob <- my_factor.as.numeric(test_points1$prob)

test_pb2 = c(rep('presence', length(test_presence2)), rep('absence', length(test_absence2)))
test_points2 <- data.frame(cbind(test_pb2, c(test_presence2,test_absence2)))
colnames(test_points2) <- c('obs','prob')
test_points2$obs <- as.factor(test_points2$obs)
test_points2$prob <- my_factor.as.numeric(test_points2$prob)

par(mfrow=c(1, 3))
boxplot(prob~obs, data=test_points1, col=c('lightblue','coral'), notch=TRUE, main='Primavera')
```

```
## Warning in bxp(structure(list(stats = structure(c(6.07466503765863e-08, :
## some notches went outside hinges ('box'): maybe set notch=FALSE
```

```r
boxplot(prob~obs, data=test_points2, col=c('lightblue','coral'), notch=TRUE, main='Nevado')
```

```
## Warning in bxp(structure(list(stats = structure(c(4.01054894609842e-05, :
## some notches went outside hinges ('box'): maybe set notch=FALSE
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

Guardando raster


```r
if (require(rgdal)) {
  rf1 <- writeRaster(pb1, 
                     filename=paste(my_specie_file,'_primavera_',my_method,'_superficie_a100.tif',sep=''), 
                     format='GTiff', overwrite=TRUE)
  rf2 <- writeRaster(pb2, 
                     filename=paste(my_specie_file,'_nevado_',my_method,'_superficie_a100.tif',sep=''), 
                     format='GTiff', overwrite=TRUE)
}
```


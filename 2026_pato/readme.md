## Proyecto de colaboración CUCBA, UdeG
1) Estudio de pato en Jalisco
2) Estudio de ciruela
3) Estudio de otras especies de árboles
4) Estudio de murcielago

### Objetivos: 
1) Desarrollar un flujo de analisis MaxEnt para búsqueda de optimos valores de regulariación
2) Buscar estrategia de seleccion de las funciones de transformación de variables
3) Estuido de otros hiperparametros del modelo
4) Analisis con validación cruzada de los modelos óptiomos

### Capas de datos
1) Variables climaticas Bioclim (19 básicas) y Envirem (16 o 18)
1a) versión CUCBA
1b) versión CHELSA
2) Variables edaficas SoilGrids y FactorK
3) Variables de distancias (distancias euclideanas de los cuerpos de agua)
4) Otros variables topográficas (orientación de laderas, pendiente, rugosidad topográfica)

### Avances
* 2026.03 Se implemento un script base para búsqueda de valores de regularización de los modelos MaxEnt con base en AICc (método grid search de un solo parámetro)
* 2026.03 Se implemento un script base para validación cruzada de modelos maxent con los hiperparámetros previamente establecidos
* 2026.03 Se llevo a cabo prueba de ambos scripts mencionados en puntos anteriores para caso de pato, con dos conjuntos de variables: [script busqueda de regularización con AICc pato primer conjunto de variables](https://github.com/vshalisko/SDM/blob/48e5cfbce5d284cf7ceccec156664e30ea99b975/2026_pato/Modelo_simple_1_MAXENT_Pato_con_AIC.Rmd), [script búsqueda de regularizacion con AICc pato segundo conjunto de variables](https://github.com/vshalisko/SDM/blob/48e5cfbce5d284cf7ceccec156664e30ea99b975/2026_pato/Modelo_simple_3_MAXENT_Pato_con_AIC.Rmd), [script validación cruzada del modelo óptimo pato promer conjunto de variables](https://github.com/vshalisko/SDM/blob/48e5cfbce5d284cf7ceccec156664e30ea99b975/2026_pato/Modelo_CV_1A_MAXENT_Pato.Rmd) 

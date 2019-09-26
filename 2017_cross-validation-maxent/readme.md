Scripts for configuration with cross-validation

integrator.Rmd - principal script de modelado de nicho con valización cruzada y guarda el resultado en el objeto (archivo) iterations_object.rds
renderer.Rmd - un sicript auxiliar que es arrancado de forma automatica por integrator.Rmd para realizar una iteración de valización cruzada (construcción del modelo) 
predictor.Rmd - un script que realiza predicción con base en los modelos almacenados en el objeto iterations_object.rds

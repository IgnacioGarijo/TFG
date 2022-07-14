# TFG
Los jóvenes españoles y la eterna Gran Depresión: radiografía de su situación socioeconómica 12 años después
He subido 3 archivos con códigos de R:
  - El primero es Codigo.R, el principal. En este se encuentra el código de la mayor parte del trabajo, a excepción de los gráficos de cuantiles y de mercado laboral. 
  - El segundo es EPA.R, donde se encuentra todo el trabajo realizado sobre los ficheros de la Encuesta de Población Activa, es decir, del mercado laboral.
  - El último es Cuantiles.R, en el que se encuentra únicamente el código utilizado para realizar los gráficos de los cuantiles. 
  
Decidí hacerlo de esta manera porque el primer archivo estaba quedando demasiado largo (+2100 líneas), y cada vez era más difícil encontrar las cosas a pesar de estar organizado. 

No obstante, como acabo de mencionar, el archivo está organizado en bloques y sub-bloques.

Para correr el código, habría que cargar los datos. Esto se puede hacer de dos maneras. La primera y más sencilla es con el archivo de datos de R (que incluye todos los ficheros menos los de la EPA) que creé. La segunda es cargando archivo a archivo, para lo que habría que descargarlos todos y quitar todos los "#" del principio para construir y cargar correctamente los dataframe. Como el archivo de datos es demasiado grande para Github (29mb y el límite es 25) puedo enviar el archivo por otro medio a quien esté interesado sin ningún problema. 

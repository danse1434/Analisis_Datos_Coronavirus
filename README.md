# Análisis de Datos de Coronavirus Colombia
Este repositorio contiene algunas herramientas para el análisis de datos de la expansión de Covid-19 en Colombia con R.


## Módulos
### Módulo 1 - Análisis comparativo por países de referencia
En el archivo `<script.R>` se encuentran las instrucciones para producir las figuras R1, R2, R3, y R4. Estas corresponden a:

1. **R1**: reporte de casos acumulados COVID-19 con países de referencia a nivel internacional (**A**) y nivel latinoamérica (**B**). Estos gráficos deben ser actualizados de acuerdo al número de días que se lleva desde la crisis. 

2. **R2**: reporte de modelamiento de tiempo de duplicación aparente de acuerdo a reporte de casos nuevos, se marcan algunos países de referencia. Se realiza un ajuste de los datos de días desde el primer reporte (*t*) vs número de casos reportados acumulados (*Casos*), con un modelo de crecimiento exponencial con parametrización con t<sub>1/2</sub> como parámetro:

<a href="https://www.codecogs.com/eqnedit.php?latex=\mathrm{Casos}&space;=&space;\mathrm{exp}\left(&space;\frac{\log{2}&space;\cdot&space;t}{t_{1/2}}&space;\right&space;)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mathrm{Casos}&space;=&space;\mathrm{exp}\left(&space;\frac{\log{2}&space;\cdot&space;t}{t_{1/2}}&space;\right&space;)" title="\mathrm{Casos} = \mathrm{exp}\left( \frac{\log{2} \cdot t}{t_{1/2}} \right )" /></a>

Se marca en rojo el caso de Colombia. La variable en el eje X es el día del primer caso detectado, y en el eje Y se muestra el valor estimado de t<sub>1/2</sub>.

3. **R3**: reporte de curvas epidémicas en países de referencia, con líneas guías de los tiempos de duplicación. Eje X: días desde el primer reporte, y Eje Y: número de casos reportados (escala LOG).

4. **R4**: reporte de curvas epidémicas en países de referencia, con líneas guías de los tiempos de duplicación. Eje X: días desde el  reporte N.° 100, y Eje Y: número de casos reportados (escala LOG).

El script requiere la lectura de un archivo que contiene funciones `<Modulo/funciones.R>`. La lectura de los datos desde la fuente se realiza mediante el script `<Modulo/lectura_datos.R>`. La fuente de datos es el [Repositorio de datos de Coronavirus del Centro de Ciencia de Sistemas e Ingeniería de la Universidad John Hopkins](https://github.com/CSSEGISandData). 

### Módulo 2 - Comparativos de la evolución de la epidemia 
Este módulo está inspirado en la [comparación](https://aatishb.com/covidtrends/) realizada por Aatish Bhatia en colaboración con _Minute of Physics_. En este se generan gráficos de tipo animación con el número de casos (confirmados en los últimos 7 días) vs el número de casos acumulados totales. En el [video](https://www.youtube.com/watch?v=54XLXg4fYsc) se explican como ventajas que: 
* Se utiliza una escala logarítmica, lo cual permite hacer comparaciones más fáciles y se permite la detección sencilla de cambios en el comportamiento de la transmisión. 
* Se enfoca en el cambio y no es los números de forma absoluta, para lo cual utiliza el número de casos (confirmados en los últimos 7 días) como eje Y, ya que permite evaluar mejor el cambio.
* No tiene en cuenta el tiempo, lo que permite detectar tendencias en el crecimiento de la epidemia, y permite conocer cuando existe un crecimiento exponencial (e incontralado). El tiempo se muestra como una tercera dimensión por medio de la animación. 

Como desventajas se explica en el video que: 
* Las escalas logarítmicas distorsionan la magnitud de la pandemia, es decir 1000 vs 10 personas infectadas no tienen tanta diferencia como 100,000 vs 1,000. 
* No presentar el tiempo en el gráfico puede ser confuso para algunas personas.
* Los casos confirmados (e inclusive las muertes) no representan la magnitud de la pandemia, pueden haber muchos más casos no detectados.
* El número de casos confirmados representa un aumento en el número de casos reales y el aumento en el número de pruebas realizadas, lo cual puede variar de forma importante entre países.
* Existe una demora a tener en cuenta en la variable en el eje de las ordenadas. 

Las gráficas corresponden a: 
1. **R5**: reporte de comparativo de crecimiento con casos confirmados.
2. **R6**: reporte de comparativo de crecimiento con muertes confirmadas. 

### Módulo 3 - Comparación de demografía en varias poblaciones 
En este modulo se pretender realizar una comparación de la situación demográfica (mediante pirámides poblacionales por sexo) en países de referencia. Esto teniendo en cuenta que la edad es un factor de riesgo en la severidad del COVID-19; por el momento sólo se cuenta con una comparación con Italia en la región más afectada por la enfermedad. 

### Módulo 4 - Análisis de Caso de Ivermectina 
En este módulo se realiza un análisis de la posible efectividad de este fármaco en el tratamiento de la **infección** por SARS-CoV-II. 

En este estudios se intenta conocer un poco sobre la posible utilidad de este fármaco, desde la simulación. Se emplean tres modelos acoplados: 
* Farmacocinético (PK) se utilizan modelos farmacocinéticos poblacionales descritos por:
   * Duthaler U, Suenderhauf C, Karlsson MO, Hussner J, Meyer zu Schwabedissen H, Krähenbühl S, et al. Population pharmacokinetics of oral ivermectin in venous plasma and dried blood spots in healthy volunteers. Br J Clin Pharmacol. 2019;85(3):626–33. 
  * El-Tahtawy A, Glue P, Andrews EN, Mardekian J, Amsden GW, Knirsch CA. The effect of azithromycin on ivermectin pharmacokinetics - A population pharmacokinetic model analysis. PLoS Negl Trop Dis. 2008;2(5). 
   * Duthaler U, Leisegang R, Karlsson MO, Krähenbühl S, Hammann F. The effect of food on the pharmacokinetics of oral ivermectin. J Antimicrob Chemother. 2020;75(2):438–40. 

Para ello se utiliza el paquete `mlxR` de Inria, que permite realizar el modelamiento a diversos regímenes de dosificación propuesto. 

* Farmacodinámico (PD) se utilizan los datos del estudio original sobre la actividad *in vitro* de ivermectina en células infectadas por virus: 
    * Caly L, Druce JD, Catton MG, Jans DA, Wagstaff KM. The FDA-approved Drug Ivermectin inhibits the replication of SARS-CoV-2 in vitro. Antiviral Res. 2020;104787. 
    
Estos datos son tomados de la imagen del artículo mediante el digitalizador de gráficos [WebPlotDigitizer](https://apps.automeris.io/wpd/), y con ellos se realiza un reajuste a un modelo de tipo Hill inhbitorio, mediante el paquete `<RStan>` a través de una estimación bayesiana. Se realiza el análisis PK-PD suponiendo que el efecto (E) es similar al porcentaje de inhibición, y las concentración en el compartimento 1 son las mismas que el compartimento de efecto. 

* Cinética viral (VK) se utiliza un modelo de cinética viral desarrollado por:
  * Kim KS, Ejima K, Ito Y, Iwanami S, Ohashi H, Koizumi Y, et al. Modelling SARS-CoV-2 Dynamics: Implications for Therapy. medRxiv. 2020;2020.03.23.20040493. 

Este fue obtenido mediante un modelamiento de efectos mixtos no lineales desde datos reportados en la literatura sobre la cinética del virus. Se realiza el acoplamiento del modelo PK-PD con E proporcional a la efectividad del tratamiento.

### Módulo 5 - Análisis de datos en Bogotá

### Módulo 6 - Análisis del interés de terapias 
Se realiza una aproximación del interés de la población en general a posibles tratamientos del Coronavirus teniendo en cuenta las consultas a Google. Se realiza la obtención de los datos mediante el paquete `<gtrendsR>` de R. 

### Módulo 7 - Comparativo de pruebas realizadas en varios países
Se realiza un comparativo de pruebas realizadas en varios países de referencias, de acuerdo a la fecha desde el primer reporte. 

### Módulo 8 -  Análisis de reportes en NZ y USA
Se realiza un análisis de reportes en NZ con las intervenciones adoptadas por este país, se observan los tipos de cuarentenas realizadas. 

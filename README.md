## neighbors_school_choice

Este repositorio investiga cómo los pares cercanos (vecinos) pueden influir en las decisiones de elección de escuela en el sistema de admisión escolar en Chile (años 2020-2022). A partir de datos administrativos centralizados y localización geográfica de cada postulante, se examinan posibles patrones de contagio social (simple o complejo) en la selección de escuelas.

## Estructura

```
neighbors_school_choice/
├─ code/
│  ├─ 01_example.R
│  ├─ 02_example.R
│  └─ ...
├─ data/
│  ├─ example_raw_data.csv
│  ├─ example_dyads_data.rds
│  └─ ...
├─ reports/
│  ├─ 01_reporte.html
│  ├─ 02_reporte.html
│  └─ ...
├─ .gitignore
├─ neighbors_school_choice.Rproj
└─ README.md
```

## Descripción de la Metodología
### Objetivo Principal

Examinar cómo la influencia de vecinos cercanos (cohortes anteriores) puede explicar la elección de escuela, controlando por factores socioeconómicos y de disponibilidad de vacantes.

### Herramientas Utilizadas

- R (≥ 4.0.0)
- Paquetes como `dplyr`, `ggplot2`, `readr`, `sf`, entre otros (ver scripts en la carpeta “code”).

### Pasos Principales

- Recolección y preparación de datos (unificación de datos de postulaciones y datos geográficos).
- Análisis exploratorio de la distribución espacial y socioeconómica de las redes de afiliación de los postulantes.
- Estimación de modelos (logísticos u otros) para medir la probabilidad de elegir la misma escuela que los vecinos.
- Elaboración de reportes y visualizaciones (carpeta “reports/”).


## Cómo Ejecutar los Scripts

1. Clonar repositorio

```{bash}
git clone https://github.com/rcantillan/neighbors_school_choice.git
```
2. Abrir el archivo neighbors_school_choice.Rproj en RStudio (recomendado) o configurar manualmente tu entorno de trabajo.

3. Instalar los paquetes necesarios (ver encabezado de cada script o instalar desde la consola de R):

```{r}
install.packages(c("dplyr", "ggplot2", "readr", "sf"))
```
4. Ejecutar los scripts en la carpeta “code/” en el orden indicado en sus nombres o en las instrucciones dentro de cada script.

## Reportes 

- En la carpeta reports/ encontrarás archivos HTML con resultados y visualizaciones que ilustran:
  - [Neighborhood networks construction](https://rcantillan.github.io/neighbors_school_choice/reports/02_neighborhood_networks.html)
  - [Theoretical framework](https://rcantillan.github.io/neighbors_school_choice/reports/03_complex_contagion.html)
  - [Egocentric / geographic adaptive system](https://rcantillan.github.io/neighbors_school_choice/reports/04_adaptive_system.html)
  
  
## Licencia
Este proyecto se publica bajo la Licencia MIT. Puedes revisar el archivo LICENSE si está disponible o la documentación de GitHub para más detalles.


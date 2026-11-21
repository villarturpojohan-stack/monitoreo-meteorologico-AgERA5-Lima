# Monitoreo meteorológico en Lima con AgERA5 y ERA5

Scripts reproducibles en **R** para extraer y analizar variables meteorológicas en cinco puntos de referencia de Lima, Perú, durante el **27 de septiembre al 27 de octubre de 2026**. Las coordenadas y el periodo pueden modificarse.

> **Importante:** son estimaciones de productos de reanálisis, no mediciones instrumentales tomadas directamente en los puntos.

## Fuentes de datos

| Producto | Variable utilizada | Resolución temporal | Uso en este repositorio |
|---|---|---|---|
| **AgERA5** | Velocidad media del viento a 10 m (`Wind-Speed-10m_Mean-24h`) | Diaria | Extracción de velocidad por estación |
| **AgERA5** | Temperatura del aire a 2 m máxima, media y mínima (`Max-24h`, `Mean-24h`, `Min-24h`) | Diaria | Comparación térmica y amplitud diaria |
| **ERA5** | Componentes `u10` y `v10` del viento a 10 m | Horaria | Velocidad, dirección meteorológica y rosas de viento |

En los archivos utilizados para este ejercicio, la resolución espacial de ERA5 es de **0,25°**. AgERA5 y ERA5 son productos **distintos** y sus series no deben confundirse.

- [Descargar datos AgERA5 (Copernicus Climate Data Store)](https://cds.climate.copernicus.eu/datasets/sis-agrometeorological-indicators)
- [Descargar series horarias ERA5 (Copernicus Climate Data Store)](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-timeseries)
- [Método de cálculo de velocidad y dirección del viento (ECMWF)](https://confluence.ecmwf.int/pages/viewpage.action?pageId=212441203)

## Estructura

```text
monitoreo-meteorologico-AgERA5-Lima/
├── README.md
├── config/
│   └── estaciones.csv          # Coordenadas editables para los 3 scripts
├── scripts/
│   ├── 01_viento_AgERA5.R
│   ├── 02_temperatura_AgERA5.R
│   └── 03_direccion_y_rosas_ERA5.R
├── datos/
│   └── README.md              # Cómo ordenar los NetCDF descargados
└── resultados/
    └── README.md              # Carpeta de exportación automática
```

## 1. Modificar las ubicaciones

Editar el archivo **[`config/estaciones.csv`](config/estaciones.csv)**. Cada fila representa una estación, con su nombre, longitud (`lon`) y latitud (`lat`) en grados decimales WGS84 (EPSG:4326):

```csv
Estacion,lon,lat
EST-01,-77.02420,-12.10547
EST-02,-76.98415,-12.16111
EST-03,-76.997983,-11.999633
EST-04,-76.83272,-12.27681
EST-05,-76.83630,-12.27630
```

**Se pueden agregar, eliminar o modificar estaciones sin cambiar el código.** Todas las estaciones deben encontrarse dentro de la cobertura espacial de los archivos NetCDF descargados. Cambiar las coordenadas **no amplía** el área de los datos: para otra ciudad o zona fuera de la cobertura hace falta descargar nuevos archivos.

## 2. Preparar las descargas NetCDF

Crear estas carpetas dentro del repositorio y copiar allí los archivos `.nc` descargados:

```text
datos/
├── agera5/
│   ├── viento/
│   │   ├── sep/       # 27 a 30 de septiembre (4 archivos)
│   │   └── oct/       # 1 a 27 de octubre (27 archivos)
│   └── temperatura/
│       ├── sep/       # 4 días × 3 variables = 12 archivos
│       └── oct/       # 27 días × 3 variables = 81 archivos
└── era5/
    └── reanalysis-era5-single-levels-timeseries-XXXXX.nc
```

Los `.nc` **no se incluyen** en el repositorio por su tamaño y origen externo. Si se desea utilizar otras carpetas, basta con editar las rutas de entrada al comienzo de cada script.

Para el script horario de ERA5 se descargó **del 27/09 al 28/10/2026 (inclusive)** para disponer de todas las horas del 27 de octubre **en horario de Lima (UTC−5)**. El programa convierte las horas UTC y conserva solo los días del periodo de monitoreo.

## 3. Ejecutar en RStudio

1. Abrir el proyecto/carpeta raíz en RStudio y verificar que `getwd()` apunte a esta carpeta.
2. Instalar los paquetes una vez:

   ```r
   install.packages(c("terra", "ncdf4", "ggplot2"))
   ```

3. Ejecutar cada script independientemente desde la raíz:

   ```r
   source("scripts/01_viento_AgERA5.R")
   source("scripts/02_temperatura_AgERA5.R")
   source("scripts/03_direccion_y_rosas_ERA5.R")
   ```

4. Abrir los archivos CSV y las figuras PNG que aparecerán en `resultados/`.

Las fechas del monitoreo se modifican en las variables `fecha_inicio` y `fecha_fin` de los scripts. **No es necesario tener siempre cinco estaciones ni exactamente 31 días**; las verificaciones se ajustan al periodo y a las ubicaciones configuradas.

## Resultados que produce cada script

| Script | Archivos de salida |
|---|---|
| 01 – Viento AgERA5 | `resultados/Viento_AgERA5_diario.csv` |
| 02 – Temperatura AgERA5 | `resultados/Temperaturas_AgERA5_largo.csv`, `resultados/Temperaturas_AgERA5_comparativa.csv` |
| 03 – Viento ERA5 | `resultados/Viento_ERA5_horario.csv`, cinco rosas de viento (o una por estación configurada), panel conjunto y serie diaria por celda |

La velocidad de ERA5 se calcula como `sqrt(u10^2 + v10^2)`. Su dirección meteorológica se calcula desde las componentes `u10` y `v10`; **indica de dónde sopla el viento**. La temperatura de AgERA5 se convierte de kelvin a grados Celsius restando 273,15.

## Limitaciones del análisis

- El raster ERA5 del ejercicio representa solo **tres celdas espaciales distintas**: EST-01 y EST-03 comparten una celda; EST-04 y EST-05 comparten otra. Es normal que sus series y rosas de viento coincidan.
- Los registros son valores de **reanálisis extraídos en la celda que contiene cada coordenada**. No equivalen a observaciones in situ ni resuelven la variabilidad a escala de una calle.
- AgERA5 entrega indicadores diarios y ERA5 componentes horarias. Aunque ambos proceden de la familia de reanálisis ERA5, **no deben combinarse como si fueran la misma serie**.
- La fecha de ERA5 debe leerse desde los atributos temporales del NetCDF; si `terra` no detecta las horas, el tercer script utiliza `ncdf4` como alternativa.

Proyecto de análisis y visualización meteorológica con fines académicos.

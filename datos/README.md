# Archivos de entrada

En esta carpeta deben colocarse los archivos NetCDF descargados para la región de Lima. Los scripts buscan por defecto:

```text
datos/agera5/viento/sep/            archivos .nc de velocidad media diaria a 10 m
datos/agera5/viento/oct/
datos/agera5/temperatura/sep/      archivos .nc Max-24h, Mean-24h y Min-24h
datos/agera5/temperatura/oct/
datos/era5/                       un NetCDF horario con u10 y v10
```

Las carpetas de septiembre y octubre pueden modificarse al inicio de cada script. Los archivos de datos no se guardan en GitHub.

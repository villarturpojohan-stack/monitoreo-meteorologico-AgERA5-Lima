# Viento HORARIO ERA5: componentes u10, v10, velocidad y direccion.
# Genera CSV, rosas de viento y comparacion diaria por celda.
# Corrige el caso en que terra::time(u) devuelve NA (sin tiempo).

for (paquete in c("terra", "ncdf4", "ggplot2")) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    stop("Instala el paquete faltante: install.packages('", paquete, "')")
  }
}

# ------------------------------------------------------------------
# 1. CONFIGURACION EDITABLE
# ------------------------------------------------------------------
fecha_inicio <- as.Date("2026-09-27")
fecha_fin <- as.Date("2026-10-27")
carpeta_era5 <- "datos/era5"
archivo_estaciones <- "config/estaciones.csv"
carpeta_salida <- "resultados"
zona_horaria <- "America/Lima"

if (fecha_fin < fecha_inicio) stop("La fecha final es anterior a la inicial.")
if (!file.exists(archivo_estaciones)) {
  stop("No encuentro config/estaciones.csv. Abre RStudio en la raiz del repositorio.")
}
if (!dir.exists(carpeta_era5)) {
  stop("No encuentro la carpeta de ERA5: ", carpeta_era5)
}

estaciones <- read.csv(archivo_estaciones, stringsAsFactors = FALSE)
if (!all(c("Estacion", "lon", "lat") %in% names(estaciones)) ||
    nrow(estaciones) < 1 || anyNA(estaciones) ||
    anyDuplicated(estaciones$Estacion) > 0 ||
    !is.numeric(estaciones$lon) || !is.numeric(estaciones$lat) ||
    any(abs(estaciones$lon) > 180) || any(abs(estaciones$lat) > 90)) {
  stop("Revisa config/estaciones.csv: Estacion, lon y lat deben ser validas.")
}

archivos <- list.files(carpeta_era5, pattern = "\\.nc$",
                       full.names = TRUE, ignore.case = TRUE)
if (length(archivos) != 1) {
  stop("Se espera un NetCDF ERA5 con u10 y v10 en ", carpeta_era5,
       ". Archivos encontrados: ", length(archivos))
}

# ------------------------------------------------------------------
# 2. EXTRAER SUBCONJUNTOS U10 Y V10
# ------------------------------------------------------------------
datos <- terra::sds(archivos[1])
indices <- match(c("u10", "v10"), tolower(names(datos)))
if (anyNA(indices)) {
  stop("El NetCDF debe tener subdatasets u10 y v10. Disponibles: ",
       paste(names(datos), collapse = ", "))
}
u <- datos[indices[1]]
v <- datos[indices[2]]
if (terra::nlyr(u) != terra::nlyr(v) ||
    !terra::compareGeom(u, v, stopOnError = FALSE)) {
  stop("u10 y v10 deben tener las mismas capas y geometria espacial.")
}

puntos <- terra::vect(estaciones, geom = c("lon", "lat"), crs = "EPSG:4326")
pts_r <- terra::project(puntos, terra::crs(u))
celdas <- terra::cellFromXY(u, terra::crds(pts_r))
if (anyNA(celdas)) {
  stop("Una o mas estaciones estan fuera de la cobertura ERA5. ",
       "Necesitas descargar datos de la nueva area.")
}
cat("Celdas de ERA5:\n")
print(data.frame(Estacion = estaciones$Estacion, Celda_ERA5 = celdas))

valores_u <- as.matrix(terra::extract(u, pts_r, ID = FALSE))
valores_v <- as.matrix(terra::extract(v, pts_r, ID = FALSE))
if (nrow(valores_u) != nrow(estaciones) ||
    nrow(valores_v) != nrow(estaciones) ||
    ncol(valores_u) != terra::nlyr(u) ||
    ncol(valores_v) != terra::nlyr(v)) {
  stop("Las dimensiones de los valores extraidos no son las esperadas.")
}

# ------------------------------------------------------------------
# 3. RECUPERAR FECHAS UTC DEL NETCDF (TERRA O NCDF4)
# ------------------------------------------------------------------
leer_fechas_netcdf <- function(archivo, ncapas) {
  nc <- ncdf4::nc_open(archivo)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  nombre_tiempo <- intersect(
    c("valid_time", "time"), c(names(nc$dim), names(nc$var))
  )[1]
  if (is.na(nombre_tiempo)) {
    stop("No se encontro una coordenada valid_time/time en el NetCDF.")
  }

  if (nombre_tiempo %in% names(nc$dim)) {
    valores <- nc$dim[[nombre_tiempo]]$vals
    unidades <- nc$dim[[nombre_tiempo]]$units
  } else {
    valores <- ncdf4::ncvar_get(nc, nombre_tiempo)
    unidades <- ncdf4::ncatt_get(nc, nombre_tiempo, "units")$value
  }
  if (length(unidades) != 1 || is.na(unidades) || !nzchar(unidades)) {
    unidades <- ncdf4::ncatt_get(nc, nombre_tiempo, "units")$value
  }
  if (!is.character(unidades) || length(unidades) != 1) {
    stop("El NetCDF no contiene unidades temporales interpretables.")
  }

  partes <- strsplit(trimws(unidades), " since ", fixed = TRUE)[[1]]
  if (length(partes) != 2) {
    stop("Formato temporal inesperado: ", unidades)
  }
  factor_segundos <- switch(
    tolower(partes[1]),
    seconds = 1, second = 1,
    minutes = 60, minute = 60,
    hours = 3600, hour = 3600,
    days = 86400, day = 86400,
    stop("Unidad de tiempo no reconocida: ", partes[1])
  )

  origen_texto <- sub("([0-9])T([0-9])", "\\1 \\2", partes[2])
  origen_texto <- sub(" ?(UTC|Z)$", "", origen_texto, ignore.case = TRUE)
  origen_texto <- sub(" ?\\+00:?00$", "", origen_texto)
  origen <- as.POSIXct(origen_texto, tz = "UTC",
                       tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d"))
  if (is.na(origen)) stop("No se interpreto el origen UTC: ", origen_texto)

  fechas <- origen + as.numeric(valores) * factor_segundos
  if (length(fechas) != ncapas || anyNA(fechas)) {
    stop("Las fechas del NetCDF no coinciden con las capas de u10/v10.")
  }
  fechas
}

fechas_utc <- terra::time(u)
if (!inherits(fechas_utc, "POSIXt") ||
    length(fechas_utc) != terra::nlyr(u) || anyNA(fechas_utc)) {
  message("terra no identifico las horas; leyendo valid_time/time con ncdf4.")
  fechas_utc <- leer_fechas_netcdf(archivos[1], terra::nlyr(u))
}
fechas_utc <- as.POSIXct(fechas_utc, tz = "UTC")
if (any(abs(diff(as.numeric(fechas_utc)) - 3600) > 1)) {
  stop("Las capas del NetCDF no estan espaciadas cada 1 hora UTC.")
}
cat("Primera hora UTC:", format(min(fechas_utc), tz = "UTC"), "\n")
cat("Ultima hora UTC:", format(max(fechas_utc), tz = "UTC"), "\n")

# ------------------------------------------------------------------
# 4. CALCULAR VELOCIDAD, DIRECCION Y HORA LOCAL DE LIMA
# ------------------------------------------------------------------
fecha_lima <- as.Date(format(fechas_utc, "%Y-%m-%d", tz = zona_horaria))
hora_lima <- format(fechas_utc, "%H:%M", tz = zona_horaria)

tabla_era5 <- do.call(rbind, lapply(seq_len(nrow(estaciones)), function(i) {
  uu <- as.numeric(valores_u[i, ])
  vv <- as.numeric(valores_v[i, ])

  velocidad <- sqrt(uu^2 + vv^2)
  # Direccion meteorologica: de donde PROVIENE el viento, 0=N, 90=E.
  direccion <- (atan2(-uu, -vv) * 180 / pi + 360) %% 360
  direccion[velocidad == 0] <- NA_real_

  data.frame(
    Fecha = fecha_lima,
    Hora = hora_lima,
    Estacion = estaciones$Estacion[i],
    Celda_ERA5 = celdas[i],
    u10 = uu,
    v10 = vv,
    Velocidad = velocidad,
    Direccion = direccion,
    stringsAsFactors = FALSE
  )
}))

tabla_era5 <- tabla_era5[
  tabla_era5$Fecha >= fecha_inicio & tabla_era5$Fecha <= fecha_fin, ]
tabla_era5 <- tabla_era5[
  order(tabla_era5$Fecha, tabla_era5$Hora, tabla_era5$Estacion), ]
rownames(tabla_era5) <- NULL

fechas_esperadas <- seq(fecha_inicio, fecha_fin, by = "day")
esperados <- length(fechas_esperadas) * 24 * nrow(estaciones)
if (nrow(tabla_era5) != esperados) {
  stop("Periodo horario incompleto. Esperaba ", esperados,
       " registros para las estaciones configuradas y obtuve ", nrow(tabla_era5),
       ". Descarga ERA5 con horas UTC suficientes para todo el periodo en Lima.")
}
if (anyDuplicated(tabla_era5[c("Fecha", "Hora", "Estacion")]) > 0) {
  stop("Se detectaron fechas/horas repetidas por estacion.")
}
if (anyNA(tabla_era5[c("u10", "v10", "Velocidad")])) {
  warning("Algunos registros horarios tienen datos faltantes.")
}

dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)
salida_csv <- file.path(carpeta_salida, "Viento_ERA5_horario.csv")
write.csv(tabla_era5, salida_csv, row.names = FALSE, fileEncoding = "UTF-8")
cat("ERA5 horario:", nrow(tabla_era5), "registros en", salida_csv, "\n")

# ------------------------------------------------------------------
# 5. ROSA DE VIENTO POR ESTACION (16 DIRECCIONES)
# ------------------------------------------------------------------
sectores <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
              "S", "SSO", "SO", "OSO", "O", "ONO", "NO", "NNO")
rango_etiquetas <- c("0.5-<2", "2-<3", "3-<4", "4-<5", "5-<6", ">=6")
colores <- c("#A3CBD3", "#6AA7B5", "#3D849A",
             "#28657E", "#174B69", "#092D4C")

tablas_rosa <- list()
for (nombre in estaciones$Estacion) {
  sub <- tabla_era5[tabla_era5$Estacion == nombre, ]
  validos <- sub[is.finite(sub$Velocidad), ]
  con_direccion <- validos[
    validos$Velocidad >= 0.5 & is.finite(validos$Direccion), ]

  if (nrow(validos) == 0 || nrow(con_direccion) == 0) {
    warning("No se puede crear rosa de viento para ", nombre)
    next
  }

  indice <- floor(((con_direccion$Direccion + 11.25) %% 360) / 22.5) + 1
  sector_factor <- factor(sectores[indice], levels = sectores)
  clase_factor <- cut(
    con_direccion$Velocidad,
    breaks = c(0.5, 2, 3, 4, 5, 6, Inf),
    labels = rango_etiquetas,
    right = FALSE
  )
  frec <- as.data.frame(table(Direccion = sector_factor, Rango = clase_factor))
  frec$Frecuencia_pct <- 100 * frec$Freq / nrow(validos)
  frec$Estacion <- nombre
  tablas_rosa[[nombre]] <- frec

  rosa <- ggplot2::ggplot(frec,
                          ggplot2::aes(x = Direccion, y = Frecuencia_pct,
                                       fill = Rango)) +
    ggplot2::geom_col(width = 1, color = "white", linewidth = 0.2) +
    ggplot2::coord_polar(start = -pi / 16, direction = 1) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_fill_manual(values = colores, drop = FALSE,
                               name = "Velocidad (m/s)") +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
    ggplot2::labs(
      title = paste("Rosa de viento -", nombre),
      subtitle = paste(format(fecha_inicio), "a", format(fecha_fin),
                       "| ERA5, viento a 10 m"),
      x = NULL, y = "Frecuencia",
      caption = "La direccion indica de donde proviene el viento. Calmas: <0.5 m/s."
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "bottom",
                   plot.title = ggplot2::element_text(face = "bold"))

  nombre_seguro <- gsub("[^A-Za-z0-9_-]", "_", nombre)
  ggplot2::ggsave(
    file.path(carpeta_salida, paste0("rosa_viento_ERA5_", nombre_seguro, ".png")),
    plot = rosa, width = 8, height = 8, dpi = 300, bg = "white"
  )
}

if (length(tablas_rosa)) {
  rosa_panel <- do.call(rbind, tablas_rosa)
  panel <- ggplot2::ggplot(
    rosa_panel,
    ggplot2::aes(x = Direccion, y = Frecuencia_pct, fill = Rango)
  ) +
    ggplot2::geom_col(width = 1, color = "white", linewidth = 0.15) +
    ggplot2::coord_polar(start = -pi / 16, direction = 1) +
    ggplot2::facet_wrap(~ Estacion, ncol = 3) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_fill_manual(values = colores, drop = FALSE,
                               name = "Velocidad (m/s)") +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
    ggplot2::labs(title = "Rosas de viento ERA5 - Lima",
                  subtitle = paste(format(fecha_inicio), "a", format(fecha_fin)),
                  x = NULL, y = "Frecuencia") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave(
    file.path(carpeta_salida, "rosas_viento_ERA5_panel.png"),
    plot = panel, width = 15, height = 10, dpi = 300, bg = "white"
  )
}

# ------------------------------------------------------------------
# 6. COMPARACION DE VELOCIDAD DIARIA POR CELDA (SIN DUPLICADOS)
# ------------------------------------------------------------------
tabla_diaria <- aggregate(
  Velocidad ~ Fecha + Celda_ERA5,
  data = tabla_era5,
  FUN = function(x) mean(x, na.rm = TRUE)
)
names(tabla_diaria)[names(tabla_diaria) == "Velocidad"] <- "Velocidad_media_ms"

estaciones_por_celda <- tapply(estaciones$Estacion, as.character(celdas),
                               paste, collapse = "/")
tabla_diaria$Grupo <- paste0(
  "Celda ", tabla_diaria$Celda_ERA5, " (",
  estaciones_por_celda[as.character(tabla_diaria$Celda_ERA5)], ")"
)
write.csv(tabla_diaria,
          file.path(carpeta_salida, "ERA5_velocidad_media_diaria_celdas.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

lineas <- ggplot2::ggplot(
  tabla_diaria, ggplot2::aes(x = Fecha, y = Velocidad_media_ms,
                             color = Grupo, group = Grupo)
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 1.5) +
  ggplot2::labs(title = "Velocidad media diaria del viento - ERA5",
                x = "Fecha", y = "Velocidad (m/s)",
                color = "Celdas y estaciones") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
  file.path(carpeta_salida, "ERA5_velocidad_media_diaria.png"),
  plot = lineas, width = 12, height = 6, dpi = 300, bg = "white"
)

cat("Rosas de viento generadas:", length(tablas_rosa), "\n")
cat("Celdas ERA5 distintas:", length(unique(celdas)), "\n")
cat("Resultados disponibles en:", carpeta_salida, "\n")

# =========================================================================
# SÍFILIS CONGÉNITA Y SÍFILIS EN EMBARAZADAS
# Limpieza, calidad, paso de multirregistro a una fila por caso
# y clasificación de los casos
# =========================================================================
#
# ÍNDICE
#   0. Librerías
#   1. Importación de los datos
#   2. Exploración inicial
#   3. Limpieza (nombres de columnas, fechas, errores de codificación)
#   4. Completitud de las variables (tabla semáforo)
#   5. Preparar la base multirregistro
#   6. FUNCIÓN: de multirregistro a una fila por caso
#   7. Aplicar la función y verificar
#   8. Clasificación de los casos
#   9. Controles y tabla final de clasificación
#
# LA IDEA
#   - Trabajamos las dos bases EN PARALELO: cada paso se hace primero para
#     congénito y después, con el mismo código, para embarazadas.
#   - Todos los objetos de este script llevan "_sf" (sífilis), así no se
#     pisan con los de los scripts de Chagas (_ch) y VIH (_vih) si se corren
#     en la misma sesión. Después de "_sf" va un sufijo con la base:
#       _sf_cg  -> sífilis congénita
#       _sf_emb -> sífilis en embarazadas
#     Lo que vale para las dos bases lleva solo "_sf" (umbral_verde_sf,
#     tipo_por_determinacion_sf, reordenar_por_caso_sf...). Se define UNA
#     sola vez: si hay que cambiar un criterio, se cambia ahí.
#   - Las pruebas se agrupan por DETERMINACIÓN en treponémicas y no
#     treponémicas (sección 5.4).
#   - El proceso va en línea, siempre hacia adelante:
#
#     pruebas_sf_cg           SifilisCongenito_caso       clas_sf_cg
#     pruebas_sf_emb    ->    SifilisEmbarazadas_caso  ->  clas_sf_emb  ->  tabla final
#     (una fila por           (una fila por caso,          (una fila por
#      prueba)                 un bloque por muestra)       caso, solo la
#                                                           clasificación)
#
#   - La tabla por caso guarda el DATO crudo. La clasificación va en otra
#     tabla, así no se mezcla el dato con la DECISIÓN que tomamos sobre él.
#
# VOCABULARIO QUE USAMOS EN ESTE SCRIPT
#   - Caso          : cada id_evento_caso (una notificación de una persona para
#                     un evento). No es lo mismo que "persona" (nro_doc) ni que
#                     "caso confirmado".
#   - Muestra       : el material que se toma al paciente en una fecha.
#   - Técnica       : el método de laboratorio (VDRL, USR, TP-PA, EIA...).
#   - Determinación : lo que mide la técnica.
#   - Prueba        : una técnica aplicada a una muestra, con su resultado.
#                     En la base original, CADA FILA ES UNA PRUEBA.
# =========================================================================



# -------------------------------------------------------------------------
# 0. Librerías -------------------------------------------------------------
# -------------------------------------------------------------------------

# p_load() carga cada paquete y, si no está instalado, lo instala.
pacman::p_load(
  rio,          # importar datos
  here,         # rutas relativas al proyecto
  tidyverse,    # manejo de datos (dplyr, tidyr, stringr...) y gráficos
  lubridate,    # fechas (ya viene con tidyverse; lo dejamos a la vista)
  skimr,        # resumen completo de una base
  janitor,      # limpiar nombres de columnas
  naniar,       # visualizar datos faltantes
  gt            # tablas con formato
)



# -------------------------------------------------------------------------
# 1. Importación de los datos ----------------------------------------------
# -------------------------------------------------------------------------

# Listamos los archivos de la carpeta "data" cuyo nombre contiene "Sifilis".
#   pattern = "Sifilis" -> solo esos archivos
#   full.names = TRUE   -> con la ruta completa, para poder abrirlos
archivos_sifilis <- list.files(here("data"), pattern = "Sifilis", full.names = TRUE)
archivos_sifilis   # tienen que aparecer los dos archivos

# Importamos todos a la vez. import_list() devuelve una lista con una base
# por archivo, y list2env() saca cada base de la lista al Environment, con
# el nombre del archivo (sin la extensión):
#   SifilisCongenito_curso y SifilisEmbarazadas_curso
# Precaución: los nombres de archivo no deben tener espacios ni empezar con
# un número, porque se usan como nombres de objeto.
import_list(archivos_sifilis, na.strings = c("", "NA")) |>
  list2env(envir = .GlobalEnv)



# -------------------------------------------------------------------------
# 2. Exploración inicial ---------------------------------------------------
# -------------------------------------------------------------------------

# Una mirada general: columnas, tipo de cada una y primeros valores
glimpse(SifilisCongenito_curso)
glimpse(SifilisEmbarazadas_curso)

# Resumen completo de cada variable (faltantes, valores únicos, rangos)
skim(SifilisCongenito_curso)
skim(SifilisEmbarazadas_curso)

# Cantidad de faltantes por variable, en un gráfico
gg_miss_var(SifilisCongenito_curso)
gg_miss_var(SifilisEmbarazadas_curso)



# -------------------------------------------------------------------------
# 3. Limpieza --------------------------------------------------------------
# -------------------------------------------------------------------------

# 3.1 Nombres de columnas -------------------------------------------------

# Los nombres con espacios obligan a escribirlos entre ` `, lo que facilita
# errores. clean_names() los pasa a minúscula, sin tildes y con "_".
# Por ejemplo: "Resultado" -> "resultado", "Determinación" -> "determinacion".

names(SifilisCongenito_curso)

# Diferencia entre IMPRIMIR y GUARDAR:
# esto solo muestra el resultado en la consola, no cambia el objeto...
SifilisCongenito_curso |>
  clean_names() |>
  names()

names(SifilisCongenito_curso)   # ...los nombres siguen igual

# ...para que el cambio quede, hay que asignarlo con <-
SifilisCongenito_curso   <- SifilisCongenito_curso   |> clean_names()
SifilisEmbarazadas_curso <- SifilisEmbarazadas_curso |> clean_names()

names(SifilisCongenito_curso)   # ahora sí cambiaron
names(SifilisEmbarazadas_curso)


# 3.2 Fechas --------------------------------------------------------------

# ¿De qué tipo son las columnas de fecha?
# sapply() aplica class() a cada columna y devuelve el tipo de cada una.
SifilisCongenito_curso   |> select(starts_with("fecha")) |> sapply(class)
SifilisEmbarazadas_curso |> select(starts_with("fecha")) |> sapply(class)

# Convertimos a fecha solo las que son texto (dd/mm/aaaa) con dmy()
# (day-month-year).
#   across()               -> aplica la misma función a varias columnas
#   starts_with("fecha")   -> las columnas cuyo nombre empieza con "fecha"
#   & where(is.character)  -> ...y que además sean texto
# where(is.character) evita pasarle dmy() a una columna que ya es fecha,
# porque eso la dejaría toda en NA.
SifilisCongenito_curso <- SifilisCongenito_curso |>
  mutate(across(starts_with("fecha") & where(is.character), dmy))

SifilisEmbarazadas_curso <- SifilisEmbarazadas_curso |>
  mutate(across(starts_with("fecha") & where(is.character), dmy))

# Verificamos que ahora sean de tipo Date
SifilisCongenito_curso   |> select(starts_with("fecha")) |> sapply(class)
SifilisEmbarazadas_curso |> select(starts_with("fecha")) |> sapply(class)


# 3.3 Errores de codificación de caracteres -------------------------------

# Al importar, algunas letras con tilde quedaron como "¿".
# str_replace_all(texto, "lo que busco", "con qué lo reemplazo")
SifilisCongenito_curso |> count(grupo_etario_diagnostico)   # antes

SifilisCongenito_curso <- SifilisCongenito_curso |>
  mutate(grupo_etario_diagnostico = grupo_etario_diagnostico |>
           str_replace_all("d¿as", "días") |>
           str_replace_all("a¿os", "años"))

SifilisEmbarazadas_curso <- SifilisEmbarazadas_curso |>
  mutate(grupo_etario_diagnostico = grupo_etario_diagnostico |>
           str_replace_all("d¿as", "días") |>
           str_replace_all("a¿os", "años"))

SifilisCongenito_curso |> count(grupo_etario_diagnostico)   # después



# -------------------------------------------------------------------------
# 4. Completitud de las variables (tabla semáforo) -------------------------
# -------------------------------------------------------------------------

# OJO: estas bases son multirregistro (varias filas por caso). Acá la
# completitud se calcula sobre FILAS, así que los casos con más pruebas
# pesan más. Para las variables del caso (edad, sexo...) conviene mirarla
# también en la tabla de una fila por caso (sección 7).

# Umbrales del semáforo (ajustalos a tu criterio). Valen para las dos bases.
umbral_verde_sf    <- 0.95   # >= 95 %  -> verde
umbral_amarillo_sf <- 0.80   # 80-95 %  -> amarillo; < 80 % -> rojo


# 4.1 CONGÉNITO: completitud por variable ---------------------------------
# map_dfr() recorre los nombres de las columnas, aplica la función a cada
# una y junta los resultados en una tabla (una fila por variable).
# Cuentan como faltante: NA, celdas vacías ("") y celdas con solo espacios.
completitud_sf_cg <- map_dfr(names(SifilisCongenito_curso), function(v) {
  x     <- SifilisCongenito_curso[[v]]          # la columna v
  x_chr <- str_trim(as.character(x))            # como texto y sin espacios
  
  n_total  <- length(x)
  n_na     <- sum(is.na(x))
  n_vacios <- sum(!is.na(x_chr) & x_chr == "")
  n_compl  <- n_total - n_na - n_vacios
  
  tibble(
    variable    = v,
    tipo        = class(x)[1],
    n_total     = n_total,
    n_NA        = n_na,
    n_vacios    = n_vacios,
    n_completos = n_compl,
    pct         = n_compl / n_total
  )
}) |>
  mutate(
    estado = case_when(
      pct >= umbral_verde_sf    ~ "🟢 Completo",
      pct >= umbral_amarillo_sf ~ "🟡 Parcial",
      TRUE                      ~ "🔴 Crítico"
    )
  ) |>
  arrange(pct)   # las variables más incompletas arriba


# 4.2 CONGÉNITO: tabla semáforo -------------------------------------------
# gt() convierte la tabla en una tabla con formato. Cada función que sigue
# le agrega algo: título, nombres de columnas, formato de %, colores, nota.
tabla_semaforo_sf_cg <- completitud_sf_cg |>
  gt() |>
  tab_header(
    title    = "Completitud del registro por variable",
    subtitle = paste0("Notificación de Sífilis Congénita (n = ",
                      nrow(SifilisCongenito_curso), " registros)")
  ) |>
  cols_label(
    variable    = "Variable",
    tipo        = "Tipo",
    n_total     = "N total",
    n_NA        = "NA",
    n_vacios    = "Vacíos (\"\")",
    n_completos = "Completos",
    pct         = "% completo",
    estado      = "Estado"
  ) |>
  fmt_percent(columns = pct, decimals = 1) |>
  tab_style(
    style     = list(cell_fill(color = "#C6EFCE"), cell_text(color = "#006100")),
    locations = cells_body(columns = c(pct, estado), rows = pct >= umbral_verde_sf)
  ) |>
  tab_style(
    style     = list(cell_fill(color = "#FFEB9C"), cell_text(color = "#9C5700")),
    locations = cells_body(columns = c(pct, estado),
                           rows = pct >= umbral_amarillo_sf & pct < umbral_verde_sf)
  ) |>
  tab_style(
    style     = list(cell_fill(color = "#FFC7CE"), cell_text(color = "#9C0006")),
    locations = cells_body(columns = c(pct, estado), rows = pct < umbral_amarillo_sf)
  ) |>
  tab_source_note(
    paste0("Criterio: verde ≥ ", umbral_verde_sf * 100, "%, amarillo ≥ ",
           umbral_amarillo_sf * 100, "%, rojo < ", umbral_amarillo_sf * 100,
           "%. Se consideran faltantes NA, celdas vacías y solo espacios.")
  )

tabla_semaforo_sf_cg   # se muestra en el panel Viewer de RStudio


# 4.3 EMBARAZADAS: completitud por variable -------------------------------
# Es el mismo código que en 4.1, cambiando la base y el nombre del resultado.
completitud_sf_emb <- map_dfr(names(SifilisEmbarazadas_curso), function(v) {
  x     <- SifilisEmbarazadas_curso[[v]]
  x_chr <- str_trim(as.character(x))
  
  n_total  <- length(x)
  n_na     <- sum(is.na(x))
  n_vacios <- sum(!is.na(x_chr) & x_chr == "")
  n_compl  <- n_total - n_na - n_vacios
  
  tibble(
    variable    = v,
    tipo        = class(x)[1],
    n_total     = n_total,
    n_NA        = n_na,
    n_vacios    = n_vacios,
    n_completos = n_compl,
    pct         = n_compl / n_total
  )
}) |>
  mutate(
    estado = case_when(
      pct >= umbral_verde_sf    ~ "🟢 Completo",
      pct >= umbral_amarillo_sf ~ "🟡 Parcial",
      TRUE                      ~ "🔴 Crítico"
    )
  ) |>
  arrange(pct)


# 4.4 EMBARAZADAS: tabla semáforo (mismo formato que 4.2) -----------------
tabla_semaforo_sf_emb <- completitud_sf_emb |>
  gt() |>
  tab_header(
    title    = "Completitud del registro por variable",
    subtitle = paste0("Notificación de Sífilis en Embarazadas (n = ",
                      nrow(SifilisEmbarazadas_curso), " registros)")
  ) |>
  cols_label(
    variable    = "Variable",
    tipo        = "Tipo",
    n_total     = "N total",
    n_NA        = "NA",
    n_vacios    = "Vacíos (\"\")",
    n_completos = "Completos",
    pct         = "% completo",
    estado      = "Estado"
  ) |>
  fmt_percent(columns = pct, decimals = 1) |>
  tab_style(
    style     = list(cell_fill(color = "#C6EFCE"), cell_text(color = "#006100")),
    locations = cells_body(columns = c(pct, estado), rows = pct >= umbral_verde_sf)
  ) |>
  tab_style(
    style     = list(cell_fill(color = "#FFEB9C"), cell_text(color = "#9C5700")),
    locations = cells_body(columns = c(pct, estado),
                           rows = pct >= umbral_amarillo_sf & pct < umbral_verde_sf)
  ) |>
  tab_style(
    style     = list(cell_fill(color = "#FFC7CE"), cell_text(color = "#9C0006")),
    locations = cells_body(columns = c(pct, estado), rows = pct < umbral_amarillo_sf)
  ) |>
  tab_source_note(
    paste0("Criterio: verde ≥ ", umbral_verde_sf * 100, "%, amarillo ≥ ",
           umbral_amarillo_sf * 100, "%, rojo < ", umbral_amarillo_sf * 100,
           "%. Se consideran faltantes NA, celdas vacías y solo espacios.")
  )

tabla_semaforo_sf_emb


# 4.5 Exportar (opcional) -------------------------------------------------
# gtsave(tabla_semaforo_sf_cg,  "semaforo_completitud_sifilis_cg.html")
# gtsave(tabla_semaforo_sf_emb, "semaforo_completitud_sifilis_emb.html")
# gtsave(tabla_semaforo_sf_cg,  "semaforo_completitud_sifilis_cg.png")   # requiere webshot2
# writexl::write_xlsx(list(congenito = completitud_sf_cg, embarazadas = completitud_sf_emb),
#                     "completitud_sifilis.xlsx")                          # una hoja por base



# -------------------------------------------------------------------------
# 5. Preparar la base multirregistro ---------------------------------------
# -------------------------------------------------------------------------

# ¿Por qué "multirregistro"?
# Un mismo caso aparece en varias filas: cada fila es una prueba (una técnica
# aplicada a una muestra). Si contáramos filas, estaríamos contando pruebas,
# no casos.


# 5.1 Copia intacta de cada base, para poder comparar siempre contra ella --
base_original_sf_cg  <- SifilisCongenito_curso
base_original_sf_emb <- SifilisEmbarazadas_curso

nrow(base_original_sf_cg)                       # filas (pruebas)
n_distinct(base_original_sf_cg$id_evento_caso)  # casos

nrow(base_original_sf_emb)
n_distinct(base_original_sf_emb$id_evento_caso)


# 5.2 Duplicados exactos --------------------------------------------------
# Filas idénticas en TODAS las columnas: son copias, no información nueva.
# duplicated() da TRUE en cada fila que repite a una anterior; sum() las cuenta.
sum(duplicated(base_original_sf_cg))
sum(duplicated(base_original_sf_emb))

# distinct() deja una sola copia de cada fila. A partir de acá trabajamos
# con estas bases: pruebas_sf_cg y pruebas_sf_emb (una fila por prueba).
pruebas_sf_cg  <- base_original_sf_cg  |> distinct()
pruebas_sf_emb <- base_original_sf_emb |> distinct()


# 5.3 Resultado simplificado ----------------------------------------------
# Agrupamos los valores de "resultado" en Reactivo / No reactivo.
# Para qué sirve: para ver de un vistazo si hay valores que no esperábamos
# (quedan como "REVISAR"). La columna "resultado" original no se toca: la
# tabla por caso (sección 7) usa esa, para no perder las diluciones
# ("Reactivo 8 dils"), que hacen falta para comparar títulos.
#
# str_detect(resultado, "^Reactivo") -> TRUE si el texto EMPIEZA con
# "Reactivo" (el ^ marca el comienzo). Así entran todas las diluciones y
# no entra "No reactivo".
pruebas_sf_cg <- pruebas_sf_cg |>
  mutate(resultado_simple = case_when(
    str_detect(resultado, "^Reactivo")                           ~ "Reactivo",
    resultado %in% c("Detectable", "Positivo")                   ~ "Reactivo",
    resultado %in% c("No reactivo", "Negativo", "No detectable") ~ "No reactivo",
    resultado == "No procesada"                                  ~ "No procesada",  # ⚠️ a decidir
    is.na(resultado)                                             ~ NA_character_,
    TRUE                                                         ~ "REVISAR"        # valores nuevos
  ))

# En embarazadas usamos el mismo criterio.
pruebas_sf_emb <- pruebas_sf_emb |>
  mutate(resultado_simple = case_when(
    str_detect(resultado, "^Reactivo")                           ~ "Reactivo",
    resultado %in% c("Detectable", "Positivo")                   ~ "Reactivo",
    resultado %in% c("No reactivo", "Negativo", "No detectable") ~ "No reactivo",
    resultado == "No procesada"                                  ~ "No procesada",  # ⚠️ a decidir
    is.na(resultado)                                             ~ NA_character_,
    TRUE                                                         ~ "REVISAR"        # valores nuevos
  ))

## CONTROL ##
# Cada valor original y a qué grupo fue. Si aparece "REVISAR", decidí a qué
# grupo va ese valor, agregalo arriba y sumalo también a los patrones de la
# sección 8.
pruebas_sf_cg  |> count(resultado_simple, resultado)
pruebas_sf_emb |> count(resultado_simple, resultado)


# 5.4 Tipo de prueba según la determinación -------------------------------
# En sífilis agrupamos las pruebas por DETERMINACIÓN (lo que se midió), no
# por técnica: para clasificar importa si la prueba es treponémica o no
# treponémica, no con qué método se hizo.
#
# Los valores ("Treponemica", "NoTreponemica") van sin espacios ni tildes
# porque después son nombres de columna (m1_Treponemica, m1_NoTreponemica).
# "NoTreponemica" va sin "_" a propósito: si fuera "No_Treponemica", la
# columna m1_No_Treponemica también terminaría en "_Treponemica" y en la
# sección 8 se mezclarían los dos grupos.

# ¿Qué determinaciones hay? Los nombres del diccionario tienen que coincidir
# EXACTO con estos (tildes y mayúsculas incluidas).
unique(pruebas_sf_cg$determinacion)
unique(pruebas_sf_emb$determinacion)

# El diccionario: un vector con nombres. A la izquierda, cómo aparece la
# determinación en la base; a la derecha, el tipo. Vale para las dos bases.
tipo_por_determinacion_sf <- c(
  "Anticuerpos anti-Treponema pallidum"             = "Treponemica",
  "Anticuerpos IgG anti-Treponema pallidum"         = "Treponemica",
  "Anticuerpos IgG y/o IgM anti-Treponema pallidum" = "Treponemica",
  "Anticuerpos IgM anti-Treponema pallidum"         = "Treponemica",
  "Detección de Ac totales"                         = "Treponemica",
  "Detección de ADN de Treponema pallidum"          = "Treponemica",
  "Detección de genoma viral"                       = "Treponemica",
  "Detección de Treponema pallidum"                 = "Treponemica",
  "Anticuerpos Inespecíficos Reaginas IgG y/o IgM"  = "NoTreponemica"
)

# tipo_por_determinacion_sf[determinacion] busca cada determinación por su
# nombre y devuelve el tipo. unname() le saca los nombres al resultado.
# Si una determinación no está en el diccionario, tipo_prueba queda NA.
pruebas_sf_cg <- pruebas_sf_cg |>
  mutate(tipo_prueba = unname(tipo_por_determinacion_sf[determinacion]))

pruebas_sf_emb <- pruebas_sf_emb |>
  mutate(tipo_prueba = unname(tipo_por_determinacion_sf[determinacion]))

## CONTROL ##
# Cruzamos con la técnica: VDRL, USR y RPR son no treponémicas; el resto,
# treponémicas. Si una de esas tres aparece como "Treponemica", la
# determinación se cargó mal (por ejemplo, una USR cargada como
# "Anticuerpos anti-Treponema pallidum").
# Fijate también cómo están escritas las técnicas: la corrección de abajo
# busca exactamente "VDRL", "USR" y "RPR".
pruebas_sf_cg  |> count(tecnica, tipo_prueba)
pruebas_sf_emb |> count(tecnica, tipo_prueba)

## CORRECCIÓN ##
# En esos casos manda la técnica: VDRL, USR y RPR pasan a "NoTreponemica".
# if_else(condición, valor si TRUE, valor si FALSE)
pruebas_sf_cg <- pruebas_sf_cg |>
  mutate(tipo_prueba = if_else(tecnica %in% c("VDRL", "USR", "RPR"),
                               "NoTreponemica", tipo_prueba))

pruebas_sf_emb <- pruebas_sf_emb |>
  mutate(tipo_prueba = if_else(tecnica %in% c("VDRL", "USR", "RPR"),
                               "NoTreponemica", tipo_prueba))

# Repetimos el control: ahora VDRL, USR y RPR tienen que aparecer solo como
# "NoTreponemica".
pruebas_sf_cg  |> count(tecnica, tipo_prueba)
pruebas_sf_emb |> count(tecnica, tipo_prueba)

## CONTROL ##
# Pruebas con resultado pero sin tipo (determinación vacía o que no está en
# el diccionario). No se descartan: en la tabla por caso aparecen como
# "SinTipo", pero no cuentan para clasificar. Si aparece alguna, revisá el
# diccionario.
pruebas_sf_cg  |> filter(!is.na(resultado), is.na(tipo_prueba)) |> count(tecnica, determinacion)
pruebas_sf_emb |> filter(!is.na(resultado), is.na(tipo_prueba)) |> count(tecnica, determinacion)


# 5.5 Variables del caso --------------------------------------------------
# Son las que tienen UN solo valor por caso (se repiten igual en todas sus
# filas). Las de laboratorio (fecha_de_toma_de_muestra, tecnica,
# determinacion, resultado, semana_epi_muestra) NO van acá.
# Estas listas se le pasan a la función (sección 7).
cols_caso_sf_cg <- c(
  # Persona
  "nro_doc", "sexo_legal", "grupo_etario_diagnostico",
  "provincia_residencia", "departamento_residencia",
  # Notificación
  "evento", "fecha_apertura", "semana_epi_apertura", "fecha_consulta",
  "fecha_de_inicio_de_sintomas", "establecimiento_de_carga",
  "clasificacion_manual_del_caso",
  # Evolución
  "internado", "fecha_internacion",
  "cuidados_intensivos", "fecha_cuidado_intensivo",
  "fallecido", "fecha_fallecimiento", "causa_fallecimiento_asociada"
)

# Embarazadas: arrancamos con la misma lista. Si el control de abajo
# muestra alguna variable que cambia dentro de un caso, sacala de esta
# lista (sin tocar la de congénito).
cols_caso_sf_emb <- c(
  # Persona
  "nro_doc", "sexo_legal", "grupo_etario_diagnostico",
  "provincia_residencia", "departamento_residencia",
  # Notificación
  "evento", "fecha_apertura", "semana_epi_apertura", "fecha_consulta",
  "fecha_de_inicio_de_sintomas", "establecimiento_de_carga",
  "clasificacion_manual_del_caso",
  # Evolución
  "internado", "fecha_internacion",
  "cuidados_intensivos", "fecha_cuidado_intensivo",
  "fallecido", "fecha_fallecimiento", "causa_fallecimiento_asociada"
)

## CONTROL 1 ## ¿Hay alguna que NO exista en la base? Tiene que dar character(0)
setdiff(cols_caso_sf_cg,  names(pruebas_sf_cg))
setdiff(cols_caso_sf_emb, names(pruebas_sf_emb))

## CONTROL 2 ## ¿Alguna cambia dentro de un mismo caso? Tiene que dar 0 filas.
# Para cada caso contamos cuántos valores distintos tiene cada variable
# (n_distinct). Después pasamos la tabla a lo largo (una fila por caso y
# variable) y nos quedamos con las que tienen más de un valor.
# Si aparece alguna variable, la función de la sección 6 va a frenar:
# sacala de la lista de arriba.
pruebas_sf_cg |>
  group_by(id_evento_caso) |>
  summarise(across(all_of(cols_caso_sf_cg), n_distinct)) |>
  pivot_longer(-id_evento_caso, names_to = "variable", values_to = "n_valores") |>
  filter(n_valores > 1) |>
  count(variable, name = "casos_con_mas_de_un_valor")

pruebas_sf_emb |>
  group_by(id_evento_caso) |>
  summarise(across(all_of(cols_caso_sf_emb), n_distinct)) |>
  pivot_longer(-id_evento_caso, names_to = "variable", values_to = "n_valores") |>
  filter(n_valores > 1) |>
  count(variable, name = "casos_con_mas_de_un_valor")


# 5.6 Resumen de la preparación -------------------------------------------
nrow(base_original_sf_cg)                  # filas originales
nrow(pruebas_sf_cg)                        # filas sin duplicados
n_distinct(pruebas_sf_cg$id_evento_caso)   # casos: igual que en 5.1

nrow(base_original_sf_emb)
nrow(pruebas_sf_emb)
n_distinct(pruebas_sf_emb$id_evento_caso)



# -------------------------------------------------------------------------
# 6. FUNCIÓN: de multirregistro a una fila por caso ------------------------
# -------------------------------------------------------------------------
#
# ¿POR QUÉ ESTE PASO ES UNA FUNCIÓN Y EL RESTO NO?
#   Es el único paso que es igual para cualquier evento y que no hay que
#   editar: solo cambia de lugar los datos. Lo que sí se edita (diccionarios,
#   criterios de clasificación) queda afuera, a la vista.
#   Correr esta sección solo DEFINE la función (aparece en el Environment,
#   en "Functions"). Todavía no hace nada con los datos: eso pasa en la
#   sección 7, cuando la usamos.
#
# QUÉ HACE
#   Toma la base multirregistro (una fila por prueba) y devuelve una tabla
#   con UNA FILA POR id_evento_caso. No elige ni descarta resultados: solo
#   los cambia de lugar.
#
# CÓMO LO HACE
#   Cada columna de la base pertenece a uno de tres niveles:
#     - CASO    -> un solo valor por caso (edad, sexo, clasificación...)
#     - MUESTRA -> lo tomado en una fecha (en esta base solo hay fecha:
#                  no hay columna de tipo de muestra ni de establecimiento)
#     - PRUEBA  -> lo que se midió en cada muestra, con su resultado
#   La función arma una tabla para cada nivel y después las une por caso.
#   Las muestras se numeran por orden de fecha (m1, m2, m3...) y cada una
#   forma un bloque de columnas: su fecha y una columna por cada tipo de
#   prueba.
#
# EJEMPLO (col_prueba = "tipo_prueba")
#   Entrada (3 filas del caso 1001):
#     id    fecha       tecnica  tipo_prueba    resultado
#     1001  10/01/2025  VDRL     NoTreponemica  Reactivo 8 dils
#     1001  10/01/2025  TP-PA    Treponemica    Reactivo
#     1001  05/04/2025  VDRL     NoTreponemica  No reactivo
#
#   Salida (1 fila):
#     id    m1_fecha    m1_NoTreponemica  m1_Treponemica  m2_fecha    m2_NoTreponemica
#     1001  10/01/2025  Reactivo 8 dils   Reactivo        05/04/2025  No reactivo
#
# ARGUMENTOS
#   base          : base multirregistro preparada (sección 5)
#   cols_caso     : nombres de las variables del caso (sección 5.5)
#   col_prueba    : columna que define las columnas de cada muestra.
#                   En sífilis, "tipo_prueba" (Treponemica / NoTreponemica)
#   col_resultado : columna de resultado que se copia en la tabla:
#                   "resultado"        -> valores originales, con diluciones
#                   "resultado_simple" -> Reactivo / No reactivo / No procesada
#
# DEVUELVE
#   Una tabla (tibble) con una fila por caso: las variables del caso,
#   n_muestras, n_pruebas y un bloque de columnas por muestra.
#   Si en una misma muestra hay más de una prueba del mismo tipo con
#   resultados distintos, quedan unidos con ";" (por ejemplo,
#   "Reactivo;No reactivo"). En la sección 7 se buscan esos casos.
# -------------------------------------------------------------------------

reordenar_por_caso_sf <- function(base, cols_caso, col_prueba = "tipo_prueba",
                                  col_resultado = "resultado") {
  
  # ---- Control de nombres -------------------------------------------------
  # Si falta alguna columna, la función frena y dice cuál.
  necesarias <- c("id_evento_caso", "fecha_de_toma_de_muestra",
                  col_prueba, col_resultado, cols_caso)
  faltan <- setdiff(necesarias, names(base))
  if (length(faltan) > 0) {
    stop("Faltan estas columnas en la base: ", paste(faltan, collapse = ", "),
         "\nRevisá names(base).")
  }
  
  
  # ---- Función auxiliar ----------------------------------------------------
  # Une los valores distintos de un grupo con ";" para no perder ninguno.
  # Ejemplo: c("Reactivo", "Reactivo", NA)   -> "Reactivo"
  #          c("Reactivo", "No reactivo")    -> "Reactivo;No reactivo"
  #          c(NA, NA)                       -> NA
  juntar <- function(x) {
    x <- unique(na.omit(x))
    if (length(x) == 0) NA_character_ else paste(x, collapse = ";")
  }
  
  
  # ---- PASO 1. Numerar las muestras de cada caso --------------------------
  # Una muestra = una fecha de toma. (En sífilis no hay columna de tipo de
  # muestra, así que dos materiales del mismo día quedan en la misma muestra.)
  # Ordenamos por fecha y numeramos dentro de cada caso: 1, 2, 3...
  # consecutive_id() da el mismo número a las filas con la misma fecha.
  # Las filas sin fecha quedan al final, con el último número.
  # También copiamos col_prueba a una columna "prueba", para usar siempre
  # el mismo nombre adentro de la función.
  base <- base |>
    mutate(prueba = .data[[col_prueba]]) |>
    arrange(id_evento_caso, fecha_de_toma_de_muestra) |>
    group_by(id_evento_caso) |>
    mutate(n_muestra = consecutive_id(fecha_de_toma_de_muestra)) |>
    ungroup()
  
  
  # ---- PASO 2. Tabla de casos (nivel CASO) ---------------------------------
  # Nos quedamos con las variables del caso y sacamos las repeticiones.
  # Si cada variable tiene un solo valor por caso, queda una fila por caso.
  casos <- base |>
    select(id_evento_caso, all_of(cols_caso)) |>
    distinct()
  
  # Control: si quedaron más filas que casos, alguna variable de cols_caso
  # cambia dentro de un mismo caso. La función frena en vez de elegir un
  # valor. (El CONTROL 2 de la sección 5.5 muestra cuál es.)
  if (nrow(casos) != n_distinct(base$id_evento_caso)) {
    stop("Alguna variable de cols_caso cambia dentro de un mismo caso. ",
         "Mirá el CONTROL 2 de la sección 5.5.")
  }
  
  # Aviso: columnas que no pasan a la tabla final porque no son del caso ni
  # se usan como datos de muestra o prueba.
  cols_laboratorio <- c("fecha_de_toma_de_muestra", col_prueba, col_resultado,
                        "prueba", "n_muestra")
  no_incluidas <- setdiff(names(base), c("id_evento_caso", cols_caso, cols_laboratorio))
  if (length(no_incluidas) > 0) {
    message("Columnas que no pasan a la tabla final: ",
            paste(no_incluidas, collapse = ", "))
  }
  
  
  # ---- PASO 3. Tabla de muestras (nivel MUESTRA) ---------------------------
  # Una fila por caso y muestra, con su fecha.
  # Sacamos las filas sin fecha, para no crear columnas de fecha vacías.
  muestras <- base |>
    filter(!is.na(fecha_de_toma_de_muestra)) |>
    group_by(id_evento_caso, n_muestra) |>
    summarise(fecha = first(fecha_de_toma_de_muestra),
              .groups = "drop")
  
  
  # ---- PASO 4. Tabla de resultados (nivel PRUEBA) --------------------------
  # Una fila por caso, muestra y tipo de prueba, con su resultado.
  # - Sacamos las filas sin prueba ni resultado (no hay nada que guardar).
  # - Si hay resultado pero falta la prueba, la llamamos "SinTipo".
  # - Si hay prueba pero falta el resultado, ponemos "Sin resultado", para
  #   distinguir "se hizo y no hay resultado" de "no se hizo" (celda vacía).
  # - n_filas cuenta cuántas pruebas había: si es más de 1, hubo más de una
  #   prueba del mismo tipo en la misma muestra (por ejemplo, TP-PA y EIA
  #   el mismo día) y juntar() une los resultados distintos con ";".
  resultados <- base |>
    filter(!(is.na(prueba) & is.na(.data[[col_resultado]]))) |>
    mutate(prueba = coalesce(prueba, "SinTipo")) |>
    group_by(id_evento_caso, n_muestra, prueba) |>
    summarise(n_filas   = n(),
              resultado = replace_na(juntar(.data[[col_resultado]]), "Sin resultado"),
              .groups = "drop")
  
  
  # ---- PASO 5. Pasar muestras y resultados a lo ancho ---------------------
  # pivot_wider() convierte filas en columnas.
  # Muestras: cada muestra pasa a ser una columna de fecha
  #   m1_fecha, m2_fecha, ...
  muestras_ancho <- muestras |>
    pivot_wider(names_from  = n_muestra,
                values_from = fecha,
                names_glue  = "m{n_muestra}_fecha")
  
  # Resultados: cada tipo de prueba de cada muestra pasa a ser una columna:
  #   m1_NoTreponemica, m1_Treponemica, m2_NoTreponemica, ...
  resultados_ancho <- resultados |>
    select(-n_filas) |>
    pivot_wider(names_from  = c(n_muestra, prueba),
                values_from = resultado,
                names_glue  = "m{n_muestra}_{prueba}")
  
  # Conteos por caso: cuántas muestras y cuántas pruebas tiene cada uno.
  # Los casos sin estudios quedan con 0.
  conteos <- casos |>
    select(id_evento_caso) |>
    left_join(count(muestras, id_evento_caso, name = "n_muestras"),
              by = "id_evento_caso") |>
    left_join(count(resultados, id_evento_caso, wt = n_filas, name = "n_pruebas"),
              by = "id_evento_caso") |>
    mutate(across(c(n_muestras, n_pruebas), ~ replace_na(.x, 0)))
  
  
  # ---- PASO 6. Unir todo por caso -----------------------------------------
  # Partimos de la tabla de casos y le pegamos lo demás con left_join, así
  # ningún caso se pierde aunque no tenga estudios.
  final <- casos |>
    left_join(conteos,          by = "id_evento_caso") |>
    left_join(muestras_ancho,   by = "id_evento_caso") |>
    left_join(resultados_ancho, by = "id_evento_caso")
  
  
  # ---- PASO 7. Ordenar las columnas muestra por muestra -------------------
  # Queremos ver juntas todas las columnas de m1, después las de m2, etc.:
  #   m1_fecha, m1_NoTreponemica, m1_Treponemica, m2_fecha, ...
  # Dentro de cada muestra, los tipos de prueba quedan en orden alfabético.
  orden <- c()
  for (k in sort(unique(c(muestras$n_muestra, resultados$n_muestra)))) {
    m <- paste0("m", k, "_")
    orden <- c(orden,
               paste0(m, "fecha"),
               sort(grep(paste0("^", m), names(resultados_ancho), value = TRUE)))
  }
  
  # any_of() usa las columnas de "orden" que existan (si una muestra no
  # tiene fecha, su m_fecha no existe y no da error). everything() al final
  # garantiza que no quede afuera ninguna columna.
  final <- final |>
    select(id_evento_caso, all_of(cols_caso), n_muestras, n_pruebas,
           any_of(orden), everything())
  
  
  # ---- Informe ------------------------------------------------------------
  message("Filas de entrada: ", nrow(base), "\n",
          "Casos en la tabla final: ", nrow(final), "\n",
          "Máximo de muestras en un caso: ", max(final$n_muestras))
  
  final
}



# -------------------------------------------------------------------------
# 7. Aplicar la función y verificar ----------------------------------------
# -------------------------------------------------------------------------

# 7.1 Aplicar la función --------------------------------------------------
# Usamos "resultado" (con diluciones) y no "resultado_simple": así en la
# tabla por caso se ven los títulos.
SifilisCongenito_caso   <- reordenar_por_caso_sf(pruebas_sf_cg,
                                                 cols_caso     = cols_caso_sf_cg,
                                                 col_prueba    = "tipo_prueba",
                                                 col_resultado = "resultado")

SifilisEmbarazadas_caso <- reordenar_por_caso_sf(pruebas_sf_emb,
                                                 cols_caso     = cols_caso_sf_emb,
                                                 col_prueba    = "tipo_prueba",
                                                 col_resultado = "resultado")


# 7.2 Verificaciones (todas tienen que dar TRUE) --------------------------

# Una fila por caso
nrow(SifilisCongenito_caso)   == n_distinct(pruebas_sf_cg$id_evento_caso)
nrow(SifilisEmbarazadas_caso) == n_distinct(pruebas_sf_emb$id_evento_caso)

# No se perdió ninguna prueba: las pruebas de la tabla por caso tienen que
# ser tantas como las filas con tipo o resultado en la multirregistro.
sum(SifilisCongenito_caso$n_pruebas) ==
  nrow(filter(pruebas_sf_cg, !(is.na(tipo_prueba) & is.na(resultado))))

sum(SifilisEmbarazadas_caso$n_pruebas) ==
  nrow(filter(pruebas_sf_emb, !(is.na(tipo_prueba) & is.na(resultado))))


# 7.3 Revisión: resultados distintos en una misma muestra -----------------
# Casos con más de un resultado en la misma muestra y tipo (celdas con ";",
# por ejemplo "Reactivo;No reactivo"). Conviene revisarlos a mano.
#   if_any() da TRUE si ALGUNA de las columnas elegidas cumple la condición.
SifilisCongenito_caso |>
  filter(if_any(ends_with(c("_Treponemica", "_NoTreponemica")),
                ~ str_detect(.x, ";"))) |>
  View()

SifilisEmbarazadas_caso |>
  filter(if_any(ends_with(c("_Treponemica", "_NoTreponemica")),
                ~ str_detect(.x, ";"))) |>
  View()

# Para volver al detalle de un caso en la base multirregistro (ahí se ve
# qué técnica se usó en cada prueba), cambiá 1001 por el id que quieras:
# pruebas_sf_cg  |> filter(id_evento_caso == 1001) |> View()
# pruebas_sf_emb |> filter(id_evento_caso == 1001) |> View()



# -------------------------------------------------------------------------
# 8. Clasificación de los casos --------------------------------------------
# -------------------------------------------------------------------------
#
# Trabajamos sobre la tabla por caso (una fila por caso), así cada resultado
# que sale es UNO POR CASO y lo que contamos después son casos.
# La clasificación se guarda en una tabla aparte (clas_sf_cg / clas_sf_emb):
# la tabla por caso no se toca y sigue teniendo el dato crudo.
#
# Para cada caso hacemos tres preguntas:
#
# PASO 1. ¿Qué dio cada tipo de prueba? (una columna por tipo: trep, notrep)
#   Miramos TODAS las columnas de ese tipo, de todas las muestras:
#     ends_with("_Treponemica")   -> m1_Treponemica, m2_Treponemica...
#     ends_with("_NoTreponemica") -> m1_NoTreponemica, m2_NoTreponemica...
#   if_any() recorre esas columnas en cada fila y da TRUE si ALGUNA
#   cumple la condición.
#   La condición busca el texto del resultado con un patrón:
#     "(^|;)(Reactivo|Detectable|Positivo)" -> reactivos.
#        (^|;) quiere decir "al principio de la celda o después de un ;"
#        (por las celdas unidas). Así no confunde "No reactivo" con
#        "Reactivo", y encuentra "Reactivo 8 dils".
#     "(^|;)(No reactivo|Negativo|No detectable)" -> no reactivos.
#     !is.na(.x) evita que las celdas vacías den NA.
#   El orden del case_when() importa: si el tipo tiene un reactivo, queda
#   "Reactivo" aunque también tenga un no reactivo ("alguna vez reactivo").
#   Si el caso no tiene ninguna prueba de ese tipo, queda NA.
#
# PASO 2. ¿Cuántos tipos dieron reactivo? (0, 1 o 2)
#   (trep %in% "Reactivo") da TRUE/FALSE; al sumar, TRUE vale 1.
#   Usamos %in% y no == porque con NA, == da NA y la suma entera da NA.
#
# PASO 3. Clasificar. El orden del case_when() es la jerarquía: cada caso
#   se queda con la PRIMERA condición que cumple.
#     Confirmado            : treponémica Y no treponémica reactivas
#     Probable              : solo uno de los dos tipos reactivo
#     Sin laboratorio       : ninguna prueba cargada
#     Sin pruebas reactivas : tiene pruebas no reactivas y ninguna reactiva
#     revisar               : tiene pruebas, pero ningún resultado que se
#                             pueda leer (No procesada, Sin resultado,
#                             SinTipo...)
#   ⚠️ El criterio lo definís vos: cambialo en el case_when().
# -------------------------------------------------------------------------

# Patrones de resultado (valen para las dos bases)
patron_reactivo_sf    <- "(^|;)(Reactivo|Detectable|Positivo)"
patron_no_reactivo_sf <- "(^|;)(No reactivo|Negativo|No detectable)"


# 8.1 EMBARAZADAS ---------------------------------------------------------
clas_sf_emb <- SifilisEmbarazadas_caso |>
  mutate(
    # PASO 1 -------------------------------------------------------------
    trep = case_when(
      if_any(ends_with("_Treponemica"),
             ~ !is.na(.x) & str_detect(.x, patron_reactivo_sf))    ~ "Reactivo",
      if_any(ends_with("_Treponemica"),
             ~ !is.na(.x) & str_detect(.x, patron_no_reactivo_sf)) ~ "No reactivo",
      TRUE ~ NA_character_
    ),
    notrep = case_when(
      if_any(ends_with("_NoTreponemica"),
             ~ !is.na(.x) & str_detect(.x, patron_reactivo_sf))    ~ "Reactivo",
      if_any(ends_with("_NoTreponemica"),
             ~ !is.na(.x) & str_detect(.x, patron_no_reactivo_sf)) ~ "No reactivo",
      TRUE ~ NA_character_
    ),
    
    # PASO 2 -------------------------------------------------------------
    n_tipos_reactivos = (trep %in% "Reactivo") + (notrep %in% "Reactivo"),
    
    # PASO 3 -------------------------------------------------------------
    clasificacion = case_when(
      n_tipos_reactivos == 2                                 ~ "Confirmado",
      n_tipos_reactivos == 1                                 ~ "Probable",
      n_pruebas == 0                                         ~ "Sin laboratorio",
      trep %in% "No reactivo" | notrep %in% "No reactivo"    ~ "Sin pruebas reactivas",
      TRUE                                                   ~ "revisar"
    )
  ) |>
  # Nos quedamos solo con el id y las columnas de la clasificación
  select(id_evento_caso, n_pruebas, trep, notrep, n_tipos_reactivos, clasificacion)


# 8.2 CONGÉNITO -----------------------------------------------------------
# Mismo código que en embarazadas.
# ⚠️ CRITERIO PROVISORIO: la definición de caso de sífilis congénita es
# otra (en el recién nacido los anticuerpos pueden ser de la madre: se
# compara el título del bebé con el materno). Hasta tenerla definida,
# usamos el mismo case_when() que en embarazadas; cuando esté, se cambia
# solo el PASO 3 de este bloque.
clas_sf_cg <- SifilisCongenito_caso |>
  mutate(
    # PASO 1 -------------------------------------------------------------
    trep = case_when(
      if_any(ends_with("_Treponemica"),
             ~ !is.na(.x) & str_detect(.x, patron_reactivo_sf))    ~ "Reactivo",
      if_any(ends_with("_Treponemica"),
             ~ !is.na(.x) & str_detect(.x, patron_no_reactivo_sf)) ~ "No reactivo",
      TRUE ~ NA_character_
    ),
    notrep = case_when(
      if_any(ends_with("_NoTreponemica"),
             ~ !is.na(.x) & str_detect(.x, patron_reactivo_sf))    ~ "Reactivo",
      if_any(ends_with("_NoTreponemica"),
             ~ !is.na(.x) & str_detect(.x, patron_no_reactivo_sf)) ~ "No reactivo",
      TRUE ~ NA_character_
    ),
    
    # PASO 2 -------------------------------------------------------------
    n_tipos_reactivos = (trep %in% "Reactivo") + (notrep %in% "Reactivo"),
    
    # PASO 3 (provisorio) -----------------------------------------------
    clasificacion = case_when(
      n_tipos_reactivos == 2                                 ~ "Confirmado",
      n_tipos_reactivos == 1                                 ~ "Probable",
      n_pruebas == 0                                         ~ "Sin laboratorio",
      trep %in% "No reactivo" | notrep %in% "No reactivo"    ~ "Sin pruebas reactivas",
      TRUE                                                   ~ "revisar"
    )
  ) |>
  select(id_evento_caso, n_pruebas, trep, notrep, n_tipos_reactivos, clasificacion)



# -------------------------------------------------------------------------
# 9. Controles y tabla final de clasificación ------------------------------
# -------------------------------------------------------------------------
# Antes de contar, comprobamos que la regla hizo lo que queríamos.


# 9.1 ¿La regla se aplicó como queríamos? ---------------------------------
# Cada combinación de resultados (trep, notrep) y la clasificación que le
# tocó. Leelo como una tabla de verdad: por ejemplo, trep = Reactivo y
# notrep = NA tiene que dar "Probable".
clas_sf_cg  |> count(trep, notrep, clasificacion)
clas_sf_emb |> count(trep, notrep, clasificacion)

# Una fila por caso también en la clasificación (tiene que dar TRUE)
nrow(clas_sf_cg)  == nrow(SifilisCongenito_caso)
nrow(clas_sf_emb) == nrow(SifilisEmbarazadas_caso)


# 9.2 Mirar algunos casos -------------------------------------------------
# Para ver el laboratorio de los casos de una categoría, se le pega a la
# clasificación la tabla por caso con left_join().
clas_sf_emb |>
  filter(clasificacion == "Probable") |>
  left_join(SifilisEmbarazadas_caso, by = c("id_evento_caso", "n_pruebas")) |>
  View()

clas_sf_cg |>
  filter(clasificacion == "revisar") |>
  left_join(SifilisCongenito_caso, by = c("id_evento_caso", "n_pruebas")) |>
  View()


# 9.3 Conteo de casos por clasificación -----------------------------------
# Orden en que queremos ver las categorías (de la más a la menos firme).
# Si agregás una categoría en el case_when() de la sección 8, sumala acá.
orden_clas_sf <- c("Confirmado", "Probable", "Sin pruebas reactivas",
                   "Sin laboratorio", "revisar")

# count() cuenta casos por clasificación; pct es la proporción sobre el
# total de casos de esa base. Cada base tiene sus propias columnas
# (n_cg / pct_cg y n_emb / pct_emb) para después poder ponerlas lado a lado.
conteo_clas_sf_cg <- clas_sf_cg |>
  count(clasificacion, name = "n_cg") |>
  mutate(pct_cg = n_cg / sum(n_cg))

conteo_clas_sf_emb <- clas_sf_emb |>
  count(clasificacion, name = "n_emb") |>
  mutate(pct_emb = n_emb / sum(n_emb))

conteo_clas_sf_cg
conteo_clas_sf_emb


# 9.4 Tabla final: las dos bases lado a lado ------------------------------
# full_join() une las dos tablas por clasificación y conserva todas las
# categorías, aunque alguna aparezca en una sola base (ahí queda NA, que
# reemplazamos por 0).
# arrange(match(...)) ordena las filas según orden_clas_sf.
tabla_clas_sf <- full_join(conteo_clas_sf_cg, conteo_clas_sf_emb,
                           by = "clasificacion") |>
  mutate(across(c(n_cg, pct_cg, n_emb, pct_emb), ~ replace_na(.x, 0))) |>
  arrange(match(clasificacion, orden_clas_sf))

# Fila de total: summarise() suma cada columna numérica y bind_rows() la
# agrega abajo. Los % del total tienen que dar 1 (100 %).
tabla_clas_sf <- tabla_clas_sf |>
  bind_rows(
    tabla_clas_sf |>
      summarise(clasificacion = "Total",
                across(c(n_cg, pct_cg, n_emb, pct_emb), sum))
  )

tabla_clas_sf   # así se ve en la consola, antes de darle formato


# La misma tabla con formato gt
#   tab_spanner()  -> un título que agrupa columnas (una base arriba de n y %)
#   fmt_percent()  -> muestra las proporciones como %
#   tab_style()    -> pone en negrita la fila de total
tabla_final_clas_sf <- tabla_clas_sf |>
  gt() |>
  tab_header(
    title    = "Clasificación final de los casos de sífilis",
    subtitle = "Casos notificados (una fila por id_evento_caso)"
  ) |>
  tab_spanner(label = "Sífilis congénita",       columns = c(n_cg, pct_cg)) |>
  tab_spanner(label = "Sífilis en embarazadas",  columns = c(n_emb, pct_emb)) |>
  cols_label(
    clasificacion = "Clasificación",
    n_cg          = "Casos",
    pct_cg        = "%",
    n_emb         = "Casos",
    pct_emb       = "%"
  ) |>
  fmt_percent(columns = c(pct_cg, pct_emb), decimals = 1) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(rows = clasificacion == "Total")
  ) |>
  tab_source_note(
    "Confirmado: prueba treponémica y no treponémica reactivas. Probable: solo un tipo reactivo."
  ) |>
  tab_source_note(
    "⚠️ Sífilis congénita: criterio provisorio, igual al de embarazadas (sección 8.2)."
  )

tabla_final_clas_sf


# 9.5 Guardar las tablas (opcional) ---------------------------------------
# dir.create() crea la carpeta "output" si no existe (si existe, no hace nada).
# dir.create(here("output"), showWarnings = FALSE)
# write_csv(SifilisCongenito_caso,   here("output", "SifilisCongenito_una_fila_por_caso.csv"))
# write_csv(SifilisEmbarazadas_caso, here("output", "SifilisEmbarazadas_una_fila_por_caso.csv"))
# write_csv(clas_sf_cg,              here("output", "SifilisCongenito_clasificacion.csv"))
# write_csv(clas_sf_emb,             here("output", "SifilisEmbarazadas_clasificacion.csv"))
# gtsave(tabla_final_clas_sf,        here("output", "tabla_clasificacion_sifilis.html"))
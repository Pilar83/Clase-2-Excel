# =========================================================================
# VIH CONGÉNITO Y VIH EN EMBARAZADAS
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
#   - Todos los objetos de este script llevan "_vih", así no se pisan con
#     los de los scripts de sífilis (_sf) y Chagas (_ch) si se corren en la
#     misma sesión. Después de "_vih" va un sufijo con la base:
#       _vih_cg  -> VIH congénito
#       _vih_emb -> VIH en embarazadas
#     Lo que vale para las dos bases lleva solo "_vih" (umbral_verde_vih,
#     nombres_tecnica_vih, reordenar_por_caso_vih...). Se define UNA sola
#     vez: si hay que cambiar un criterio, se cambia ahí.
#   - El proceso va en línea, siempre hacia adelante:
#
#     pruebas_vih_cg          VIHCongenito_caso        clas_vih_cg
#     pruebas_vih_emb   ->    VIHEmbarazadas_caso  ->  clas_vih_emb  ->  tabla final
#     (una fila por           (una fila por caso,      (una fila por
#      prueba)                 un bloque por muestra)   caso, solo la
#                                                       clasificación)
#
#   - La tabla por caso guarda el DATO crudo. La clasificación va en otra
#     tabla, así no se mezcla el dato con la DECISIÓN que tomamos sobre él.
#
# VOCABULARIO QUE USAMOS EN ESTE SCRIPT
#   - Caso          : cada id_evento_caso (una notificación de una persona para
#                     un evento). No es lo mismo que "persona" (nro_doc) ni que
#                     "caso confirmado".
#   - Muestra       : el material que se toma al paciente en una fecha.
#   - Técnica       : el método de laboratorio (ELISA, test rápido, RT-PCR...).
#   - Determinación : lo que mide la técnica (anticuerpos, carga viral...).
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
  janitor,      # limpiar nombres de columnas y buscar duplicados
  naniar,       # visualizar datos faltantes
  gt            # tablas con formato
)



# -------------------------------------------------------------------------
# 1. Importación de los datos ----------------------------------------------
# -------------------------------------------------------------------------

# Listamos los archivos de la carpeta "data" cuyo nombre EMPIEZA con "VIH".
#   pattern = "^VIH"  -> el ^ marca el comienzo del nombre (y distingue
#                        mayúsculas: "vih" no entra)
#   full.names = TRUE -> con la ruta completa, para poder abrirlos
archivos_vih <- list.files(here("data"), pattern = "^VIH", full.names = TRUE)

archivos_vih   # control: tienen que aparecer los dos archivos

# Importamos todos a la vez. import_list() devuelve una lista con una base
# por archivo, y list2env() saca cada base de la lista al Environment, con
# el nombre del archivo (sin la extensión):
#   VIHCongenito_curso y VIHEmbarazadas_curso
# Las celdas vacías y las que dicen "NA" se leen como dato faltante.
# Precaución: los nombres de archivo no deben tener espacios ni empezar con
# un número, porque se usan como nombres de objeto.
import_list(archivos_vih, na.strings = c("", "NA")) |>
  list2env(envir = .GlobalEnv)



# -------------------------------------------------------------------------
# 2. Exploración inicial ---------------------------------------------------
# -------------------------------------------------------------------------

# Una mirada general: columnas, tipo de cada una y primeros valores
glimpse(VIHCongenito_curso)
glimpse(VIHEmbarazadas_curso)

# Resumen completo de cada variable (faltantes, valores únicos, rangos)
skim(VIHCongenito_curso)
skim(VIHEmbarazadas_curso)

# Cantidad de faltantes por variable, en un gráfico
gg_miss_var(VIHCongenito_curso)
gg_miss_var(VIHEmbarazadas_curso)

# Los valores de laboratorio (técnica, determinación, resultado) los
# miramos en la sección 5, después de limpiar los nombres de las columnas.



# -------------------------------------------------------------------------
# 3. Limpieza --------------------------------------------------------------
# -------------------------------------------------------------------------

# 3.1 Nombres de columnas -------------------------------------------------

# Los nombres con espacios obligan a escribirlos entre ` `, lo que facilita
# errores. clean_names() los pasa a minúscula, sin tildes y con "_".
# Por ejemplo: "Técnica" -> "tecnica", "Determinación" -> "determinacion".

names(VIHCongenito_curso)

# Diferencia entre IMPRIMIR y GUARDAR:
# esto solo muestra el resultado en la consola, no cambia el objeto...
VIHCongenito_curso |>
  clean_names() |>
  names()

names(VIHCongenito_curso)   # ...los nombres siguen igual

# ...para que el cambio quede, hay que asignarlo con <-
VIHCongenito_curso   <- VIHCongenito_curso   |> clean_names()
VIHEmbarazadas_curso <- VIHEmbarazadas_curso |> clean_names()

names(VIHCongenito_curso)   # ahora sí cambiaron
names(VIHEmbarazadas_curso)


# 3.2 Fechas --------------------------------------------------------------

# ¿De qué tipo son las columnas de fecha?
# sapply() aplica class() a cada columna y devuelve el tipo de cada una.
VIHCongenito_curso   |> select(starts_with("fecha")) |> sapply(class)
VIHEmbarazadas_curso |> select(starts_with("fecha")) |> sapply(class)

# Convertimos a fecha solo las que son texto (dd/mm/aaaa) con dmy()
# (day-month-year).
#   across()               -> aplica la misma función a varias columnas
#   starts_with("fecha")   -> las columnas cuyo nombre empieza con "fecha"
#   & where(is.character)  -> ...y que además sean texto
# where(is.character) evita pasarle dmy() a una columna que ya es fecha,
# porque eso la dejaría toda en NA. Así el paso se puede correr más de una vez.
VIHCongenito_curso <- VIHCongenito_curso |>
  mutate(across(starts_with("fecha") & where(is.character), dmy))

VIHEmbarazadas_curso <- VIHEmbarazadas_curso |>
  mutate(across(starts_with("fecha") & where(is.character), dmy))

# Verificamos que ahora sean de tipo Date
VIHCongenito_curso   |> select(starts_with("fecha")) |> sapply(class)
VIHEmbarazadas_curso |> select(starts_with("fecha")) |> sapply(class)


# 3.3 Errores de codificación de caracteres -------------------------------

# Al importar, algunas letras con tilde quedaron como "¿".
# str_replace_all(texto, "lo que busco", "con qué lo reemplazo")
VIHCongenito_curso |> count(grupo_etario_diagnostico)   # antes

VIHCongenito_curso <- VIHCongenito_curso |>
  mutate(grupo_etario_diagnostico = grupo_etario_diagnostico |>
           str_replace_all("d¿as", "días") |>
           str_replace_all("a¿os", "años"))

VIHEmbarazadas_curso <- VIHEmbarazadas_curso |>
  mutate(grupo_etario_diagnostico = grupo_etario_diagnostico |>
           str_replace_all("d¿as", "días") |>
           str_replace_all("a¿os", "años"))

VIHCongenito_curso |> count(grupo_etario_diagnostico)   # después



# -------------------------------------------------------------------------
# 4. Completitud de las variables (tabla semáforo) -------------------------
# -------------------------------------------------------------------------

# OJO: estas bases son multirregistro (varias filas por caso). Acá la
# completitud se calcula sobre FILAS, así que los casos con más pruebas
# pesan más. Para las variables del caso (edad, sexo...) conviene mirarla
# también en la tabla de una fila por caso (sección 7).

# Umbrales del semáforo (ajustalos a tu criterio). Valen para las dos bases.
umbral_verde_vih    <- 0.95   # >= 95 %  -> verde
umbral_amarillo_vih <- 0.80   # 80-95 %  -> amarillo; < 80 % -> rojo


# 4.1 CONGÉNITO: completitud por variable ---------------------------------
# map_dfr() recorre los nombres de las columnas, aplica la función a cada
# una y junta los resultados en una tabla (una fila por variable).
# Cuentan como faltante: NA, celdas vacías ("") y celdas con solo espacios.
completitud_vih_cg <- map_dfr(names(VIHCongenito_curso), function(v) {
  x     <- VIHCongenito_curso[[v]]              # la columna v
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
      pct >= umbral_verde_vih    ~ "🟢 Completo",
      pct >= umbral_amarillo_vih ~ "🟡 Parcial",
      TRUE                       ~ "🔴 Crítico"
    )
  ) |>
  arrange(pct)   # las variables más incompletas arriba


# 4.2 CONGÉNITO: tabla semáforo -------------------------------------------
# gt() convierte la tabla en una tabla con formato. Cada función que sigue
# le agrega algo: título, nombres de columnas, formato de %, colores, nota.
tabla_semaforo_vih_cg <- completitud_vih_cg |>
  gt() |>
  tab_header(
    title    = "Completitud del registro por variable",
    subtitle = paste0("Notificación de VIH Congénito (n = ",
                      nrow(VIHCongenito_curso), " registros)")
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
    locations = cells_body(columns = c(pct, estado), rows = pct >= umbral_verde_vih)
  ) |>
  tab_style(
    style     = list(cell_fill(color = "#FFEB9C"), cell_text(color = "#9C5700")),
    locations = cells_body(columns = c(pct, estado),
                           rows = pct >= umbral_amarillo_vih & pct < umbral_verde_vih)
  ) |>
  tab_style(
    style     = list(cell_fill(color = "#FFC7CE"), cell_text(color = "#9C0006")),
    locations = cells_body(columns = c(pct, estado), rows = pct < umbral_amarillo_vih)
  ) |>
  tab_source_note(
    paste0("Criterio: verde ≥ ", umbral_verde_vih * 100, "%, amarillo ≥ ",
           umbral_amarillo_vih * 100, "%, rojo < ", umbral_amarillo_vih * 100,
           "%. Se consideran faltantes NA, celdas vacías y solo espacios.")
  )

tabla_semaforo_vih_cg   # se muestra en el panel Viewer de RStudio


# 4.3 EMBARAZADAS: completitud por variable -------------------------------
# Es el mismo código que en 4.1, cambiando la base y el nombre del resultado.
completitud_vih_emb <- map_dfr(names(VIHEmbarazadas_curso), function(v) {
  x     <- VIHEmbarazadas_curso[[v]]
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
      pct >= umbral_verde_vih    ~ "🟢 Completo",
      pct >= umbral_amarillo_vih ~ "🟡 Parcial",
      TRUE                       ~ "🔴 Crítico"
    )
  ) |>
  arrange(pct)


# 4.4 EMBARAZADAS: tabla semáforo (mismo formato que 4.2) -----------------
tabla_semaforo_vih_emb <- completitud_vih_emb |>
  gt() |>
  tab_header(
    title    = "Completitud del registro por variable",
    subtitle = paste0("Notificación de VIH en Embarazadas (n = ",
                      nrow(VIHEmbarazadas_curso), " registros)")
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
    locations = cells_body(columns = c(pct, estado), rows = pct >= umbral_verde_vih)
  ) |>
  tab_style(
    style     = list(cell_fill(color = "#FFEB9C"), cell_text(color = "#9C5700")),
    locations = cells_body(columns = c(pct, estado),
                           rows = pct >= umbral_amarillo_vih & pct < umbral_verde_vih)
  ) |>
  tab_style(
    style     = list(cell_fill(color = "#FFC7CE"), cell_text(color = "#9C0006")),
    locations = cells_body(columns = c(pct, estado), rows = pct < umbral_amarillo_vih)
  ) |>
  tab_source_note(
    paste0("Criterio: verde ≥ ", umbral_verde_vih * 100, "%, amarillo ≥ ",
           umbral_amarillo_vih * 100, "%, rojo < ", umbral_amarillo_vih * 100,
           "%. Se consideran faltantes NA, celdas vacías y solo espacios.")
  )

tabla_semaforo_vih_emb


# 4.5 Exportar (opcional) -------------------------------------------------
# gtsave(tabla_semaforo_vih_cg,  "semaforo_completitud_vih_cg.html")
# gtsave(tabla_semaforo_vih_emb, "semaforo_completitud_vih_emb.html")
# gtsave(tabla_semaforo_vih_cg,  "semaforo_completitud_vih_cg.png")   # requiere webshot2
# writexl::write_xlsx(list(congenito = completitud_vih_cg, embarazadas = completitud_vih_emb),
#                     "completitud_vih.xlsx")                          # una hoja por base



# -------------------------------------------------------------------------
# 5. Preparar la base multirregistro ---------------------------------------
# -------------------------------------------------------------------------

# ¿Por qué "multirregistro"?
# Un mismo caso aparece en varias filas: cada fila es una prueba (una técnica
# aplicada a una muestra). Si contáramos filas, estaríamos contando pruebas,
# no casos.
#
# En esta sección NO cambiamos la estructura (sigue siendo una fila por
# prueba) y NO tomamos decisiones de vigilancia. Solo dejamos la base prolija.


# 5.1 Copia intacta de cada base, para poder comparar siempre contra ella --
base_original_vih_cg  <- VIHCongenito_curso
base_original_vih_emb <- VIHEmbarazadas_curso

nrow(base_original_vih_cg)                       # filas (pruebas)
n_distinct(base_original_vih_cg$id_evento_caso)  # casos

nrow(base_original_vih_emb)
n_distinct(base_original_vih_emb$id_evento_caso)


# 5.2 Duplicados exactos --------------------------------------------------
# Filas idénticas en TODAS las columnas: son copias, no información nueva.
# duplicated() da TRUE en cada fila que repite a una anterior; sum() las cuenta.
sum(duplicated(base_original_vih_cg))
sum(duplicated(base_original_vih_emb))

# Si da más de 0, get_dupes() muestra esas filas (con cuántas veces se
# repiten), para decidir si son cargas repetidas:
# base_original_vih_cg |> get_dupes()

# distinct() deja una sola copia de cada fila. A partir de acá trabajamos
# con estas bases: pruebas_vih_cg y pruebas_vih_emb (una fila por prueba).
pruebas_vih_cg  <- base_original_vih_cg  |> distinct()
pruebas_vih_emb <- base_original_vih_emb |> distinct()


# 5.3 Nombres cortos de técnica -------------------------------------------
# Los nombres de las técnicas se usan para armar nombres de columna
# (m1_ELISA_resultado, m1_RTPCR_resultado...), así que tienen que ser cortos
# y sin espacios ni símbolos.

# ¿Qué técnicas hay?
pruebas_vih_cg  |> count(tecnica)
pruebas_vih_emb |> count(tecnica)

# El diccionario: un vector con nombres. A la izquierda, cómo aparece la
# técnica en la base (tiene que coincidir EXACTO, con tildes y mayúsculas);
# a la derecha, el nombre corto. Vale para las dos bases.
nombres_tecnica_vih <- c(
  "ELISA"               = "ELISA",
  "Quimioluminiscencia" = "QL",
  "Test rápido"         = "TR",
  "RT-PCR"              = "RTPCR"
)

# nombres_tecnica_vih[tecnica] busca cada técnica por su nombre y devuelve
# el nombre corto. unname() le saca los nombres al resultado.
# as.character() asegura que busque por el TEXTO (si tecnica fuera un
# factor, R buscaría por posición).
# Si una técnica no está en la lista, da NA, y coalesce() la reemplaza por
# el nombre original (coalesce = "el primer valor que no sea NA").
pruebas_vih_cg <- pruebas_vih_cg |>
  mutate(tecnica_corta = coalesce(unname(nombres_tecnica_vih[as.character(tecnica)]), tecnica))

pruebas_vih_emb <- pruebas_vih_emb |>
  mutate(tecnica_corta = coalesce(unname(nombres_tecnica_vih[as.character(tecnica)]), tecnica))

## CONTROL ##
# Si en tecnica_corta ves un nombre largo, esa técnica no está en la lista
# (o está escrita distinto): agregala al diccionario con un nombre corto.
pruebas_vih_cg  |> count(tecnica, tecnica_corta)
pruebas_vih_emb |> count(tecnica, tecnica_corta)


# 5.4 Resultado simplificado ----------------------------------------------
# Unificamos los distintos vocabularios (Reactivo, Detectable, Positivo...)
# en pocas categorías. Creamos una columna nueva y dejamos "resultado" sin
# tocar. Es el ÚNICO lugar del script donde se define qué cuenta como
# positivo, negativo o indeterminado.
#
# str_detect(resultado, "^(No reactivo|No detectable|Negativo)")
#   -> TRUE si el texto EMPIEZA con alguno de esos valores (el ^ marca el
#      comienzo; la | quiere decir "o").
# El orden importa: case_when() usa la primera condición que se cumple.
# Los negativos van ANTES que los positivos para que "No reactivo" no se
# lea como "reactivo".
pruebas_vih_cg <- pruebas_vih_cg |>
  mutate(resultado_simple = case_when(
    is.na(resultado)                                               ~ NA_character_,
    str_detect(resultado, "^(No reactivo|No detectable|Negativo)") ~ "Negativo",
    str_detect(resultado, "^(Reactivo|Detectable|Positivo)")       ~ "Positivo",       # incluye "Reactivo 1/8", etc.
    str_detect(resultado, "^(Indeterminado|En estudio)")           ~ "Indeterminado",  # ⚠️ a decidir
    resultado == "No procesada"                                    ~ "No procesada",   # ⚠️ a decidir
    TRUE                                                           ~ "REVISAR"         # valores nuevos
  ))

# En embarazadas usamos el mismo criterio. Si aparecen valores que no están
# en la lista, van a quedar como "REVISAR": decidí a qué categoría van y
# agregalos acá Y en congénito, para que las dos bases sigan iguales.
pruebas_vih_emb <- pruebas_vih_emb |>
  mutate(resultado_simple = case_when(
    is.na(resultado)                                               ~ NA_character_,
    str_detect(resultado, "^(No reactivo|No detectable|Negativo)") ~ "Negativo",
    str_detect(resultado, "^(Reactivo|Detectable|Positivo)")       ~ "Positivo",
    str_detect(resultado, "^(Indeterminado|En estudio)")           ~ "Indeterminado",  # ⚠️ a decidir
    resultado == "No procesada"                                    ~ "No procesada",   # ⚠️ a decidir
    TRUE                                                           ~ "REVISAR"         # valores nuevos
  ))

## CONTROL ##
# Cada valor original y a qué grupo fue. Si aparece "REVISAR", agregá ese
# valor en los case_when() de arriba.
pruebas_vih_cg  |> count(resultado_simple, resultado)
pruebas_vih_emb |> count(resultado_simple, resultado)

# Qué determinación se midió con cada técnica (para la sección 8)
pruebas_vih_cg  |> count(tecnica_corta, determinacion)
pruebas_vih_emb |> count(tecnica_corta, determinacion)


# 5.5 Variables del caso --------------------------------------------------
# Son las que tienen UN solo valor por caso (se repiten igual en todas sus
# filas). Las de laboratorio (fecha_de_toma_de_muestra, tecnica,
# determinacion, resultado) NO van acá: cambian de una prueba a otra.
# Estas listas se le pasan a la función (sección 7).
cols_caso_vih_cg <- c(
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

# Embarazadas: arrancamos con la misma lista. Si el CONTROL 2 muestra
# alguna variable que cambia dentro de un caso, sacala de esta lista (sin
# tocar la de congénito).
cols_caso_vih_emb <- c(
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
setdiff(cols_caso_vih_cg,  names(pruebas_vih_cg))
setdiff(cols_caso_vih_emb, names(pruebas_vih_emb))

## CONTROL 2 ## ¿Alguna cambia dentro de un mismo caso? Tiene que dar 0 filas.
# Para cada caso contamos cuántos valores distintos tiene cada variable
# (n_distinct; un NA y un valor cuentan como dos). Después pasamos la tabla
# a lo largo (una fila por caso y variable) y nos quedamos con las que
# tienen más de un valor. Si aparece alguna, la función de la sección 6 va
# a frenar: sacala de la lista de arriba.
pruebas_vih_cg |>
  group_by(id_evento_caso) |>
  summarise(across(all_of(cols_caso_vih_cg), n_distinct)) |>
  pivot_longer(-id_evento_caso, names_to = "variable", values_to = "n_valores") |>
  filter(n_valores > 1) |>
  count(variable, name = "casos_con_mas_de_un_valor") 


pruebas_vih_emb |>
  group_by(id_evento_caso) |>
  summarise(across(all_of(cols_caso_vih_emb), n_distinct)) |>
  pivot_longer(-id_evento_caso, names_to = "variable", values_to = "n_valores") |>
  filter(n_valores > 1) |>
  count(variable, name = "casos_con_mas_de_un_valor")


# 5.6 Resumen de la preparación -------------------------------------------
nrow(base_original_vih_cg)                  # filas originales
nrow(pruebas_vih_cg)                        # filas sin duplicados
n_distinct(pruebas_vih_cg$id_evento_caso)   # casos: igual que en 5.1

nrow(base_original_vih_emb)
nrow(pruebas_vih_emb)
n_distinct(pruebas_vih_emb$id_evento_caso)



# -------------------------------------------------------------------------
# 6. FUNCIÓN: de multirregistro a una fila por caso ------------------------
# -------------------------------------------------------------------------
#
# ¿POR QUÉ ESTE PASO ES UNA FUNCIÓN Y EL RESTO NO?
#   Es el único paso que es igual para cualquier base y que no hay que
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
#     - MUESTRA -> lo tomado en una fecha (en esta base solo hay fecha)
#     - PRUEBA  -> cada técnica aplicada a una muestra, con su resultado
#   La función arma una tabla para cada nivel y después las une por caso.
#   Las muestras se numeran por orden de fecha (m1, m2, m3...). En VIH,
#   dentro de cada muestra cada técnica tiene TRES columnas: la
#   determinación, el resultado original y el resultado simplificado (la
#   determinación importa: no es lo mismo anticuerpos que carga viral).
#
# EJEMPLO
#   Entrada (2 filas del caso 1001):
#     id    fecha       tecnica_corta  determinacion  resultado  resultado_simple
#     1001  10/01/2025  ELISA          Ac anti-VIH    Reactivo   Positivo
#     1001  05/04/2025  RTPCR          Carga viral    Detectable Positivo
#
#   Salida (1 fila):
#     id    m1_fecha    m1_ELISA_determinacion  m1_ELISA_resultado  m1_ELISA_resultado_simple  m2_fecha    m2_RTPCR_...
#     1001  10/01/2025  Ac anti-VIH             Reactivo            Positivo                   05/04/2025  ...
#
# ARGUMENTOS
#   base      : base multirregistro preparada (sección 5, con tecnica_corta
#               y resultado_simple)
#   cols_caso : nombres de las variables del caso (sección 5.5)
#
# DEVUELVE
#   Una tabla (tibble) con una fila por caso: las variables del caso,
#   n_muestras, n_pruebas y un bloque de columnas por muestra.
#   Si la misma técnica se repite en una misma muestra, sus valores quedan
#   unidos con ";" (por ejemplo, "Detectable;No detectable"). En la
#   sección 7 se buscan esos casos.
# -------------------------------------------------------------------------

reordenar_por_caso_vih <- function(base, cols_caso) {
  
  # ---- Control de nombres -------------------------------------------------
  # Si falta alguna columna, la función frena y dice cuál.
  cols_laboratorio <- c("fecha_de_toma_de_muestra", "tecnica", "tecnica_corta",
                        "determinacion", "resultado", "resultado_simple")
  faltan <- setdiff(c("id_evento_caso", cols_laboratorio, cols_caso), names(base))
  if (length(faltan) > 0) {
    stop("Faltan estas columnas en la base: ", paste(faltan, collapse = ", "),
         "\n¿Corriste la sección 5 (tecnica_corta y resultado_simple)?")
  }
  
  
  # ---- Función auxiliar ----------------------------------------------------
  # Une los valores distintos de un grupo con ";" para no perder ninguno.
  # Ejemplo: c("Detectable", "Detectable", NA) -> "Detectable"
  #          c("Detectable", "No detectable")  -> "Detectable;No detectable"
  #          c(NA, NA)                         -> NA
  juntar <- function(x) {
    x <- unique(na.omit(as.character(x)))
    if (length(x) == 0) NA_character_ else paste(x, collapse = ";")
  }
  
  
  # ---- PASO 1. Tabla de casos (nivel CASO) ---------------------------------
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
  # de laboratorio.
  no_incluidas <- setdiff(names(base), c("id_evento_caso", cols_caso, cols_laboratorio))
  if (length(no_incluidas) > 0) {
    message("Columnas que no pasan a la tabla final: ",
            paste(no_incluidas, collapse = ", "))
  }
  
  
  # ---- PASO 2. Numerar las muestras de cada caso ---------------------------
  # Dejamos afuera las filas sin ningún dato de laboratorio (casos sin
  # estudios): esos casos quedan igual en la tabla final, con n_pruebas = 0.
  # Una muestra = una fecha de toma. Ordenamos por fecha y numeramos dentro
  # de cada caso: 1, 2, 3... consecutive_id() da el mismo número a las filas
  # con la misma fecha.
  con_prueba <- base |>
    filter(!(is.na(tecnica) & is.na(determinacion) & is.na(resultado))) |>
    mutate(tecnica_corta = coalesce(tecnica_corta, "SinTecnica")) |>
    arrange(id_evento_caso, fecha_de_toma_de_muestra) |>
    group_by(id_evento_caso) |>
    mutate(n_muestra = consecutive_id(fecha_de_toma_de_muestra)) |>
    ungroup()
  
  # Aviso: pruebas sin fecha de toma. consecutive_id() trata los NA como una
  # misma fecha y arrange() los deja al final: dentro de cada caso quedan
  # juntas en la última muestra.
  sin_fecha <- sum(is.na(con_prueba$fecha_de_toma_de_muestra))
  if (sin_fecha > 0) {
    message("Pruebas sin fecha de toma: ", sin_fecha,
            ". Dentro de cada caso quedan juntas en la última muestra.")
  }
  
  
  # ---- PASO 3. Tabla de muestras (nivel MUESTRA) ---------------------------
  # Una fila por caso y muestra, con su fecha.
  muestras <- con_prueba |>
    distinct(id_evento_caso, n_muestra, fecha = fecha_de_toma_de_muestra)
  
  
  # ---- PASO 4. Tabla de pruebas (nivel PRUEBA) -----------------------------
  # Una fila por caso, muestra y técnica.
  # - Si hay resultado pero falta la técnica, la llamamos "SinTecnica" (PASO 2).
  # - n_filas cuenta cuántas pruebas había: si es más de 1, la misma técnica
  #   se repitió en la misma muestra y juntar() une los valores con ";".
  pruebas_tec <- con_prueba |>
    group_by(id_evento_caso, n_muestra, tecnica_corta) |>
    summarise(n_filas          = n(),
              determinacion    = juntar(determinacion),
              resultado        = juntar(resultado),
              resultado_simple = juntar(resultado_simple),
              .groups = "drop")
  
  repetidas <- sum(pruebas_tec$n_filas > 1)
  if (repetidas > 0) {
    message("Muestras con una técnica repetida: ", repetidas,
            ". Sus valores quedaron unidos con ';'.")
  }
  
  
  # ---- PASO 5. Pasar muestras y pruebas a lo ancho ------------------------
  # pivot_wider() convierte filas en columnas.
  # Fechas: m1_fecha, m2_fecha...
  ancho_fechas <- muestras |>
    pivot_wider(id_cols     = id_evento_caso,
                names_from  = n_muestra,
                values_from = fecha,
                names_glue  = "m{n_muestra}_fecha")
  
  # Pruebas: m1_ELISA_determinacion, m1_ELISA_resultado,
  # m1_ELISA_resultado_simple, m1_RTPCR_...
  #   values_from con tres columnas -> tres columnas por técnica
  #   names_vary = "slowest"        -> las tres columnas de cada técnica
  #                                    quedan una al lado de la otra
  # (ordenamos antes para que salgan en orden de muestra y técnica)
  ancho_pruebas <- pruebas_tec |>
    arrange(n_muestra, tecnica_corta) |>
    select(-n_filas) |>
    pivot_wider(id_cols     = id_evento_caso,
                names_from  = c(n_muestra, tecnica_corta),
                values_from = c(determinacion, resultado, resultado_simple),
                names_glue  = "m{n_muestra}_{tecnica_corta}_{.value}",
                names_vary  = "slowest")
  
  
  # ---- PASO 6. Contar muestras y pruebas por caso --------------------------
  conteos <- con_prueba |>
    group_by(id_evento_caso) |>
    summarise(n_muestras = n_distinct(n_muestra),
              n_pruebas  = n(),
              .groups = "drop")
  
  
  # ---- PASO 7. Unir todo por caso -----------------------------------------
  # Partimos de la tabla de casos y le pegamos lo demás con left_join, así
  # ningún caso se pierde aunque no tenga estudios (quedan con 0).
  final <- casos |>
    left_join(conteos,       by = "id_evento_caso") |>
    mutate(across(c(n_muestras, n_pruebas), ~ replace_na(.x, 0L))) |>
    left_join(ancho_fechas,  by = "id_evento_caso") |>
    left_join(ancho_pruebas, by = "id_evento_caso")
  
  
  # ---- PASO 8. Ordenar las columnas muestra por muestra -------------------
  # Para cada muestra, primero la fecha y después sus técnicas.
  orden <- c()
  for (k in sort(unique(muestras$n_muestra))) {
    m <- paste0("m", k, "_")
    orden <- c(orden,
               paste0(m, "fecha"),
               grep(paste0("^", m), names(ancho_pruebas), value = TRUE))
  }
  
  # any_of() usa las columnas de "orden" que existan; everything() al final
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
# Ojo: cada tabla con SU base de pruebas y SU lista (cg con cg, emb con emb)
VIHCongenito_caso   <- reordenar_por_caso_vih(pruebas_vih_cg,  cols_caso = cols_caso_vih_cg)
VIHEmbarazadas_caso <- reordenar_por_caso_vih(pruebas_vih_emb, cols_caso = cols_caso_vih_emb)


# 7.2 Verificaciones (todas tienen que dar TRUE) --------------------------

# Una fila por caso
nrow(VIHCongenito_caso)   == n_distinct(pruebas_vih_cg$id_evento_caso)
nrow(VIHEmbarazadas_caso) == n_distinct(pruebas_vih_emb$id_evento_caso)

# No se perdió ninguna prueba: las pruebas de la tabla por caso tienen que
# ser tantas como las filas con algún dato de laboratorio en la base
# multirregistro.
sum(VIHCongenito_caso$n_pruebas) ==
  nrow(filter(pruebas_vih_cg, !(is.na(tecnica) & is.na(determinacion) & is.na(resultado))))

sum(VIHEmbarazadas_caso$n_pruebas) ==
  nrow(filter(pruebas_vih_emb, !(is.na(tecnica) & is.na(determinacion) & is.na(resultado))))


# 7.3 Revisión: resultados distintos en una misma muestra -----------------
# Casos con la misma técnica repetida en una muestra y resultados distintos
# (celdas con ";", por ejemplo "Positivo;Negativo"). Conviene revisarlos.
#   if_any() da TRUE si ALGUNA de las columnas elegidas cumple la condición.

VIHEmbarazadas_caso |>
  filter(if_any(ends_with("_resultado_simple"), ~ str_detect(.x, ";"))) |>
  View()


# 7.4 Mirar algunos casos -------------------------------------------------
VIHCongenito_caso |> filter(n_pruebas == 0) |> View()                  # sin estudios
VIHCongenito_caso |> filter(n_muestras == max(n_muestras)) |> View()   # los de más muestras

VIHEmbarazadas_caso |> filter(n_pruebas == 0) |> View()
VIHEmbarazadas_caso |> filter(n_muestras == max(n_muestras)) |> View()

# Para volver al detalle de un caso en la base multirregistro, cambiá 1001
# por el id que quieras:
# pruebas_vih_cg |> filter(id_evento_caso == 1001) |> View()



# -------------------------------------------------------------------------
# 8. Clasificación de los casos --------------------------------------------
# -------------------------------------------------------------------------
#
# Trabajamos sobre la tabla por caso (una fila por caso), así cada resultado
# que sale es UNO POR CASO y lo que contamos después son casos.
# La clasificación se guarda en una tabla aparte (clas_vih_cg / clas_vih_emb):
# la tabla por caso no se toca y sigue teniendo el dato crudo.
#
# CRITERIO (simplificado): con un resultado positivo, el caso es Positivo.
#
# Para cada caso hacemos dos preguntas:
#
# PASO 1. ¿Qué hay entre TODAS las pruebas del caso?
#   hay_positivo, hay_indeterminado, hay_negativo (TRUE / FALSE)
#   Miramos todas las columnas _resultado_simple, de todas las muestras y
#   técnicas (ends_with("_resultado_simple")).
#
#   Cómo se arma la pregunta "¿alguna columna dice X?":
#     across(..., ~ str_detect(.x, "Positivo"))
#        -> en cada columna pregunta "¿dice Positivo?" (TRUE / FALSE / NA
#           si la celda está vacía). str_detect() y no ==, por las celdas
#           unidas con ";" (por ejemplo "Negativo;Positivo").
#     rowSums(..., na.rm = TRUE)
#        -> cuenta, en cada caso, cuántos TRUE hubo (TRUE vale 1; las
#           celdas vacías no cuentan)
#     > 0
#
# PASO 2. Clasificar. El orden del case_when() es la jerarquía: cada caso
#   se queda con la PRIMERA condición que cumple.
#     Positivo        : al menos un resultado positivo (cualquier técnica)
#     Sin laboratorio : ninguna prueba cargada (va a clasificación manual)
#     Indeterminado   : sin positivos, con algún indeterminado
#     Negativo        : sin positivos ni indeterminados, con algún negativo
#     Sin resultado   : tiene pruebas, pero ningún resultado clasificable
#                       (por ejemplo, solo "No procesada")


# 8.1 CONGÉNITO -----------------------------------------------------------
clas_vih_cg <- VIHCongenito_caso |>
  mutate(
    # PASO 1 -------------------------------------------------------------
    hay_positivo      = rowSums(across(ends_with("_resultado_simple"),
                                       ~ str_detect(.x, "Positivo")), na.rm = TRUE) > 0,
    hay_indeterminado = rowSums(across(ends_with("_resultado_simple"),
                                       ~ str_detect(.x, "Indeterminado")), na.rm = TRUE) > 0,
    hay_negativo      = rowSums(across(ends_with("_resultado_simple"),
                                       ~ str_detect(.x, "Negativo")), na.rm = TRUE) > 0,
    
    # PASO 2 -------------------------------------------------------------
    clasificacion = case_when(
      hay_positivo      ~ "Positivo",
      n_pruebas == 0    ~ "Sin laboratorio",
      hay_indeterminado ~ "Indeterminado",
      hay_negativo      ~ "Negativo",
      TRUE              ~ "Sin resultado"
    )
  ) |>
  # Nos quedamos solo con el id y las columnas de la clasificación
  select(id_evento_caso, n_pruebas, hay_positivo, hay_indeterminado,
         hay_negativo, clasificacion, clasificacion_manual_del_caso)


# 8.2 EMBARAZADAS ---------------------------------------------------------
# Mismo código que en congénito.
clas_vih_emb <- VIHEmbarazadas_caso |>
  mutate(
    # PASO 1 -------------------------------------------------------------
    hay_positivo      = rowSums(across(ends_with("_resultado_simple"),
                                       ~ str_detect(.x, "Positivo")), na.rm = TRUE) > 0,
    hay_indeterminado = rowSums(across(ends_with("_resultado_simple"),
                                       ~ str_detect(.x, "Indeterminado")), na.rm = TRUE) > 0,
    hay_negativo      = rowSums(across(ends_with("_resultado_simple"),
                                       ~ str_detect(.x, "Negativo")), na.rm = TRUE) > 0,
    
    # PASO 2 -------------------------------------------------------------
    clasificacion = case_when(
      hay_positivo      ~ "Positivo",
      n_pruebas == 0    ~ "Sin laboratorio",
      hay_indeterminado ~ "Indeterminado",
      hay_negativo      ~ "Negativo",
      TRUE              ~ "Sin resultado"
    )
  ) |>
  select(id_evento_caso, n_pruebas, hay_positivo, hay_indeterminado,
         hay_negativo, clasificacion, clasificacion_manual_del_caso)



# -------------------------------------------------------------------------
# 9. Controles y tabla final de clasificación ------------------------------
# -------------------------------------------------------------------------
# Antes de contar, comprobamos que la regla hizo lo que queríamos.


# 9.1 ¿La regla se aplicó como queríamos? ---------------------------------
# Una fila por caso también en la clasificación (tiene que dar TRUE)
nrow(clas_vih_cg)  == nrow(VIHCongenito_caso)
nrow(clas_vih_emb) == nrow(VIHEmbarazadas_caso)

# Los positivos tienen que coincidir con los casos que tienen al menos una
# prueba positiva en la base multirregistro (tiene que dar TRUE).
n_distinct(filter(pruebas_vih_cg,  resultado_simple == "Positivo")$id_evento_caso) ==
  sum(clas_vih_cg$clasificacion == "Positivo")

n_distinct(filter(pruebas_vih_emb, resultado_simple == "Positivo")$id_evento_caso) ==
  sum(clas_vih_emb$clasificacion == "Positivo")

# Qué combinaciones aparecen y cómo se clasificó cada una.
# Leelo como una tabla de verdad: por ejemplo, hay_positivo = TRUE tiene
# que dar siempre "Positivo".
clas_vih_cg  |> count(hay_positivo, hay_indeterminado, hay_negativo, clasificacion)
clas_vih_emb |> count(hay_positivo, hay_indeterminado, hay_negativo, clasificacion)

# Comparar con la clasificación manual cargada en el sistema. Donde no
# coinciden, conviene mirar el caso: puede ser un error de carga o un
# límite de esta regla.
clas_vih_cg  |> count(clasificacion, clasificacion_manual_del_caso)
clas_vih_emb |> count(clasificacion, clasificacion_manual_del_caso)


# 9.2 Mirar algunos casos -------------------------------------------------
# Para ver el laboratorio de los casos de una categoría, se le pega a la
# clasificación la tabla por caso con left_join().

clas_vih_emb |>
  filter(clasificacion %in% c("Indeterminado", "Sin resultado")) |>
  select(id_evento_caso, clasificacion) |>
  left_join(VIHEmbarazadas_caso, by = "id_evento_caso") |>
  View()


# 9.3 Conteo de casos por clasificación -----------------------------------
# count() cuenta casos por clasificación; pct es la proporción sobre el
# total de casos de esa base. Cada base tiene sus propias columnas
# (n_cg / pct_cg y n_emb / pct_emb) para después ponerlas lado a lado.
conteo_clas_vih_cg <- clas_vih_cg |>
  count(clasificacion, name = "n_cg") |>
  mutate(pct_cg = n_cg / sum(n_cg))

conteo_clas_vih_emb <- clas_vih_emb |>
  count(clasificacion, name = "n_emb") |>
  mutate(pct_emb = n_emb / sum(n_emb))

conteo_clas_vih_cg
conteo_clas_vih_emb


# 9.4 Tabla final: las dos bases lado a lado ------------------------------
# Orden en que queremos ver las categorías.
# Si agregás una categoría en el case_when() de la sección 8, sumala acá.
orden_clas_vih <- c("Positivo", "Indeterminado", "Negativo",
                    "Sin resultado", "Sin laboratorio")

# full_join() une las dos tablas por clasificación y conserva todas las
# categorías, aunque alguna aparezca en una sola base (ahí queda NA, que
# reemplazamos por 0).
# arrange(match(...)) ordena las filas según orden_clas_vih.
tabla_clas_vih <- full_join(conteo_clas_vih_cg, conteo_clas_vih_emb,
                            by = "clasificacion") |>
  mutate(across(c(n_cg, pct_cg, n_emb, pct_emb), ~ replace_na(.x, 0))) |>
  arrange(match(clasificacion, orden_clas_vih))

# Fila de total: summarise() suma cada columna numérica y bind_rows() la
# agrega abajo. Los % del total tienen que dar 1 (100 %).
tabla_clas_vih <- tabla_clas_vih |>
  bind_rows(
    tabla_clas_vih |>
      summarise(clasificacion = "Total",
                across(c(n_cg, pct_cg, n_emb, pct_emb), sum))
  )

tabla_clas_vih   # así se ve en la consola, antes de darle formato


# La misma tabla con formato gt
#   tab_spanner()  -> un título que agrupa columnas (una base arriba de n y %)
#   fmt_percent()  -> muestra las proporciones como %
#   tab_style()    -> pone en negrita la fila de total
tabla_final_clas_vih <- tabla_clas_vih |>
  gt() |>
  tab_header(
    title    = "Clasificación final de los casos de VIH",
    subtitle = "Casos notificados (una fila por id_evento_caso)"
  ) |>
  tab_spanner(label = "VIH congénito",       columns = c(n_cg, pct_cg)) |>
  tab_spanner(label = "VIH en embarazadas",  columns = c(n_emb, pct_emb)) |>
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
    "Positivo: al menos un resultado positivo en cualquier técnica. Sin laboratorio: va a clasificación manual."
  )

tabla_final_clas_vih


# 9.5 Guardar las tablas (opcional) ---------------------------------------
# dir.create() crea la carpeta "output" si no existe (si existe, no hace nada).
# dir.create(here("output"), showWarnings = FALSE)
# write_csv(VIHCongenito_caso,    here("output", "VIHCongenito_una_fila_por_caso.csv"))
# write_csv(VIHEmbarazadas_caso,  here("output", "VIHEmbarazadas_una_fila_por_caso.csv"))
# write_csv(clas_vih_cg,          here("output", "VIHCongenito_clasificacion.csv"))
# write_csv(clas_vih_emb,         here("output", "VIHEmbarazadas_clasificacion.csv"))
# gtsave(tabla_final_clas_vih,    here("output", "tabla_clasificacion_vih.html"))

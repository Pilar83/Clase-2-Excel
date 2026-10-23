# =========================================================================
# CHAGAS CONGÉNITO Y CHAGAS EN EMBARAZADAS
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
#   8. Clasificación de casos (embarazadas)
#   9. Clasificación de casos (congénito)
#  10. Tabla final de clasificación
#
# LA IDEA
#   - En cada sección se trabaja primero con la base de CONGÉNITO y después
#     con la de EMBARAZADAS, con el mismo código.
#     (En la clasificación el orden se invierte: embarazadas primero, porque
#     es la regla más simple y congénito le suma la edad.)
#   - Todos los objetos de este script llevan "_ch" (Chagas), así no se
#     pisan con los de los scripts de sífilis (_sf) y VIH (_vih) si se
#     corren en la misma sesión. Después de "_ch" va un sufijo con la base:
#       _ch_cg  -> Chagas congénito
#       _ch_emb -> Chagas en embarazadas
#     Lo que vale para las dos bases lleva solo "_ch" (umbral_verde_ch,
#     nombres_tecnica_ch, grupo_sero1_ch, reordenar_por_caso_ch...). Se
#     define UNA sola vez: si hay que cambiar un criterio, se cambia ahí.
#   - El proceso va en línea, siempre hacia adelante:
#
#     pruebas_ch_cg           ChCongenito_caso        clas_ch_cg
#     pruebas_ch_emb    ->    ChEmbarazadas_caso  ->  clas_ch_emb  ->  tabla final
#     (una fila por           (una fila por caso,     (una fila por
#      prueba)                 un bloque por muestra)  caso, solo la
#                                                      clasificación)
#
#   - La tabla por caso guarda el DATO crudo. La clasificación va en otra
#     tabla, así no se mezcla el dato con la DECISIÓN que tomamos sobre él.
#
# VOCABULARIO QUE USAMOS EN ESTE SCRIPT
#   - Caso    : cada id_evento_caso (una notificación de una persona para un
#               evento). No es lo mismo que "persona" (nro_doc) ni que
#               "caso confirmado".
#   - Muestra : el material que se toma al paciente en una fecha (suero,
#               plasma, sangre entera).
#   - Técnica : el método de laboratorio (ELISA, HAI, PCR...).
#   - Prueba  : una técnica aplicada a una muestra, con su resultado.
#               En la base original, CADA FILA ES UNA PRUEBA.
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

# Listamos los archivos de la carpeta "data" cuyo nombre EMPIEZA con "Ch".
#   pattern = "^Ch"   -> el ^ marca el comienzo del nombre. Sin él, entraría
#                        cualquier archivo que tenga "Ch" en algún lugar.
#   full.names = TRUE -> con la ruta completa, para poder abrirlos
archivos_chagas <- list.files(here("data"), pattern = "^Ch", full.names = TRUE)

archivos_chagas   # control: tienen que aparecer los dos archivos

# Importamos todos a la vez. import_list() devuelve una lista con una base
# por archivo, y list2env() saca cada base de la lista al Environment, con
# el nombre del archivo (sin la extensión):
#   ChCongenito_curso y ChEmbarazadas_curso
# Precaución: los nombres de archivo no deben tener espacios ni empezar con
# un número, porque se usan como nombres de objeto.
import_list(archivos_chagas, na.strings = c("", "NA")) |>
  list2env(envir = .GlobalEnv)



# -------------------------------------------------------------------------
# 2. Exploración inicial ---------------------------------------------------
# -------------------------------------------------------------------------

# Una mirada general: columnas, tipo de cada una y primeros valores
glimpse(ChCongenito_curso)
glimpse(ChEmbarazadas_curso)

# Resumen completo de cada variable (faltantes, valores únicos, rangos)
skim(ChCongenito_curso)
skim(ChEmbarazadas_curso)

# Cantidad de faltantes por variable, en un gráfico
gg_miss_var(ChCongenito_curso)
gg_miss_var(ChEmbarazadas_curso)



# -------------------------------------------------------------------------
# 3. Limpieza --------------------------------------------------------------
# -------------------------------------------------------------------------

# 3.1 Nombres de columnas -------------------------------------------------

# Los nombres con espacios obligan a escribirlos entre ` `, lo que facilita
# errores. clean_names() los pasa a minúscula, sin tildes y con "_".
# Por ejemplo: "Resultado" -> "resultado", "Técnica" -> "tecnica".

names(ChCongenito_curso)

# Diferencia entre IMPRIMIR y GUARDAR:
# esto solo muestra el resultado en la consola, no cambia el objeto...
ChCongenito_curso |>
  clean_names() |>
  names()

names(ChCongenito_curso)   # ...los nombres siguen igual

# ...para que el cambio quede, hay que asignarlo con <-
ChCongenito_curso   <- ChCongenito_curso   |> clean_names()
ChEmbarazadas_curso <- ChEmbarazadas_curso |> clean_names()

names(ChCongenito_curso)     # ahora sí cambiaron
names(ChEmbarazadas_curso)


# 3.2 Fechas --------------------------------------------------------------

# ¿De qué tipo son las columnas de fecha?
# sapply() aplica class() a cada columna y devuelve el tipo de cada una.
ChCongenito_curso   |> select(starts_with("fecha")) |> sapply(class)
ChEmbarazadas_curso |> select(starts_with("fecha")) |> sapply(class)

# Convertimos a fecha solo las que son texto (dd/mm/aaaa) con dmy()
# (day-month-year).
#   across()               -> aplica la misma función a varias columnas
#   starts_with("fecha")   -> las columnas cuyo nombre empieza con "fecha"
#   & where(is.character)  -> ...y que además sean texto
# where(is.character) evita pasarle dmy() a una columna que ya es fecha,
# porque eso la dejaría toda en NA. Así el paso se puede correr más de una vez.
ChCongenito_curso <- ChCongenito_curso |>
  mutate(across(starts_with("fecha") & where(is.character), dmy))

ChEmbarazadas_curso <- ChEmbarazadas_curso |>
  mutate(across(starts_with("fecha") & where(is.character), dmy))

# Verificamos que ahora sean de tipo Date
ChCongenito_curso   |> select(starts_with("fecha")) |> sapply(class)
ChEmbarazadas_curso |> select(starts_with("fecha")) |> sapply(class)


# 3.3 Errores de codificación de caracteres -------------------------------

# Al importar, algunas letras con tilde quedaron como "¿".
# str_replace_all(texto, "lo que busco", "con qué lo reemplazo")
ChCongenito_curso |> count(grupo_etario_diagnostico)   # antes

ChCongenito_curso <- ChCongenito_curso |>
  mutate(grupo_etario_diagnostico = grupo_etario_diagnostico |>
           str_replace_all("d¿as", "días") |>
           str_replace_all("a¿os", "años"))

ChEmbarazadas_curso <- ChEmbarazadas_curso |>
  mutate(grupo_etario_diagnostico = grupo_etario_diagnostico |>
           str_replace_all("d¿as", "días") |>
           str_replace_all("a¿os", "años"))

ChCongenito_curso |> count(grupo_etario_diagnostico)   # después



# -------------------------------------------------------------------------
# 4. Completitud de las variables (tabla semáforo) -------------------------
# -------------------------------------------------------------------------

# OJO: estas bases son multirregistro (congénito: 391 filas, 167 casos).
# Acá la completitud se calcula sobre FILAS, así que los casos con más
# pruebas pesan más. Para las variables del caso (edad, sexo...) conviene
# mirarla también en la tabla de una fila por caso (sección 7).

# Umbrales del semáforo (ajustalos a tu criterio). Valen para las dos bases.
umbral_verde_ch    <- 0.95   # >= 95 %  -> verde
umbral_amarillo_ch <- 0.80   # 80-95 %  -> amarillo; < 80 % -> rojo


# 4.1 CONGÉNITO: completitud por variable ---------------------------------
# map_dfr() recorre los nombres de las columnas, aplica la función a cada
# una y junta los resultados en una tabla (una fila por variable).
# Cuentan como faltante: NA, celdas vacías ("") y celdas con solo espacios.
completitud_ch_cg <- map_dfr(names(ChCongenito_curso), function(v) {
  x     <- ChCongenito_curso[[v]]               # la columna v
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
      pct >= umbral_verde_ch    ~ "🟢 Completo",
      pct >= umbral_amarillo_ch ~ "🟡 Parcial",
      TRUE                      ~ "🔴 Crítico"
    )
  ) |>
  arrange(pct)   # las variables más incompletas arriba


# 4.2 CONGÉNITO: tabla semáforo -------------------------------------------
# gt() convierte la tabla en una tabla con formato. Cada función que sigue
# le agrega algo: título, nombres de columnas, formato de %, colores, nota.
tabla_semaforo_ch_cg <- completitud_ch_cg |>
  gt() |>
  tab_header(
    title    = "Completitud del registro por variable",
    subtitle = paste0("Notificación de Chagas Congénito (n = ",
                      nrow(ChCongenito_curso), " registros)")
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
    locations = cells_body(columns = c(pct, estado), rows = pct >= umbral_verde_ch)
  ) |>
  tab_style(
    style     = list(cell_fill(color = "#FFEB9C"), cell_text(color = "#9C5700")),
    locations = cells_body(columns = c(pct, estado),
                           rows = pct >= umbral_amarillo_ch & pct < umbral_verde_ch)
  ) |>
  tab_style(
    style     = list(cell_fill(color = "#FFC7CE"), cell_text(color = "#9C0006")),
    locations = cells_body(columns = c(pct, estado), rows = pct < umbral_amarillo_ch)
  ) |>
  tab_source_note(
    paste0("Criterio: verde ≥ ", umbral_verde_ch * 100, "%, amarillo ≥ ",
           umbral_amarillo_ch * 100, "%, rojo < ", umbral_amarillo_ch * 100,
           "%. Se consideran faltantes NA, celdas vacías y solo espacios.")
  )

tabla_semaforo_ch_cg   # se muestra en el panel Viewer de RStudio


# 4.3 EMBARAZADAS: completitud por variable -------------------------------
# Es el mismo código que en 4.1, cambiando la base y el nombre del resultado.
completitud_ch_emb <- map_dfr(names(ChEmbarazadas_curso), function(v) {
  x     <- ChEmbarazadas_curso[[v]]
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
      pct >= umbral_verde_ch    ~ "🟢 Completo",
      pct >= umbral_amarillo_ch ~ "🟡 Parcial",
      TRUE                      ~ "🔴 Crítico"
    )
  ) |>
  arrange(pct)


# 4.4 EMBARAZADAS: tabla semáforo (mismo formato que 4.2) -----------------
tabla_semaforo_ch_emb <- completitud_ch_emb |>
  gt() |>
  tab_header(
    title    = "Completitud del registro por variable",
    subtitle = paste0("Notificación de Chagas en Embarazadas (n = ",
                      nrow(ChEmbarazadas_curso), " registros)")
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
    locations = cells_body(columns = c(pct, estado), rows = pct >= umbral_verde_ch)
  ) |>
  tab_style(
    style     = list(cell_fill(color = "#FFEB9C"), cell_text(color = "#9C5700")),
    locations = cells_body(columns = c(pct, estado),
                           rows = pct >= umbral_amarillo_ch & pct < umbral_verde_ch)
  ) |>
  tab_style(
    style     = list(cell_fill(color = "#FFC7CE"), cell_text(color = "#9C0006")),
    locations = cells_body(columns = c(pct, estado), rows = pct < umbral_amarillo_ch)
  ) |>
  tab_source_note(
    paste0("Criterio: verde ≥ ", umbral_verde_ch * 100, "%, amarillo ≥ ",
           umbral_amarillo_ch * 100, "%, rojo < ", umbral_amarillo_ch * 100,
           "%. Se consideran faltantes NA, celdas vacías y solo espacios.")
  )

tabla_semaforo_ch_emb


# 4.5 Exportar (opcional) -------------------------------------------------
# gtsave(tabla_semaforo_ch_cg,  "semaforo_completitud_chagas_cg.html")
# gtsave(tabla_semaforo_ch_emb, "semaforo_completitud_chagas_emb.html")
# gtsave(tabla_semaforo_ch_cg,  "semaforo_completitud_chagas_cg.png")   # requiere webshot2
# writexl::write_xlsx(list(congenito = completitud_ch_cg, embarazadas = completitud_ch_emb),
#                     "completitud_chagas.xlsx")                          # una hoja por base



# -------------------------------------------------------------------------
# 5. Preparar la base multirregistro ---------------------------------------
# -------------------------------------------------------------------------

# ¿Por qué "multirregistro"?
# Un mismo caso aparece en varias filas: cada fila es una prueba (una técnica
# aplicada a una muestra). En congénito hay 391 filas pero 167 casos. Si
# contáramos filas, estaríamos contando pruebas, no casos.
#
# En esta sección NO cambiamos la estructura (sigue siendo una fila por
# prueba) y NO tomamos decisiones de vigilancia. Solo dejamos la base prolija.


# 5.1 Copia intacta de cada base, para poder comparar siempre contra ella --
base_original_ch_cg  <- ChCongenito_curso
base_original_ch_emb <- ChEmbarazadas_curso

nrow(base_original_ch_cg)                       # filas (pruebas)
n_distinct(base_original_ch_cg$id_evento_caso)  # casos

nrow(base_original_ch_emb)
n_distinct(base_original_ch_emb$id_evento_caso)


# 5.2 Duplicados exactos --------------------------------------------------
# Filas idénticas en TODAS las columnas: son copias, no información nueva.
# duplicated() da TRUE en cada fila que repite a una anterior; sum() las cuenta.
sum(duplicated(base_original_ch_cg))    # en congénito dio 0
sum(duplicated(base_original_ch_emb))

# distinct() deja una sola copia de cada fila. A partir de acá trabajamos
# con estas bases: pruebas_ch_cg y pruebas_ch_emb (una fila por prueba).
pruebas_ch_cg  <- base_original_ch_cg  |> distinct()
pruebas_ch_emb <- base_original_ch_emb |> distinct()


# 5.3 Nombres cortos de técnica -------------------------------------------
# Los nombres largos ("Hemoaglutinación Indirecta (HAI)") darían nombres de
# columna imposibles de escribir. Creamos una columna nueva con un nombre
# corto y dejamos la original sin tocar.
#
# El diccionario: un vector con nombres. A la izquierda, cómo aparece la
# técnica en la base (tiene que coincidir EXACTO, con tildes y mayúsculas);
# a la derecha, el nombre corto. Vale para las dos bases: si en alguna
# aparece una técnica que no está, agregala acá.
nombres_tecnica_ch <- c(
  "PCR"                                 = "PCR",
  "ELISA"                               = "ELISA",
  "Quimioluminiscencia"                 = "QL",
  "Hemoaglutinación Indirecta (HAI)"    = "HAI",
  "Microhematocrito"                    = "MH",
  "Microstrout"                         = "Microstrout",
  "Micrométodo"                         = "Micrometodo",
  "Gota fresca"                         = "GotaFresca",
  "InmunoFluorescencia Indirecta (IFI)" = "IFI",
  "Electroquimioluminiscencia"          = "ECLIA"
)

# nombres_tecnica_ch[tecnica] busca cada técnica por su nombre y devuelve
# el nombre corto. unname() le saca los nombres al resultado.
# as.character() asegura que busque por el TEXTO: si tecnica fuera un
# factor, R buscaría por posición (1, 2, 3...) y daría cualquier cosa.
# Si una técnica no está en la lista, da NA, y coalesce() la reemplaza por
# el nombre original (coalesce = "el primer valor que no sea NA").
pruebas_ch_cg <- pruebas_ch_cg |>
  mutate(tecnica_corta = coalesce(unname(nombres_tecnica_ch[as.character(tecnica)]), tecnica))

pruebas_ch_emb <- pruebas_ch_emb |>
  mutate(tecnica_corta = coalesce(unname(nombres_tecnica_ch[as.character(tecnica)]), tecnica))

## CONTROL ##
# Si en tecnica_corta ves un nombre largo, esa técnica no está en la lista
# (o está escrita distinto, por ejemplo con "¿").
pruebas_ch_cg  |> count(tecnica, tecnica_corta)
pruebas_ch_emb |> count(tecnica, tecnica_corta)


# 5.4 Resultado simplificado ----------------------------------------------
# Unificamos vocabularios distintos en Positivo / Negativo / Indeterminado.
# Creamos una columna nueva y dejamos "resultado" sin tocar.
# %in% pregunta "¿el valor está en esta lista?".
pruebas_ch_cg <- pruebas_ch_cg |>
  mutate(resultado_simple = case_when(
    resultado %in% c("Positivo", "Detectable", "Reactivo")       ~ "Positivo",
    resultado %in% c("Negativo", "No detectable", "No reactivo") ~ "Negativo",
    resultado == "Ausencia"                                      ~ "Negativo",       # ⚠️ a confirmar
    resultado %in% c("Indeterminado", "En estudio")              ~ "Indeterminado",  # ⚠️ a decidir
    is.na(resultado)                                             ~ NA_character_,
    TRUE                                                         ~ "REVISAR"         # valores nuevos
  ))

# En embarazadas usamos el mismo criterio. Si aparecen valores que no están
# en la lista, van a quedar como "REVISAR": decidí a qué categoría van y
# agregalos acá Y en congénito, para que las dos bases sigan iguales.
pruebas_ch_emb <- pruebas_ch_emb |>
  mutate(resultado_simple = case_when(
    resultado %in% c("Positivo", "Detectable", "Reactivo")       ~ "Positivo",
    resultado %in% c("Negativo", "No detectable", "No reactivo") ~ "Negativo",
    resultado == "Ausencia"                                      ~ "Negativo",       # ⚠️ a confirmar
    resultado %in% c("Indeterminado", "En estudio")              ~ "Indeterminado",  # ⚠️ a decidir
    is.na(resultado)                                             ~ NA_character_,
    TRUE                                                         ~ "REVISAR"         # valores nuevos
  ))

## CONTROL ##
# Cada valor original y a qué grupo fue, técnica por técnica.
pruebas_ch_cg  |> count(tecnica_corta, resultado, resultado_simple)
pruebas_ch_emb |> count(tecnica_corta, resultado, resultado_simple)


# 5.5 Variables del caso --------------------------------------------------
# Son las que tienen UN solo valor por caso (se repiten igual en todas sus
# filas). Se le pasan a la función (sección 7).

# ¿Cómo saber cuáles son? Para cada caso contamos cuántos valores distintos
# tiene cada columna (n_distinct) y nos quedamos con el máximo.
# Si el máximo es 1, la columna nunca cambia dentro de un caso: es del caso.
# (n_distinct cuenta NA como un valor más: "dato" y NA en un mismo caso da 2.)
pruebas_ch_cg |>
  group_by(id_evento_caso) |>
  summarise(across(everything(), n_distinct)) |>
  summarise(across(-id_evento_caso, max)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "max_valores_por_caso") |>
  arrange(max_valores_por_caso) |>
  View()

pruebas_ch_emb |>
  group_by(id_evento_caso) |>
  summarise(across(everything(), n_distinct)) |>
  summarise(across(-id_evento_caso, max)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "max_valores_por_caso") |>
  arrange(max_valores_por_caso) |>
  View()

cols_caso_ch_cg <- c("edad_diagnostico", "departamento_residencia", "fecha_apertura",
                     "evento", "establecimiento_de_carga", "fecha_consulta",
                     "clasificacion_manual_del_caso", "tratamiento", "sexo_legal",
                     "estadio_clinico", "dni_del_responsable_a_cargo", "nro_doc",
                     "grupo_etario_diagnostico")

# ⚠️ EMBARAZADAS: arrancamos con la misma lista que congénito, pero las
# columnas pueden no ser las mismas. Ajustala con la tabla de arriba: tienen que
# estar las columnas con máximo 1 que NO sean de laboratorio.
cols_caso_ch_emb <- c("edad_diagnostico", "departamento_residencia", "fecha_apertura",
                      "evento", "establecimiento_de_carga", "fecha_consulta",
                      "clasificacion_manual_del_caso", "tratamiento", "sexo_legal",
                      "estadio_clinico", "dni_del_responsable_a_cargo", "nro_doc",
                      "grupo_etario_diagnostico")

## CONTROL ## Columnas de la lista que NO existen en la base.
# Tiene que dar character(0). Si aparece alguna, sacala de la lista: si no,
# la función de la sección 6 va a frenar.
setdiff(cols_caso_ch_cg,  names(pruebas_ch_cg))
setdiff(cols_caso_ch_emb, names(pruebas_ch_emb))


# 5.6 Resumen de la preparación -------------------------------------------
nrow(base_original_ch_cg)                 # filas originales (391)
nrow(pruebas_ch_cg)                       # filas sin duplicados
n_distinct(pruebas_ch_cg$id_evento_caso)  # casos: tiene que seguir dando 167

nrow(base_original_ch_emb)
nrow(pruebas_ch_emb)
n_distinct(pruebas_ch_emb$id_evento_caso) # casos: lo usamos en la sección 7



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
#   los cambia de lugar. Sirve igual para congénito y para embarazadas.
#
# CÓMO LO HACE
#   Cada columna de la base pertenece a uno de tres niveles:
#     - CASO    -> un solo valor por caso (edad, sexo, clasificación...)
#     - MUESTRA -> material tomado en una fecha (fecha, tipo, establecimiento)
#     - PRUEBA  -> cada técnica aplicada a una muestra, con su resultado
#   La función arma una tabla para cada nivel y después las une por caso.
#   Las muestras se numeran por orden de fecha (m1, m2, m3...) y cada una
#   forma un bloque de columnas con sus datos y sus técnicas.
#
# EJEMPLO
#   Entrada (3 filas del caso 1001):
#     id    fecha       muestra  tecnica  resultado
#     1001  10/01/2025  Suero    ELISA    Positivo
#     1001  10/01/2025  Suero    HAI      Positivo
#     1001  05/12/2025  Suero    ELISA    Negativo
#
#   Salida (1 fila):
#     id    m1_fecha    m1_muestra  m1_ELISA  m1_HAI    m2_fecha    m2_muestra  m2_ELISA
#     1001  10/01/2025  Suero       Positivo  Positivo  05/12/2025  Suero       Negativo
#
# ARGUMENTOS
#   base          : base multirregistro preparada (sección 5, con tecnica_corta)
#   cols_caso     : nombres de las variables del caso (sección 5.5)
#   col_resultado : qué columna de resultado usar:
#                   "resultado_simple" (Positivo/Negativo/Indeterminado) o
#                   "resultado" (valores originales)
#
# DEVUELVE
#   Una tabla (tibble) con una fila por caso: las variables del caso,
#   n_muestras, n_pruebas y un bloque de columnas por muestra.
#   Si la misma técnica se repite en una misma muestra con resultados
#   distintos, quedan unidos con ";" (por ejemplo, "Positivo;Negativo").
#   En la sección 7 se buscan esos casos.
# -------------------------------------------------------------------------

reordenar_por_caso_ch <- function(base, cols_caso, col_resultado = "resultado_simple") {
  
  # ---- Control de nombres -------------------------------------------------
  # Si falta alguna columna, la función frena y dice cuál.
  necesarias <- c("id_evento_caso", "fecha_de_toma_de_muestra", "muestra",
                  "establecimiento_diagnostico", "tecnica_corta",
                  col_resultado, cols_caso)
  faltan <- setdiff(necesarias, names(base))
  if (length(faltan) > 0) {
    stop("Faltan estas columnas en la base: ", paste(faltan, collapse = ", "),
         "\nRevisá names(base).")
  }
  
  
  # ---- Función auxiliar ----------------------------------------------------
  # Une los valores distintos de un grupo con ";" para no perder ninguno.
  # Ejemplo: c("Positivo", "Positivo", NA) -> "Positivo"
  #          c("Positivo", "Negativo")     -> "Positivo;Negativo"
  #          c(NA, NA)                     -> NA
  juntar <- function(x) {
    x <- unique(na.omit(x))
    if (length(x) == 0) NA_character_ else paste(x, collapse = ";")
  }
  
  
  # ---- PASO 1. Numerar las muestras de cada caso --------------------------
  # Una muestra = una fecha de toma + un tipo de muestra.
  # (El mismo día se puede tomar suero para ELISA y sangre entera para PCR:
  #  son dos muestras distintas.)
  # Ordenamos por fecha y numeramos dentro de cada caso: 1, 2, 3...
  # consecutive_id() da el mismo número a las filas con la misma combinación
  # de fecha y tipo de muestra. Las filas sin fecha quedan al final.
  base <- base |>
    arrange(id_evento_caso, fecha_de_toma_de_muestra, muestra) |>
    group_by(id_evento_caso) |>
    mutate(n_muestra = consecutive_id(fecha_de_toma_de_muestra, muestra)) |>
    ungroup()
  
  
  # ---- PASO 2. Tabla de casos (nivel CASO) ---------------------------------
  # Nos quedamos con las variables del caso y sacamos las repeticiones.
  # Si cada variable tiene un solo valor por caso, queda una fila por caso.
  casos <- base |>
    select(id_evento_caso, all_of(cols_caso)) |>
    distinct()
  
  # Control: si quedaron más filas que casos, alguna variable de cols_caso
  # cambia dentro de un mismo caso. La función frena en vez de elegir un
  # valor. (La tabla de la sección 5.5 muestra cuál es: máximo mayor a 1.)
  if (nrow(casos) != n_distinct(base$id_evento_caso)) {
    stop("Alguna variable de cols_caso cambia dentro de un mismo caso. ",
         "Mirá la tabla de la sección 5.5.")
  }
  
  # Aviso: columnas que no pasan a la tabla final porque no son del caso ni
  # se usan como datos de muestra o prueba. En congénito son determinacion,
  # semana_epi_muestra y la columna de resultado que no se eligió; las dos
  # primeras se reconstruyen a partir de la técnica y de la fecha.
  cols_laboratorio <- c("fecha_de_toma_de_muestra", "muestra",
                        "establecimiento_diagnostico", "tecnica",
                        "tecnica_corta", col_resultado, "n_muestra")
  no_incluidas <- setdiff(names(base), c("id_evento_caso", cols_caso, cols_laboratorio))
  if (length(no_incluidas) > 0) {
    message("Columnas que no pasan a la tabla final: ",
            paste(no_incluidas, collapse = ", "))
  }
  
  
  # ---- PASO 3. Tabla de muestras (nivel MUESTRA) ---------------------------
  # Una fila por caso y muestra, con su fecha, tipo y establecimiento.
  # Sacamos las filas sin fecha ni tipo de muestra (casos sin estudios), para
  # no crear bloques vacíos.
  muestras <- base |>
    filter(!(is.na(fecha_de_toma_de_muestra) & is.na(muestra))) |>
    group_by(id_evento_caso, n_muestra) |>
    summarise(fecha           = first(fecha_de_toma_de_muestra),
              muestra         = first(muestra),
              establecimiento = juntar(establecimiento_diagnostico),
              .groups = "drop")
  
  
  # ---- PASO 4. Tabla de resultados (nivel PRUEBA) --------------------------
  # Una fila por caso, muestra y técnica, con su resultado.
  # - Sacamos las filas sin técnica ni resultado (no hay prueba que guardar).
  # - Si hay resultado pero falta la técnica, la llamamos "SinTecnica".
  # - Si hay técnica pero falta el resultado, ponemos "Sin resultado", para
  #   distinguir "se hizo y no hay resultado" de "no se hizo" (celda vacía).
  # - n_filas cuenta cuántas pruebas había: si es más de 1, la misma técnica
  #   se repitió en la misma muestra y juntar() une los valores con ";".
  resultados <- base |>
    filter(!(is.na(tecnica_corta) & is.na(.data[[col_resultado]]))) |>
    mutate(tecnica_corta = coalesce(tecnica_corta, "SinTecnica")) |>
    group_by(id_evento_caso, n_muestra, tecnica_corta) |>
    summarise(n_filas   = n(),
              resultado = replace_na(juntar(.data[[col_resultado]]), "Sin resultado"),
              .groups = "drop")
  
  
  # ---- PASO 5. Pasar muestras y resultados a lo ancho ---------------------
  # pivot_wider() convierte filas en columnas.
  # Muestras: cada muestra pasa a ser un bloque de columnas
  #   m1_fecha, m1_muestra, m1_establecimiento, m2_fecha, ...
  muestras_ancho <- muestras |>
    pivot_wider(names_from  = n_muestra,
                values_from = c(fecha, muestra, establecimiento),
                names_glue  = "m{n_muestra}_{.value}")
  
  # Resultados: cada técnica de cada muestra pasa a ser una columna
  #   m1_ELISA, m1_HAI, m2_PCR, ...
  resultados_ancho <- resultados |>
    select(-n_filas) |>
    pivot_wider(names_from  = c(n_muestra, tecnica_corta),
                values_from = resultado,
                names_glue  = "m{n_muestra}_{tecnica_corta}")
  
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
  # Partimos de la tabla de casos (una fila por caso) y le pegamos lo demás
  # con left_join, así ningún caso se pierde aunque no tenga estudios.
  final <- casos |>
    left_join(conteos,          by = "id_evento_caso") |>
    left_join(muestras_ancho,   by = "id_evento_caso") |>
    left_join(resultados_ancho, by = "id_evento_caso")
  
  
  # ---- PASO 7. Ordenar las columnas muestra por muestra -------------------
  # Queremos ver juntas todas las columnas de m1, después las de m2, etc.:
  #   m1_fecha, m1_muestra, m1_establecimiento, m1_ELISA, m1_HAI, m2_fecha...
  orden <- c()
  for (k in sort(unique(c(muestras$n_muestra, resultados$n_muestra)))) {
    m <- paste0("m", k, "_")
    orden <- c(orden,
               paste0(m, c("fecha", "muestra", "establecimiento")),
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
# col_resultado no se escribe: usa el valor por defecto, "resultado_simple".
ChCongenito_caso   <- reordenar_por_caso_ch(pruebas_ch_cg,  cols_caso = cols_caso_ch_cg)
ChEmbarazadas_caso <- reordenar_por_caso_ch(pruebas_ch_emb, cols_caso = cols_caso_ch_emb)


# 7.2 Verificaciones (todas tienen que dar TRUE) --------------------------

# Una fila por caso
nrow(ChCongenito_caso)   == n_distinct(pruebas_ch_cg$id_evento_caso)    # 167
nrow(ChEmbarazadas_caso) == n_distinct(pruebas_ch_emb$id_evento_caso)

# No se perdió ninguna prueba: las pruebas contadas en la tabla por caso
# (n_pruebas) tienen que ser tantas como las filas con técnica o resultado
# en la base multirregistro.
sum(ChCongenito_caso$n_pruebas) ==
  nrow(filter(pruebas_ch_cg, !(is.na(tecnica_corta) & is.na(resultado_simple))))

sum(ChEmbarazadas_caso$n_pruebas) ==
  nrow(filter(pruebas_ch_emb, !(is.na(tecnica_corta) & is.na(resultado_simple))))


# 7.3 Columnas de técnica de la tabla por caso ----------------------------
# Las guardamos porque las usamos en la sección 8.2.
# La expresión regular toma las columnas m1_..., m2_... que NO son fecha,
# muestra ni establecimiento, o sea, las de técnica.
#   ^m\\d+_  -> empieza con "m", uno o más números y "_"
#   (?!...)  -> "y lo que sigue NO es..." (por eso perl = TRUE)
cols_tecnica_ch_cg <- grep("^m\\d+_(?!fecha$|muestra$|establecimiento$)",
                           names(ChCongenito_caso), value = TRUE, perl = TRUE)

cols_tecnica_ch_emb <- grep("^m\\d+_(?!fecha$|muestra$|establecimiento$)",
                            names(ChEmbarazadas_caso), value = TRUE, perl = TRUE)


# 7.4 Revisión: resultados distintos en una misma muestra -----------------
# Casos con la misma técnica repetida en una muestra y resultados distintos
# (celdas con ";", por ejemplo "Positivo;Negativo"). Conviene revisarlos.
#   if_any() da TRUE si ALGUNA de las columnas elegidas cumple la condición.
ChCongenito_caso |>
  filter(if_any(all_of(cols_tecnica_ch_cg), ~ str_detect(.x, ";"))) |>
  View()

ChEmbarazadas_caso |>
  filter(if_any(all_of(cols_tecnica_ch_emb), ~ str_detect(.x, ";"))) |>
  View()


# 7.5 Mirar algunos casos -------------------------------------------------
ChCongenito_caso |> filter(n_muestras == 0) |> View()                  # sin estudios
ChCongenito_caso |> filter(n_muestras == max(n_muestras)) |> View()    # los de más muestras

ChEmbarazadas_caso |> filter(n_muestras == 0) |> View()
ChEmbarazadas_caso |> filter(n_muestras == max(n_muestras)) |> View()

# Para volver al detalle de un caso en la base multirregistro, cambiá 1001
# por el id que quieras:
# pruebas_ch_cg |> filter(id_evento_caso == 1001) |> View()



# -------------------------------------------------------------------------
# 8. Clasificación de casos (embarazadas) ----------------------------------
# -------------------------------------------------------------------------
#
# Trabajamos sobre la tabla por caso (una fila por caso), así cada resultado
# que sale es UNO POR CASO y lo que contamos después son casos.
# La clasificación se guarda en una tabla NUEVA (clas_ch_emb):
# ChEmbarazadas_caso no se toca y sigue teniendo el dato crudo.
#
# CRITERIO
#   - Confirmado      : algún método directo positivo, o
#                       dos o más grupos de serología distintos positivos
#   - Descartado      : dos o más grupos de serología distintos negativos
#   - Sin laboratorio : el caso no tiene ninguna prueba
#   - A revisar       : todo lo demás (una sola serología, sin resultado...)
#
# Se cuentan GRUPOS de serología, no técnicas: ELISA + QL positivos son el
# mismo grupo (serología 2) y cuentan como UNA serología positiva.


# 8.1 Grupos de técnicas --------------------------------------------------
# Usan los nombres cortos de la sección 5.3 (los que quedaron en las columnas
# de la tabla por caso: m1_ELISA, m2_HAI...). Si cambia el criterio o aparece
# una técnica nueva, se edita SOLO acá. Valen también para congénito.
grupo_directo_ch <- c("PCR", "MH", "Microstrout", "Micrometodo", "GotaFresca")
grupo_sero1_ch   <- c("HAI")
grupo_sero2_ch   <- c("ELISA", "QL", "ECLIA")
grupo_sero3_ch   <- c("IFI")


# 8.2 Control: ¿alguna técnica quedó sin grupo? ---------------------------
# sub("^m\\d+_", "", ...) borra el prefijo m1_, m2_... y deja la técnica.
# setdiff() muestra las técnicas de la tabla que no están en ningún grupo.
# Tiene que dar character(0): si aparece algo (por ejemplo "SinTecnica"),
# esos resultados NO se cuentan para clasificar.
todas_las_tecnicas_ch <- c(grupo_directo_ch, grupo_sero1_ch, grupo_sero2_ch, grupo_sero3_ch)

setdiff(unique(sub("^m\\d+_", "", cols_tecnica_ch_emb)), todas_las_tecnicas_ch)
setdiff(unique(sub("^m\\d+_", "", cols_tecnica_ch_cg)),  todas_las_tecnicas_ch)  # para la sección 9


# 8.3 Clasificación -------------------------------------------------------
# PASO 1. Una columna por grupo con su resultado en el caso.
#   paste0("_", grupo_sero2_ch) -> c("_ELISA", "_QL", "_ECLIA")
#   ends_with(...)              -> todas las columnas de esas técnicas, en
#                                  todas las muestras (m1_ELISA, m2_QL...)
#   across(..., ~ str_detect(.x, "Positivo"))
#                               -> en cada una de esas columnas pregunta
#                                  "¿dice Positivo?" (TRUE / FALSE / NA si
#                                  la celda está vacía). str_detect() y no
#                                  ==, por las celdas unidas con ";"
#                                  (por ejemplo "Negativo;Positivo").
#   rowSums(..., na.rm = TRUE)  -> cuenta, en cada caso, cuántos TRUE hubo
#                                  (las celdas vacías no cuentan)
#   > 0                         -> ¿hubo al menos uno?
#   El orden importa: si el grupo tiene un positivo, queda "Positivo"
#   aunque también tenga un negativo.
#   Si un grupo no tiene columnas en la base (por ejemplo, no hay IFI), la
#   cuenta da 0 y el grupo queda NA.
#
# PASO 2. Contar cuántos grupos de serología dieron positivo y negativo.
#   (sero1 %in% "Positivo") da TRUE/FALSE; al sumar, TRUE vale 1.
#   Usamos %in% y no == porque con NA, == da NA y la suma entera da NA.
#
# PASO 3. Clasificar. El orden del case_when() es la jerarquía: cada caso
#   se queda con la PRIMERA condición que cumple.

clas_ch_emb <- ChEmbarazadas_caso |>
  mutate(
    # PASO 1 -------------------------------------------------------------
    directo = case_when(
      rowSums(across(ends_with(paste0("_", grupo_directo_ch)),
                     ~ str_detect(.x, "Positivo")), na.rm = TRUE) > 0 ~ "Positivo",
      rowSums(across(ends_with(paste0("_", grupo_directo_ch)),
                     ~ str_detect(.x, "Negativo")), na.rm = TRUE) > 0 ~ "Negativo",
      TRUE ~ NA_character_
    ),
    sero1 = case_when(
      rowSums(across(ends_with(paste0("_", grupo_sero1_ch)),
                     ~ str_detect(.x, "Positivo")), na.rm = TRUE) > 0 ~ "Positivo",
      rowSums(across(ends_with(paste0("_", grupo_sero1_ch)),
                     ~ str_detect(.x, "Negativo")), na.rm = TRUE) > 0 ~ "Negativo",
      TRUE ~ NA_character_
    ),
    sero2 = case_when(
      rowSums(across(ends_with(paste0("_", grupo_sero2_ch)),
                     ~ str_detect(.x, "Positivo")), na.rm = TRUE) > 0 ~ "Positivo",
      rowSums(across(ends_with(paste0("_", grupo_sero2_ch)),
                     ~ str_detect(.x, "Negativo")), na.rm = TRUE) > 0 ~ "Negativo",
      TRUE ~ NA_character_
    ),
    sero3 = case_when(
      rowSums(across(ends_with(paste0("_", grupo_sero3_ch)),
                     ~ str_detect(.x, "Positivo")), na.rm = TRUE) > 0 ~ "Positivo",
      rowSums(across(ends_with(paste0("_", grupo_sero3_ch)),
                     ~ str_detect(.x, "Negativo")), na.rm = TRUE) > 0 ~ "Negativo",
      TRUE ~ NA_character_
    ),
    
    # PASO 2 -------------------------------------------------------------
    n_sero_pos = (sero1 %in% "Positivo") + (sero2 %in% "Positivo") + (sero3 %in% "Positivo"),
    n_sero_neg = (sero1 %in% "Negativo") + (sero2 %in% "Negativo") + (sero3 %in% "Negativo"),
    
    # PASO 3 -------------------------------------------------------------
    clasificacion = case_when(
      directo %in% "Positivo" ~ "Confirmado",
      n_sero_pos >= 2         ~ "Confirmado",
      n_sero_neg >= 2         ~ "Descartado",
      n_pruebas == 0          ~ "Sin laboratorio",
      TRUE                    ~ "A revisar"
    )
  ) |>
  # Nos quedamos solo con el id y las columnas de la clasificación
  select(id_evento_caso, n_pruebas, directo, sero1, sero2, sero3,
         n_sero_pos, n_sero_neg, clasificacion)


# 8.4 Controles -----------------------------------------------------------

# Una fila por caso (tiene que dar TRUE)
nrow(clas_ch_emb) == nrow(ChEmbarazadas_caso)

# Qué combinaciones de grupos aparecen y cómo se clasificó cada una.
# Leelo como una tabla de verdad: sirve para comprobar que la regla se
# aplicó como queríamos.
clas_ch_emb |> count(directo, sero1, sero2, sero3, clasificacion)

# Conteo por clasificación (la tabla con formato va en la sección 10)
clas_ch_emb |> count(clasificacion)

# Mirar en detalle los casos "A revisar", con todos sus datos crudos:
# left_join() les pega la tabla por caso por id_evento_caso.
clas_ch_emb |>
  filter(clasificacion == "A revisar") |>
  left_join(ChEmbarazadas_caso, by = c("id_evento_caso", "n_pruebas")) |>
  View()



# -------------------------------------------------------------------------
# 9. Clasificación de casos (congénito) ------------------------------------
# -------------------------------------------------------------------------
#
# Usa los mismos grupos de técnicas de la sección 8.1 y la misma forma de
# resumir cada grupo. Lo nuevo es la EDAD: en un bebé, la serología solo
# cuenta desde los 10 meses (antes puede detectar anticuerpos de la madre).
#
# EL PROBLEMA: la base no tiene fecha de nacimiento. edad_diagnostico está
# en años (0 para neonatos y posneonatos), así que no sirve para el corte.
#
# LA APROXIMACIÓN: tomamos la fecha de apertura como si fuera la fecha de
# nacimiento. Si la última muestra es de 10 meses o más después de la
# apertura, las serologías cuentan.
#   - Funciona bien en NEONATOS (hasta 28 días): se notificaron casi al
#     nacer, así que la apertura es casi el nacimiento.
#   - ⚠️ En POSNEONATOS y en los MAYORES de 1 año la regla subestima la edad:
#     un caso notificado a los 3 años con la muestra del mismo día queda en
#     mayor_10m = "No" y se clasifica "Sospechoso", aunque su serología sí
#     cuenta. Nunca se equivoca hacia el "Sí", pero puede dejar casos
#     confirmables como sospechosos.
#   - Esos casos hay que revisarlos a mano (control de la 9.3) o, mejor,
#     usar la fecha de nacimiento si la base la tiene (9.4).
#
# CRITERIO
#   - Confirmado      : estadio clínico positivo, o directo positivo (a
#                       cualquier edad), o 10 meses o más con 2 o más grupos
#                       de serología positivos
#   - Descartado      : estadio clínico negativo, o 10 meses o más con 2 o
#                       más grupos de serología negativos
#   - Sin laboratorio : el caso no tiene ninguna prueba
#   - Sospechoso      : menor de 10 meses (según la aproximación de arriba)
#                       sin directo positivo (en seguimiento)
#   - A revisar       : todo lo demás (10 meses o más con una sola
#                       serología, casos sin fecha de muestra, etc.)


# 9.1 Fecha de la última muestra de cada caso ------------------------------
# Se calcula en la base multirregistro, donde todas las fechas están en una
# sola columna. filter() saca las filas sin fecha para que max() no dé -Inf.
fecha_ultima_ch_cg <- pruebas_ch_cg |>
  filter(!is.na(fecha_de_toma_de_muestra)) |>
  group_by(id_evento_caso) |>
  summarise(fecha_ultima_muestra = max(fecha_de_toma_de_muestra))


# 9.2 Clasificación -------------------------------------------------------
clas_ch_cg <- ChCongenito_caso |>
  left_join(fecha_ultima_ch_cg, by = "id_evento_caso") |>
  mutate(
    # PASO 0 -------------------------------------------------------------
    # Fecha en que se cumplen 10 meses desde la apertura.
    # %m+% suma meses sin pasarse de mes (31/01 + 1 mes = 28/02, no 03/03).
    # Si la última muestra es de esa fecha o posterior, las serologías cuentan.
    fecha_10m = fecha_apertura %m+% months(10),
    mayor_10m = case_when(
      fecha_ultima_muestra >= fecha_10m ~ "Sí",
      fecha_ultima_muestra <  fecha_10m ~ "No",
      TRUE                              ~ "No se sabe"   # sin fecha de muestra
    ),
    
    # PASO 1 (igual que en embarazadas) ---------------------------------
    directo = case_when(
      rowSums(across(ends_with(paste0("_", grupo_directo_ch)),
                     ~ str_detect(.x, "Positivo")), na.rm = TRUE) > 0 ~ "Positivo",
      rowSums(across(ends_with(paste0("_", grupo_directo_ch)),
                     ~ str_detect(.x, "Negativo")), na.rm = TRUE) > 0 ~ "Negativo",
      TRUE ~ NA_character_
    ),
    sero1 = case_when(
      rowSums(across(ends_with(paste0("_", grupo_sero1_ch)),
                     ~ str_detect(.x, "Positivo")), na.rm = TRUE) > 0 ~ "Positivo",
      rowSums(across(ends_with(paste0("_", grupo_sero1_ch)),
                     ~ str_detect(.x, "Negativo")), na.rm = TRUE) > 0 ~ "Negativo",
      TRUE ~ NA_character_
    ),
    sero2 = case_when(
      rowSums(across(ends_with(paste0("_", grupo_sero2_ch)),
                     ~ str_detect(.x, "Positivo")), na.rm = TRUE) > 0 ~ "Positivo",
      rowSums(across(ends_with(paste0("_", grupo_sero2_ch)),
                     ~ str_detect(.x, "Negativo")), na.rm = TRUE) > 0 ~ "Negativo",
      TRUE ~ NA_character_
    ),
    sero3 = case_when(
      rowSums(across(ends_with(paste0("_", grupo_sero3_ch)),
                     ~ str_detect(.x, "Positivo")), na.rm = TRUE) > 0 ~ "Positivo",
      rowSums(across(ends_with(paste0("_", grupo_sero3_ch)),
                     ~ str_detect(.x, "Negativo")), na.rm = TRUE) > 0 ~ "Negativo",
      TRUE ~ NA_character_
    ),
    
    # PASO 2 (igual que en embarazadas) ---------------------------------
    n_sero_pos = (sero1 %in% "Positivo") + (sero2 %in% "Positivo") + (sero3 %in% "Positivo"),
    n_sero_neg = (sero1 %in% "Negativo") + (sero2 %in% "Negativo") + (sero3 %in% "Negativo"),
    
    # PASO 3 -------------------------------------------------------------
    # El orden es la jerarquía: se queda con la PRIMERA condición que se cumple.
    # ⚠️ Los textos de estadio_clinico tienen que coincidir EXACTO con los de
    # la base: revisalos con count(ChCongenito_caso, estadio_clinico).
    clasificacion = case_when(
      estadio_clinico %in% c("Diagnóstico positivo por parasitología constatado",
                             "Diagnóstico positivo por 2 técnicas serológicas")   ~ "Confirmado",
      estadio_clinico %in% "Serología negativa por 2 técnicas a partir de 10 m"   ~ "Descartado",
      directo %in% "Positivo"                                                     ~ "Confirmado",
      mayor_10m == "Sí" & n_sero_pos >= 2                                         ~ "Confirmado",
      mayor_10m == "Sí" & n_sero_neg >= 2                                         ~ "Descartado",
      n_pruebas == 0                                                              ~ "Sin laboratorio",
      mayor_10m == "No"                                                           ~ "Sospechoso",
      TRUE                                                                        ~ "A revisar"
    )
  ) |>
  select(id_evento_caso, grupo_etario_diagnostico, estadio_clinico, n_pruebas,
         fecha_apertura, fecha_10m, fecha_ultima_muestra, mayor_10m,
         directo, sero1, sero2, sero3, n_sero_pos, n_sero_neg,
         clasificacion, clasificacion_manual_del_caso)


# 9.3 Controles -----------------------------------------------------------

# Una fila por caso (tiene que dar TRUE)
nrow(clas_ch_cg) == nrow(ChCongenito_caso)

# ¿Cómo quedó la edad, según el grupo etario?
# "No se sabe" = casos sin fecha de muestra.
clas_ch_cg |> count(grupo_etario_diagnostico, mayor_10m)

# ⚠️ Casos que la regla puede haber clasificado mal: quedaron "Sospechoso"
# por mayor_10m = "No", pero NO son neonatos (al notificarse ya tenían más
# de 28 días). Revisar su edad real: si tenían 10 meses o más en la última
# muestra, su serología cuenta.
clas_ch_cg |>
  filter(clasificacion == "Sospechoso",
         !str_detect(grupo_etario_diagnostico, "Neonato")) |>
  count(grupo_etario_diagnostico)

# Qué combinaciones aparecen y cómo se clasificó cada una
clas_ch_cg |> count(mayor_10m, directo, sero1, sero2, sero3, clasificacion)

# Comparar con la clasificación manual cargada en el sistema.
# Donde no coinciden (por ejemplo "Confirmado" vs "Caso sospechoso"),
# conviene mirar el caso: puede ser un error de carga o un límite de esta regla.
clas_ch_cg |> count(clasificacion, clasificacion_manual_del_caso)

# Conteo por clasificación (la tabla con formato va en la sección 10)
clas_ch_cg |> count(clasificacion)

# Mirar en detalle los casos "A revisar", con todos sus datos crudos
clas_ch_cg |>
  filter(clasificacion == "A revisar") |>
  select(id_evento_caso, mayor_10m, clasificacion) |>
  left_join(ChCongenito_caso, by = "id_evento_caso") |>
  View()


# 9.4 Si la base tiene FECHA DE NACIMIENTO ---------------------------------
# Con fecha de nacimiento se calcula la edad real del bebé en la última
# muestra, sin depender del grupo etario ni de la fecha de apertura.
# Hay que cambiar TRES cosas (el resto del script queda igual):
#
# (1) Sección 5.5: agregar la fecha a las variables del caso.
#     Después de clean_names() la columna se llama fecha_nacimiento, y la
#     sección 3.2 ya la pasa a fecha porque empieza con "fecha".
#     Antes, verificá en la tabla de 5.5 que dé máximo 1.
#
#     cols_caso_ch_cg <- c(cols_caso_ch_cg, "fecha_nacimiento")
#
#     (y volvé a correr la función de la sección 7)
#
# (2) Sección 9.2, PASO 0: dejar fecha_10m como está y reemplazar
#     mayor_10m por:
#
#     edad_meses = interval(fecha_nacimiento, fecha_ultima_muestra) %/% months(1),
#     mayor_10m = case_when(
#       edad_meses >= 10 ~ "Sí",
#       edad_meses <  10 ~ "No",
#       # Si a algún caso le falta la fecha de nacimiento, se usa la regla
#       # de la apertura como respaldo:
#       fecha_ultima_muestra >= fecha_10m ~ "Sí",
#       fecha_ultima_muestra <  fecha_10m ~ "No",
#       TRUE ~ "No se sabe"
#     ),
#
#     case_when() usa la primera condición que se cumple: si hay fecha de
#     nacimiento, manda la edad real; solo los casos sin esa fecha caen en
#     la regla de la apertura.
#
# (3) Sección 9.2, select(): agregar fecha_nacimiento y edad_meses, para
#     ver con qué edad se clasificó cada caso.
#
# Los PASOS 1 a 3 y la sección 9.3 no cambian: la clasificación solo usa
# mayor_10m, no importa cómo se calculó.



# -------------------------------------------------------------------------
# 10. Tabla final de clasificación -----------------------------------------
# -------------------------------------------------------------------------
# Ya controlamos en 8.4 y 9.3 que las reglas hicieron lo que queríamos.
# Ahora contamos casos por clasificación y armamos la tabla para presentar.


# 10.1 Conteo de casos por clasificación ----------------------------------
# count() cuenta casos por clasificación; pct es la proporción sobre el
# total de casos de esa base. Cada base tiene sus propias columnas
# (n_cg / pct_cg y n_emb / pct_emb) para después ponerlas lado a lado.
conteo_clas_ch_cg <- clas_ch_cg |>
  count(clasificacion, name = "n_cg") |>
  mutate(pct_cg = n_cg / sum(n_cg))

conteo_clas_ch_emb <- clas_ch_emb |>
  count(clasificacion, name = "n_emb") |>
  mutate(pct_emb = n_emb / sum(n_emb))

conteo_clas_ch_cg
conteo_clas_ch_emb


# 10.2 Tabla final: las dos bases lado a lado -----------------------------
# Orden en que queremos ver las categorías. "Sospechoso" existe solo en
# congénito: en embarazadas va a quedar en 0.
# Si agregás una categoría en algún case_when(), sumala acá.
orden_clas_ch <- c("Confirmado", "Sospechoso", "Descartado",
                   "Sin laboratorio", "A revisar")

# full_join() une las dos tablas por clasificación y conserva todas las
# categorías, aunque aparezcan en una sola base (ahí queda NA, que
# reemplazamos por 0).
# arrange(match(...)) ordena las filas según orden_clas_ch.
tabla_clas_ch <- full_join(conteo_clas_ch_cg, conteo_clas_ch_emb,
                           by = "clasificacion") |>
  mutate(across(c(n_cg, pct_cg, n_emb, pct_emb), ~ replace_na(.x, 0))) |>
  arrange(match(clasificacion, orden_clas_ch))

# Fila de total: summarise() suma cada columna numérica y bind_rows() la
# agrega abajo. Los % del total tienen que dar 1 (100 %).
tabla_clas_ch <- tabla_clas_ch |>
  bind_rows(
    tabla_clas_ch |>
      summarise(clasificacion = "Total",
                across(c(n_cg, pct_cg, n_emb, pct_emb), sum))
  )

tabla_clas_ch   # así se ve en la consola, antes de darle formato


# La misma tabla con formato gt
#   tab_spanner()  -> un título que agrupa columnas (una base arriba de n y %)
#   fmt_percent()  -> muestra las proporciones como %
#   tab_style()    -> pone en negrita la fila de total
tabla_final_clas_ch <- tabla_clas_ch |>
  gt() |>
  tab_header(
    title    = "Clasificación final de los casos de Chagas",
    subtitle = "Casos notificados (una fila por id_evento_caso)"
  ) |>
  tab_spanner(label = "Chagas congénito",        columns = c(n_cg, pct_cg)) |>
  tab_spanner(label = "Chagas en embarazadas",   columns = c(n_emb, pct_emb)) |>
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
    "Confirmado: método directo positivo, o 2 o más grupos de serología positivos (en congénito, desde los 10 meses)."
  ) |>
  tab_source_note(
    "Sospechoso: solo congénito; menor de 10 meses sin método directo positivo."
  ) |>
  tab_source_note(
    "⚠️ Congénito: la edad se aproxima con la fecha de apertura (ver sección 9)."
  )

tabla_final_clas_ch


# 10.3 Guardar las tablas (opcional) --------------------------------------
# dir.create() crea la carpeta "output" si no existe (si existe, no hace nada).
# dir.create(here("output"), showWarnings = FALSE)
# write_csv(ChCongenito_caso,   here("output", "ChCongenito_una_fila_por_caso.csv"))
# write_csv(ChEmbarazadas_caso, here("output", "ChEmbarazadas_una_fila_por_caso.csv"))
# write_csv(clas_ch_cg,         here("output", "ChCongenito_clasificacion.csv"))
# write_csv(clas_ch_emb,        here("output", "ChEmbarazadas_clasificacion.csv"))
# gtsave(tabla_final_clas_ch,   here("output", "tabla_clasificacion_chagas.html"))




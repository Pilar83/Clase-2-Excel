
# =============================================================================
#  CLASE: IMPORTACIÓN DE DATOS EN R
#  Cómo importar archivos CSV y Excel usando rutas relativas
# =============================================================================
#
#  Antes de empezar:
#  - Abrí siempre el PROYECTO de RStudio (el archivo .Rproj), no el script suelto.
#    Así R "se para" en la carpeta del proyecto y las rutas relativas funcionan.
#  - Los datos tienen que estar dentro de la carpeta "data" del proyecto:
#
#      mi_proyecto/
#      ├── mi_proyecto.Rproj
#      ├── data/
#      │   └── ChCongenito_curso.csv
#      └── scripts/
#          └── clase_importacion.R
#
#  Tip: los títulos de sección como el de abajo (texto seguido de ----) se
#  crean con Ctrl + Shift + R y permiten navegar el script desde el índice.
# =============================================================================



# 1. Instalación de paquetes -----------------------------------------------

# Se corre UNA SOLA VEZ por computadora. Después se puede comentar con #.
# Instalamos "pacman", un paquete que sirve para administrar otros paquetes.

# Importante: install.packages() necesita los nombres dentro de c( ).
# Si se escriben sueltos, R los toma como otros argumentos y da error.


install.packages(c("rio", "here", "tidyverse", "pacman"))

# 2. Carga de paquetes -----------------------------------------------------



#  Cargar (cada vez que se abre R o se reinicia la sesión)
library(rio)        # para importar (y exportar) datos
library(here)       # para construir rutas relativas
library(tidyverse)  # para manejo de datos y visualización

# pacman::p_load() carga los paquetes que le indiquemos.
# Ventaja: si algún paquete no está instalado, lo instala automáticamente.

pacman::p_load(
  rio,          # para importar (y exportar) datos
  here,         # para construir rutas relativas a los archivos
  tidyverse   # para manejo de datos y visualización
)


# 3. Rutas: absolutas vs. relativas ----------------------------------------

# Para importar un archivo, R necesita saber DÓNDE está. Hay dos formas de
# indicárselo:

# a) Ruta ABSOLUTA: la dirección completa en MI computadora.
#    Es frágil: en otra computadora esa carpeta no existe y el código falla.
#    EVITARLA.
#
#    import("C:/Users/nombre/Documents/curso/data/ChCongenito_curso.csv")

# b) Ruta RELATIVA: el camino desde la carpeta del proyecto.
#    Funciona en cualquier computadora que tenga el mismo proyecto.
#
#    import("data/ChCongenito_curso.csv")

# c) Con here(): armamos la ruta relativa indicando carpetas y archivo
#    separados por comas, sin preocuparnos por las barras / o \
#    (que cambian entre Windows y Mac).

here("data", "ChCongenito_curso.csv")   # muestra la ruta que se construye

# Para chequear antes de importar:
here()                     # ¿desde qué carpeta está partiendo R?
list.files(here("data"))   # ¿qué archivos hay en la carpeta data?



# 4. Importar un archivo ---------------------------------------------------

# El paquete "rio" se llama así por "R I/O" (Input/Output: entrada/salida).

# Sus funciones import() y export() manejan muchos formatos (.csv, .xlsx,
# .rds, .tsv, .sav, entre otros). rio mira la EXTENSIÓN del archivo y elige
# solo la herramienta adecuada para leerlo. Para CSV usa data.table::fread()
# y para Excel usa readxl, dos de las opciones más rápidas que existen.

# El paso final es poner la ruta de here() dentro de import().

# El operador de asignación <- guarda los datos como un objeto con un nombre
# (en este caso, ChCongenito), que aparece en el panel Environment.

ChCongenito_curso <- import(here("data", "ChCongenito_curso.csv"))




# --- Lo mismo con un archivo Excel (.xlsx) ---
# Es la MISMA función: solo cambia el nombre y la extensión del archivo.
# (Reemplazar "ejemplo.xlsx" por el nombre de un archivo real.)

base_excel <- import(here("data", "ChCongenito_excel.xlsx"))

# Por defecto lee la primera hoja. Para elegir otra, se indica su nombre:
# base_hoja2 <- import(here("data", "ejemplo.xlsx"), which = "hoja2")


# --- Problemas frecuentes ---

# Si aparece "cannot open file ... No such file or directory", revisar:
#   1. ¿Abrí el proyecto (.Rproj)?
#   2. ¿El nombre está bien escrito? (mayúsculas, tildes, extensión)
#   3. ¿El archivo está en la carpeta data?
# Recomendación: nombres de archivo sin espacios, sin tildes y sin ñ.
#   Bien:  encuesta_2025.csv     Mal:  Encuesta Año 2025.csv



# 5. Importar todos los archivos de una carpeta ----------------------------

# Paso 1: listamos los archivos CSV y Excel que hay en la carpeta data.
#   pattern = "\\.(csv|xlsx)$" -> solo los que terminan en .csv o .xlsx
#   full.names = TRUE          -> con la ruta completa, para poder abrirlos

archivos <- list.files(here("data"), pattern = "\\.(csv|xlsx)$", full.names = TRUE)

archivos   # conviene mirar qué encontró antes de importar


# Paso 2: importamos todos a la vez. Hay dos formas de hacerlo.

# a) Como objetos separados: cada archivo aparece en el Environment como un
#    objeto propio, con el nombre del archivo sin la extensión.
#    Precaución: los nombres de archivo no deben tener espacios ni empezar
#    con un número, porque se usan como nombres de objeto.

import_list(archivos) |>
  list2env(envir = .GlobalEnv)

# b) Como una lista: todas las bases quedan guardadas dentro de un solo objeto.
#    Útil cuando son muchos archivos y no queremos llenar el Environment.

# bases <- import_list(archivos)
# names(bases)       # nombres de las bases dentro de la lista
# bases$ChCongenito_curso   # para usar una en particular



# 6. Importar solo algunas columnas ---------------------------------------

# No siempre necesitamos toda la base. Leer solo las columnas que vamos a
# usar es más rápido y ocupa menos memoria.
# Ojo: la forma de hacerlo cambia según el formato del archivo.


# En Excel, range indica qué parte de la planilla leer, con las letras de
# las columnas como en Excel. Para columnas completas se usa cell_cols().

base_excel <- import(here("data", "ChCongenito_excel.xlsx"),
                     range = readxl::cell_cols("A:C"))       # columnas A a C


# En CSV, se usan select o drop:
#   select = las columnas que QUEREMOS traer
#   drop   = las columnas que NO queremos traer (se trae todo lo demás)
# Los nombres tienen que estar escritos exactamente igual que en la base
# (con espacios, tildes y mayúsculas).

#ChCongenito_colum  <- import(here("data", "ChCongenito_curso.csv"),
#                             select = c("Edad Diagnostico", "Técnica"))

#ChCongenito_colum2 <- import(here("data", "ChCongenito_curso.csv"),
#                             drop = c("Edad Diagnostico"))



# 7. Evitar la primera fila (doble encabezado) ----------------------------

# Algunas planillas tienen una fila de título o de agrupación ARRIBA de los
# nombres reales de las columnas. Si no le decimos nada, R toma esa primera
# fila como nombres de columna y los nombres reales quedan como datos.

# skip = 1 saltea la primera fila. Funciona igual en Excel y en CSV.
# header = TRUE indica que la fila siguiente tiene los nombres de las columnas.

# datos <- import(here("data", "base.csv"), skip = 1, header = TRUE)
  




# =============================================================================
#  EXPLORAR LA BASE ANTES DE MODIFICARLA
#  Estructura, tipos de datos y datos faltantes
# =============================================================================
#
#  Antes de limpiar o transformar una base, hay que CONOCERLA:
#  - ¿Cuántas filas y columnas tiene?
#  - ¿Qué tipo de dato tiene cada columna? ¿Es el que esperamos?
#  - ¿Hay datos faltantes? ¿Dónde y cuántos?
#  - ¿Hay faltantes "disfrazados" (NS/NC, 99, "Sin dato") que R no reconoce?
#
#   En esta etapa SOLO MIRAMOS. No modificamos la base.
# =============================================================================



# 1. Carga de paquetes -----------------------------------------------------

pacman::p_load(
  rio,          # para importar datos
  here,         # para rutas relativas
  tidyverse,    # para manejo de datos y visualización
  skimr,        # para obtener un resumen completo de la base
  janitor,      # para tablas de frecuencia y detección de duplicados
  naniar        # para visualizar datos faltantes
)



# 3. Primera mirada: el tamaño y la forma de la base -----------------------

View(ChCongenito_curso)    # abre la base como una planilla, para recorrerla

dim(ChCongenito_curso)     # cantidad de filas y columnas (en ese orden)
nrow(ChCongenito_curso)    # solo filas (cada fila suele ser un caso o registro)
ncol(ChCongenito_curso)    # solo columnas (cada columna es una variable)

names(ChCongenito_curso)   # los nombres de todas las columnas

head(ChCongenito_curso)    # las primeras 6 filas
tail(ChCongenito_curso)    # las últimas 6 filas

unique(ChCongenito_curso$`Grupo etario diagnóstico`)  # valores únicos por columna

# 4. Estructura: ¿qué tipo de dato tiene cada columna? ---------------------

glimpse(ChCongenito_curso)   #  muestra cada columna con su tipo y sus primeros valores.


# Los tipos más comunes que van a aparecer:
#   <chr>  texto (character)         ej: "Mendoza", "F"
#   <dbl>  número con decimales      ej: 3.5, 120
#   <int>  número entero             ej: 1, 2, 3
#   <lgl>  lógico (verdadero/falso)  ej: TRUE, FALSE
#   <date> fecha                     ej: 2025-03-14


# Qué revisar:
#   - ¿Hay números guardados como texto <chr>? Suele pasar si alguna celda
#     tiene letras o símbolos (por ejemplo "s/d" o "12 años").
#   - ¿Las fechas se reconocieron como <date> o quedaron como texto?

# Para ver el tipo de una sola columna:
class(ChCongenito_curso$`Fecha Apertura`)



# 5. Resumen general de la base --------------------------------------------



# skim(): un resumen más completo y ordenado por tipo de dato. Incluye
# cantidad de faltantes, porcentaje de completitud y un mini histograma
# para las variables numéricas.

skim(ChCongenito_curso)


# Por qué n_missing da 0 y empty no
# 
# Cuando import() lee un CSV, las celdas vacías de las columnas de texto 
# quedan como "", un texto sin caracteres, y no como NA. Para R, "" es un valor más, 
# así que no lo cuenta como faltante.
# 
# Por eso pasa esto:
#   
#   Tratamiento: n_missing es 0 y complete_rate es 1, 
# pero empty es 372 de 391 filas. En la práctica, la columna está casi vacía.
# Estadío clínico: tiene 382 vacíos.
# Fecha consulta: tiene 199 vacíos, que son aproximadamente el 51 %.

# Cómo hacer que R los reconozca como faltantes, lo mejor es declararlo en la importación
#  ver como aparecen los datos faltantes: "NS/NC", "Sin dato", "s/d", "-", 99, 
# 999, o una celda vacía ("")

ChCongenito_curso <- import(here("data", "ChCongenito_curso.csv"),
                      na.strings = c("", "NA"))


# Volvemos a chequear

skim(ChCongenito_curso)


# 6. Variables categóricas: ¿qué valores aparecen? -------------------------

# Las tablas de frecuencia muestran cada valor distinto y cuántas veces
# aparece. Son la mejor forma de detectar errores de carga, por ejemplo
# "Mendoza", "mendoza" y "MENDOZA" como si fueran tres provincias.

# Con tidyverse:
 ChCongenito_curso |> count(`Departamento Residencia`)

# Con janitor (agrega el porcentaje):
ChCongenito_curso |> tabyl(Técnica)

# Para ver los valores únicos de una columna:
unique(ChCongenito_curso$Técnica)



# 7. Datos faltantes (NA) --------------------------------------------------

# En R, un dato faltante se representa como NA ("Not Available").

# ¿Cuántos NA hay en total?
sum(is.na(ChCongenito_curso))

# ¿Cuántos NA hay en cada columna?
colSums(is.na(ChCongenito_curso))


# ¿Qué porcentaje de cada columna falta? (ordenado de mayor a menor)
sort(colMeans(is.na(ChCongenito_curso)) * 100, decreasing = TRUE)

# ¿Cuántas filas están completas, sin ningún NA?
sum(complete.cases(ChCongenito_curso))


# --- Visualizar los faltantes (paquete naniar) ---



# Gráfico con la cantidad de faltantes por variable:
gg_miss_var(ChCongenito_curso)



# =============================================================================
#  8. EJERCITACIÓN: filter, select, arrange y mutate (dplyr)

# =============================================================================
# Antes de empezar: chequeá con glimpse() que "Edad Diagnostico" sea numérica
# (<dbl> o <int>). Si es <chr>, convertirla: mutate(`Edad Diagnostico` = as.numeric(`Edad Diagnostico`))

# --- Los 4 verbos -------------------------------------------------------------
#  filter()  -> se queda con FILAS que cumplen una condición
#  select()  -> se queda con COLUMNAS
#  arrange() -> ORDENA las filas
#  mutate()  -> CREA o modifica columnas
# El pipe %>%  (Ctrl + Shift + M) pasa el resultado de un paso al siguiente.




# -------------------------------------------------------------------------
# --- 8.1 filter() -------------------------------------------------------------
ChCongenito_curso |> filter(`Edad Diagnostico` >= 4)

# Ejercicio 1a: ¿cuántos casos tienen 4 años o más? (nrow o count)
# Ejercicio 1b: mirá los valores con count(`Departamento Residencia`) y
#               filtrá los casos de UN departamento.
# Ejercicio 1c: combiná dos condiciones con & (y) o | (o).
#               Ej.: edad >= 4 Y un departamento puntual.


# --- Ejercicio 1: filter() ----------------------------------------------------
# 1a) Casos con 50 años o más
ChCongenito_curso %>%  filter(`Edad Diagnostico` >= 4) |> nrow()
# # o bien:
ChCongenito_curso %>%  filter(`Edad Diagnostico` >= 4) |> count()
# Nota: filter() descarta las filas donde la edad es NA.

# 1b) Casos de un departamento (reemplazar "..." por un valor que aparezca en count())
ChCongenito_curso %>%  count(`Departamento Residencia`)
ChCongenito_curso %>%  filter(`Departamento Residencia` == "Capital")

# 1c) Dos condiciones: edad >= 50 Y un departamento
ChCongenito_curso %>%  filter(`Edad Diagnostico` >= 4 & `Departamento Residencia` == "Capital")
# # con | (o): cualquiera de las dos condiciones
 ChCongenito_curso %>% 
   filter(`Edad Diagnostico` >= 4 | `Departamento Residencia` == "Capital")

 
 
 
# ------------------------------------------------------------------------------
# --- 8.2 select() -------------------------------------------------------------
ChCongenito_curso %>% 
  select(`Fecha Apertura`, `Departamento Residencia`, `Edad Diagnostico`, Técnica)

# Ejercicio 2a: guardá esa selección en un objeto llamado base_reducida.
# Ejercicio 2b: descartá una columna con el signo menos: select(-Tratamiento)
# Ejercicio 2c: seleccioná todas las columnas de fecha: select(starts_with("Fecha"))


 # --- Ejercicio 2: select() ----------------------------------------------------
 # 2a) Guardar la selección en un objeto
 base_reducida <- ChCongenito_curso %>% 
    select(`Fecha Apertura`, `Departamento Residencia`, `Edad Diagnostico`, Técnica)
 
 # 2b) Descartar una columna
 
 ChCongenito_curso  %>%  select(-Tratamiento)
 
 # 2c) Todas las columnas que empiezan con "Fecha"
 
 ChCongenito_curso %>%  select(starts_with("Fecha"))
 
 

# -------------------------------------------------------------------------
 # --- 8.3 arrange() ------------------------------------------------------------
ChCongenito_curso %>%  arrange(`Edad Diagnostico`)         # de menor a mayor
ChCongenito_curso %>% arrange(desc(`Edad Diagnostico`))   # de mayor a menor

# Ejercicio 3a: ¿cuál es la edad del caso más joven y del más grande?
# Ejercicio 3b: ordená por departamento (A-Z) y, dentro de cada uno, por edad
#               descendente: arrange(`Departamento Residencia`, desc(`Edad Diagnostico`))

# --- Ejercicio 3: arrange() ---------------------------------------------------
# 3a) Edad del caso más joven y del más grande

ChCongenito_curso %>%  arrange(`Edad Diagnostico`) |> head(1)         # más joven

ChCongenito_curso %>%  arrange(desc(`Edad Diagnostico`)) |> head(1)   # más grande


# # otra forma, en una sola tabla:

ChCongenito_curso %>% summarise(minima = min(`Edad Diagnostico`, na.rm = TRUE),
             maxima = max(`Edad Diagnostico`, na.rm = TRUE))

# Nota: arrange() deja los NA al final, en orden ascendente y descendente.

# 3b) Departamento A-Z y, dentro de cada uno, edad descendente

ChCongenito_curso %>% arrange(`Departamento Residencia`, desc(`Edad Diagnostico`))




# -------------------------------------------------------------------------
# --- 8.4 mutate() -------------------------------------------------------------
ChCongenito_curso %>%  mutate(mayor_50 = `Edad Diagnostico` >= 50)

ChCongenito_curso %>% 
  mutate(grupo_edad = case_when(
    `Edad Diagnostico` < 18 ~ "menor de 18",
    `Edad Diagnostico` < 60 ~ "18 a 59",
    TRUE                    ~ "60 o más"))

# Ejercicio 4a: creá edad_meses = `Edad Diagnostico` * 12

# Ejercicio 4b: contá cuántos casos hay en cada grupo_edad (count o tabyl).

# --- Ejercicio 4: mutate() ----------------------------------------------------
# 4a) Edad en meses

 ChCongenito_curso %>%  mutate(edad_meses = `Edad Diagnostico` * 12) #¿Qué problema tengo? Los valores de 0 no estan siendo considerados

# 4b) Casos por grupo de edad (hay que GUARDAR el mutate para poder usarlo)

base_grupos <- ChCongenito_curso %>% mutate(grupo_edad = case_when(is.na(`Edad Diagnostico`) ~ NA_character_, #se excluyen NA
     `Edad Diagnostico` < 1 ~ "menor de 1",
     `Edad Diagnostico` < 4 ~ "1 a 3",
     TRUE                    ~ "4 o más"))



base_grupos %>%  count(grupo_edad)
base_grupos %>%  tabyl(grupo_edad)      # con porcentaje




# -------------------------------------------------------------------------
# --- 8.5 Ejercicio integrador -------------------------------------------------
# En UN solo pipe y guardando el resultado en base_final:
#   1. filtrá los casos con edad >= 1
#   2. seleccioná fecha de apertura, departamento, edad y técnica
#   3. creá la columna grupo_edad
#   4. ordená por edad descendente
# Pista: base_final <- ChCongenito_curso %>%  filter(...) %>%  select(...) %>%  ...


# --- Ejercicio integrador -----------------------------------------------------


base_final <- ChCongenito_curso %>% 
  filter(`Edad Diagnostico` >= 1) %>% 
  select(`Fecha Apertura`, `Departamento Residencia`, `Edad Diagnostico`, Técnica) %>% 
  mutate(grupo_edad = case_when(
    `Edad Diagnostico` < 4 ~ "1 a 3",
    TRUE                   ~ "4 o más")) %>% 
  arrange(desc(`Edad Diagnostico`))

# Orden importante: select() va ANTES de mutate() y arrange(), así que sólo
# se pueden usar las columnas que se hayan conservado.







# =============================================================================
#  9. CONECTAR RSTUDIO CON GIT Y GITHUB
# =============================================================================

# 9.1 Verificar que RStudio encuentra git (en la Consola o Terminal):
#     git --version
#     Si falla: Tools > Global Options > Git/SVN y buscar el ejecutable de git.

# 9.2 Presentarse ante git (UNA VEZ por computadora).
#     El mail tiene que ser el mismo de la cuenta de GitHub.
usethis::use_git_config(user.name  = "Nombre Apellido",
                        user.email = "tu_mail@ejemplo.com")

# 9.3 Token de acceso (GitHub ya no acepta la contraseña).
usethis::create_github_token()   # se abre GitHub: tildar "repo" y "user:email",
# generar el token y COPIARLO
gitcreds::gitcreds_set()         # pegar el token en la consola

# 9.4 Conectar el proyecto
#   Camino A (el repo ya existe en GitHub):
#     File > New Project > Version Control > Git > pegar la URL > Create Project
#   Camino B (el proyecto ya existe en tu compu):
usethis::use_git()      # crea el repo local y hace el primer commit
usethis::use_github()   # crea el repo en GitHub y sube todo (push)

# 9.5 Ciclo de trabajo diario (pestaña Git, arriba a la derecha):
#   1. Pull   -> traer los cambios del remoto (siempre antes de empezar)
#   2. Stage  -> tildar los archivos a guardar
#   3. Commit -> escribir un mensaje claro
#   4. Push   -> subir a GitHub
#   Si el push falla con "rejected": hacer pull y volver a intentar.






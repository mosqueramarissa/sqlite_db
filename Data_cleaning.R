library(dplyr)
library(readxl)
library(tidyverse)
library(writexl)
library(RSQLite)
library(stringr)
library(table1)


#Lectura de hojas
data_2018 <- read_xlsx('/Users/metamorphosus/Downloads/Copy_shared_HTA_2018_2019_2022 (1).xlsx', sheet = "2018")
data_2019 <- read_xlsx('/Users/metamorphosus/Downloads/Copy_shared_HTA_2018_2019_2022 (1).xlsx', sheet = "2019")
data_2022 <- read_xlsx('/Users/metamorphosus/Downloads/Copy_shared_HTA_2018_2019_2022 (1).xlsx', sheet = "2022")

str(data_2018)
str(data_2019)
str(data_2022)


# Purga de las columnas que están vacías en un 50% o más
columnas_a_purgar <- c("ESTATUS_DIAGNOSTICO_DIABETES_T2D", 
                       "PADECIMIENTO_HIPERTENSION", 
                       "ANTIHIPERTENSIVO", 
                       "ESTATUS_DIAGNOSTICO_HIPERTENSION", 
                       "HA_PERDIDO_EL_APETITO_CRIBAJE_A", 
                       "PUNTOS_CRIBAJE_A", 
                       "EN_LOS_ULTIMOS_3_MESES_HA_PERDIDO_PESO_CRIBAJE_B", 
                       "PUNTOS_CRIBAJE_B",
                       "MOVILIDAD_CRIBAJE_C",
                       "PUNTOS_CRIBAJE_C",
                       "EN_LOS_ULTIMOS_3_MESES_HA_TENIDO_UNA_ENFERMEDAD_AGUDA_O_SITUACION_DE_ESTRES_PSICOLOGICO_CRIBAJE_D", 
                       "PUNTOS_CRIBAJE_D", 
                       "PROBLEMAS_NEUROPSICOLOGICOS_CRIBAJE_E",
                       "PUNTOS_CRIBAJE_E",
                       "IMC_CRIBAJE_F",
                       "PUNTOS_CRIBAJE_F",
                       "EL_PACIENTE_VIVE_INDEPENDIENTE_EN_SU_DOMICILIO_CRIBAJE_G",
                       "PUNTOS_CRIBAJE_G",
                       "TOMA_MAS_DE_TRES_MEDICAMENTOS_CRIBAJE_H",
                       "PUNTOS_CRIBAJE_H",
                       "ULCERAS_O_LESIONES_CUTANEAS_CRIBAJE_I",
                       "PUNTOS_CRIBAJE_I",
                       "CUANTAS_COMIDAS_COMPLETAS_REALIZA_AL_DIA_CRIBAJE_J",
                       "PUNTOS_CRIBAJE_J",
                       "DETECCION_Y_CONTROL_DIABETES",
                       "CUANTAS_VECES_AL_ANIO_SE_REALIZA_ESTUDIOS_PARA_DETECCION_Y_CONTROL_DE_DIABETES",
                       "DETECCION_Y_CONTROL_HIPERTENSION",
                       "CUANTAS_VECES_AL_ANIO_SE_REALIZA_ESTUDIOS_PARA_DETECCION_Y_CONTROL_DE_HIPERTENSION")

# Ver el porcentaje de NA en todo el data.frame
na_porcentajes_2018 <- sapply(data_2018[, columnas_a_purgar, drop=FALSE], 
                              function(x) mean(is.na(x))*100)
na_porcentajes_2019 <- sapply(data_2019[, columnas_a_purgar, drop=FALSE], 
                              function(x) mean(is.na(x))*100)
na_porcentajes_2022 <- sapply(data_2022[, columnas_a_purgar, drop=FALSE], 
                              function(x) mean(is.na(x))*100)

columnas_vacias_en_mas_del_50_2018 <- names(na_porcentajes_2018[na_porcentajes_2018 > 50])
columnas_vacias_en_mas_del_50_2019 <- names(na_porcentajes_2019[na_porcentajes_2019 > 50])
columnas_vacias_en_mas_del_50_2022 <- names(na_porcentajes_2022[na_porcentajes_2022 > 50])

print(columnas_vacias_en_mas_del_50_2018)
print(columnas_vacias_en_mas_del_50_2019)
print(columnas_vacias_en_mas_del_50_2022)

df_2018 <- data_2018
df_2019 <- data_2019
df_2022 <- data_2022

df_2018 <- df_2018[, !(names(df_2018) %in% columnas_vacias_en_mas_del_50_2018)]
df_2019 <- df_2019[, !(names(df_2019) %in% columnas_vacias_en_mas_del_50_2019)]
df_2022 <- df_2022[, !(names(df_2022) %in% columnas_vacias_en_mas_del_50_2022)]

# Eliminar si tiene menos del 70% de información de los pacientes
columnas_a_purgar_2 <- c("PUNTOS_CRIBAJE_M",
                         "FORMA_DE_ALIMENTARSE_CRIBAJE_N",
                         "PUNTOS_CRIBAJE_N",
                         "EL_PACIENTE_SE_CONSIDERA_A_SI_MISMO_BIEN_NUTRIDO_CRIBAJE_O",
                         "PUNTOS_CRIBAJE_O",
                         "EN_COMPARACION_CON_LAS_PERSONAS_DE_SU_EDAD_COMO_ENCUENTRA_SU_ESTADO_DE_SALUD_CRIBAJE_P",
                         "PUNTOS_CRIBAJE_P",
                         "PROMEDIO_CIRCUNFERENCIA_BRAQUIAL_EN_CM_CRIBAJE_Q",
                         "PUNTOS_CRIBAJE_Q",
                         "CIRCUNFERENCIA_PANTORILLA_EN_CM_CRIBAJE_R",
                         "PUNTOS_CRIBAJE_R",
                         "EVALUACION_CRIBAJE_CATORCE_PUNTOS",
                         "CLASIFICACION_CRIBAJE_CATORCE_PUNTOS",
                         "EVALUACION_GLOBAL_TREINTA_PUNTOS",
                         "CLASIFICACION_GLOBAL_TREINTA_PUNTOS",
                         "EVALUACION_DIECISEIS_PUNTOS")

na_porcentajes_2018_2 <- sapply(df_2018[, columnas_a_purgar_2, drop=FALSE], 
                              function(x) mean(is.na(x))*100)
print(na_porcentajes_2018_2)
na_porcentajes_2019_2 <- sapply(df_2019[, columnas_a_purgar_2, drop=FALSE], 
                              function(x) mean(is.na(x))*100)
print(na_porcentajes_2019_2)
na_porcentajes_2022_2 <- sapply(df_2022[, columnas_a_purgar_2, drop=FALSE], 
                              function(x) mean(is.na(x))*100)
print(na_porcentajes_2022_2)

columnas_vacias_en_mas_del_30_2018 <- names(na_porcentajes_2018_2[na_porcentajes_2018_2 > 30])
columnas_vacias_en_mas_del_30_2019 <- names(na_porcentajes_2019_2[na_porcentajes_2019_2 > 30])
columnas_vacias_en_mas_del_30_2022 <- names(na_porcentajes_2022_2[na_porcentajes_2022_2 > 30])

df_2018 <- df_2018[, !(names(df_2018) %in% columnas_vacias_en_mas_del_30_2018)]
df_2019 <- df_2019[, !(names(df_2019) %in% columnas_vacias_en_mas_del_30_2019)]
df_2022 <- df_2022[, !(names(df_2022) %in% columnas_vacias_en_mas_del_30_2022)]



# Preguntas como cantidad de información, rango dinámico de cada columna, promedio, max, min.
variables_generales <- c("Sx", "agey", "weightKg", "heightm", "SBP", "DBP", "pulse", "glu", "chol", "tg", "hdlc", "ldlc")


df_general <- bind_rows(
  select(df_2018, all_of(variables_generales)),
  select(df_2019, all_of(variables_generales)),
  select(df_2022, all_of(variables_generales)),
  .id = "recruit"
)

df_general$recruit <- factor(df_general$recruit, labels = c("2018", "2019", "2022"))

df_general$Sx <- 
  factor(df_general$Sx, 
         levels = c(1, 2),
         labels = c("Hombre", "Mujer"))

label(df_general$Sx) <- "Sexo"
label(df_general$agey) <- "Edad"
label(df_general$weightKg) <- "Weight (kg)"
label(df_general$heightm) <- "Altura (m)"
label(df_general$SBP) <- "Presión sistólica"
label(df_general$DBP) <- "Presión diastólica"
label(df_general$pulse) <- "Pulso"
label(df_general$glu) <- "Glucosa"
label(df_general$chol) <- "Colesterol"
label(df_general$tg) <- "Trigliceridos"
label(df_general$hdlc) <- "HDL colesterol"
label(df_general$ldlc) <- "LDL colesterol"

table1(~ Sx + agey + weightKg + heightm + SBP + DBP + pulse + glu + chol + tg + hdlc + ldlc | recruit, data = df_general)

# Diseñar la base de datos relacional con acceso y manipulación SQL a través de R

### Renombrar la variable de ID20...
df_2018 <- rename(df_2018, "ID" = "ID2018")
df_2019 <- rename(df_2019, "ID" = "ID2019")
df_2022 <- rename(df_2022, "ID" = "ID2022")

## Variables comunes entre las bases de datos
vars_2018 <- names(df_2018)
vars_2019 <- names(df_2019)
vars_2022 <- names(df_2022)

variables_en_comun <- Reduce(intersect, list(vars_2018, vars_2019, vars_2022))
intersect(vars_2018, vars_2019)
intersect(vars_2019, vars_2022)
intersect(vars_2018, vars_2022)

print(variables_en_comun)

# Ver si existen pacientes duplicados
pacientes_2018 <- df_2018$ID
pacientes_2019 <- df_2019$ID
pacientes_2022 <- df_2022$ID

pacientes_duplicados <- Reduce(intersect, list(pacientes_2018, pacientes_2019, pacientes_2022))
print(pacientes_duplicados)


pacientes_duplicado_2018_2019 <- intersect(pacientes_2018, pacientes_2019)
pacientes_duplicado_2019_2022 <- intersect(pacientes_2019, pacientes_2022)
pacientes_duplicado_2018_2022 <- intersect(pacientes_2018, pacientes_2022)

# Creación de un nuevo data.frame con las variables en común y sin data redundacy
df_variables_en_comun <- bind_rows(
  select(df_2018, all_of(variables_en_comun)),
  select(df_2019, all_of(variables_en_comun)),
  select(df_2022, all_of(variables_en_comun)),
  .id = "recruit_year"
)

df_variables_en_comun$recruit_year <- factor(df_variables_en_comun$recruit_year, labels = c("2018", "2019", "2022"))
str(df_variables_en_comun)

# Creación de la base de datos
if(file.exists("HTA_2018_2019_2022.sqlite")) {
  file.remove("HTA_2018_2019_2022.sqlite")
}

conn <- dbConnect(RSQLite::SQLite(), "HTA_2018_2019_2022.sqlite")

dbExecute(conn, "
          CREATE TABLE paciente(
          ID INTEGER,
          Sx NUMERIC,
          DIA_NACIMIENTO INTEGER,
          MES_NACIMIENTO INTEGER,
          ANIO_NACIMIENTO INTEGER,
          NACIO_EN_MEXICO TEXT,
          NACIO_EN_EXTRANJERO TEXT,
          LUGAR_NACIMIENTO TEXT,
          PRIMARY KEY (ID)
          );
        ")

dbExecute(conn, "
          CREATE TABLE paciente_duplicado(
          recruit_year TEXT,
          ID INTEGER,
          Sx NUMERIC,
          FOREIGN KEY (ID) REFERENCES paciente(ID),
          FOREIGN KEY (recruit_year) REFERENCES informacion_recruit(recruit_year)
          )")

dbExecute(conn, "
          CREATE TABLE informacion_recruit(
          recruit_year TEXT,
          ID INTEGER,
          agey INTEGER, 
          PRIMARY KEY (recruit_year, ID)
          FOREIGN KEY (ID) REFERENCES paciente(ID)
          );
        ")

dbExecute(conn, "
          CREATE TABLE datos_antropometricos(
          recruit_year TEXT,
          ID INTEGER,
          weightKg REAL,
          heightm REAL,
          TALLA_PROMEDIO REAL,
          PESO_PROMEDIO REAL,
          pulse INTEGER,
          DBP INTEGER,
          SBP INTEGER,
          CINTURA_PROMEDIO REAL,
          CADERA_PROMEDIO REAL,
          IMC_KG REAL,
          PRIMARY KEY (recruit_year, ID)
          FOREIGN KEY (ID) REFERENCES paciente(ID),
          FOREIGN KEY (recruit_year) REFERENCES informacion_recruit(recruit_year)
          );
        ")

dbExecute(conn, "
          CREATE TABLE laboratorio(
          recruit_year TEXT,
          ID INTEGER,
          glu	INTEGER, 
          chol INTEGER, 
          tg INTEGER,
          hdlc INTEGER, 
          ldlc INTEGER,
          PRIMARY KEY (recruit_year, ID)
          FOREIGN KEY (ID) REFERENCES paciente(ID),
          FOREIGN KEY (recruit_year) REFERENCES informacion_recruit(recruit_year)
          );
        ")

dbExecute(conn, "
          CREATE TABLE historial_paciente(
          recruit_year TEXT,
          ID INTEGER,
          ESPECIFICAR_OTRA_ENFERMEDAD TEXT,
          TIENE_DIFICULTAD_PARA_CAMINAR_EN_TERRENO_PLANO TEXT,
          SABE_PARA_QUE_TOMA_SUS_MEDICAMENTOS TEXT,
          NECESITA_AYUDA_PARA_PREPARAR_SUS_MEDICAMENTOS	TEXT,
          DETECCION_Y_CONTROL_DIABETES TEXT,
          DETECCION_Y_CONTROL_HIPERTENSION TEXT,
          PADECIMIENTO_DIABETES TEXT,
          HIPOGLUCEMIANTE TEXT,
          PRIMARY KEY (ID, recruit_year, HIPOGLUCEMIANTE, PADECIMIENTO_DIABETES)
          FOREIGN KEY (ID) REFERENCES paciente(ID),
          FOREIGN KEY (recruit_year) REFERENCES informacion_recruit(recruit_year)
          );
        ")

dbExecute(conn, "
          CREATE TABLE farmacos(
          recruit_year TEXT,
          ID INTEGER,
          aHTNdrugs	INTEGER,
          ACEi NUMERIC,
          ARA2 NUMERIC,
          CCB NUMERIC,
          Diuretic NUMERIC,
          BB NUMERIC,
          otheraHTN INTEGER,
          PADECIMIENTO_DIABETES TEXT,
          HIPOGLUCEMIANTE TEXT,
          PRIMARY KEY (recruit_year, ID)
          FOREIGN KEY (ID) REFERENCES paciente(ID),
          FOREIGN KEY (recruit_year) REFERENCES informacion_recruit(recruit_year),
          FOREIGN KEY (HIPOGLUCEMIANTE, PADECIMIENTO_DIABETES) REFERENCES historial_paciente(HIPOGLUCEMIANTE, PADECIMIENTO_DIABETES)
          );
        ")

dbExecute(conn, "
          CREATE TABLE actividad_fisica(
          recruit_year TEXT,
          ID INTEGER,
          CUANTOS_MIN_POR_SEMANA_REALIZABA_ACTIVIDAD_FISICA_A_LOS_50_ANIOS TEXT,	
          CUANTOS_MIN_POR_SEMANA_REALIZABA_ACTIVIDAD_FISICA_A_LOS_55_ANIOS TEXT,
          CUANTOS_MIN_POR_SEMANA_REALIZABA_ACTIVIDAD_FISICA_A_LOS_60_ANIOS TEXT,
          CUANTOS_MIN_POR_SEMANA_REALIZABA_ACTIVIDAD_FISICA_A_LOS_65_ANIOS TEXT,
          CUANTOS_MIN_POR_SEMANA_REALIZA_ACTIVIDAD_FISICA_ACTUALMENTE	EN_LA_ULTIMA_SEMANA_CUANTOS_DIAS_REALIZO_ACTIVIDADES_SEDENTARIAS TEXT,	
          EN_LA_ULTIMA_SEMANA_CUANTOS_DIAS_REALIZO_CAMINATAS TEXT,
          EN_LA_ULTIMA_SEMANA_CUANTAS_HORAS_POR_DIA_HA_CAMINADO	EN_LA_ULTIMA_SEMANA_CUANTOS_DIAS_REALIZO_ACTIVIDADES_RECREATIVAS_O_DEPORTE TEXT,	
          EN_LA_ULTIMA_SEMANA_CUANTOS_DIAS_REALIZO_ACTIVIDADES_RECREATIVAS_O_DEPORTE_EXTENUANTE TEXT,
          EN_PROMEDIO_CUANTAS_HORAS_POR_DIA_REALIZA_ACTIVIDADES_RECREATIVAS_O_DEPORTE_EXTENUANTE TEXT,
          PRIMARY KEY (recruit_year, ID)
          FOREIGN KEY (ID) REFERENCES paciente(ID),
          FOREIGN KEY (recruit_year) REFERENCES informacion_recruit(recruit_year)
          );
        ")

dbExecute(conn, "
          CREATE TABLE medicion_nutricional(
          recruit_year TEXT,
          ID INTEGER,
          CONSUME_LACTEOS_AL_MENOS_UNA_VEZ_AL_DIA_CRIBAJE_K_1 TEXT,
          CONSUME_HUEVOS_O_LEGUMBRES_UNA_O_DOS_VECES_A_LA_SEMANA_CRIBAJE_K_2 TEXT,
          CONSUME_CARNE_PESCADO_O_POLLO_DIARIAMENTE_CRIBAJE_K_3 TEXT,
          CONSUME_FRUTA_O_VERDURA_AL_MENOS_DOS_VECES_AL_DIA_CRIBAJE_L TEXT,
          PUNTOS_CRIBAJE_L TEXT,
          CUANTOS_VASOS_DE_AGUA_U_OTROS_LIQUIDOS_CONSUME_AL_DIA_CRIBAJE_M INTEGER,
          CUANTAS_CUCHARADAS_TOTALES_DE_AZUCAR_AGREGA_A_SUS_COMIDAS_EN_UN_DIA INTEGER,
          AGREGA_SAL_A_SUS_ALIMENTOS_ANTES_DE_PROBARLOS TEXT,
          PRIMARY KEY (recruit_year, ID)
          FOREIGN KEY(ID) REFERENCES paciente(ID),
          FOREIGN KEY(recruit_year) REFERENCES informacion_recruit(recruit_year)
          );
        ")

dbExecute(conn, "
          CREATE TABLE clasificacion_diagnostico(
          recruit_year TEXT,
          ID INTEGER,
          T2D_CLASIF_II_3_10_20_DRA TEXT,
          RESOLVER_DRA TEXT,
          CLS_PROBABLE_DRA TEXT,
          PRIMARY KEY (recruit_year, ID)
          FOREIGN KEY(ID) REFERENCES paciente(ID),
          FOREIGN KEY(recruit_year) REFERENCES informacion_recruit(recruit_year)
          );
        ")

dbExecute(conn, "
          CREATE TABLE morisky_green (
          recruit_year TEXT,
          ID INTEGER,
          OLVIDA_TOMAR_LOS_MEDICAMENTOS_PARA_TRATAR_SU_ENFERMEDAD_MORISKY_GREEN TEXT,
          OLVIDA_TOMAR_SUS_MEDICAMENTOS_A_LAS_HORAS_INDICADAS_MORISKY_GREEN TEXT,
          CUANDO_SE_ENCUENTRA_EN_BIEN_DEJA_DE_TOMAR_SU_MEDICACION_MORISKY_GREEN TEXT,
          SI_ALGUNA_VEZ_LE_SIENTA_MAL_LA_MEDICACION_DEJA_DE_TOMARLA_MORISKY_GREEN TEXT,
          GRADO_DE_ADHERENCIA_MORISKY_GREEN TEXT,
          PRIMARY KEY (recruit_year, ID)
          FOREIGN KEY(ID) REFERENCES paciente(ID),
          FOREIGN KEY(recruit_year) REFERENCES informacion_recruit(recruit_year)
          );
        ")

dbExecute(conn, "
          CREATE TABLE medicamentos_paciente (
          ID INTEGER,
          recruit_year TEXT,
          MEDICAMENTO_01 TEXT,
          CLASE_FARMA_01 TEXT,
          INDICACION_TERAPEUTICA_01 TEXT,
          NOTAS_MEDICAMENTO_01 TEXT,
          MEDICAMENTO_02 TEXT,
          CLASE_FARMA_02 TEXT,
          INDICACION_TERAPEUTICA_02 TEXT,
          NOTAS_MEDICAMENTO_02 TEXT,
          MEDICAMENTO_03 TEXT,
          CLASE_FARMA_03 TEXT,
          INDICACION_TERAPEUTICA_03 TEXT,
          NOTAS_MEDICAMENTO_03 TEXT,
          MEDICAMENTO_04 TEXT,
          CLASE_FARMA_04 TEXT, 
          INDICACION_TERAPEUTICA_04 TEXT,
          NOTAS_MEDICAMENTO_04 TEXT,
          MEDICAMENTO_05 TEXT,
          CLASE_FARMA_05 TEXT,
          INDICACION_TERAPEUTICA_05 TEXT,
          NOTAS_MEDICAMENTO_05 TEXT,
          MEDICAMENTO_06 TEXT,
          CLASE_FARMA_06 TEXT,
          INDICACION_TERAPEUTICA_06 TEXT,
          NOTAS_MEDICAMENTO_06 TEXT,
          MEDICAMENTO_07 TEXT,
          CLASE_FARMA_07 TEXT,
          INDICACION_TERAPEUTICA_07 TEXT,
          NOTAS_MEDICAMENTO_07 TEXT,
          MEDICAMENTO_08 TEXT,
          CLASE_FARMA_08 TEXT,
          INDICACION_TERAPEUTICA_08 TEXT,
          NOTAS_MEDICAMENTO_08 TEXT,
          MEDICAMENTO_09 TEXT,
          CLASE_FARMA_09 TEXT,
          INDICACION_TERAPEUTICA_09 TEXT,
          NOTAS_MEDICAMENTO_09 TEXT,
          MEDICAMENTO_10 TEXT,
          CLASE_FARMA_10 TEXT,
          INDICACION_TERAPEUTICA_10 TEXT,
          NOTAS_MEDICAMENTO_10 TEXT,
          MEDICAMENTO_11 TEXT,
          CLASE_FARMA_11 TEXT,
          INDICACION_TERAPEUTICA_11 TEXT,
          NOTAS_MEDICAMENTO_11 TEXT,
          MEDICAMENTO_12 TEXT,
          CLASE_FARMA_12 TEXT,
          INDICACION_TERAPEUTICA_12 TEXT,
          NOTAS_MEDICAMENTO_12 TEXT,
          MEDICAMENTO_13 TEXT,
          CLASE_FARMA_13 TEXT,
          INDICACION_TERAPEUTICA_13 TEXT,
          NOTAS_MEDICAMENTO_13 TEXT,
          PRIMARY KEY (recruit_year, ID)
          FOREIGN KEY(ID) REFERENCES paciente(ID)
          FOREIGN KEY (recruit_year) REFERENCES informacion_recruit(recruit_year)
          )
        ")

# Creación de data.frames con la información de cada tabla relacionar
paciente <- c("ID", 
              "Sx", 
              "DIA_NACIMIENTO",
              "MES_NACIMIENTO", 
              "ANIO_NACIMIENTO", 
              "NACIO_EN_MEXICO", 
              "NACIO_EN_EXTRANJERO", 
              "LUGAR_NACIMIENTO")

paciente_duplicado <- c("recruit_year", 
                       "ID",
                       "Sx")

informacion_recruit <- c("recruit_year", 
                         "ID", 
                         "agey")

datos_antropometricos <- c("recruit_year",
                           "ID",
                           "weightKg",
                           "heightm",
                           "TALLA_PROMEDIO",
                           "PESO_PROMEDIO",
                           "pulse",
                           "DBP",
                           "SBP",
                           "CINTURA_PROMEDIO",
                           "CADERA_PROMEDIO",
                           "IMC_KG")

laboratorio <- c("recruit_year",
                 "ID",
                 "glu",
                 "chol",
                 "tg",
                 "hdlc",
                 "ldlc")

farmacos <- c("recruit_year",
              "ID",
              "PADECIMIENTO_DIABETES",
              "HIPOGLUCEMIANTE",
              "aHTNdrugs",
              "ACEi",
              "ARA2",
              "CCB",
              "Diuretic",
              "BB",
              "otheraHTN")

actividad_fisica <- c("recruit_year",
                      "ID", 
                      "CUANTOS_MIN_POR_SEMANA_REALIZABA_ACTIVIDAD_FISICA_A_LOS_50_ANIOS", 
                      "CUANTOS_MIN_POR_SEMANA_REALIZABA_ACTIVIDAD_FISICA_A_LOS_55_ANIOS", 
                      "CUANTOS_MIN_POR_SEMANA_REALIZABA_ACTIVIDAD_FISICA_A_LOS_60_ANIOS",
                      "CUANTOS_MIN_POR_SEMANA_REALIZABA_ACTIVIDAD_FISICA_A_LOS_65_ANIOS",
                      "CUANTOS_MIN_POR_SEMANA_REALIZA_ACTIVIDAD_FISICA_ACTUALMENTE",
                      "EN_LA_ULTIMA_SEMANA_CUANTOS_DIAS_REALIZO_CAMINATAS",
                      "EN_LA_ULTIMA_SEMANA_CUANTAS_HORAS_POR_DIA_HA_CAMINADO",
                      "EN_LA_ULTIMA_SEMANA_CUANTOS_DIAS_REALIZO_ACTIVIDADES_RECREATIVAS_O_DEPORTE_EXTENUANTE",
                      "EN_PROMEDIO_CUANTAS_HORAS_POR_DIA_REALIZA_ACTIVIDADES_RECREATIVAS_O_DEPORTE_EXTENUANTE")

historial_paciente <- c("recruit_year",
                        "ID",
                        "PADECIMIENTO_DIABETES",
                        "HIPOGLUCEMIANTE",
                        "ESPECIFICAR_OTRA_ENFERMEDAD",
                        "TIENE_DIFICULTAD_PARA_CAMINAR_EN_TERRENO_PLANO",
                        "SABE_PARA_QUE_TOMA_SUS_MEDICAMENTOS",
                        "NECESITA_AYUDA_PARA_PREPARAR_SUS_MEDICAMENTOS",
                        "DETECCION_Y_CONTROL_DIABETES",
                        "DETECCION_Y_CONTROL_HIPERTENSION")

morisky_green <- c("recruit_year",
                   "ID",
                   "OLVIDA_TOMAR_LOS_MEDICAMENTOS_PARA_TRATAR_SU_ENFERMEDAD_MORISKY_GREEN",
                   "OLVIDA_TOMAR_SUS_MEDICAMENTOS_A_LAS_HORAS_INDICADAS_MORISKY_GREEN",
                   "CUANDO_SE_ENCUENTRA_EN_BIEN_DEJA_DE_TOMAR_SU_MEDICACION_MORISKY_GREEN",
                   "SI_ALGUNA_VEZ_LE_SIENTA_MAL_LA_MEDICACION_DEJA_DE_TOMARLA_MORISKY_GREEN",
                   "GRADO_DE_ADHERENCIA_MORISKY_GREEN")

medicion_nutricional <- c("recruit_year",
                          "ID",
                          "CONSUME_LACTEOS_AL_MENOS_UNA_VEZ_AL_DIA_CRIBAJE_K_1",
                          "CONSUME_HUEVOS_O_LEGUMBRES_UNA_O_DOS_VECES_A_LA_SEMANA_CRIBAJE_K_2",
                          "CONSUME_CARNE_PESCADO_O_POLLO_DIARIAMENTE_CRIBAJE_K_3",
                          "CONSUME_FRUTA_O_VERDURA_AL_MENOS_DOS_VECES_AL_DIA_CRIBAJE_L",
                          "PUNTOS_CRIBAJE_L",
                          "CUANTOS_VASOS_DE_AGUA_U_OTROS_LIQUIDOS_CONSUME_AL_DIA_CRIBAJE_M",
                          "CUANTAS_CUCHARADAS_TOTALES_DE_AZUCAR_AGREGA_A_SUS_COMIDAS_EN_UN_DIA",
                          "AGREGA_SAL_A_SUS_ALIMENTOS_ANTES_DE_PROBARLOS")
medicamentos_paciente <- c("recruit_year",
                           "ID",
                           "MEDICAMENTO_01",
                           "CLASE_FARMA_01",
                           "INDICACION_TERAPEUTICA_01",
                           "NOTAS_MEDICAMENTO_01",
                           "MEDICAMENTO_02",
                           "CLASE_FARMA_02",
                           "INDICACION_TERAPEUTICA_02",
                           "NOTAS_MEDICAMENTO_02",
                           "MEDICAMENTO_03",
                           "CLASE_FARMA_03",
                           "INDICACION_TERAPEUTICA_03",
                           "NOTAS_MEDICAMENTO_03",
                           "MEDICAMENTO_04",
                           "CLASE_FARMA_04", 
                           "INDICACION_TERAPEUTICA_04",
                           "NOTAS_MEDICAMENTO_04",
                           "MEDICAMENTO_05",
                           "CLASE_FARMA_05",
                           "INDICACION_TERAPEUTICA_05",
                           "NOTAS_MEDICAMENTO_05",
                           "MEDICAMENTO_06",
                           "CLASE_FARMA_06",
                           "INDICACION_TERAPEUTICA_06",
                           "NOTAS_MEDICAMENTO_06",
                           "MEDICAMENTO_07",
                           "CLASE_FARMA_07",
                           "INDICACION_TERAPEUTICA_07",
                           "NOTAS_MEDICAMENTO_07",
                           "MEDICAMENTO_08",
                           "CLASE_FARMA_08",
                           "INDICACION_TERAPEUTICA_08",
                           "NOTAS_MEDICAMENTO_08",
                           "MEDICAMENTO_09",
                           "CLASE_FARMA_09",
                           "INDICACION_TERAPEUTICA_09",
                           "NOTAS_MEDICAMENTO_09",
                           "MEDICAMENTO_10",
                           "CLASE_FARMA_10",
                           "INDICACION_TERAPEUTICA_10",
                           "NOTAS_MEDICAMENTO_10",
                           "MEDICAMENTO_11",
                           "CLASE_FARMA_11",
                           "INDICACION_TERAPEUTICA_11",
                           "NOTAS_MEDICAMENTO_11",
                           "MEDICAMENTO_12",
                           "CLASE_FARMA_12",
                           "INDICACION_TERAPEUTICA_12",
                           "NOTAS_MEDICAMENTO_12",
                           "MEDICAMENTO_13",
                           "CLASE_FARMA_13",
                           "INDICACION_TERAPEUTICA_13",
                           "NOTAS_MEDICAMENTO_13")

clasificacion_diagnostico <- c("recruit_year",
                               "ID",
                               "T2D_CLASIF_II_3_10_20_DRA",
                               "RESOLVER_DRA",
                               "CLS_PROBABLE_DRA")

df_paciente <- df_variables_en_comun[!duplicated(df_variables_en_comun$ID), paciente]
df_paciente_duplicado <- df_variables_en_comun[duplicated(df_variables_en_comun$ID), paciente_duplicado]
df_paciente_duplicado$ID <- paste0(df_paciente_duplicado$ID, "_")
df_informacion_recruit <- df_variables_en_comun[, informacion_recruit]
df_datos_antropometricos <- df_variables_en_comun[, datos_antropometricos]
df_laboratorio <- df_variables_en_comun[, laboratorio]
df_farmacos <- df_variables_en_comun[, farmacos]
df_actividad_fisica <- df_variables_en_comun[, actividad_fisica]
df_historial_paciente <- df_variables_en_comun[, historial_paciente]
df_morisky_green <- df_variables_en_comun[, morisky_green]
df_medicion_nutricional <- df_variables_en_comun[, medicion_nutricional]
df_medicamentos_paciente <- df_variables_en_comun[, medicamentos_paciente]
df_clasificacion_diagnostico <- df_variables_en_comun[, clasificacion_diagnostico]

dbWriteTable(conn, "paciente", df_paciente, append = TRUE, row.names = FALSE)
dbWriteTable(conn, "paciente_duplicado", df_paciente_duplicado, append = TRUE, row.names = FALSE)
dbWriteTable(conn, "informacion_recruit", df_informacion_recruit, append = TRUE, row.names = FALSE)
dbWriteTable(conn, "datos_antropometricos", df_datos_antropometricos, append = TRUE, row.names = FALSE)
dbWriteTable(conn, "laboratorio", df_laboratorio, append = TRUE, row.names = FALSE)
dbWriteTable(conn, "farmacos", df_farmacos, append = TRUE, row.names = FALSE)
dbWriteTable(conn, "actividad_fisica", df_actividad_fisica, append = TRUE, row.names = FALSE)
dbWriteTable(conn, "historial_paciente", df_historial_paciente, append = TRUE, row.names = FALSE)
dbWriteTable(conn, "morisky_green", df_morisky_green, append = TRUE, row.names = FALSE)
dbWriteTable(conn, "medicion_nutricional", df_medicion_nutricional, append = TRUE, row.names = FALSE)
dbWriteTable(conn, "medicamentos_paciente", df_medicamentos_paciente, append = TRUE, row.names = FALSE)
dbWriteTable(conn, "clasificacion_diagnostico", df_clasificacion_diagnostico, append = TRUE, row.names = FALSE)

dbDisconnect(conn)

library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(stats)
library(tmap)
library(lubridate)
library(ggplot2)
library(gridExtra)

#### descarga datos actualizados ####
urlCABA <-
  "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/casos-covid-19/casos_covid19.csv"
download.file(urlCABA, "datos/covidCABA.csv")

#### crea DF con IIE por comuna ####
INDEX <-
  data.frame(
    COMUNA = c(
      'COMUNA.01',
      'COMUNA.02',
      'COMUNA.03',
      'COMUNA.04',
      'COMUNA.05',
      'COMUNA.06',
      'COMUNA.07',
      'COMUNA.08',
      'COMUNA.09',
      'COMUNA.10',
      'COMUNA.11',
      'COMUNA.12',
      'COMUNA.13',
      'COMUNA.14',
      'COMUNA.15',
      'TOTAL'
    ),
    INDEX = c(
      1.099528002,
      1.148891108,
      0.736967111,
      0.720190009,
      0.891667423,
      1.051556685,
      0.911500408,
      0.747692959,
      0.813848295,
      0.973298841,
      0.952936371,
      1.149813924,
      1.211037488,
      1.286814317,
      0.959471726,
      1
    ),
    ZONA = c(
      "Centro",
      "Norte",
      "Centro",
      "Sur",
      "Centro",
      "Centro",
      "Centro",
      "Sur",
      "Sur",
      "Sur",
      "Centro",
      "Centro",
      "Norte",
      "Norte",
      "Centro",
      "Total"
    )
  ) %>% dplyr::mutate(link = paste0("02", "0", substr(COMUNA, 8, 10)))

# procesa datos

#### SEGUNDA OLA #####
covidCABA <- read.csv("datos/covidCABA.csv")

resumenArchivo <- list(
  registrosTotales <-
    paste0(
      "FECHA: ",
      Sys.Date(),
      " - REGISTROS TOTALES:",
      " ",
      nrow(covidCABA)
    ),
  confirmados <-
    paste0(
      "FECHA: ",
      Sys.Date(),
      " - CASOS CONFIRMADOS:",
      " ",
      nrow(
        covidCABA %>% dplyr::filter(clasificacion == "confirmado" &
                                      provincia == "CABA")
      )
    ),
  eliminados <-
    paste0(
      "FECHA: ",
      Sys.Date(),
      " - CASOS CONFIRMADOS CABA SIN EDAD O COMUNA:",
      " ",
      nrow(
        covidCABA %>% dplyr::filter(
          clasificacion == "confirmado" &
            provincia ==
            "CABA" &
            is.na(edad) ==
            T &
            (is.na(comuna) ==
               T | comuna < 1 | comuna > 15)
        )
      )
    )
)

covidCABA <- covidCABA %>% dplyr::filter(provincia == "CABA" &
                                           between(comuna, 1, 15) &
                                           is.na(edad) == FALSE) %>%
  dplyr::mutate(
    fecha_clasificacion = as.Date(parse_date_time(
      substr(fecha_clasificacion, 1, 9), orders = c("dmy")
    )),
    comuna = paste0("COMUNA.", str_pad(comuna, 2, "left", "0")),
    fecha_fallecimiento = as.Date(parse_date_time(
      substr(fecha_fallecimiento, 1, 9), orders = c("dmy")
    ))
  )%>% dplyr::filter(fecha_clasificacion<="2021-04-30",
                     fecha_clasificacion>="2020-11-29")


# df de todos los registrados
TODOS <-
  covidCABA %>% dplyr::group_by(COMUNA = comuna) %>% dplyr::tally()
colnames(TODOS)[2] <- "TODOS"
TODOS[16, 2] <- count(covidCABA)
TODOS[16, 1] <- "TOTAL"

covidCABA <-
  covidCABA %>% dplyr::filter(clasificacion == "confirmado") %>%
  dplyr::mutate(
    GRUPEDAD = case_when(
      between(edad, 0, 4) ~ "0004",
      between(edad, 5, 9) ~ "0509",
      between(edad, 10, 14) ~ "1014",
      between(edad, 15, 19) ~ "1519",
      between(edad, 20, 24) ~ "2024",
      between(edad, 25, 29) ~ "2529",
      between(edad, 30, 34) ~ "3034",
      between(edad, 35, 39) ~ "3539",
      between(edad, 40, 44) ~ "4044",
      between(edad, 45, 49) ~ "4549",
      between(edad, 50, 54) ~ "5054",
      between(edad, 55, 59) ~ "5559",
      between(edad, 60, 64) ~ "6064",
      between(edad, 65, 69) ~ "6569",
      between(edad, 70, 74) ~ "7074",
      between(edad, 75, 79) ~ "7579",
      between(edad, 80, 130) ~ "80XX",
      is.na(edad) ~ "9999"
    )
  )

# agrega datos de población
load("datos/poblaCABA.Rdata")
poblaCABA <-
  poblaCABA %>% gather(COMUNA, POBTOT, 2:17) %>% dplyr::filter(GRUPEDAD !=
                                                                 "TOTAL")
poblaCABA$TOTEST <- sum(poblaCABA$POBTOT[poblaCABA$COMUNA == "TOTAL"])

# agrega población estandar
poblaCABA <-
  merge(poblaCABA,
        poblaCABA[poblaCABA$COMUNA == "TOTAL", ] %>%
          dplyr::select(GRUPEDAD, POBEST =POBTOT)) %>%
  arrange(COMUNA, GRUPEDAD)

# agrega casos
poblaCABA <-
  merge(
    poblaCABA,
    covidCABA %>% dplyr::group_by(GRUPEDAD, COMUNA = comuna) %>%
      dplyr::summarise(CASOS =n()) %>% dplyr::select(GRUPEDAD, COMUNA, CASOS),
    all.y  = TRUE
  )
poblaCABA <-
  rbind(
    poblaCABA,
    poblaCABA %>% group_by(GRUPEDAD) %>% summarise(
      COMUNA = "TOTAL",
      POBTOT = sum(POBTOT),
      TOTEST = max(TOTEST),
      POBEST = max(POBEST),
      CASOS = sum(CASOS)
    )
  )

# df de incidencia por edad
INCID.POR.EDAD <- poblaCABA
INCID.POR.EDAD$INCID <-
  INCID.POR.EDAD$CASOS / INCID.POR.EDAD$POBTOT * 1000


# agrega mortalidad
poblaCABA <-
  merge(
    poblaCABA,
    covidCABA %>% dplyr::filter(fallecido == "si") %>%
      dplyr::group_by(GRUPEDAD, COMUNA =comuna) %>% summarise(MUERTES = n()) %>% dplyr::select(GRUPEDAD, COMUNA, MUERTES),
    all.x = TRUE
  )
poblaCABA$GRUPEDAD <- as.character(poblaCABA$GRUPEDAD)
poblaCABA[is.na(poblaCABA)] <- 0

for (row in 1:nrow(poblaCABA)){
  grupedad_row <- poblaCABA$GRUPEDAD[row]
  if (poblaCABA$COMUNA[row] == "TOTAL"){
    poblaCABA$MUERTES[row] <-
      sum(poblaCABA$MUERTES[poblaCABA$GRUPEDAD == grupedad_row])
  }
}


# agrega tasas estandar
poblaCABA <-
  merge(
    poblaCABA,
    poblaCABA %>% dplyr::filter(poblaCABA$COMUNA == "TOTAL") %>%
      mutate(TASAS_REF =MUERTES / POBTOT) %>% dplyr::select(GRUPEDAD, TASAS_REF)
  )
poblaCABA$MUERTES.ESPERADAS <- poblaCABA$TASAS_REF * poblaCABA$POBTOT
poblaCABA[is.na(poblaCABA)] <- 0
sum(poblaCABA$MUERTES[poblaCABA$COMUNA == "TOTAL"])


# df frecuencia acumulada por edad
MEDIANA.EDAD <-
  poblaCABA %>% filter(GRUPEDAD != "9999" &
                         COMUNA != "COMUNA.Sin esp.")
MEDIANA.EDAD <- MEDIANA.EDAD %>% arrange(COMUNA, GRUPEDAD)
MEDIANA.EDAD <-
  MEDIANA.EDAD %>% group_by(COMUNA) %>%
  mutate(MUERTES.ESP.TOTAL = sum(MUERTES.ESPERADAS))
MEDIANA.EDAD <-
  MEDIANA.EDAD %>% group_by(COMUNA) %>%
  mutate(MUERTES.ESP.ACUM = cumsum(MUERTES.ESPERADAS) /
                                                 MUERTES.ESP.TOTAL)
MEDIANA.EDAD <-
  MEDIANA.EDAD %>% group_by(COMUNA) %>% mutate(MUERTES.OBS.TOTAL = sum(MUERTES))
MEDIANA.EDAD <-
  MEDIANA.EDAD %>% group_by(COMUNA) %>% mutate(MUERTES.OBS.ACUM = cumsum(MUERTES) /
                                                 MUERTES.OBS.TOTAL)
MEDIANA.EDAD$MEDIANA.ESP <- ""
MEDIANA.EDAD$MEDIANA.OBS <- ""

for (row in 2:nrow(MEDIANA.EDAD)){
  if (MEDIANA.EDAD$MUERTES.ESP.ACUM[row] >= 0.5 &
      MEDIANA.EDAD$MUERTES.ESP.ACUM[row - 1] < 0.5)
  {
    MEDIANA.EDAD$MEDIANA.ESP[row] <- "X"
  }
  print(
    c(
      row,
      MEDIANA.EDAD$MUERTES.ESP.ACUM[row],
      MEDIANA.EDAD$MUERTES.ESP.ACUM[row - 1],
      MEDIANA.EDAD$MEDIANA.ESP[row]
    )
  )
}

for (row in 2:nrow(MEDIANA.EDAD)){
  if (MEDIANA.EDAD$MUERTES.OBS.ACUM[row] >= 0.5 &
      MEDIANA.EDAD$MUERTES.OBS.ACUM[row - 1] < 0.5)
  {
    MEDIANA.EDAD$MEDIANA.OBS[row] <- "X"
  }
  print(
    c(
      MEDIANA.EDAD$MUERTES.OBS.ACUM[row],
      MEDIANA.EDAD$MUERTES.OBS.ACUM[row - 1],
      MEDIANA.EDAD$MEDIANA.OBS[row]
    )
  )
}


# df con RMEs
RME <-
  poblaCABA %>% filter(GRUPEDAD != "9999" &
                         COMUNA != "COMUNA.Sin esp.")
RME <-
  poblaCABA %>% dplyr::group_by(COMUNA) %>% dplyr::summarise(
    OBS = sum(MUERTES),
    ESP = sum(MUERTES.ESPERADAS),
    RME = (sum(MUERTES) / sum(MUERTES.ESPERADAS) * 100)
  )
RME$LI = (RME$OBS * ((1 - (1 / (
  9 * RME$OBS
)) - (1.96 / 3) * sqrt((
  1 / RME$OBS
))) ^ 3)) / RME$ESP
RME$LS = (RME$OBS + 1) * ((1 - (1 / (9 * (
  RME$OBS + 1
))) + (1.96 / 3) * sqrt((1 / (
  RME$OBS + 1
)))) ^ 3) / RME$ESP
RME$link <- paste0("02", "0", substr(RME$COMUNA, 8, 10))
RME$RME <- round(RME$RME, digits = 1)

# df con incidencia acumulada
INCID <-
  poblaCABA %>% filter(GRUPEDAD != "9999" &
                         COMUNA != "COMUNA.Sin esp.")
INCID <-
  INCID %>% dplyr::group_by(COMUNA) %>% dplyr::summarise(
    CASOS = sum(CASOS),
    POBLACION = sum(POBTOT),
    INCID.POR.MIL = sum(CASOS) / sum(POBTOT)
  )
INCID$link <- paste0("02", "0", substr(INCID$COMUNA, 8, 10))
INCID$LI_INCID.POR.MIL <-
  INCID$INCID.POR.MIL - 1.96 * sqrt((INCID$INCID.POR.MIL * (1 - INCID$INCID.POR.MIL)) /
                                      INCID$POBLACION)
INCID$LS_INCID.POR.MIL <-
  INCID$INCID.POR.MIL + 1.96 * sqrt((INCID$INCID.POR.MIL * (1 - INCID$INCID.POR.MIL)) /
                                      INCID$POBLACION)
INCID$INCID.POR.MIL <- INCID$INCID.POR.MIL * 1000
INCID$LI_INCID.POR.MIL <- INCID$LI_INCID.POR.MIL * 1000
INCID$LS_INCID.POR.MIL <- INCID$LS_INCID.POR.MIL * 1000

# df con datos por edad
EDAD <-
  union_all(
    covidCABA %>% filter(is.na(edad) == FALSE) %>% dplyr::select(comuna, edad),
    covidCABA %>% filter(is.na(edad) == FALSE) %>% dplyr::select("TOTAL" =
                                                                   comuna, edad)
  )
EDAD$comuna[is.na(EDAD$comuna) == TRUE] <- "TOTAL"
EDAD$TOTAL <- NULL
EDAD$edad <- str_pad(as.character(EDAD$edad), 3, "left", "0")
EDAD <-
  EDAD %>% dplyr::group_by(comuna) %>% dplyr::summarise(
    CASOS.00A14 = sum(between(edad, "000", "014")),
    CASOS.15A49 =
      sum(between(edad, "015", "049")),
    CASOS.50A59 =
      sum(between(edad, "050", "059")),
    CASOS.60A69 =
      sum(between(edad, "060", "069")),
    CASOS.70A79 =
      sum(between(edad, "070", "079")),
    CASOS.80MAS =
      sum(between(edad, "080", "999"))
  )

EDAD_F <-
  union_all(
    covidCABA %>% filter(is.na(edad) == FALSE &
                           fallecido == "si") %>% dplyr::select(comuna, edad),
    covidCABA %>% filter(is.na(edad) == FALSE &
                           fallecido == "si") %>% dplyr::select("TOTAL" = comuna, edad)
  )

EDAD_F$comuna[is.na(EDAD_F$comuna) == TRUE] <- "TOTAL"
EDAD_F$TOTAL <- NULL
EDAD_F$edad <- str_pad(as.character(EDAD_F$edad), 3, "left", "0")
EDAD_F <-
  EDAD_F %>% dplyr::group_by(comuna) %>% dplyr::summarise(
    MUERTES.00A14 = sum(between(edad, "000", "014")),
    MUERTES.15A49 =
      sum(between(edad, "015", "049")),
    MUERTES.50A59 =
      sum(between(edad, "050", "059")),
    MUERTES.60A69 =
      sum(between(edad, "060", "069")),
    MUERTES.70A79 =
      sum(between(edad, "070", "079")),
    MUERTES.80MAS =
      sum(between(edad, "080", "999"))
  )

EDAD <- merge(EDAD, EDAD_F)

# df con poblacion por grupos de edad
POBLACION <-
  reshape(poblaCABA,
          timevar = "GRUPEDAD",
          idvar = "COMUNA",
          direction = "wide")
POBLACION <- data.frame(
  POBLACION$COMUNA,
  POBLACION$POBTOT.0004,
  POBLACION$POBTOT.0509,
  POBLACION$POBTOT.1014,
  POBLACION$POBTOT.1519,
  POBLACION$POBTOT.2024,
  POBLACION$POBTOT.2529,
  POBLACION$POBTOT.3034,
  POBLACION$POBTOT.3539,
  POBLACION$POBTOT.4044,
  POBLACION$POBTOT.4549,
  POBLACION$POBTOT.5054,
  POBLACION$POBTOT.5559,
  POBLACION$POBTOT.6064,
  POBLACION$POBTOT.6569,
  POBLACION$POBTOT.7074,
  POBLACION$POBTOT.7579,
  POBLACION$POBTOT.80XX
)

# df con positividad
POSITIVIDAD <- data.frame(
  COMUNA = TODOS$COMUNA,
  TODOS = TODOS$TODOS,
  CASOS = INCID$CASOS,
  POSITIVIDAD = INCID$CASOS / TODOS$TODOS
)

POSITIVIDAD$LI_POSITIVIDAD <-
  POSITIVIDAD$POSITIVIDAD - 1.96 * sqrt((POSITIVIDAD$POSITIVIDAD * (1 - POSITIVIDAD$POSITIVIDAD)) /
                                          POSITIVIDAD$TODOS)
POSITIVIDAD$LS_POSITIVIDAD <-
  POSITIVIDAD$POSITIVIDAD + 1.96 * sqrt((POSITIVIDAD$POSITIVIDAD * (1 - POSITIVIDAD$POSITIVIDAD)) /
                                          POSITIVIDAD$TODOS)
POSITIVIDAD$link <- paste0("02", "0", substr(POSITIVIDAD$COMUNA, 8, 10))

# df con letalidad
LETALIDAD <- data.frame(
  INCID$COMUNA,
  RME$OBS / INCID$CASOS * 100,
  RME$OBS / INCID$CASOS * 100 - 1.96 * sqrt((RME$OBS / INCID$CASOS * 100) * (100 - RME$OBS / INCID$CASOS * 100) / INCID$CASOS),
  RME$OBS / INCID$CASOS * 100 + 1.96 * sqrt((RME$OBS / INCID$CASOS * 100) * (100 - RME$OBS / INCID$CASOS * 100) / INCID$CASOS
  )
)


colnames(LETALIDAD)[1] <- "COMUNA"
colnames(LETALIDAD)[2] <- "LETALIDAD"
colnames(LETALIDAD)[3] <- "LI_LETALIDAD"
colnames(LETALIDAD)[4] <- "LS_LETALIDAD"

LETALIDAD$link <- paste0("02", "0", substr(LETALIDAD$COMUNA, 8, 10))

LETALIDAD$LET_00A14 <- EDAD$MUERTES.00A14 / EDAD$CASOS.00A14 * 100
LETALIDAD$LI_LET_00A14 <-
  LETALIDAD$LET_00A14 - 1.96 * sqrt((LETALIDAD$LET_00A14 * (100 - LETALIDAD$LET_00A14)) /
                                      EDAD$CASOS.00A14)
LETALIDAD$LS_LET_00A14 <-
  LETALIDAD$LET_00A14 + 1.96 * sqrt((LETALIDAD$LET_00A14 * (100 - LETALIDAD$LET_00A14)) /
                                      EDAD$CASOS.00A14)
LETALIDAD$LET_15A49 <- EDAD$MUERTES.15A49 / EDAD$CASOS.15A49 * 100
LETALIDAD$LI_LET_15A49 <-
  LETALIDAD$LET_15A49 - 1.96 * sqrt((LETALIDAD$LET_15A49 * (100 - LETALIDAD$LET_15A49)) /
                                      EDAD$CASOS.15A49)
LETALIDAD$LS_LET_15A49 <-
  LETALIDAD$LET_15A49 + 1.96 * sqrt((LETALIDAD$LET_15A49 * (100 - LETALIDAD$LET_15A49)) /
                                      EDAD$CASOS.15A49)
LETALIDAD$LET_50A59 <- EDAD$MUERTES.50A59 / EDAD$CASOS.50A59 * 100
LETALIDAD$LI_LET_50A59 <-
  LETALIDAD$LET_50A59 - 1.96 * sqrt((LETALIDAD$LET_50A59 * (100 - LETALIDAD$LET_50A59)) /
                                      EDAD$CASOS.50A59)
LETALIDAD$LS_LET_50A59 <-
  LETALIDAD$LET_50A59 + 1.96 * sqrt((LETALIDAD$LET_50A59 * (100 - LETALIDAD$LET_50A59)) /
                                      EDAD$CASOS.50A59)
LETALIDAD$LET_60A69 <- EDAD$MUERTES.60A69 / EDAD$CASOS.60A69 * 100
LETALIDAD$LI_LET_60A69 <-
  LETALIDAD$LET_60A69 - 1.96 * sqrt((LETALIDAD$LET_60A69 * (100 - LETALIDAD$LET_60A69)) /
                                      EDAD$CASOS.60A69)
LETALIDAD$LS_LET_60A69 <-
  LETALIDAD$LET_60A69 + 1.96 * sqrt((LETALIDAD$LET_60A69 * (100 - LETALIDAD$LET_60A69)) /
                                      EDAD$CASOS.60A69)
LETALIDAD$LET_70A79 <- EDAD$MUERTES.70A79 / EDAD$CASOS.70A79 * 100
LETALIDAD$LI_LET_70A79 <-
  LETALIDAD$LET_70A79 - 1.96 * sqrt((LETALIDAD$LET_70A79 * (100 - LETALIDAD$LET_70A79)) /
                                      EDAD$CASOS.70A79)
LETALIDAD$LS_LET_70A79 <-
  LETALIDAD$LET_70A79 + 1.96 * sqrt((LETALIDAD$LET_70A79 * (100 - LETALIDAD$LET_70A79)) /
                                      EDAD$CASOS.70A79)
LETALIDAD$LET_80MAS <- EDAD$MUERTES.80MAS / EDAD$CASOS.80MAS * 100
LETALIDAD$LI_LET_80MAS <-
  LETALIDAD$LET_80MAS - 1.96 * sqrt((LETALIDAD$LET_80MAS * (100 - LETALIDAD$LET_80MAS)) /
                                      EDAD$CASOS.80MAS)
LETALIDAD$LS_LET_80MAS <-
  LETALIDAD$LET_80MAS + 1.96 * sqrt((LETALIDAD$LET_80MAS * (100 - LETALIDAD$LET_80MAS)) /
                                      EDAD$CASOS.80MAS)

# mapa CABA
load("mapa/mapaCaba.Rdata")
RME$comuna2 <- c("C 1", "C 2","C 3","C 4", "C 5", "C 6",
                 "C 7","C 8","C 9","C 10","C 11","C 12",
                 "C 13", "C 14", "C 15", "Total")

LETALIDAD$comuna2 <- c("C 1", "C 2","C 3","C 4", "C 5",
                       "C 6","C 7","C 8","C 9","C 10",
                       "C 11","C 12","C 13", "C 14", 
                       "C 15", "Total")

INCID$comuna2 <- c("C 1", "C 2","C 3","C 4", "C 5",
                   "C 6","C 7","C 8","C 9",
                   "C 10","C 11","C 12","C 13",
                   "C 14", "C 15", "Total")

POSITIVIDAD$comuna2 <- c("C 1", "C 2","C 3","C 4", "C 5",
                         "C 6","C 7","C 8","C 9",
                         "C 10","C 11","C 12","C 13",
                         "C 14", "C 15", "Total")

# función para mapas
func_tmap <-
  function(map,
           df,
           vector,
           titulo_graf = "Título mapa",
           titulo_leyenda = "Título leyenda"){
    mapa <- raster::merge(map, df)
    tm_shape(mapa) + tm_borders() +
      tm_fill(
        title = titulo_leyenda,
        palette = "Greys",
        col = vector,
        style = "quantile",
        n = 5
      ) +
    
      tm_text("comuna2",  size= 0.6,fontface = 1) +
      tm_layout(legend.outside = TRUE) +
      tm_scale_bar(position = c("right", "bottom")) +
      tm_compass(
        type = "arrow",
        position = c("right", "top"),
        show.labels = 0,
        size = 3
      ) +
      tm_layout(
        frame = FALSE,
        legend.outside = TRUE,
        legend.outside.position = "right",
        main.title = titulo_graf
      ) +
      tm_credits("",
                 size = 0.5,
                 position = c("center", "bottom"))
    
  }

# mapa de RMEs
mapa_rme <- func_tmap(mapa, RME, "RME", "RME", "RME")
mapa_let <- func_tmap(mapa, LETALIDAD, "LETALIDAD", "LETALIDAD", "LETALIDAD")
mapa_incid <- func_tmap(mapa, INCID, "INCID.POR.MIL", "INCID.POR.MIL", "Incidencia c/100 mil. hab.")
mapa_positiv <- func_tmap(mapa, POSITIVIDAD, "POSITIVIDAD", "Positividad", "Positividad")
# grafico
grafico <- ggplot(
  INCID.POR.EDAD %>% filter(COMUNA %in% c("COMUNA.04", "COMUNA.12")) %>% arrange(GRUPEDAD, COMUNA),
  aes(x = GRUPEDAD, y = INCID, fill = COMUNA)
) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_grey()


# function cociente de tasas
IC_coc_tasas <- function(df = POSITIVIDAD,
                         tasa = POSITIVIDAD$POSITIVIDAD,
                         numerador = POSITIVIDAD$CASOS,
                         denominador = POSITIVIDAD$TODOS,
                         gradiente = INDEX$INDEX)
{
  tasa_tot <- tasa[16]
  df <- df[1:15, ]
  df$COMUNA <- as.character(df$COMUNA)
  tasa_max = max(tasa[gradiente == min(gradiente)])
  tasa_min = min(tasa[gradiente == max(gradiente)])
  cmaxg = df$COMUNA[tasa == tasa_max]
  cming = df$COMUNA[tasa == tasa_min]
  cmin = df$COMUNA[tasa == min(tasa)]
  cmax = df$COMUNA[tasa == max(tasa)]
  num_max = numerador[tasa == tasa_max]
  num_min = numerador[tasa == tasa_min]
  den_max = denominador[tasa == tasa_max]
  den_min = denominador[tasa == tasa_min]
  COCIENTE = tasa_max / tasa_min
  LI = qbeta(0.025, num_max, num_min + 1) * den_min / (den_max * (1 - qbeta(0.025, num_max, num_min +
                                                                              1)))
  LS = qbeta(0.975, num_max + 1, num_min) * den_min / (den_max * (1 - qbeta(0.975, num_max +
                                                                              1, num_min)))
  COCIENTE = COCIENTE
  return(print(data.frame(
    MEDIDA = c(
      "TASA TOTAL",
      "TASA MAXIMA",
      "TASA MINIMA",
      "TASA MAXIMA CON GRADIENTE",
      "TASA MINIMA CON GRADIENTE",
      "COCIENTE DE TASAS EXTREMAS",
      "LI",
      "LS"
    ),
    VALOR = c(
      tasa_tot,
      max(tasa),
      min(tasa),
      tasa_max,
      tasa_min,
      COCIENTE,
      LI,
      LS
    ),
    COMUNA = c("CABA",
               cmax,
               cmin,
               cmaxg,
               cming,
               "",
               "",
               "")
  )))
  
}
IC_coc_tasas(INCID,
             INCID$INCID.POR.MIL,
             INCID$CASOS,
             INCID$POBLACION,
             INDEX$INDEX)


data.frame(RME$COMUNA, INCID$POBLACION, RME$RME, INDEX$INDEX, INDEX$ZONA) %>%
  filter(RME.COMUNA !="TOTAL")

INCID_segundo <- INCID 
library(ggplot2)
library(ggrepel)
graf_desigualdad <- data.frame(RME$COMUNA, INCID$POBLACION,
                               RME$RME, INDEX$INDEX, INDEX$ZONA)

graf_desigualdad$comuna <-c("C 1", "C 2","C 3","C 4", "C 5", "C 6","C 7","C 8","C 9",
                            "C 10","C 11","C 12","C 13", "C 14", "C 15", "Total")
graf_desigualdad_segunda <- ggplot(
  graf_desigualdad %>%
    filter(RME.COMUNA !="TOTAL"),
  aes(INDEX.INDEX, RME.RME, color = INDEX$ZONA[1:15])
) +
  geom_point(aes(size = RME$RME[1:15]))  +
  geom_smooth(aes(shape = NULL), method = "lm",se=F, col="grey")+
  ggpubr::stat_cor(aes(color = NULL),method = "pearson", label.x = 1.05, label.y = 125)+
  theme_light() +
  ylab("Razón de mortalidad estandarizada") +
  xlab("Indice de ingresos estandarizado") +
  ggtitle(
    "Razones de mortalidad estandarizadas por COVID-10, según comuna y zona. \nCiudad Autónoma de Buenos Aires. Año 2020-2021.",
    subtitle = waiver()
  ) +
  labs(color = 'Zona', size="RME") +
  geom_hline(yintercept = 100, linetype = "dashed") +
  geom_vline(xintercept = 1, linetype = "dashed")+
  geom_label_repel(aes(label = comuna),
                   box.padding   = 0.8, 
                   point.padding = 0.5,
                   label.size = NA, 
                   na.rm=TRUE,
                   color = 'black',
                   alpha = 0.4,
                   segment.color = 'grey50',) 

View(
  MEDIANA.EDAD %>% select(COMUNA, GRUPEDAD, MEDIANA.ESP, MEDIANA.OBS) %>% filter(MEDIANA.ESP != "" |
                                                                                   MEDIANA.OBS != "")
)

tabla_lolipop_segunda <- INCID.POR.EDAD %>% left_join(INDEX, by="COMUNA") %>% 
  group_by(ZONA, GRUPEDAD) %>% summarise(mean_incid=mean(INCID)) %>% 
  spread(ZONA,mean_incid) %>% select(-Total)

lollipop_segunda <- ggplot(tabla_lolipop_segunda) +
  geom_segment( aes(x=GRUPEDAD, xend=GRUPEDAD, y=Norte, yend=Sur), color="grey", size=1.5) +
  geom_point( aes(x=GRUPEDAD, y=Sur, color="Sur"), size=6 ) +
  geom_point( aes(x=GRUPEDAD, y=Centro, color="Centro"), size=6 ) +
  geom_point( aes(x=GRUPEDAD, y=Norte, color="Norte"), size=6 ) +
  scale_colour_manual(labels = c( "Centro","Norte","Sur"),
                      values = c( "#8A8A8A","#D4D4D4","#666666"))+
  # ylim(0, 90)+
  coord_flip()+
  theme(legend.position = "bottom") +
  labs(x="Grupo de edad", y="Promedio de incidencia acumulada",
       color="Zona:",
       title = " Gráfico de desigualdades. Segunda ola")


###### analisis de primer ola, comparado con segunda ola
#hay 14 casos outliers (previos al 24 de feb 2020)

#determinamos fin de primera ola e inicio de segunda a partir del
#la semana epidmeiologica con menos casos nuevos en la ciudad

casos_por_sepi <- covidCABA %>% 
  mutate(sepi=epiweek(fecha_clasificacion),
         ano=year(fecha_clasificacion)) %>% 
  group_by(ano,sepi) %>% 
  tally()


#podemos considerar la semna 48 del 2020 como la semana con menos casos de todo el periodo
#exceptuanod la SE 53 que es una semana corta por ser la ultima del año.
## semana 7 del 2021 fue la semana con menos casos del 2020en caba pero coincide con una 
# semana corta por el feriado de carnavales.

# se propone corte al final de la se 47, el dia 21 de noviembre

##### PRIMERA OLA ####
covidCABA_primera <- read.csv("datos/covidCABA.csv")

resumenArchivo <- list(
  registrosTotales <-
    paste0(
      "FECHA: ",
      Sys.Date(),
      " - REGISTROS TOTALES:",
      " ",
      nrow(covidCABA_primera )
    ),
  confirmados <-
    paste0(
      "FECHA: ",
      Sys.Date(),
      " - CASOS CONFIRMADOS:",
      " ",
      nrow(
        covidCABA_primera  %>% dplyr::filter(clasificacion == "confirmado" &
                                      provincia == "CABA")
      )
    ),
  eliminados <-
    paste0(
      "FECHA: ",
      Sys.Date(),
      " - CASOS CONFIRMADOS CABA SIN EDAD O COMUNA:",
      " ",
      nrow(
        covidCABA_primera  %>% dplyr::filter(
          clasificacion == "confirmado" &
            provincia ==
            "CABA" &
            is.na(edad) ==
            T &
            (is.na(comuna) ==
               T | comuna < 1 | comuna > 15)
        )
      )
    )
)

covidCABA_primera  <- covidCABA_primera  %>% dplyr::filter(provincia == "CABA" &
                                           between(comuna, 1, 15) &
                                           is.na(edad) == FALSE) %>%
  dplyr::mutate(
    fecha_clasificacion = as.Date(parse_date_time(
      substr(fecha_clasificacion, 1, 9), orders = c("dmy")
    )),
    comuna = paste0("COMUNA.", str_pad(comuna, 2, "left", "0")),
    fecha_fallecimiento = as.Date(parse_date_time(
      substr(fecha_fallecimiento, 1, 9), orders = c("dmy")
    ))
  )%>% dplyr::filter(fecha_clasificacion<="2020-11-28")



# df de todos los registrados#
TODOS <-
  covidCABA_primera %>% dplyr::group_by(COMUNA = comuna) %>% dplyr::tally()
colnames(TODOS)[2] <- "TODOS"
TODOS[16, 2] <- count(covidCABA_primera)
TODOS[16, 1] <- "TOTAL"

covidCABA_primera <-
  covidCABA_primera %>% dplyr::filter(clasificacion == "confirmado") %>%
  dplyr::mutate(
    GRUPEDAD = case_when(
      between(edad, 0, 4) ~ "0004",
      between(edad, 5, 9) ~ "0509",
      between(edad, 10, 14) ~ "1014",
      between(edad, 15, 19) ~ "1519",
      between(edad, 20, 24) ~ "2024",
      between(edad, 25, 29) ~ "2529",
      between(edad, 30, 34) ~ "3034",
      between(edad, 35, 39) ~ "3539",
      between(edad, 40, 44) ~ "4044",
      between(edad, 45, 49) ~ "4549",
      between(edad, 50, 54) ~ "5054",
      between(edad, 55, 59) ~ "5559",
      between(edad, 60, 64) ~ "6064",
      between(edad, 65, 69) ~ "6569",
      between(edad, 70, 74) ~ "7074",
      between(edad, 75, 79) ~ "7579",
      between(edad, 80, 130) ~ "80XX",
      is.na(edad) ~ "9999"
    )
  )

# agrega datos de población
load("datos/poblaCABA.Rdata")
poblaCABA <-
  poblaCABA %>% gather(COMUNA, POBTOT, 2:17) %>% dplyr::filter(GRUPEDAD !=
                                                                 "TOTAL")
poblaCABA$TOTEST <- sum(poblaCABA$POBTOT[poblaCABA$COMUNA == "TOTAL"])

# agrega población estandar
poblaCABA <-
  merge(poblaCABA,
        poblaCABA[poblaCABA$COMUNA == "TOTAL", ] %>%
          dplyr::select(GRUPEDAD, POBEST =POBTOT)) %>%
  arrange(COMUNA, GRUPEDAD)

# agrega casos
poblaCABA <-
  merge(
    poblaCABA,
    covidCABA_primera %>% dplyr::group_by(GRUPEDAD, COMUNA = comuna) %>%
      dplyr::summarise(CASOS =n()) %>% dplyr::select(GRUPEDAD, COMUNA, CASOS),
    all.y  = TRUE
  )
poblaCABA <-
  rbind(
    poblaCABA,
    poblaCABA %>% group_by(GRUPEDAD) %>% summarise(
      COMUNA = "TOTAL",
      POBTOT = sum(POBTOT),
      TOTEST = max(TOTEST),
      POBEST = max(POBEST),
      CASOS = sum(CASOS)
    )
  )

# df de incidencia por edad
INCID.POR.EDAD <- poblaCABA
INCID.POR.EDAD$INCID <-
  INCID.POR.EDAD$CASOS / INCID.POR.EDAD$POBTOT * 1000


# agrega mortalidad
poblaCABA <-
  merge(
    poblaCABA,
    covidCABA_primera %>% dplyr::filter(fallecido == "si") %>%
      dplyr::group_by(GRUPEDAD, COMUNA =comuna) %>% summarise(MUERTES = n()) %>% dplyr::select(GRUPEDAD, COMUNA, MUERTES),
    all.x = TRUE
  )
poblaCABA$GRUPEDAD <- as.character(poblaCABA$GRUPEDAD)
poblaCABA[is.na(poblaCABA)] <- 0

for (row in 1:nrow(poblaCABA)){
  grupedad_row <- poblaCABA$GRUPEDAD[row]
  if (poblaCABA$COMUNA[row] == "TOTAL"){
    poblaCABA$MUERTES[row] <-
      sum(poblaCABA$MUERTES[poblaCABA$GRUPEDAD == grupedad_row])
  }
}


# agrega tasas estandar
poblaCABA <-
  merge(
    poblaCABA,
    poblaCABA %>% dplyr::filter(poblaCABA$COMUNA == "TOTAL") %>%
      mutate(TASAS_REF =MUERTES / POBTOT) %>% dplyr::select(GRUPEDAD, TASAS_REF)
  )
poblaCABA$MUERTES.ESPERADAS <- poblaCABA$TASAS_REF * poblaCABA$POBTOT
poblaCABA[is.na(poblaCABA)] <- 0
sum(poblaCABA$MUERTES[poblaCABA$COMUNA == "TOTAL"])


# df frecuencia acumulada por edad
MEDIANA.EDAD <-
  poblaCABA %>% filter(GRUPEDAD != "9999" &
                         COMUNA != "COMUNA.Sin esp.")
MEDIANA.EDAD <- MEDIANA.EDAD %>% arrange(COMUNA, GRUPEDAD)
MEDIANA.EDAD <-
  MEDIANA.EDAD %>% group_by(COMUNA) %>%
  mutate(MUERTES.ESP.TOTAL = sum(MUERTES.ESPERADAS))
MEDIANA.EDAD <-
  MEDIANA.EDAD %>% group_by(COMUNA) %>%
  mutate(MUERTES.ESP.ACUM = cumsum(MUERTES.ESPERADAS) /
           MUERTES.ESP.TOTAL)
MEDIANA.EDAD <-
  MEDIANA.EDAD %>% group_by(COMUNA) %>% mutate(MUERTES.OBS.TOTAL = sum(MUERTES))
MEDIANA.EDAD <-
  MEDIANA.EDAD %>% group_by(COMUNA) %>% mutate(MUERTES.OBS.ACUM = cumsum(MUERTES) /
                                                 MUERTES.OBS.TOTAL)
MEDIANA.EDAD$MEDIANA.ESP <- ""
MEDIANA.EDAD$MEDIANA.OBS <- ""

for (row in 2:nrow(MEDIANA.EDAD)){
  if (MEDIANA.EDAD$MUERTES.ESP.ACUM[row] >= 0.5 &
      MEDIANA.EDAD$MUERTES.ESP.ACUM[row - 1] < 0.5)
  {
    MEDIANA.EDAD$MEDIANA.ESP[row] <- "X"
  }
  print(
    c(
      row,
      MEDIANA.EDAD$MUERTES.ESP.ACUM[row],
      MEDIANA.EDAD$MUERTES.ESP.ACUM[row - 1],
      MEDIANA.EDAD$MEDIANA.ESP[row]
    )
  )
}

for (row in 2:nrow(MEDIANA.EDAD)){
  if (MEDIANA.EDAD$MUERTES.OBS.ACUM[row] >= 0.5 &
      MEDIANA.EDAD$MUERTES.OBS.ACUM[row - 1] < 0.5)
  {
    MEDIANA.EDAD$MEDIANA.OBS[row] <- "X"
  }
  print(
    c(
      MEDIANA.EDAD$MUERTES.OBS.ACUM[row],
      MEDIANA.EDAD$MUERTES.OBS.ACUM[row - 1],
      MEDIANA.EDAD$MEDIANA.OBS[row]
    )
  )
}


# df con RMEs
RME <-
  poblaCABA %>% filter(GRUPEDAD != "9999" &
                         COMUNA != "COMUNA.Sin esp.")
RME <-
  poblaCABA %>% dplyr::group_by(COMUNA) %>% dplyr::summarise(
    OBS = sum(MUERTES),
    ESP = sum(MUERTES.ESPERADAS),
    RME = (sum(MUERTES) / sum(MUERTES.ESPERADAS) * 100)
  )
RME$LI = (RME$OBS * ((1 - (1 / (
  9 * RME$OBS
)) - (1.96 / 3) * sqrt((
  1 / RME$OBS
))) ^ 3)) / RME$ESP
RME$LS = (RME$OBS + 1) * ((1 - (1 / (9 * (
  RME$OBS + 1
))) + (1.96 / 3) * sqrt((1 / (
  RME$OBS + 1
)))) ^ 3) / RME$ESP
RME$link <- paste0("02", "0", substr(RME$COMUNA, 8, 10))
RME$RME <- round(RME$RME, digits = 1)

# df con incidencia acumulada
INCID <-
  poblaCABA %>% filter(GRUPEDAD != "9999" &
                         COMUNA != "COMUNA.Sin esp.")
INCID <-
  INCID %>% dplyr::group_by(COMUNA) %>% dplyr::summarise(
    CASOS = sum(CASOS),
    POBLACION = sum(POBTOT),
    INCID.POR.MIL = sum(CASOS) / sum(POBTOT)
  )
INCID$link <- paste0("02", "0", substr(INCID$COMUNA, 8, 10))
INCID$LI_INCID.POR.MIL <-
  INCID$INCID.POR.MIL - 1.96 * sqrt((INCID$INCID.POR.MIL * (1 - INCID$INCID.POR.MIL)) /
                                      INCID$POBLACION)
INCID$LS_INCID.POR.MIL <-
  INCID$INCID.POR.MIL + 1.96 * sqrt((INCID$INCID.POR.MIL * (1 - INCID$INCID.POR.MIL)) /
                                      INCID$POBLACION)
INCID$INCID.POR.MIL <- INCID$INCID.POR.MIL * 1000
INCID$LI_INCID.POR.MIL <- INCID$LI_INCID.POR.MIL * 1000
INCID$LS_INCID.POR.MIL <- INCID$LS_INCID.POR.MIL * 1000

# df con datos por edad
EDAD <-
  union_all(
    covidCABA_primera %>% filter(is.na(edad) == FALSE) %>% dplyr::select(comuna, edad),
    covidCABA_primera %>% filter(is.na(edad) == FALSE) %>% dplyr::select("TOTAL" =
                                                                   comuna, edad)
  )
EDAD$comuna[is.na(EDAD$comuna) == TRUE] <- "TOTAL"
EDAD$TOTAL <- NULL
EDAD$edad <- str_pad(as.character(EDAD$edad), 3, "left", "0")
EDAD <-
  EDAD %>% dplyr::group_by(comuna) %>% dplyr::summarise(
    CASOS.00A14 = sum(between(edad, "000", "014")),
    CASOS.15A49 =
      sum(between(edad, "015", "049")),
    CASOS.50A59 =
      sum(between(edad, "050", "059")),
    CASOS.60A69 =
      sum(between(edad, "060", "069")),
    CASOS.70A79 =
      sum(between(edad, "070", "079")),
    CASOS.80MAS =
      sum(between(edad, "080", "999"))
  )

EDAD_F <-
  union_all(
    covidCABA_primera %>% filter(is.na(edad) == FALSE &
                           fallecido == "si") %>% dplyr::select(comuna, edad),
    covidCABA_primera %>% filter(is.na(edad) == FALSE &
                           fallecido == "si") %>% dplyr::select("TOTAL" = comuna, edad)
  )

EDAD_F$comuna[is.na(EDAD_F$comuna) == TRUE] <- "TOTAL"
EDAD_F$TOTAL <- NULL
EDAD_F$edad <- str_pad(as.character(EDAD_F$edad), 3, "left", "0")
EDAD_F <-
  EDAD_F %>% dplyr::group_by(comuna) %>% dplyr::summarise(
    MUERTES.00A14 = sum(between(edad, "000", "014")),
    MUERTES.15A49 =
      sum(between(edad, "015", "049")),
    MUERTES.50A59 =
      sum(between(edad, "050", "059")),
    MUERTES.60A69 =
      sum(between(edad, "060", "069")),
    MUERTES.70A79 =
      sum(between(edad, "070", "079")),
    MUERTES.80MAS =
      sum(between(edad, "080", "999"))
  )

EDAD <- merge(EDAD, EDAD_F)

# df con poblacion por grupos de edad
POBLACION <-
  reshape(poblaCABA,
          timevar = "GRUPEDAD",
          idvar = "COMUNA",
          direction = "wide")
POBLACION <- data.frame(
  POBLACION$COMUNA,
  POBLACION$POBTOT.0004,
  POBLACION$POBTOT.0509,
  POBLACION$POBTOT.1014,
  POBLACION$POBTOT.1519,
  POBLACION$POBTOT.2024,
  POBLACION$POBTOT.2529,
  POBLACION$POBTOT.3034,
  POBLACION$POBTOT.3539,
  POBLACION$POBTOT.4044,
  POBLACION$POBTOT.4549,
  POBLACION$POBTOT.5054,
  POBLACION$POBTOT.5559,
  POBLACION$POBTOT.6064,
  POBLACION$POBTOT.6569,
  POBLACION$POBTOT.7074,
  POBLACION$POBTOT.7579,
  POBLACION$POBTOT.80XX
)

# df con positividad
POSITIVIDAD <- data.frame(
  COMUNA = TODOS$COMUNA,
  TODOS = TODOS$TODOS,
  CASOS = INCID$CASOS,
  POSITIVIDAD = INCID$CASOS / TODOS$TODOS
)

POSITIVIDAD$LI_POSITIVIDAD <-
  POSITIVIDAD$POSITIVIDAD - 1.96 * sqrt((POSITIVIDAD$POSITIVIDAD * (1 - POSITIVIDAD$POSITIVIDAD)) /
                                          POSITIVIDAD$TODOS)
POSITIVIDAD$LS_POSITIVIDAD <-
  POSITIVIDAD$POSITIVIDAD + 1.96 * sqrt((POSITIVIDAD$POSITIVIDAD * (1 - POSITIVIDAD$POSITIVIDAD)) /
                                          POSITIVIDAD$TODOS)
POSITIVIDAD$link <- paste0("02", "0", substr(POSITIVIDAD$COMUNA, 8, 10))

# df con letalidad
LETALIDAD <- data.frame(
  INCID$COMUNA,
  RME$OBS / INCID$CASOS * 100,
  RME$OBS / INCID$CASOS * 100 - 1.96 * sqrt((RME$OBS / INCID$CASOS * 100) * (100 - RME$OBS / INCID$CASOS * 100) / INCID$CASOS),
  RME$OBS / INCID$CASOS * 100 + 1.96 * sqrt((RME$OBS / INCID$CASOS * 100) * (100 - RME$OBS / INCID$CASOS * 100) / INCID$CASOS
  )
)


colnames(LETALIDAD)[1] <- "COMUNA"
colnames(LETALIDAD)[2] <- "LETALIDAD"
colnames(LETALIDAD)[3] <- "LI_LETALIDAD"
colnames(LETALIDAD)[4] <- "LS_LETALIDAD"

LETALIDAD$link <- paste0("02", "0", substr(LETALIDAD$COMUNA, 8, 10))

LETALIDAD$LET_00A14 <- EDAD$MUERTES.00A14 / EDAD$CASOS.00A14 * 100
LETALIDAD$LI_LET_00A14 <-
  LETALIDAD$LET_00A14 - 1.96 * sqrt((LETALIDAD$LET_00A14 * (100 - LETALIDAD$LET_00A14)) /
                                      EDAD$CASOS.00A14)
LETALIDAD$LS_LET_00A14 <-
  LETALIDAD$LET_00A14 + 1.96 * sqrt((LETALIDAD$LET_00A14 * (100 - LETALIDAD$LET_00A14)) /
                                      EDAD$CASOS.00A14)
LETALIDAD$LET_15A49 <- EDAD$MUERTES.15A49 / EDAD$CASOS.15A49 * 100
LETALIDAD$LI_LET_15A49 <-
  LETALIDAD$LET_15A49 - 1.96 * sqrt((LETALIDAD$LET_15A49 * (100 - LETALIDAD$LET_15A49)) /
                                      EDAD$CASOS.15A49)
LETALIDAD$LS_LET_15A49 <-
  LETALIDAD$LET_15A49 + 1.96 * sqrt((LETALIDAD$LET_15A49 * (100 - LETALIDAD$LET_15A49)) /
                                      EDAD$CASOS.15A49)
LETALIDAD$LET_50A59 <- EDAD$MUERTES.50A59 / EDAD$CASOS.50A59 * 100
LETALIDAD$LI_LET_50A59 <-
  LETALIDAD$LET_50A59 - 1.96 * sqrt((LETALIDAD$LET_50A59 * (100 - LETALIDAD$LET_50A59)) /
                                      EDAD$CASOS.50A59)
LETALIDAD$LS_LET_50A59 <-
  LETALIDAD$LET_50A59 + 1.96 * sqrt((LETALIDAD$LET_50A59 * (100 - LETALIDAD$LET_50A59)) /
                                      EDAD$CASOS.50A59)
LETALIDAD$LET_60A69 <- EDAD$MUERTES.60A69 / EDAD$CASOS.60A69 * 100
LETALIDAD$LI_LET_60A69 <-
  LETALIDAD$LET_60A69 - 1.96 * sqrt((LETALIDAD$LET_60A69 * (100 - LETALIDAD$LET_60A69)) /
                                      EDAD$CASOS.60A69)
LETALIDAD$LS_LET_60A69 <-
  LETALIDAD$LET_60A69 + 1.96 * sqrt((LETALIDAD$LET_60A69 * (100 - LETALIDAD$LET_60A69)) /
                                      EDAD$CASOS.60A69)
LETALIDAD$LET_70A79 <- EDAD$MUERTES.70A79 / EDAD$CASOS.70A79 * 100
LETALIDAD$LI_LET_70A79 <-
  LETALIDAD$LET_70A79 - 1.96 * sqrt((LETALIDAD$LET_70A79 * (100 - LETALIDAD$LET_70A79)) /
                                      EDAD$CASOS.70A79)
LETALIDAD$LS_LET_70A79 <-
  LETALIDAD$LET_70A79 + 1.96 * sqrt((LETALIDAD$LET_70A79 * (100 - LETALIDAD$LET_70A79)) /
                                      EDAD$CASOS.70A79)
LETALIDAD$LET_80MAS <- EDAD$MUERTES.80MAS / EDAD$CASOS.80MAS * 100
LETALIDAD$LI_LET_80MAS <-
  LETALIDAD$LET_80MAS - 1.96 * sqrt((LETALIDAD$LET_80MAS * (100 - LETALIDAD$LET_80MAS)) /
                                      EDAD$CASOS.80MAS)
LETALIDAD$LS_LET_80MAS <-
  LETALIDAD$LET_80MAS + 1.96 * sqrt((LETALIDAD$LET_80MAS * (100 - LETALIDAD$LET_80MAS)) /
                                      EDAD$CASOS.80MAS)

# mapa CABA
load("mapa/mapaCaba.Rdata")
RME$comuna2 <- c("C 1", "C 2","C 3","C 4", "C 5", "C 6",
                 "C 7","C 8","C 9","C 10","C 11","C 12",
                 "C 13", "C 14", "C 15", "Total")

LETALIDAD$comuna2 <- c("C 1", "C 2","C 3","C 4", "C 5",
                       "C 6","C 7","C 8","C 9","C 10",
                       "C 11","C 12","C 13", "C 14", 
                       "C 15", "Total")

INCID$comuna2 <- c("C 1", "C 2","C 3","C 4", "C 5",
                   "C 6","C 7","C 8","C 9",
                   "C 10","C 11","C 12","C 13",
                   "C 14", "C 15", "Total")

POSITIVIDAD$comuna2 <- c("C 1", "C 2","C 3","C 4", "C 5",
                         "C 6","C 7","C 8","C 9",
                         "C 10","C 11","C 12","C 13",
                         "C 14", "C 15", "Total")

# función para mapas
func_tmap <-
  function(map,
           df,
           vector,
           titulo_graf = "Título mapa",
           titulo_leyenda = "Título leyenda"){
    mapa <- raster::merge(map, df)
    tm_shape(mapa) + tm_borders() +
      tm_fill(
        title = titulo_leyenda,
        palette = "Greys",
        col = vector,
        style = "quantile",
        n = 5
      ) +
      
      tm_text("comuna2",  size= 0.6,fontface = 1) +
      tm_layout(legend.outside = TRUE) +
      tm_scale_bar(position = c("right", "bottom")) +
      tm_compass(
        type = "arrow",
        position = c("right", "top"),
        show.labels = 0,
        size = 3
      ) +
      tm_layout(
        frame = FALSE,
        legend.outside = TRUE,
        legend.outside.position = "right",
        main.title = titulo_graf
      ) +
      tm_credits("",
                 size = 0.5,
                 position = c("center", "bottom"))
    
  }

#### mapas ####
mapa_rme_primera <- func_tmap(mapa, RME, "RME", "RME", "RME")
mapa_let_primera <- func_tmap(mapa, LETALIDAD, "LETALIDAD", "LETALIDAD", "LETALIDAD")
mapa_incid_primera <- func_tmap(mapa, INCID, "INCID.POR.MIL", "INCID.POR.MIL", "Incidencia c/100 mil. hab.")
mapa_positiv_primera <- func_tmap(mapa, POSITIVIDAD, "POSITIVIDAD", "Positividad", "Positividad")
# grafico
grafico_primera<- ggplot(
  INCID.POR.EDAD %>% filter(COMUNA %in% c("COMUNA.04", "COMUNA.12")) %>% arrange(GRUPEDAD, COMUNA),
  aes(x = GRUPEDAD, y = INCID, fill = COMUNA)
) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_grey()


# function cociente de tasas
IC_coc_tasas <- function(df = POSITIVIDAD,
                         tasa = POSITIVIDAD$POSITIVIDAD,
                         numerador = POSITIVIDAD$CASOS,
                         denominador = POSITIVIDAD$TODOS,
                         gradiente = INDEX$INDEX)
{
  tasa_tot <- tasa[16]
  df <- df[1:15, ]
  df$COMUNA <- as.character(df$COMUNA)
  tasa_max = max(tasa[gradiente == min(gradiente)])
  tasa_min = min(tasa[gradiente == max(gradiente)])
  cmaxg = df$COMUNA[tasa == tasa_max]
  cming = df$COMUNA[tasa == tasa_min]
  cmin = df$COMUNA[tasa == min(tasa)]
  cmax = df$COMUNA[tasa == max(tasa)]
  num_max = numerador[tasa == tasa_max]
  num_min = numerador[tasa == tasa_min]
  den_max = denominador[tasa == tasa_max]
  den_min = denominador[tasa == tasa_min]
  COCIENTE = tasa_max / tasa_min
  LI = qbeta(0.025, num_max, num_min + 1) * den_min / (den_max * (1 - qbeta(0.025, num_max, num_min +
                                                                              1)))
  LS = qbeta(0.975, num_max + 1, num_min) * den_min / (den_max * (1 - qbeta(0.975, num_max +
                                                                              1, num_min)))
  COCIENTE = COCIENTE
  return(print(data.frame(
    MEDIDA = c(
      "TASA TOTAL",
      "TASA MAXIMA",
      "TASA MINIMA",
      "TASA MAXIMA CON GRADIENTE",
      "TASA MINIMA CON GRADIENTE",
      "COCIENTE DE TASAS EXTREMAS",
      "LI",
      "LS"
    ),
    VALOR = c(
      tasa_tot,
      max(tasa),
      min(tasa),
      tasa_max,
      tasa_min,
      COCIENTE,
      LI,
      LS
    ),
    COMUNA = c("CABA",
               cmax,
               cmin,
               cmaxg,
               cming,
               "",
               "",
               "")
  )))
  
}
IC_coc_tasas(INCID,
             INCID$INCID.POR.MIL,
             INCID$CASOS,
             INCID$POBLACION,
             INDEX$INDEX)


data.frame(RME$COMUNA, INCID$POBLACION, RME$RME, INDEX$INDEX, INDEX$ZONA) %>% filter(RME.COMUNA !=
                                                                                       "TOTAL")
library(ggplot2)
library(ggrepel)
graf_desigualdad <- data.frame(RME$COMUNA, INCID$POBLACION,
                               RME$RME, INDEX$INDEX, INDEX$ZONA)

graf_desigualdad$comuna <-c("C 1", "C 2","C 3","C 4", "C 5", "C 6","C 7","C 8","C 9",
                            "C 10","C 11","C 12","C 13", "C 14", "C 15", "Total")
graf_desigualdad_primera <- ggplot(
  graf_desigualdad %>%
    filter(RME.COMUNA !="TOTAL"),
  aes(INDEX.INDEX, RME.RME, color = INDEX$ZONA[1:15])
) +
  geom_point(aes(size = RME$RME[1:15]))  +
  geom_smooth(aes(shape = NULL), method = "lm",se=F, col="grey")+
  ggpubr::stat_cor(aes(color = NULL),method = "pearson", label.x = 1.05, label.y = 125)+
  theme_light() +
  ylab("Razón de mortalidad estandarizada") +
  xlab("Indice de ingresos estandarizado") +
  ggtitle(
    "Razones de mortalidad estandarizadas por COVID-10, según comuna y zona. \nCiudad Autónoma de Buenos Aires. Año 2020-2021.",
    subtitle = waiver() ) +
  labs(color = 'Zona', size="RME") +
  geom_hline(yintercept = 100, linetype = "dashed") +
  geom_vline(xintercept = 1, linetype = "dashed")+
  geom_label_repel(aes(label = comuna),
                   box.padding   = 0.8, 
                   point.padding = 0.5,
                   label.size = NA, 
                   na.rm=TRUE,
                   color = 'black',
                   alpha = 0.4,
                   segment.color = 'grey50',) 

graf_desigualdad_primera
View(
  MEDIANA.EDAD %>% select(COMUNA, GRUPEDAD, MEDIANA.ESP, MEDIANA.OBS) %>% filter(MEDIANA.ESP != "" |
                                                                                   MEDIANA.OBS != "")
)
### inequality plots
tabla_lolipop_primera <- INCID.POR.EDAD %>% left_join(INDEX, by="COMUNA") %>% 
  group_by(ZONA, GRUPEDAD) %>% summarise(mean_incid=mean(INCID)) %>% 
  spread(ZONA,mean_incid) %>% select(-Total)

lollipop_primera <- ggplot(tabla_lolipop_primera) +
  geom_segment( aes(x=GRUPEDAD, xend=GRUPEDAD, y=Norte, yend=Sur), color="grey", size=1.5) +
  geom_point( aes(x=GRUPEDAD, y=Sur, color="Sur"), size=6 ) +
  geom_point( aes(x=GRUPEDAD, y=Centro, color="Centro"), size=6 ) +
  geom_point( aes(x=GRUPEDAD, y=Norte, color="Norte"), size=6 ) +
  scale_colour_manual(labels = c( "Centro","Norte","Sur"),
                     values = c( "#8A8A8A","#D4D4D4","#666666"))+
 # ylim(0, 90)+
  coord_flip()+
  theme(legend.position = "bottom") +
  labs(x="Grupo de edad", y="Promedio de incidencia acumulada",
       color="Zona:",
       title = " Gráfico de desigualdades. Primera ola")

#### inequality plots letalidad primera ola #####
tabla_letalidad_primera <- LETALIDAD %>% left_join(INDEX, by="COMUNA") %>% 
  select(c(6,9,12,15,18,21,26)) %>% 
  gather("grupo_edad", "let", -ZONA) %>% group_by(ZONA,grupo_edad) %>%
  summarise(promedio_let=mean(let)) %>% spread(ZONA, promedio_let) %>% 
  select(-Total)
tabla_letalidad_primera <- tabla_letalidad_primera[,c(1,3,2,4)]
lollipop_let_primera <- ggplot(tabla_letalidad_primera) +
  geom_segment( aes(x=grupo_edad, xend=grupo_edad, y=Norte, yend=Sur), color="grey", size=1.5) +
  geom_point( aes(x=grupo_edad, y=Sur, color="Sur"), size=6 ) +
  geom_point( aes(x=grupo_edad, y=Centro, color="Centro"), size=6 ) +
  geom_point( aes(x=grupo_edad, y=Norte, color="Norte"), size=6 ) +
  scale_colour_manual(labels = c( "Centro","Norte","Sur"),
                      values = c( "#8A8A8A","#D4D4D4","#666666"))+
  # ylim(0, 90)+
  coord_flip()+
  theme(legend.position = "bottom") +
  theme_bw()+
  labs(x="Grupo de edad", y="Promedio de letalidad por zona",
       color="Zona:",
       title = " Gráfico de desigualdades. Primera ola. Letalidad")

lollipop_let_primera
##### comparo graficos entre la primera ola y la segunda#####
#incidencia
mapa_incid_primera_grob <- tmap_grob(mapa_incid_primera)
mapa_incid_grob <- tmap_grob(mapa_incid)
grid.arrange(mapa_incid_primera_grob , mapa_incid_grob,
             top = "Titulo,incidencia comparación primera y segunda ola", nrow = 1)

#letalidad
mapa_let_primera_grob <- tmap_grob(mapa_let_primera)
mapa_let_grob <- tmap_grob(mapa_let)
grid.arrange(mapa_let_primera_grob,mapa_let_grob , 
             top = "Titulo,letalidad comparación primera y segunda ola", nrow = 1)

#positividad
mapa_positiv_primera_grob <- tmap_grob(mapa_positiv_primera)
mapa_positiv_grob <- tmap_grob(mapa_positiv)
grid.arrange(mapa_positiv_primera_grob , mapa_positiv_grob ,
             top = "Titulo,positividad comparación primera y segunda ola", nrow = 1)

#rme
mapa_rme_primera_grob <- tmap_grob(mapa_rme_primera)
mapa_rme_grob <- tmap_grob(mapa_rme)
grid.arrange(mapa_rme_primera_grob , mapa_rme_grob,
             top = "Titulo,RME comparación primera y segunda ola", nrow = 1)

gridExtra::grid.arrange(graf_desigualdad_primera,graf_desigualdad_segunda,ncol = 2)
gridExtra::grid.arrange(lollipop_primera,lollipop_segunda,ncol = 2)

##### PERIODO COMPLETO ####
###### grafico de desigualdades para todo el periodo #####

covidCABA_tot<- read.csv("datos/covidCABA.csv")

resumenArchivo <- list(
  registrosTotales <-
    paste0(
      "FECHA: ",
      Sys.Date(),
      " - REGISTROS TOTALES:",
      " ",
      nrow(covidCABA_tot)
    ),
  confirmados <-
    paste0(
      "FECHA: ",
      Sys.Date(),
      " - CASOS CONFIRMADOS:",
      " ",
      nrow(
        covidCABA_tot %>% dplyr::filter(clasificacion == "confirmado" &
                                      provincia == "CABA")
      )
    ),
  eliminados <-
    paste0(
      "FECHA: ",
      Sys.Date(),
      " - CASOS CONFIRMADOS CABA SIN EDAD O COMUNA:",
      " ",
      nrow(
        covidCABA_tot %>% dplyr::filter(
          clasificacion == "confirmado" &
            provincia ==
            "CABA" &
            is.na(edad) ==
            T &
            (is.na(comuna) ==
               T | comuna < 1 | comuna > 15)
        )
      )
    )
)

covidCABA_tot <- covidCABA_tot %>% dplyr::filter(provincia == "CABA" &
                                           between(comuna, 1, 15) &
                                           is.na(edad) == FALSE) %>%
  dplyr::mutate(
    fecha_clasificacion = as.Date(parse_date_time(
      substr(fecha_clasificacion, 1, 9), orders = c("dmy")
    )),
    comuna = paste0("COMUNA.", str_pad(comuna, 2, "left", "0")),
    fecha_fallecimiento = as.Date(parse_date_time(
      substr(fecha_fallecimiento, 1, 9), orders = c("dmy")
    ))
  )%>% dplyr::filter(fecha_clasificacion<="2021-04-30")

# df de todos los registrados
TODOS <-
  covidCABA_tot %>% dplyr::group_by(COMUNA = comuna) %>% dplyr::tally()
colnames(TODOS)[2] <- "TODOS"
TODOS[16, 2] <- count(covidCABA_tot)
TODOS[16, 1] <- "TOTAL"

covidCABA_tot <-
  covidCABA_tot %>% dplyr::filter(clasificacion == "confirmado") %>%
  dplyr::mutate(
    GRUPEDAD = case_when(
      between(edad, 0, 4) ~ "0004",
      between(edad, 5, 9) ~ "0509",
      between(edad, 10, 14) ~ "1014",
      between(edad, 15, 19) ~ "1519",
      between(edad, 20, 24) ~ "2024",
      between(edad, 25, 29) ~ "2529",
      between(edad, 30, 34) ~ "3034",
      between(edad, 35, 39) ~ "3539",
      between(edad, 40, 44) ~ "4044",
      between(edad, 45, 49) ~ "4549",
      between(edad, 50, 54) ~ "5054",
      between(edad, 55, 59) ~ "5559",
      between(edad, 60, 64) ~ "6064",
      between(edad, 65, 69) ~ "6569",
      between(edad, 70, 74) ~ "7074",
      between(edad, 75, 79) ~ "7579",
      between(edad, 80, 130) ~ "80XX",
      is.na(edad) ~ "9999"
    )
  )

# agrega datos de población
load("datos/poblaCABA.Rdata")
poblaCABA <-
  poblaCABA %>% gather(COMUNA, POBTOT, 2:17) %>% dplyr::filter(GRUPEDAD !=
                                                                 "TOTAL")
poblaCABA$TOTEST <- sum(poblaCABA$POBTOT[poblaCABA$COMUNA == "TOTAL"])

# agrega población estandar
poblaCABA <-
  merge(poblaCABA,
        poblaCABA[poblaCABA$COMUNA == "TOTAL", ] %>%
          dplyr::select(GRUPEDAD, POBEST =POBTOT)) %>%
  arrange(COMUNA, GRUPEDAD)

# agrega casos
poblaCABA <-
  merge(
    poblaCABA,
    covidCABA_tot %>% dplyr::group_by(GRUPEDAD, COMUNA = comuna) %>%
      dplyr::summarise(CASOS =n()) %>% dplyr::select(GRUPEDAD, COMUNA, CASOS),
    all.y  = TRUE
  )
poblaCABA <-
  rbind(
    poblaCABA,
    poblaCABA %>% group_by(GRUPEDAD) %>% summarise(
      COMUNA = "TOTAL",
      POBTOT = sum(POBTOT),
      TOTEST = max(TOTEST),
      POBEST = max(POBEST),
      CASOS = sum(CASOS)
    )
  )

# df de incidencia por edad
INCID.POR.EDAD <- poblaCABA
INCID.POR.EDAD$INCID <-
  INCID.POR.EDAD$CASOS / INCID.POR.EDAD$POBTOT * 1000


# agrega mortalidad
poblaCABA <-
  merge(
    poblaCABA,
    covidCABA_tot %>% dplyr::filter(fallecido == "si") %>%
      dplyr::group_by(GRUPEDAD, COMUNA =comuna) %>% summarise(MUERTES = n()) %>% dplyr::select(GRUPEDAD, COMUNA, MUERTES),
    all.x = TRUE
  )
poblaCABA$GRUPEDAD <- as.character(poblaCABA$GRUPEDAD)
poblaCABA[is.na(poblaCABA)] <- 0

for (row in 1:nrow(poblaCABA)){
  grupedad_row <- poblaCABA$GRUPEDAD[row]
  if (poblaCABA$COMUNA[row] == "TOTAL"){
    poblaCABA$MUERTES[row] <-
      sum(poblaCABA$MUERTES[poblaCABA$GRUPEDAD == grupedad_row])
  }
}


# agrega tasas estandar
poblaCABA <-
  merge(
    poblaCABA,
    poblaCABA %>% dplyr::filter(poblaCABA$COMUNA == "TOTAL") %>%
      mutate(TASAS_REF =MUERTES / POBTOT) %>% dplyr::select(GRUPEDAD, TASAS_REF)
  )
poblaCABA$MUERTES.ESPERADAS <- poblaCABA$TASAS_REF * poblaCABA$POBTOT
poblaCABA[is.na(poblaCABA)] <- 0
sum(poblaCABA$MUERTES[poblaCABA$COMUNA == "TOTAL"])


# df frecuencia acumulada por edad
MEDIANA.EDAD <-
  poblaCABA %>% filter(GRUPEDAD != "9999" &
                         COMUNA != "COMUNA.Sin esp.")
MEDIANA.EDAD <- MEDIANA.EDAD %>% arrange(COMUNA, GRUPEDAD)
MEDIANA.EDAD <-
  MEDIANA.EDAD %>% group_by(COMUNA) %>%
  mutate(MUERTES.ESP.TOTAL = sum(MUERTES.ESPERADAS))
MEDIANA.EDAD <-
  MEDIANA.EDAD %>% group_by(COMUNA) %>%
  mutate(MUERTES.ESP.ACUM = cumsum(MUERTES.ESPERADAS) /
           MUERTES.ESP.TOTAL)
MEDIANA.EDAD <-
  MEDIANA.EDAD %>% group_by(COMUNA) %>% mutate(MUERTES.OBS.TOTAL = sum(MUERTES))
MEDIANA.EDAD <-
  MEDIANA.EDAD %>% group_by(COMUNA) %>% mutate(MUERTES.OBS.ACUM = cumsum(MUERTES) /
                                                 MUERTES.OBS.TOTAL)
MEDIANA.EDAD$MEDIANA.ESP <- ""
MEDIANA.EDAD$MEDIANA.OBS <- ""

for (row in 2:nrow(MEDIANA.EDAD)){
  if (MEDIANA.EDAD$MUERTES.ESP.ACUM[row] >= 0.5 &
      MEDIANA.EDAD$MUERTES.ESP.ACUM[row - 1] < 0.5)
  {
    MEDIANA.EDAD$MEDIANA.ESP[row] <- "X"
  }
  print(
    c(
      row,
      MEDIANA.EDAD$MUERTES.ESP.ACUM[row],
      MEDIANA.EDAD$MUERTES.ESP.ACUM[row - 1],
      MEDIANA.EDAD$MEDIANA.ESP[row]
    )
  )
}

for (row in 2:nrow(MEDIANA.EDAD)){
  if (MEDIANA.EDAD$MUERTES.OBS.ACUM[row] >= 0.5 &
      MEDIANA.EDAD$MUERTES.OBS.ACUM[row - 1] < 0.5)
  {
    MEDIANA.EDAD$MEDIANA.OBS[row] <- "X"
  }
  print(
    c(
      MEDIANA.EDAD$MUERTES.OBS.ACUM[row],
      MEDIANA.EDAD$MUERTES.OBS.ACUM[row - 1],
      MEDIANA.EDAD$MEDIANA.OBS[row]
    )
  )
}


# df con RMEs
RME <-
  poblaCABA %>% filter(GRUPEDAD != "9999" &
                         COMUNA != "COMUNA.Sin esp.")
RME <-
  poblaCABA %>% dplyr::group_by(COMUNA) %>% dplyr::summarise(
    OBS = sum(MUERTES),
    ESP = sum(MUERTES.ESPERADAS),
    RME = (sum(MUERTES) / sum(MUERTES.ESPERADAS) * 100)
  )
RME$LI = (RME$OBS * ((1 - (1 / (
  9 * RME$OBS
)) - (1.96 / 3) * sqrt((
  1 / RME$OBS
))) ^ 3)) / RME$ESP
RME$LS = (RME$OBS + 1) * ((1 - (1 / (9 * (
  RME$OBS + 1
))) + (1.96 / 3) * sqrt((1 / (
  RME$OBS + 1
)))) ^ 3) / RME$ESP
RME$link <- paste0("02", "0", substr(RME$COMUNA, 8, 10))
RME$RME <- round(RME$RME, digits = 1)

# df con incidencia acumulada
INCID <-
  poblaCABA %>% filter(GRUPEDAD != "9999" &
                         COMUNA != "COMUNA.Sin esp.")
INCID <-
  INCID %>% dplyr::group_by(COMUNA) %>% dplyr::summarise(
    CASOS = sum(CASOS),
    POBLACION = sum(POBTOT),
    INCID.POR.MIL = sum(CASOS) / sum(POBTOT)
  )
INCID$link <- paste0("02", "0", substr(INCID$COMUNA, 8, 10))
INCID$LI_INCID.POR.MIL <-
  INCID$INCID.POR.MIL - 1.96 * sqrt((INCID$INCID.POR.MIL * (1 - INCID$INCID.POR.MIL)) /
                                      INCID$POBLACION)
INCID$LS_INCID.POR.MIL <-
  INCID$INCID.POR.MIL + 1.96 * sqrt((INCID$INCID.POR.MIL * (1 - INCID$INCID.POR.MIL)) /
                                      INCID$POBLACION)
INCID$INCID.POR.MIL <- INCID$INCID.POR.MIL * 1000
INCID$LI_INCID.POR.MIL <- INCID$LI_INCID.POR.MIL * 1000
INCID$LS_INCID.POR.MIL <- INCID$LS_INCID.POR.MIL * 1000

# df con datos por edad
EDAD <-
  union_all(
    covidCABA_tot %>% filter(is.na(edad) == FALSE) %>% dplyr::select(comuna, edad),
    covidCABA_tot %>% filter(is.na(edad) == FALSE) %>% dplyr::select("TOTAL" =
                                                                   comuna, edad)
  )
EDAD$comuna[is.na(EDAD$comuna) == TRUE] <- "TOTAL"
EDAD$TOTAL <- NULL
EDAD$edad <- str_pad(as.character(EDAD$edad), 3, "left", "0")
EDAD <-
  EDAD %>% dplyr::group_by(comuna) %>% dplyr::summarise(
    CASOS.00A14 = sum(between(edad, "000", "014")),
    CASOS.15A49 =
      sum(between(edad, "015", "049")),
    CASOS.50A59 =
      sum(between(edad, "050", "059")),
    CASOS.60A69 =
      sum(between(edad, "060", "069")),
    CASOS.70A79 =
      sum(between(edad, "070", "079")),
    CASOS.80MAS =
      sum(between(edad, "080", "999"))
  )

EDAD_F <-
  union_all(
    covidCABA_tot %>% filter(is.na(edad) == FALSE &
                           fallecido == "si") %>% dplyr::select(comuna, edad),
    covidCABA_tot %>% filter(is.na(edad) == FALSE &
                           fallecido == "si") %>% dplyr::select("TOTAL" = comuna, edad)
  )

EDAD_F$comuna[is.na(EDAD_F$comuna) == TRUE] <- "TOTAL"
EDAD_F$TOTAL <- NULL
EDAD_F$edad <- str_pad(as.character(EDAD_F$edad), 3, "left", "0")
EDAD_F <-
  EDAD_F %>% dplyr::group_by(comuna) %>% dplyr::summarise(
    MUERTES.00A14 = sum(between(edad, "000", "014")),
    MUERTES.15A49 =
      sum(between(edad, "015", "049")),
    MUERTES.50A59 =
      sum(between(edad, "050", "059")),
    MUERTES.60A69 =
      sum(between(edad, "060", "069")),
    MUERTES.70A79 =
      sum(between(edad, "070", "079")),
    MUERTES.80MAS =
      sum(between(edad, "080", "999"))
  )

EDAD <- merge(EDAD, EDAD_F)

# df con poblacion por grupos de edad
POBLACION <-
  reshape(poblaCABA,
          timevar = "GRUPEDAD",
          idvar = "COMUNA",
          direction = "wide")
POBLACION <- data.frame(
  POBLACION$COMUNA,
  POBLACION$POBTOT.0004,
  POBLACION$POBTOT.0509,
  POBLACION$POBTOT.1014,
  POBLACION$POBTOT.1519,
  POBLACION$POBTOT.2024,
  POBLACION$POBTOT.2529,
  POBLACION$POBTOT.3034,
  POBLACION$POBTOT.3539,
  POBLACION$POBTOT.4044,
  POBLACION$POBTOT.4549,
  POBLACION$POBTOT.5054,
  POBLACION$POBTOT.5559,
  POBLACION$POBTOT.6064,
  POBLACION$POBTOT.6569,
  POBLACION$POBTOT.7074,
  POBLACION$POBTOT.7579,
  POBLACION$POBTOT.80XX
)

# df con positividad
POSITIVIDAD <- data.frame(
  COMUNA = TODOS$COMUNA,
  TODOS = TODOS$TODOS,
  CASOS = INCID$CASOS,
  POSITIVIDAD = INCID$CASOS / TODOS$TODOS
)

POSITIVIDAD$LI_POSITIVIDAD <-
  POSITIVIDAD$POSITIVIDAD - 1.96 * sqrt((POSITIVIDAD$POSITIVIDAD * (1 - POSITIVIDAD$POSITIVIDAD)) /
                                          POSITIVIDAD$TODOS)
POSITIVIDAD$LS_POSITIVIDAD <-
  POSITIVIDAD$POSITIVIDAD + 1.96 * sqrt((POSITIVIDAD$POSITIVIDAD * (1 - POSITIVIDAD$POSITIVIDAD)) /
                                          POSITIVIDAD$TODOS)
POSITIVIDAD$link <- paste0("02", "0", substr(POSITIVIDAD$COMUNA, 8, 10))

# df con letalidad
LETALIDAD <- data.frame(
  INCID$COMUNA,
  RME$OBS / INCID$CASOS * 100,
  RME$OBS / INCID$CASOS * 100 - 1.96 * sqrt((RME$OBS / INCID$CASOS * 100) * (100 - RME$OBS / INCID$CASOS * 100) / INCID$CASOS),
  RME$OBS / INCID$CASOS * 100 + 1.96 * sqrt((RME$OBS / INCID$CASOS * 100) * (100 - RME$OBS / INCID$CASOS * 100) / INCID$CASOS
  )
)


colnames(LETALIDAD)[1] <- "COMUNA"
colnames(LETALIDAD)[2] <- "LETALIDAD"
colnames(LETALIDAD)[3] <- "LI_LETALIDAD"
colnames(LETALIDAD)[4] <- "LS_LETALIDAD"

LETALIDAD$link <- paste0("02", "0", substr(LETALIDAD$COMUNA, 8, 10))

LETALIDAD$LET_00A14 <- EDAD$MUERTES.00A14 / EDAD$CASOS.00A14 * 100
LETALIDAD$LI_LET_00A14 <-
  LETALIDAD$LET_00A14 - 1.96 * sqrt((LETALIDAD$LET_00A14 * (100 - LETALIDAD$LET_00A14)) /
                                      EDAD$CASOS.00A14)
LETALIDAD$LS_LET_00A14 <-
  LETALIDAD$LET_00A14 + 1.96 * sqrt((LETALIDAD$LET_00A14 * (100 - LETALIDAD$LET_00A14)) /
                                      EDAD$CASOS.00A14)
LETALIDAD$LET_15A49 <- EDAD$MUERTES.15A49 / EDAD$CASOS.15A49 * 100
LETALIDAD$LI_LET_15A49 <-
  LETALIDAD$LET_15A49 - 1.96 * sqrt((LETALIDAD$LET_15A49 * (100 - LETALIDAD$LET_15A49)) /
                                      EDAD$CASOS.15A49)
LETALIDAD$LS_LET_15A49 <-
  LETALIDAD$LET_15A49 + 1.96 * sqrt((LETALIDAD$LET_15A49 * (100 - LETALIDAD$LET_15A49)) /
                                      EDAD$CASOS.15A49)
LETALIDAD$LET_50A59 <- EDAD$MUERTES.50A59 / EDAD$CASOS.50A59 * 100
LETALIDAD$LI_LET_50A59 <-
  LETALIDAD$LET_50A59 - 1.96 * sqrt((LETALIDAD$LET_50A59 * (100 - LETALIDAD$LET_50A59)) /
                                      EDAD$CASOS.50A59)
LETALIDAD$LS_LET_50A59 <-
  LETALIDAD$LET_50A59 + 1.96 * sqrt((LETALIDAD$LET_50A59 * (100 - LETALIDAD$LET_50A59)) /
                                      EDAD$CASOS.50A59)
LETALIDAD$LET_60A69 <- EDAD$MUERTES.60A69 / EDAD$CASOS.60A69 * 100
LETALIDAD$LI_LET_60A69 <-
  LETALIDAD$LET_60A69 - 1.96 * sqrt((LETALIDAD$LET_60A69 * (100 - LETALIDAD$LET_60A69)) /
                                      EDAD$CASOS.60A69)
LETALIDAD$LS_LET_60A69 <-
  LETALIDAD$LET_60A69 + 1.96 * sqrt((LETALIDAD$LET_60A69 * (100 - LETALIDAD$LET_60A69)) /
                                      EDAD$CASOS.60A69)
LETALIDAD$LET_70A79 <- EDAD$MUERTES.70A79 / EDAD$CASOS.70A79 * 100
LETALIDAD$LI_LET_70A79 <-
  LETALIDAD$LET_70A79 - 1.96 * sqrt((LETALIDAD$LET_70A79 * (100 - LETALIDAD$LET_70A79)) /
                                      EDAD$CASOS.70A79)
LETALIDAD$LS_LET_70A79 <-
  LETALIDAD$LET_70A79 + 1.96 * sqrt((LETALIDAD$LET_70A79 * (100 - LETALIDAD$LET_70A79)) /
                                      EDAD$CASOS.70A79)
LETALIDAD$LET_80MAS <- EDAD$MUERTES.80MAS / EDAD$CASOS.80MAS * 100
LETALIDAD$LI_LET_80MAS <-
  LETALIDAD$LET_80MAS - 1.96 * sqrt((LETALIDAD$LET_80MAS * (100 - LETALIDAD$LET_80MAS)) /
                                      EDAD$CASOS.80MAS)
LETALIDAD$LS_LET_80MAS <-
  LETALIDAD$LET_80MAS + 1.96 * sqrt((LETALIDAD$LET_80MAS * (100 - LETALIDAD$LET_80MAS)) /
                                      EDAD$CASOS.80MAS)

# mapa CABA
load("mapa/mapaCaba.Rdata")
RME$comuna2 <- c("C 1", "C 2","C 3","C 4", "C 5", "C 6",
                 "C 7","C 8","C 9","C 10","C 11","C 12",
                 "C 13", "C 14", "C 15", "Total")

LETALIDAD$comuna2 <- c("C 1", "C 2","C 3","C 4", "C 5",
                       "C 6","C 7","C 8","C 9","C 10",
                       "C 11","C 12","C 13", "C 14", 
                       "C 15", "Total")

INCID$comuna2 <- c("C 1", "C 2","C 3","C 4", "C 5",
                   "C 6","C 7","C 8","C 9",
                   "C 10","C 11","C 12","C 13",
                   "C 14", "C 15", "Total")

POSITIVIDAD$comuna2 <- c("C 1", "C 2","C 3","C 4", "C 5",
                         "C 6","C 7","C 8","C 9",
                         "C 10","C 11","C 12","C 13",
                         "C 14", "C 15", "Total")

# función para mapas
func_tmap <-
  function(map,
           df,
           vector,
           titulo_graf = "Título mapa",
           titulo_leyenda = "Título leyenda"){
    mapa <- raster::merge(map, df)
    tm_shape(mapa) + tm_borders() +
      tm_fill(
        title = titulo_leyenda,
        palette = "Greys",
        col = vector,
        style = "quantile",
        n = 5
      ) +
      
      tm_text("comuna2",  size= 0.6,fontface = 1) +
      tm_layout(legend.outside = TRUE) +
      tm_scale_bar(position = c("right", "bottom")) +
      tm_compass(
        type = "arrow",
        position = c("right", "top"),
        show.labels = 0,
        size = 3
      ) +
      tm_layout(
        frame = FALSE,
        legend.outside = TRUE,
        legend.outside.position = "right",
        main.title = titulo_graf
      ) +
      tm_credits("",
                 size = 0.5,
                 position = c("center", "bottom"))
    
  }

# mapa de comparando olas
mapa_rme_tot <- func_tmap(mapa, RME, "RME", "RME", "RME",)
mapa_let_tot <- func_tmap(mapa, LETALIDAD, "LETALIDAD", "LETALIDAD", "LETALIDAD")
mapa_incid_tot <- func_tmap(mapa, INCID, "INCID.POR.MIL", "INCID.POR.MIL", "Incidencia c/100 mil. hab.")
mapa_positiv_tot <- func_tmap(mapa, POSITIVIDAD, "POSITIVIDAD", "Positividad", "Positividad")
# grafico
grafico <- ggplot(
  INCID.POR.EDAD %>% filter(COMUNA %in% c("COMUNA.04", "COMUNA.12")) %>% arrange(GRUPEDAD, COMUNA),
  aes(x = GRUPEDAD, y = INCID, fill = COMUNA)
) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_grey()


# function cociente de tasas
IC_coc_tasas <- function(df = POSITIVIDAD,
                         tasa = POSITIVIDAD$POSITIVIDAD,
                         numerador = POSITIVIDAD$CASOS,
                         denominador = POSITIVIDAD$TODOS,
                         gradiente = INDEX$INDEX)
{
  tasa_tot <- tasa[16]
  df <- df[1:15, ]
  df$COMUNA <- as.character(df$COMUNA)
  tasa_max = max(tasa[gradiente == min(gradiente)])
  tasa_min = min(tasa[gradiente == max(gradiente)])
  cmaxg = df$COMUNA[tasa == tasa_max]
  cming = df$COMUNA[tasa == tasa_min]
  cmin = df$COMUNA[tasa == min(tasa)]
  cmax = df$COMUNA[tasa == max(tasa)]
  num_max = numerador[tasa == tasa_max]
  num_min = numerador[tasa == tasa_min]
  den_max = denominador[tasa == tasa_max]
  den_min = denominador[tasa == tasa_min]
  COCIENTE = tasa_max / tasa_min
  LI = qbeta(0.025, num_max, num_min + 1) * den_min / (den_max * (1 - qbeta(0.025, num_max, num_min +
                                                                              1)))
  LS = qbeta(0.975, num_max + 1, num_min) * den_min / (den_max * (1 - qbeta(0.975, num_max +
                                                                              1, num_min)))
  COCIENTE = COCIENTE
  return(print(data.frame(
    MEDIDA = c(
      "TASA TOTAL",
      "TASA MAXIMA",
      "TASA MINIMA",
      "TASA MAXIMA CON GRADIENTE",
      "TASA MINIMA CON GRADIENTE",
      "COCIENTE DE TASAS EXTREMAS",
      "LI",
      "LS"
    ),
    VALOR = c(
      tasa_tot,
      max(tasa),
      min(tasa),
      tasa_max,
      tasa_min,
      COCIENTE,
      LI,
      LS
    ),
    COMUNA = c("CABA",
               cmax,
               cmin,
               cmaxg,
               cming,
               "",
               "",
               "")
  )))
  
}
IC_coc_tasas(INCID,
             INCID$INCID.POR.MIL,
             INCID$CASOS,
             INCID$POBLACION,
             INDEX$INDEX)


data.frame(RME$COMUNA, INCID$POBLACION, RME$RME, INDEX$INDEX, INDEX$ZONA) %>% filter(RME.COMUNA !=
                                                                                       "TOTAL")
library(ggplot2)
library(ggrepel)
graf_desigualdad <- data.frame(RME$COMUNA, INCID$POBLACION,
                               RME$RME, INDEX$INDEX, INDEX$ZONA)

graf_desigualdad$comuna <-c("C 1", "C 2","C 3","C 4", "C 5", "C 6","C 7","C 8","C 9",
                            "C 10","C 11","C 12","C 13", "C 14", "C 15", "Total")
graf_desigualdad_tot <- ggplot(
  graf_desigualdad %>%
    filter(RME.COMUNA !="TOTAL"),
  aes(INDEX.INDEX, RME.RME, color = INDEX$ZONA[1:15])
) +
  geom_point(aes(size = RME$RME[1:15]))  +
  geom_smooth(aes(shape = NULL), method = "lm",se=F, col="grey")+
  ggpubr::stat_cor(aes(color = NULL),method = "pearson", label.x = 1.05, label.y = 125)+
  theme_light() +
  ylab("Razón de mortalidad estandarizada") +
  xlab("Indice de ingresos estandarizado") +
  ggtitle(
    "Razones de mortalidad estandarizadas por COVID-10, según comuna y zona. \nCiudad Autónoma de Buenos Aires. Año 2020-2021.",
    subtitle = waiver()
  ) +
  labs(color = 'Zona', size="RME") +
  geom_hline(yintercept = 100, linetype = "dashed") +
  geom_vline(xintercept = 1, linetype = "dashed")+
  geom_label_repel(aes(label = comuna),
                   box.padding   = 0.8, 
                   point.padding = 0.5,
                   label.size = NA, 
                   na.rm=TRUE,
                   color = 'black',
                   alpha = 0.4,
                   segment.color = 'grey50',) 

graf_desigualdad_tot
View(
  MEDIANA.EDAD %>% select(COMUNA, GRUPEDAD, MEDIANA.ESP, MEDIANA.OBS) %>% filter(MEDIANA.ESP != "" |
                                                                                   MEDIANA.OBS != "")
)

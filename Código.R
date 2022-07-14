library(haven) #Para leer los datos del cis, formato sav
library(tidyverse)
#library(dplyr)
#library(Hmisc)
library(cowplot) #para poner dos gráficos juntos (plot_grid)
library(ggbreak) #para romper ejes en gráficos
library(gt)     #Para hacer tablas 
library(gtExtras) #Extras para las tablas (columnas de colores)
library(ggthemes) #Para Theme_tufte
library(broom)   #Para los mapas
library(rgdal)   #Para los mapas
#ctrl + shift + c

# cis06 <- as.data.frame(read_sav("CIS/2006/2666.sav"))
# cis18 <- as.data.frame(read_sav("CIS/2018/3234.sav"))

# ech18 <- read.csv("ECH/ECHPersonas_2018.csv", sep = "")
# 
# d06 <- read.csv("ECV/2006/datos_ecv2006/esudb06d.csv")
# p06 <- read.csv("ECV/2006/datos_ecv2006/esudb06p.csv")
# h06 <- read.csv("ECV/2006/datos_ecv2006/esudb06h.csv")
# r06 <- read.csv("ECV/2006/datos_ecv2006/esudb06r.csv")
# 
# #Construyendo ECV06
# per06 <- left_join(r06,p06, by= c("RB030"="PB030")) %>% 
# mutate(id_hh= as.integer(str_sub(RB030, 1, -3)))
# hog06 <- left_join(d06,h06, by= c("DB030"="HB030"))
# ecv06 <- left_join(per06, hog06, by=c("id_hh" = "DB030"))
# 
# d18 <- read.csv("ECV/2018/disreg_ecv18/esudb18d.csv")
# p18 <- read.csv("ECV/2018/disreg_ecv18/esudb18p.csv")
# h18 <- read.csv("ECV/2018/disreg_ecv18/esudb18h.csv")
# r18 <- read.csv("ECV/2018/disreg_ecv18/esudb18r.csv")
# 
# #Construyendo ECV18
# 
# per18 <- left_join(r18,p18, by= c("RB030"="PB030")) %>% 
#   mutate(id_hh= as.integer(str_sub(RB030, 1, -3)))
# hog18 <- left_join(d18,h18, by= c("DB030"="HB030"))
# ecv18 <- left_join(per18, hog18, by=c("id_hh" = "DB030"))
# 
# rm(d18, p18, h18, r18, d06, p06, h06, r06, hog06, hog18, per06, per18)
# 
# #save(ecv06, file= "ECV/2006/ecv06.RData")
# #save(ecv18, file = "ECV/2018/ecv18.RData")
# 
# #load("ECV/2006/ecv06.RData")
# #load("ECV/2018/ecv18.RData")
# 
# ees06 <- read.csv("EES/2006/CSV/EES_cuatrienal_2006.csv", sep = "")
# ees18 <- read.csv("EES/2018/CSV/EES_2018.csv", sep = "")
# 
# save(cis06, cis18, ees06, ees18, ecv06, ecv18,ech18, file = "data.RData")

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/5º/Segundo semestre/TFG eco/Ficheros y bases de datos")
load("data.Rdata")

#Vectores
ajuste <- 97.875/81.254
buenavista=c('#4EA4B3',"#DFD29B",'#2A1812','#B12913',"#80AB79",'#219089',"#937D57","#0D3D49",'#C5DFE0')
badbunny1=c("#0D6BA6","#A9D1E9","#FD9247","#6C4191","#0B92B4","#8B4E66","#9E2F2E","#2E3E99","#615911")
nicky=c("#336B66","#BE7E57","#563A22","#773824","#5D7A77","#7D5234","#27271A","#4A5A51","#A68F7A")
badbunny2=c("#181234","#B93021","#F8B45E","#E85433","#698AB5","#E4BB3C","#552328","#22446A","#EB8259")
wyy=c("#272729","#A92624","#C3BBAA","#5B2127","#5D6F73","#AC9F89","#555551","#8C7A5C","#B19994")
semaforo= c("grey", "green", "orange", "red")
#Preparando la ECV

ecv06 <- ecv06 %>% 
  mutate(pesos=RB050,
         ingresos_anuales=PY010N,
         edad= (2006-RB080),
         grupoedad =case_when(edad<23 ~ "<23",
                         edad %in% 23:34 ~ "23-34", 
                         edad %in% 35:44 ~"35-44",
                         edad %in% 45:54 ~ "45-54",
                         edad %in% 55:65 ~ "55-64", 
                         edad > 65 ~ "+65"),
         mayoresmenores = case_when(edad <=34 ~ "<35",
                                    edad >34 ~ "35+"),
         child= ifelse(edad<18, 1,0),
         empleo =ifelse(PL030 %in% 1:2, 1, 0),
         activo= ifelse(empleo==1 | PL020 == 1, 1, 0),
         tcompleto= case_when(PL030==1 ~ 1,
                              PL030==2 ~ 0),
         arope = ifelse(vrEU2020 == 1, 1,0),
         ingcapital= (HY090N+HY040N)*ajuste,
         horastrabajo = PL060,
         temporal= case_when(PL140==1 ~ "Fijo",
                             PL140==2 ~ "Temporal"),
         estudiando = ifelse(PE010==1, 1, 0),
         trabajarmas= case_when(PL120== 1~ "Estudios",
                                PL120 == 3 ~ "No encuentra",
                                PL120==4 ~ "No quiere",
                                PL120==6 ~ "Trabaja en el hogar",
                                PL120 %in% c(2, 5, 7) ~ "Otros"),
         edadtrabajo= PL190, #a qué edad empezó a trabajar regularmente
         tenencia = case_when(HH020 ==1 ~ 1, #Propiedad
                              HH020 %in% 2:3 ~ 0), #Alquiler
         alquiler = HH060*ajuste,
         gastovivienda = HH070*ajuste,
         dentista = case_when(PH070==1 ~ "No se lo podía permitir",
                              PH070==3 ~ "No disponía de tiempo", 
                              PH070 %in% c(2,4:8) ~ "Otras razones"),
         pensiones = PY035N,
         dummypensiones= ifelse(PY035N>0, 1, 0),
         retraso = case_when(HS010 == 1 ~ 1, #retraso en el pago de la hipoteca/alquiler
                             HS010 == 2 ~ 0),
         retrasofacturas = case_when(HS020 == 1 ~ 1,
                                     HS020 == 2 ~ 0),
         retrasootros = case_when(HS030 == 1 ~ 1,
                                     HS030 == 2 ~ 0),
         vacaciones = case_when(HS040==1 ~ 1,
                                HS040==2 ~ 0),
         women=ifelse(RB090==2, "Mujeres", "Hombres"),
         nivel_edu=case_when(PE040 %in% 1:2 ~"Bajo",
                             PE040 %in% 3:4 ~ "medio",
                             PE040 ==5 ~ "alto"),
         emancipado=case_when(RB220>0 | RB230>0 ~ 0,
                               is.na(RB230) & is.na(RB220) ~ 1),
         # region= case_when(DB040=="ES11" ~  "Galicia",
         #                   DB040=="ES12" ~  "Asturias",
         #                   DB040=="ES13" ~  "Cantabria",
         #                   DB040=="ES21" ~  "País Vasco",
         #                   DB040=="ES22" ~  "Navarra",
         #                   DB040=="ES23" ~  "La Rioja",
         #                   DB040=="ES24" ~  "Aragón",
         #                   DB040=="ES30" ~  "Madrid",
         #                   DB040=="ES41" ~  "Castilla y León",
         #                   DB040=="ES42" ~  "Castilla La Mancha",
         #                   DB040=="ES43" ~  "Extremadura",
         #                   DB040=="ES51" ~  "Cataluña",
         #                   DB040=="ES52" ~  "Comunidad Valenciana"
           id= case_when(DB040=="ES11" ~  11,
                         DB040=="ES12" ~  2,
                         DB040=="ES13" ~  5,
                         DB040=="ES21" ~  15,
                         DB040=="ES22" ~  14,
                         DB040=="ES23" ~  16,
                         DB040=="ES24" ~  1,
                         DB040=="ES30" ~  12,
                         DB040=="ES41" ~  6,
                         DB040=="ES42" ~  7,
                         DB040=="ES43" ~  10,
                         DB040=="ES51" ~  8,
                         DB040=="ES52" ~  9,
                         DB040=="ES53" ~  3, #Baleares
                         DB040=="ES61" ~  0, #Andalucía
                         DB040=="ES62" ~  13, #Murcia
                         DB040=="ES63" ~  17, #Ceuta
                         DB040=="ES64" ~  18, #Melilla
                         DB040=="ES70" ~  4) #Canarias
         ) %>% 
  filter(edad %in% 24:65)

ecv18 <- ecv18 %>% 
  mutate(pesos=RB050,
         ingresos_anuales=PY010N,
         edad= (2018-RB080),
         grupoedad =case_when(edad<23 ~ "<23",
                              edad %in% 23:34 ~ "23-34", 
                              edad %in% 35:44 ~"35-44",
                              edad %in% 45:54 ~ "45-54",
                              edad %in% 55:65 ~ "55-64", 
                              edad > 65 ~ "+65"),
         mayoresmenores = case_when(edad <=34 ~ "<35",
                                    edad >34 ~ "35+"),
         child= ifelse(edad<18, 1,0),
         empleo =ifelse(PL031 %in% 1:4, 1, 0),
         activo= ifelse(empleo==1 | PL020 == 1, 1, 0),
         tcompleto= case_when(PL031 %in% c(1,3)~  1,
                              PL031 %in% c(2,4) ~ 0),
         arope = ifelse(vrEU2020 == 1, 1, 0),
         ingcapital= HY040N+HY090N,
         horastrabajo = PL060,
         temporal= case_when(PL140==1 ~ "Fijo",
                              PL140==2 ~ "Temporal"),
         estudiando = case_when(PE010==1 ~ 1,
                                PE010==2 ~ 0),
         trabajarmas= case_when(PL120== 1~ "Estudios",
                                PL120 == 3 ~ "No encuentra",
                                PL120==4 ~ "No quiere",
                                PL120==6 ~ "Trabaja en el hogar",
                                PL120 %in% c(2, 5, 7) ~ "Otros"),
         edadtrabajo= PL190,
         tenencia = case_when(HH021 %in% 1:2 ~ 1, #Propiedad
                              HH021 %in% 3:4 ~ 0), #Alquiler
         alquiler = HH060,
         gastovivienda = HH070,
         dentista = case_when(PH070==1 ~ "No se lo podía permitir",
                              PH070==3 ~ "No disponía de tiempo", 
                              PH070 %in% c(2,4:8) ~ "Otras razones"),
         pensiones = PY035N,
         dummypensiones= ifelse(PY035N>0, 1, 0),
         retraso= case_when(HS011 %in% 1:2 ~ 1,
                            HS011 == 3 ~ 0),
         retrasofacturas= case_when(HS021 %in% 1:2 ~1,
                            HS021 == 3 ~ 0),
         retrasootros= case_when(HS031 %in% 1:2 ~ 1,
                                    HS031 == 3 ~ 0),
         vacaciones = case_when(HS040==1 ~ 1,
                                HS040==2 ~ 0),
         ayuda = ifelse(H39A_U==1, 1, 0),
         women=ifelse(RB090==2, "Mujeres", "Hombres"),
         nivel_edu=case_when(PE040<201~"bajo",
                             PE040 %in% 300:450 ~ "medio",
                             PE040==500~ "alto"),
         emancipado=case_when(RB220>0 | RB230>0 ~ 0,
                               is.na(RB230) & is.na(RB220) ~ 1),
         id= case_when(DB040=="ES11" ~  11,
                       DB040=="ES12" ~  2,
                       DB040=="ES13" ~  5,
                       DB040=="ES21" ~  15,
                       DB040=="ES22" ~  14,
                       DB040=="ES23" ~  16,
                       DB040=="ES24" ~  1,
                       DB040=="ES30" ~  12,
                       DB040=="ES41" ~  6,
                       DB040=="ES42" ~  7,
                       DB040=="ES43" ~  10,
                       DB040=="ES51" ~  8,
                       DB040=="ES52" ~  9,
                       DB040=="ES53" ~  3, #Baleares
                       DB040=="ES61" ~  0, #Andalucía
                       DB040=="ES62" ~  13, #Murcia
                       DB040=="ES63" ~  17, #Ceuta
                       DB040=="ES64" ~  18, #Melilla
                       DB040=="ES70" ~  4) #Canarias
         ) %>% 
  filter(edad %in% 24:65)

#Preparando la EES
ees06 <- ees06 %>% 
  mutate(edad =case_when(ANOS2 ==1 ~ "<19",
                          ANOS2 ==2 ~ "19-29", 
                          ANOS2 ==3 ~"30-39",
                          ANOS2 ==4 ~ "40-49",
                          ANOS2 ==5 ~ "50-59", 
                          ANOS2 ==6 ~ "+59"),
         ingresos_anuales =SALBRUTO*ajuste,
         women= ifelse(SEXO==6, "Mujeres", "Hombres"), 
         nivel_edu=case_when(ESTU<5 ~"Estudios Secundarios",
                             ESTU %in% 5:6 ~"Formación Profesional",
                             ESTU>6 ~"Estudios Universitarios"))

ees18 <- ees18 %>% 
  mutate(edad =case_when(ANOS2 ==1 ~ "<19",
                         ANOS2 ==2 ~ "19-29", 
                         ANOS2 ==3 ~"30-39",
                         ANOS2 ==4 ~ "40-49",
                         ANOS2 ==5 ~ "50-59", 
                         ANOS2 ==6 ~ "+59"),
         ingresos_anuales =RETRINOIN,
         women= ifelse(SEXO==6, "Mujeres", "Hombres"), 
         nivel_edu=case_when(ESTU<5 ~"Estudios Secundarios",
                             ESTU == 5 ~"Formación Profesional",
                             ESTU>5 ~"Estudios Universitarios"))
#Preparando CIS


cis06<- cis06 %>% 
  mutate(edad= P26, 
         women=ifelse(P24==2, "Mujer", "Hombre"),
         grupoedad =case_when(edad<23 ~ "<23",
                              edad %in% 23:34 ~ "23-34", 
                              edad %in% 35:44 ~"35-44",
                              edad %in% 45:54 ~ "45-54",
                              edad %in% 55:65 ~ "55-64", 
                              edad > 65 ~ "+65"),
         mayoresmenores = case_when(edad <=34 ~ "<35",
                                    edad >34 ~ "35+"),
         emancipado= ifelse(RELA01 %in% c(5:8,16), "No", "Sí"), 
         españafuturo= case_when(P2 == 1 ~"Mejor",
                                 P2 == 2 ~"Igual",
                                 P2 == 3 ~"Peor",
                                 P2 %in% 8:9 ~"NS/NC"),
         personalactual= case_when(P16 %in% 1:2 ~"Muy o bastante satisfecho",
                                   P16 %in% 3:4 ~ "Poco o nada satisfecho",
                                   P16 == 9 ~ "NS/NC"),
         personalfuturo= case_when(P17 == 1 ~ "Mejor",
                                   P17 == 2 ~ "Igual", 
                                   P17 == 3 ~ "Peor", 
                                   P17 %in% 8:9 ~ "NS/NC"),
         #satisfaccion= case_when(P13 %in% 1:2 ~ "Bastante o muy satisfecho",
                                  # P13 == 3 ~ "Ni satisfecho ni insatisfecho", 
                                   #P13 %in% 4:5 ~ "Bastante o muy insatisfecho", 
                                   #P13 == 9 ~ "NS/NC"),
         ppr= case_when(P501==1 ~ "Paro",
                        P501==3 ~ "Inseguridad",
                        P501==4 ~ "Terrorismo",
                        P501==7 ~ "Vivienda",
                        P501==8 ~ "Económicos",
                        P501==11 ~ "Corrupción",
                        P501==13 ~ "Políticos",
                        P501==18 ~ "Inmigración",
                        P501==98 | P501==99 ~ "NS/NC"),
         
         princprob = case_when(P501==1 ~ "Paro",
                               P501==2 ~ "Drogas",
                               P501==3 ~ "Inseguridad",
                               P501==4 ~ "Terrorismo",
                               P501== 5 ~ "Infraestructuras",
                               P501==6 ~ "Sanidad",
                               P501==7 ~ "Vivienda",
                               P501==8 ~ "Problemas 
económicos",
                               P501 == 9 ~ "Calidad Empleo",
                               P501 == 10 ~ "Agrarios",
                               P501 ==11 ~ "Corrupción",
                               P501 == 12 ~ "Pensiones", 
                               P501 ==13 ~ "Los políticos",
                               P501 == 14 ~ "Guerras",
                               P501 == 15 ~ "Admón Justicia",
                               P501 == 16 ~ "Índole Social", 
                               P501 ==17 ~ "Racismo",
                               P501 ==18 ~ "La inmigración",
                               P501 == 19 ~ "Violencia machista", 
                               P501 == 20 ~ "Relacionados juventud",
                               P501 == 21 ~ "Crisis valores",
                               P501 == 22 ~ "Educación",
                               P501 == 23 ~ "Medioambientales",
                               P501 == 24 ~ "Gobierno, políticos y partidos", 
                               P501 == 25 ~ "Sevicios Públicos", 
                               P501 == 26 ~ "Nacionalismos", 
                               P501 == 27 ~ "Problemas mujer", 
                               P501 == 28 ~ "Terrorismo Inter.",
                               P501 == 29 ~ "Preocupaciones personales",
                               P501 == 30 ~ "Estatuto de Cataluña", 
                               P501 == 31 ~ "Negociaciones con ETA",
                               P501 == 96 ~ "Otras",
                               P501 ==98 | P501==99 ~ "NS/NC"),
         pparo= ifelse(P501 == 1 | P502 == 1 | P503 == 1, 1, 0),
         pinseguridad = ifelse(P501 == 3 | P502 == 3 | P503 == 3, 1, 0),
         pterrorismo = ifelse(P501 == 4 | P502 == 4 | P503 == 4, 1, 0),
         pvivienda = ifelse(P501 == 7 | P502 == 7 | P503 == 7, 1, 0),
         peconomico = ifelse(P501 == 8 | P502 == 8 | P503 == 8, 1, 0),
         pcorrupción = ifelse(P501 ==11 | P502 == 11 | P503 == 11, 1, 0),
         ppoliticos = ifelse(P501 == 13 | P502 == 13 | P503 == 13, 1, 0),
         pinmigracion = ifelse(P501 == 18 | P502 == 18 | P503 == 18, 1, 0))

cis18<- cis18 %>% 
  mutate(edad= P44, 
         women=ifelse(P43==2, "Mujer", "Hombre"), 
         grupoedad =case_when(edad<23 ~ "<23",
                              edad %in% 23:34 ~ "23-34", 
                              edad %in% 35:44 ~"35-44",
                              edad %in% 45:54 ~ "45-54",
                              edad %in% 55:65 ~ "55-64", 
                              edad > 65 ~ "+65"),
         mayoresmenores = case_when(edad <=34 ~ "<35",
                                    edad >34 ~ "35+"),
         #emancipado= ifelse(RELA01 %in% c(5:8,16), "No", "Sí"),
         españaactual= case_when(P1 %in% 1:2 ~"Buena o muy buena",
                                 P1 %in% 3:4 ~"Regular, mala o muy mala",
                                 P1 %in% 8:9 ~"NS/NC"), 
         españafuturo= case_when(P3 == 1 ~"Mejor",
                                 P3 == 2 ~"Igual",
                                 P3 == 3 ~"Peor",
                                 P3 %in% 8:9 ~"NS/NC"),
         personalfuturo= case_when(P5 == 1 ~ "Mejor",
                                   P5 == 2 ~ "Igual", 
                                   P5 == 3 ~ "Peor", 
                                   P5 %in% 8:9 ~ "NS/NC"),
         #satisfaccion= case_when(P13 %in% 1:2 ~ "Bastante o muy satisfecho",
                               #  P13 == 3 ~ "Ni satisfecho ni insatisfecho", 
                                # P13 %in% 4:5 ~ "Bastante o muy insatisfecho", 
                                 #P13 == 9 ~ "NA"),
         ppr= case_when(P901==1 ~ "Paro",
                        P901==3 ~ "Inseguridad",
                        P901==4 ~ "Terrorismo",
                        P901==7 ~ "Vivienda",
                        P901==8 ~ "Económicos",
                        P901==11 ~ "Corrupción",
                        P901==13 ~ "Políticos",
                        P901==18 ~ "Inmigración",
                        P901==45 ~ "Independencia Cataluña",
                        P901==96 ~ "Sociales",
                        P901==98 | P901==99 ~ "NS/NC"),
         princprob = case_when(P901==1 ~ "Paro",
                               P901==2 ~ "Drogas",
                               P901==3 ~ "Inseguridad",
                               P901==4 ~ "Terrorismo",
                               P901== 5 ~ "Infraestructuras",
                               P901==6 ~ "Sanidad",
                               P901==7 ~ "Vivienda",
                               P901==8 ~ "Problemas económicos",
                               P901 == 9 ~ "Calidad Empleo",
                               P901 == 10 ~ "Agrarios",
                               P901 ==11 ~ "Corrupción",
                               P901 == 12 ~ "Pensiones", 
                               P901 ==13 ~ "Los políticos",
                               P901 == 14 ~ "Guerras",
                               P901 == 15 ~ "Admón Justicia",
                               P901 == 16 ~ "Índole Social", 
                               P901 ==17 ~ "Racismo",
                               P901 ==18 ~ "La inmigración",
                               P901 == 19 ~ "Violencia machista", 
                               P901 == 20 ~ "Relacionados juventud",
                               P901 == 21 ~ "Crisis valores",
                               P901 == 22 ~ "Educación",
                               P901 == 23 ~ "Medioambientales",
                               P901 == 24 ~ "Gobierno, políticos y partidos", 
                               P901 == 25 ~ "Sevicios Públicos", 
                               P901 == 26 ~ "Nacionalismos", 
                               P901 == 27 ~ "Problemas mujer", 
                               P901 == 28 ~ "Terrorismo Inter.",
                               P901 == 29 ~ "Preocupaciones personales",
                               P901 == 30 ~ "Estatuto de Cataluña", 
                               P901 == 31 ~ "Negociaciones con ETA",
                               P901 == 32 ~ "Reforma laboral", 
                               P901 == 33 ~ "Recortes",
                               P901 == 34 ~ "Bancos", 
                               P901 == 35 ~ "Subida IVA", 
                               P901 == 36 ~ "Desahucios", 
                               P901 == 37 ~ "Fraude fiscal", 
                               P901== 38 ~ "Hipotecas", 
                               P901 == 39 ~ "Monarquía", 
                               P901 == 40 ~ "Excarcelaciones", 
                               P901 == 41 ~ "Ley Aborto", 
                               P901 == 42 ~ "Subida energía", 
                               P901 == 43 ~ "Ébola", 
                               P901 == 44 ~ "Refugiados", 
                               P901 == 45 ~ "Independencia Cataluña",
                               P901 == 46 ~ "Situación política",
                               P901 == 47 ~ "Emigración", 
                               P901 == 48 ~ "Relacionado autónomos", 
                               P901 == 49 ~ "Falta inv. ind. e I+D",
                               P901 == 96 ~ "Otras",
                               P901 ==98 | P901==99 ~ "NS/NC"),
         pparo= ifelse(P901 == 1 | P902 == 1 | P903 == 1, 1, 0),
         pinseguridad = ifelse(P901 == 3 | P902 == 3 | P903 == 3, 1, 0),
         pterrorismo = ifelse(P901 == 4 | P902 == 4 | P903 == 4, 1, 0),
         pvivienda = ifelse(P901 == 7 | P902 == 7 | P903 == 7, 1, 0),
         peconomico = ifelse(P901 == 8 | P902 == 8 | P903 == 8, 1, 0),
         pcorrupción = ifelse(P901 ==11 | P902 == 11 | P903 == 11, 1, 0),
         ppoliticos = ifelse(P901 == 13 | P902 == 13 | P903 == 13, 1, 0),
         pinmigracion = ifelse(P901 == 18 | P902 == 18 | P903 == 18, 1, 0))

#Preparando ECH
ech18 <- ech18 %>% 
  mutate(emancipado= ifelse(P01 %in% c(1,5) |P02 %in% c(1,5) | P03 %in% c(1,5) |P04 %in% c(1,5)
                            |P05 %in% c(1,5) |P06 %in% c(1,5) |P07 %in% c(1,5) | P08 %in% c(1,5)
                            |P09 %in% c(1,5) |P10 %in% c(1,5), "No", "Sí"),
         women = ifelse(SEXO == 6, "Sí", "No"), 
         edad = EDAD, 
         andalucia= ifelse(CA==01, "Sí", "No"),
         jovenemancipado = case_when(emancipado== "Sí" & edad %in% 20:29 ~ "Sí",
                                     emancipado== "No" & edad %in% 20:29 ~ "No")
         )
rm(ech18)

#Theme_set

theme_set(theme_minimal()+
            theme(plot.background = element_rect(colour = "white"),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  axis.title = element_blank(),
                  panel.grid.major = element_blank(),
                  text = element_text(family = "serif", colour = "black"),
                  axis.text = element_text(colour = "black"),
                  axis.line.x = element_line(colour = "lightgrey"),
                  axis.ticks.x = element_line(colour = "lightgrey", lineend = "round")))


#GRÁFICOS PERCEPCIONES


  ##Españafuturo

    ###2006

cc4 <- scales::seq_gradient_pal("green", "red", "Lab")(seq(0,1,length.out=4))

cis06$españafuturo <- factor(cis06$españafuturo, levels= c("NS/NC", "Mejor", "Igual", "Peor"))

cis06 %>% 
  select(edad, españafuturo, women) %>% 
  filter(edad %in% 24:35) %>% 
  group_by(edad, españafuturo) %>% 
  mutate(uno=1) %>% 
  ggplot(aes(x=edad, y= women, color=españafuturo))+
  geom_jitter(position = position_jitter(width = .2), size=1.75)+
  scale_color_manual(values = semaforo)+
  theme_minimal()+
  theme(legend.position = "bottom", 
        axis.title = element_blank(),
       # axis.text.y = element_blank(), 
        legend.title = element_blank(),
       panel.grid = element_blank(),
        #axis.ticks.y = element_blank(),
        text = element_text(family = "serif")
        )+
  scale_x_continuous(breaks = seq(23,36, by=1))

cis18$españafuturo <- factor(cis18$españafuturo, levels= c("NS/NC", "Mejor", "Igual", "Peor"))

left_join(cis06 %>% 
  filter(grupoedad == "23-34", personalfuturo!="NS/NC") %>% 
  group_by(grupoedad, personalfuturo) %>% 
  count() %>% 
  group_by(grupoedad) %>% 
  mutate(h=n/sum(n)),
cis18 %>% 
  filter(grupoedad == "23-34", personalfuturo!="NS/NC") %>% 
  group_by(grupoedad, personalfuturo) %>% 
  count() %>% 
  group_by(grupoedad) %>% 
  mutate(hj=n/sum(n)), by= c("grupoedad", "personalfuturo")) %>% mutate((hj-h)*100)

ggef<- 
cis06 %>% 
  select(grupoedad, españafuturo) %>% 
  filter(grupoedad == "23-34", españafuturo!="NS/NC") %>% 
  group_by(grupoedad, españafuturo) %>% 
  arrange(grupoedad) %>% 
  ggplot(aes(x= grupoedad, y= 1, fill=españafuturo))+
  geom_bar(position = "fill", stat= "identity", alpha=0.85)+
  scale_fill_manual(values = c("#9bc9be", "#497c7d", "#11545a"))+
  theme(legend.position = "")+
  scale_x_discrete(labels="2006")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  cis18 %>% 
  select(grupoedad, españafuturo) %>% 
  filter(grupoedad=="23-34", españafuturo!= "NS/NC") %>% 
  group_by(grupoedad, españafuturo) %>% 
  arrange(grupoedad) %>% 
  ggplot(aes(x= grupoedad, y= 1, fill=españafuturo))+
  geom_bar(position = "fill", stat= "identity", alpha=0.85)+
  scale_fill_manual(values = c("#9bc9be", "#497c7d", "#11545a"))+
  theme(axis.text.y = element_blank())+
  theme(legend.position = "", 
        axis.text = element_text())+
  scale_x_discrete(labels="2018")

#ggsave(plot=ggef, filename = "../Imágenes finales/españafuturo.png", type="cairo")

  ##Personal futuro

    ###2006 


    ###2018 
cis06$personalfuturo <- factor(cis06$personalfuturo, levels= c("NS/NC", "Mejor", "Igual", "Peor"))
cis18$personalfuturo <- factor(cis18$personalfuturo, levels= c("NS/NC", "Mejor", "Igual", "Peor"))

ggpf<-
  cis06 %>% 
  select(grupoedad, personalfuturo) %>% 
  filter(grupoedad == "23-34", personalfuturo!= "NS/NC") %>% 
  group_by(grupoedad, personalfuturo) %>% 
  arrange(grupoedad) %>% 
  ggplot(aes(x= grupoedad, y= 1, fill=personalfuturo))+
  geom_bar(position = "fill", stat= "identity", alpha=0.85)+
  scale_fill_manual(values = c("#9bc9be", "#497c7d", "#11545a"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(legend.position = "",
        axis.text.y = element_blank())+
  scale_x_discrete(labels="2006")+
  
cis18 %>% 
  select(grupoedad, personalfuturo) %>% 
  filter(grupoedad == "23-34", personalfuturo!= "NS/NC") %>% 
  group_by(grupoedad, personalfuturo) %>% 
  arrange(grupoedad) %>% 
  ggplot(aes(x= grupoedad, y= 1, fill=personalfuturo))+
  geom_bar(position = "fill", stat= "identity", alpha=0.85)+
  scale_fill_manual(values = c("#9bc9be", "#497c7d", "#11545a"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(axis.text.y = element_blank(),
        legend.position = "right")+
  scale_x_discrete(labels="2018")+
    theme(axis.text.y = element_blank())



#camavi
#ggsave(plot=ggpf, filename = "../Imágenes finales/personalfuturo.png")

ggsave(plot = plot_grid(ggef, ggpf, labels = "auto", rel_widths = c(1,1.2)), 
       filename = "../Imágenes finales/futuro.png", type= "cairo")

  ## Problemas principales

    ###Primer problema 

      ####2006

        #####Gráfico puntos de colores
cc <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=40))

dfpuncol<- cis06 %>% 
  group_by(grupoedad, princprob) %>% 
  count(princprob)
dfpuncol<- dfpuncol %>% 
  group_by(grupoedad) %>% 
  mutate(sum=sum(n),
         p=n/sum)

dfpuncol$grupoedad <- factor(dfpuncol$grupoedad, levels = c("<23","23-34","35-44", "45-54", "55-64", "+65" ))


ggpuncol1<- 
  dfpuncol %>% 
 # filter(edad<65) %>% 
  drop_na(princprob) %>% 
  ggplot(aes(x=grupoedad, 
             y=princprob, 
             size= p, 
             alpha=p, 
             color=princprob))+
  geom_point()+
  theme(legend.position = "")+
  scale_color_manual(values = cc)+
  guides(colour = "none")+
  theme(text = element_text(family = "serif"),
        plot.title = element_blank(),
        plot.subtitle = element_text(hjust = 0.02, size = 16,),
        axis.title.x = element_blank()
  )+
  ggtitle("",subtitle = "2006")

ggsave(plot=ggpuncol1, filename = "../Imágenes finales/ggpuncol1.png", height = 7, type="cairo")

ggpuncol2 <- 
cis06 %>% 
  drop_na(princprob) %>% 
  ggplot(aes(x=edad, y=princprob, color=princprob))+
  geom_point(stat = "sum")+
  theme(legend.position = "")+
  scale_color_manual(values = cc)+
  guides(colour = "none")+
  ggtitle("",subtitle = "2006")

ggsave(plot=ggpuncol2, filename = "../Imágenes finales/ggpuncol2.png", 
       height = 7, width = 5.5, type="cairo")




        #####conteo de los temas y fill por grupos de edad

cis06$grupoedad <- factor(cis06$grupoedad, levels = c("+65", "55-64", "45-54", "35-44", "23-34", "<23"))
cis06$mayoresmenores <- factor(cis06$mayoresmenores, levels = c("35+", "<35"))
cis06$ppr <- factor(cis06$ppr,
                    levels = c( "Vivienda", "Paro","Inmigración","Otros",
                                "Terrorismo", "Económicos", "Inseguridad", "Políticos", "Corrupción", "NS/NC"))

plotprpr06 <- 
  cis06 %>% 
  replace_na(list(ppr= "Otros")) %>%
  arrange(ppr) %>% 
  ggplot(aes(y= ppr, fill= mayoresmenores))+
  geom_bar(aes(x=(..count..)/sum(..count..)*100), alpha=0.85)+
  scale_fill_manual(values = buenavista)+
  theme(legend.position = "")+
  scale_fill_manual(values = c("#9bc9be", "#66979b"))
  
    

cis06$grupoedad <- factor(cis06$grupoedad, levels = c("+65", "55-64", "45-54", "35-44", "23-34", "<23"))

      ####2018 

        #####Gráfico puntos de colores

dfpuncol2<- cis18 %>% 
  group_by(grupoedad, princprob) %>% 
  count(princprob)

dfpuncol2<- dfpuncol2 %>% 
  group_by(grupoedad) %>% 
  mutate(sum=sum(n),
         p=n/sum)

dfpuncol2$grupoedad <- factor(dfpuncol2$grupoedad, levels = c("<23","23-34","35-44", "45-54", "55-64", "+65" ))


ggpuncol181<-
  dfpuncol2 %>% 
  # filter(edad<65) %>% 
  drop_na(princprob) %>% 
  ggplot(aes(x=grupoedad, 
             y=princprob, 
             size= p, 
             alpha=p, 
             color=princprob))+
  geom_point()+
  theme(axis.title.y = element_blank(),
        legend.title = element_blank())+
  scale_color_manual(values = cc)+
  guides(colour = "none")+
  theme(legend.position = "",
  )+
  ggtitle("",subtitle = "2018")

ggsave(plot=ggpuncol181, filename = "../Imágenes finales/ggpuncol181.png", 
       height = 7, type="cairo")

ggpuncol182<- 
  cis18 %>%  
  drop_na(princprob) %>% 
  ggplot(aes(x=edad, y=princprob, color=princprob))+
  geom_point(stat = "sum")+
  theme(legend.position = "")+
  scale_color_manual(values = cc)+
  ggtitle("",subtitle = "2018")

ggsave(plot=ggpuncol182, filename = "../Imágenes finales/ggpuncol182.png", 
       height = 7, width = 5.5, type="cairo")
  
      #####conteo de los temas y fill por grupos de edad

#cis18$grupoedad <- factor(cis18$grupoedad, levels = c("+65", "55-64", "45-54", "35-44", "23-34", "<23"))
cis18$mayoresmenores <- factor(cis18$mayoresmenores, levels = c("35+", "<35"))
cis18$ppr <- factor(cis18$ppr, levels = c("Paro", "Otros", "Políticos", "Corrupción", "Económicos", "Sociales", "Inmigración", "Independencia Cataluña", "Vivienda", "Inseguridad", "NS/NC"))

plotprpr18<- 
  cis18 %>% 
  replace_na(list(ppr= "Otros")) %>%
  arrange(ppr) %>% 
  ggplot(aes(y= ppr, fill= mayoresmenores))+
  geom_bar(aes(x=(..count..)/sum(..count..)*100))+
  scale_x_break(c(20,30))+
  scale_fill_manual(values = c("#9bc9be", "#66979b"))+
  theme(legend.position = "right")+
  guides(fill= guide_legend(reverse = T))

ggsave(plot_grid(plotprpr06, plotprpr18, labels = c("2006", "2018"),
          rel_widths = c(1,1.7)), filename = "../Imágenes finales/ggprincprob.png", type="cairo")

  ##Otros gráficos

    ###2006
dfprinc06<-
  cis06 %>% 
  select(ppr, mayoresmenores) %>% 
  group_by(mayoresmenores, ppr) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) %>% 
  group_by(ppr) %>% 
  mutate(sumper= sum(count))

ggprinc06<-dfprinc06 %>% #Porcentaje de cada grupo de edad que ha votado cada cosa
  replace_na(list(ppr= "Otros")) %>% 
  ggplot(aes(y=reorder(ppr, perc), x=perc, fill=mayoresmenores))+ 
  geom_col(alpha=.85)+
  scale_fill_manual(values = c("#9bc9be", "#66979b"))+
  theme(legend.position = "")

    ###2018
dfprinc18<-
  cis18 %>% 
  select(ppr, mayoresmenores) %>% 
  group_by(mayoresmenores, ppr) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) %>% 
  group_by(ppr) %>% 
  mutate(sumper= sum(count))

ggprinc18<- dfprinc18 %>% #Porcentaje de cada grupo de edad que ha votado cada cosa
  replace_na(list(ppr= "Otros")) %>% 
ggplot(aes(y=reorder(ppr, perc), x=perc, fill=mayoresmenores))+ 
  geom_col()+
  scale_fill_manual(values = c("#9bc9be", "#66979b"))+
  theme(legend.position = c(0.8,0.1))+
  guides(fill=guide_legend(reverse = T))

ggsave(plot = plot_grid(ggprinc06, ggprinc18, labels = c("2006", "2018")),
       filename = "../Imágenes finales/ggprob2.png", type="cairo")


cis18$grupoedad <- factor(cis18$grupoedad, levels = c("+65", "55-64", "45-54", "35-44", "23-34", "<23"))

# cis18 %>%
#   replace_na(list(ppr= "Otros")) %>%
#   arrange(ppr) %>% 
#   #filter(edad <45) %>% 
#   ggplot(aes(x= ppr, fill= mayoresmenores))+
#   geom_bar(aes(y=(..count..)/sum(..count..)))+
#   theme_bw()+
#   scale_fill_manual(values = buenavista)+
#   theme(legend.title = element_blank(), 
#         axis.title = element_blank(),
#         text = element_text(family = "serif"))+
#   ggtitle("Principal problema de España según los españoles en 2018")
# table(cis18$P901)

dfprob06<- rbind(
  cis06 %>% 
  group_by(mayoresmenores) %>% 
  summarise(prob= "Paro",
            p=sum(pparo, na.rm = T)/length(pparo)),
  cis06 %>% 
    group_by(mayoresmenores) %>% 
    summarise(prob= "Inseguridad",
              p=sum(pinseguridad, na.rm = T)/length(pinseguridad)),
  cis06 %>% 
    group_by(mayoresmenores) %>% 
    summarise(prob= "Terrorismo",
              p=sum(pterrorismo, na.rm = T)/length(pterrorismo)),
  cis06 %>% 
    group_by(mayoresmenores) %>% 
    summarise(prob= "Vivienda",
              p=sum(pvivienda, na.rm = T)/length(pvivienda)),
  cis06 %>% 
    group_by(mayoresmenores) %>% 
    summarise(prob= "Problemas
económicos",
              p=sum(peconomico, na.rm = T)/length(peconomico)),
  cis06 %>% 
    group_by(mayoresmenores) %>% 
    summarise(prob= "Corrupción",
              p=sum(pcorrupción, na.rm = T)/length(pcorrupción)),
  cis06 %>% 
    group_by(mayoresmenores) %>% 
    summarise(prob= "Políticos",
              p=sum(ppoliticos, na.rm = T)/length(ppoliticos)),
  cis06 %>% 
    group_by(mayoresmenores) %>% 
    summarise(prob= "Inmigración",
              p=sum(pinmigracion, na.rm = T)/length(pinmigracion)))


dfprob18<- rbind(
  cis18 %>% 
    group_by(mayoresmenores) %>% 
    summarise(prob= "Paro",
              p18=sum(pparo, na.rm = T)/length(pparo)),
  cis18 %>% 
    group_by(mayoresmenores) %>% 
    summarise(prob= "Inseguridad",
              p18=sum(pinseguridad, na.rm = T)/length(pinseguridad)),
  cis18 %>% 
    group_by(mayoresmenores) %>% 
    summarise(prob= "Terrorismo",
              p18=sum(pterrorismo, na.rm = T)/length(pterrorismo)),
  cis18 %>% 
    group_by(mayoresmenores) %>% 
    summarise(prob= "Vivienda",
              p18=sum(pvivienda, na.rm = T)/length(pvivienda)),
  cis18 %>% 
    group_by(mayoresmenores) %>% 
    summarise(prob= "Problemas
económicos",
              p18=sum(peconomico, na.rm = T)/length(peconomico)),
  cis18 %>% 
    group_by(mayoresmenores) %>% 
    summarise(prob= "Corrupción",
              p18=sum(pcorrupción, na.rm = T)/length(pcorrupción)),
  cis18 %>% 
    group_by(mayoresmenores) %>% 
    summarise(prob= "Políticos",
              p18=sum(ppoliticos, na.rm = T)/length(ppoliticos)),
  cis18 %>% 
    group_by(mayoresmenores) %>% 
    summarise(prob= "Inmigración",
              p18=sum(pinmigracion, na.rm = T)/length(pinmigracion))) 

dfprob<- left_join(dfprob06, dfprob18)%>% 
  mutate(var= p18-p) %>% arrange(mayoresmenores)

dfprob %>%  pivot

ggplot()

dfprob$mayoresmenores<- factor(dfprob$mayoresmenores, levels = c("<35", "35+"))
#ggvarprob<-
  dfprob %>% 
  ggplot(aes(x=prob, y=var, fill= mayoresmenores))+
  geom_bar(stat = "identity", position = "dodge", alpha=0.85)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(legend.position = c(0.9,0.9))+
    scale_fill_manual(values = c("#66979b","#9bc9be"))

ggsave(plot = ggvarprob, filename = "../Imágenes finales/varprincprob.png", width = 6, type= "cairo")



# GRÁFICOS DE INGRESOS, RIQUEZA/POBREZA

  
  ##Tabla rentas

    ###rentas

rentas<- left_join(ees06 %>% 
                     filter(edad=="19-29") %>% 
                     select(edad, ingresos_anuales, women, FACTOTAL) %>% 
                     group_by(edad) %>% 
                     summarise("2006"= weighted.mean(ingresos_anuales, FACTOTAL)),
                   ees18 %>% 
                     filter(edad=="19-29") %>% 
                     select(edad, ingresos_anuales, women, FACTOTAL) %>% 
                     group_by(edad) %>% 
                     summarise("2018"= weighted.mean(ingresos_anuales, FACTOTAL)), by="edad") %>% 
  mutate(cat="Renta salarial")

    ###Proprenta
ees06 %>% 
  select(edad, ingresos_anuales, FACTOTAL) %>% 
  filter(edad== "19-29") %>% 
  summarise("2006"=weighted.mean(ingresos_anuales, FACTOTAL))/
  ees06 %>% 
  select(edad, ingresos_anuales, FACTOTAL) %>% 
  filter(edad == "30-39" | edad == "40-49" |edad == "edad 50-59") %>% 
  summarise(weighted.mean(ingresos_anuales, FACTOTAL))

ees18 %>% 
  select(edad, ingresos_anuales, FACTOTAL) %>% 
  filter(edad== "19-29") %>% 
  summarise(weighted.mean(ingresos_anuales, FACTOTAL))/
  ees18 %>% 
  select(edad, ingresos_anuales, FACTOTAL) %>% 
  filter(edad == "30-39" | edad == "40-49" |edad == "edad 50-59") %>% 
  summarise(weighted.mean(ingresos_anuales, FACTOTAL))

edad <- c("19-29" ) 
`2006` <- c(0.6478381) 
`2018` <- c(0.5882055)  
cat <- c("Proporción de la renta media juvenil sobre la renta media nacional")  
proprenta <- data.frame(edad, `2006`,`2018`, cat)  
proprenta$`2006` <- proprenta$X2006  
proprenta$`2018`<- proprenta$X2018  
proprenta <- proprenta %>% 
 select(-c(X2006, X2018))


    ###Rentas del capital

rentacap <- left_join(
  ecv06 %>%                       
  filter(edad %in% 19:29, emancipado==1) %>% 
  summarise(edad= "19-29", 
            "2006"= weighted.mean(ingcapital, pesos)),
ecv18 %>% 
  filter(edad %in% 19:29, emancipado==1) %>% 
  summarise(edad= "19-29", 
            "2018"= weighted.mean(ingcapital, pesos)),
by="edad") %>% 
  mutate(cat="Rentas del capital")
  

    ###Género
rentagen<- left_join(ees06 %>% 
  filter(edad== "19-29") %>% 
  group_by(women) %>% 
  summarise(edad="19-29", 
            "2006"= weighted.mean(ingresos_anuales, FACTOTAL)),
ees18 %>% 
  filter(edad== "19-29") %>% 
  group_by(women) %>% 
  summarise("2018"= weighted.mean(ingresos_anuales, FACTOTAL)),
by="women") %>% 
  mutate(cat=women) %>% 
  select(-women)



    ###Por nivel de estudios
ees06$nivel_edu <- factor(ees06$nivel_edu, levels = c("Estudios Secundarios","Formación Profesional", "Estudios Universitarios"))
rentanest<- left_join(
ees06 %>% 
  filter(edad== "19-29") %>% 
  group_by(nivel_edu) %>% 
  summarise(edad="19-29",
            "2006"= weighted.mean(ingresos_anuales, FACTOTAL)),

ees18 %>% 
  filter(edad== "19-29") %>% 
  group_by(nivel_edu) %>% 
  summarise("2018"= weighted.mean(ingresos_anuales, FACTOTAL)),
by="nivel_edu"
) %>% 
  mutate(cat=nivel_edu) %>% 
  select(-nivel_edu)
  
    ###Tabla final
dftable<- rbind(rentas, rentacap, rentagen, rentanest) %>% 
  select(-edad) %>% 
  select(cat, `2006`, `2018`) %>% 
  mutate(Proporción = `2018`/`2006`*100,
         "-" = `2018`/`2006`) 

dftable %>% 
  gt(rowname_col = "cat") %>% 
  gt_theme_nytimes() %>% 
  tab_header(
    title = "Renta media anual percibida por la juventud",
    subtitle = "2006 vs 2018"
  ) %>% 
  fmt_number(
    columns = c(`2006`, `2018`),
    decimals = 0,
    sep_mark = " "
  ) %>% 
  fmt_percent(
    columns =`-`,
    decimals = 2,
    dec_mark = "."
  ) %>% 
  fmt_markdown(
    columns = cat,
  ) %>% 
  gt_plt_bar_pct(
    column = Proporción,
    height = 16,
    scaled = TRUE, 
    fill = "#0e387a",
    background = "#9fafca"
  ) %>% 
  gt_color_rows(`-`,
                palette = c("#FF6863", "#fbbf77", "#90EE90"), 
                domain = c(0.82,1.07),
                use_paletteer = F) %>% 
  tab_style(style = cell_fill(color = "#90EE90"),
    locations = cells_body(
      columns = `-`,
      rows = `-`>1.2
    )
  ) %>% 
  tab_footnote(footnote = "Sobre el total de emancipados", 
               locations = cells_stub(rows = c(2))
  ) %>% 
  tab_options(table.font.names = "serif", 
              table.font.color = "black",
              footnotes.marks = c("*"))

  ##Gráfico AROPE

dfarope <- left_join(ecv06 %>%
                       filter(emancipado==1) %>% 
                       group_by(grupoedad) %>%  
                       drop_na(arope) %>% 
                       summarise(p=sum(arope)/length(arope)),
                     ecv18 %>% 
                       filter(emancipado==1) %>% 
                       group_by(grupoedad) %>% 
                       drop_na(arope) %>% 
                       summarise(p=sum(arope)/length(arope)), 
                     by= "grupoedad"
) %>% 
  mutate(var = p.y-p.x)

 ggarope <- 
   dfarope %>% 
  ggplot(aes(x=grupoedad, y=var, fill=grupoedad)) + 
  geom_col(alpha=0.85)+
  theme(legend.position = "")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    scale_fill_manual(values = c("#497c7d", "#9bc9be", "#b67182",  "#d0a5ae"))
 
ggsave(plot = ggarope, filename = "../Imágenes finales/ggarope.png", type= "cairo")

ggarope2 <- 
  dfarope %>% 
  ggplot() + 
  geom_linerange(aes(x=grupoedad, ymin=p.x, ymax=p.y),
                  size=1.2, color= "#9bc9be", alpha=0.9)+
  geom_point(aes(x=grupoedad, y=p.y, color="2018"), size=2.5, alpha=0.9)+
  geom_point(aes(x=grupoedad, y=p.x, color="2006"), size=3.5, alpha=0.9)+
  scale_color_manual(values = c("#497c7d", "mediumseagreen"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  

ggsave(plot=ggarope2, filename = "../Imágenes finales/ggarope2.png", type="cairo")

# ecv18 %>% 
#   group_by(edad) %>% 
#   drop_na(arope) %>% 
#   summarise(p=sum(arope)/length(arope)) %>% 
#   ggplot(aes(x=edad, y=p, fill=edad))+
#   geom_col()+
#   theme_classic()+
#   scale_fill_gradient(low = "#384e78", high ="#5874dc" )+
#   theme(legend.position = "", 
#         axis.title = element_blank())


  ##Gráfico renta por grupo de edad
cc6 <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=6))


 ees06$edad <- factor(ees06$edad, levels = c("<19", "19-29", "30-39", "40-49", "50-59", "+59"))
incperage <-  ggplot(ees06, aes(x=edad, y=ingresos_anuales, fill = edad))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim =  c(0, 80000))+ theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.position = 0)+
  scale_fill_manual(values = wyy)
  

ees18$edad <- factor(ees18$edad, levels = c("<19", "19-29", "30-39", "40-49", "50-59", "+59"))
incperage2 <-
  ggplot(ees18, aes(x=edad, y=ingresos_anuales, fill = edad))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim =  c(0, 80000)) + theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.position = 0)+
  scale_fill_manual(values = wyy)

#par(mar=c(2,2,2,2))
plot_grid(incperage, incperage2, labels = c("2006", "2008"), hjust=-1.3, vjust = 1.8)

  ##Renta mediana por grupo de edad
dfrentas<- rbind(ees06 %>% 
  select(edad, ingresos_anuales, women) %>% 
  group_by(edad, women) %>% 
  summarise(median= median(ingresos_anuales), 
            año="2006"),
  ees18 %>% 
    select(edad, ingresos_anuales, women) %>% 
    group_by(edad, women) %>% 
    summarise(median= median(ingresos_anuales),
              año= "2018"))

dfrentas$edad <- factor(dfrentas$edad, levels= c("<19", "19-29", "30-39", "40-49", "50-59", "+59"))

p<-   
  dfrentas %>% 
  filter(edad!="<19") %>% 
  group_by(año, edad) %>% 
ggplot(aes(x=edad, y=median, group= interaction(año, women), color= interaction(as.character(año), women)))+
  geom_line(aes(linetype=año), size=1.0005)+ 
  geom_point(shape=19, size=1.5)+
  scale_linetype_manual(values = c("2006" = "dashed", "2018" = "solid"))+
  theme_minimal()+
    scale_color_manual(values = c("#497c7d", "#9bc9be", "#b67182",  "#d0a5ae"),
                      labels= c("Hombres-06", "Hombres-18", "Mujeres-06", "Mujeres18")
                      )+
    theme(plot.background = element_rect(color = "white"),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(),
        text = element_text(family = "serif")
        )+
  guides(linetype="none")
  
ggsave(p, filename = "../Imágenes finales/RentasMedianasgenero.png", width= 5.5 ,dpi = 300, type= "cairo")

  ##Rentas del capital

    ###Dataset para rentas del capital
dfrentascap <- rbind(
  ecv06 %>% 
    select(grupoedad, ingcapital, mayoresmenores, emancipado, pesos) %>% 
    filter(emancipado==1) %>% 
    group_by(grupoedad) %>% 
    mutate(dummycap= ifelse(ingcapital>0, 1, 0)) %>% 
    summarise(mediancap= median(ingcapital[ingcapital>0]),
              meancap= weighted.mean(ingcapital, pesos),
              coefvar= sd(ingcapital)/meancap,
              dummycap= weighted.mean(dummycap, pesos), 
              año ="2006"),
  
  ecv18 %>% 
    select(grupoedad, ingcapital, mayoresmenores, emancipado, pesos) %>%
    filter(emancipado==1) %>% 
    group_by(grupoedad) %>% 
    mutate(dummycap= ifelse(ingcapital>0, 1, 0)) %>% 
    summarise(mediancap= median(ingcapital[ingcapital>0]),
              meancap= weighted.mean(ingcapital, pesos),
              coefvar= sd(ingcapital)/meancap,
              dummycap= weighted.mean(dummycap, pesos),
              año= "2018")
)

###Primer gráfico rentas capital

ggrentcap1<- 
  dfrentascap %>% 
  ggplot(aes(x=grupoedad, y=mediancap, fill=año))+
  geom_bar(stat= "identity", position = "dodge", alpha=0.9)+
    scale_fill_manual(values = c("#66979b", "#9bc9be"))+
  guides(fill="none")
    

ggrentcap2<-  
  dfrentascap %>%  
  ggplot()+
  #geom_rect(aes(xmin=-Inf, xmax=35,
   #             ymin=-Inf, ymax=Inf,
    #            fill=mayoresmenores), fill="springgreen4", alpha =0.01)+
  geom_bar(aes(x=grupoedad, y=dummycap, fill= año),
           position= "dodge", stat= "identity", alpha=0.85)+
  geom_text(aes(x=grupoedad, y=dummycap, label=ifelse(año== "2006",
                             paste0(scales::percent(dummycap,2)),
                             "")), 
            vjust= -0.5,
            hjust=1.1,
            family= "serif")+
  geom_text(aes(x=grupoedad, y=dummycap, label=ifelse(año== "2018",
                                                      paste0(scales::percent(dummycap,2)),
                                                      "")), 
            vjust= -0.5,
            hjust=-0.1,
            family= "serif")+
    scale_fill_manual(values = c("#66979b", "#9bc9be"))+
  scale_color_manual(values = c("#00203FFF","#007A72"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(legend.position = c(0.1,0.9))

###Unión dos gráficos 
ggsave(plot_grid(ggrentcap2,ggrentcap1, labels = "auto"),
       filename = "../Imágenes finales/Rentas capital3.png",
       width = 7,
       height = 4,
       dpi = 600, type= "cairo")

#Ingresos del capital, de inmuebles o de empreseas?

rbind(ecv06 %>% 
  drop_na(HY090N, HY040N) %>%  
  filter(HY090N>0, HY040N>0) %>%
    group_by(grupoedad) %>% 
  summarise(año=2006, 
            emp=median(HY090N),
            inm=median(HY040N),
            pemp= emp/(emp+inm),
            pinm=inm/(inm+emp)),

ecv18 %>% 
  drop_na(HY090N, HY040N) %>%  
  filter(HY090N>0, HY040N>0) %>% 
  group_by(grupoedad) %>% 
  summarise(año=2018, 
            emp=median(HY090N),
            inm=median(HY040N),
            pemp= emp/(emp+inm),
            pinm=inm/(inm+emp)))

#MERCADO DE TRABAJO


#PL160 cambio de trabajo

  ##Tiempo completo/Temporal

dftcompleto<- rbind(ecv06 %>% 
  drop_na(tcompleto, nivel_edu) %>% 
  group_by(mayoresmenores) %>% 
  summarise(año= "2006",
            p= sum(tcompleto, na.rm = T)/length(tcompleto)),

ecv18 %>% 
  drop_na(tcompleto, nivel_edu) %>% 
  group_by(mayoresmenores) %>% 
  summarise(año="2018",
            p= sum(tcompleto)/length(tcompleto))) 

#ggtcompleto<- 
  dftcompleto %>% 
  ggplot(aes(x=mayoresmenores, y= p, fill=año))+
  geom_col(position="dodge", alpha=0.9)+
  theme_minimal()+
  theme(plot.background = element_rect(colour = "white"),
        legend.position = "bottom",
        axis.title = element_blank(),
        legend.title = element_blank(),
        text = element_text(family = "serif"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values = c("#835d6c", "#6faca7"))
  
ggsave(plot = ggtcompleto, filename = "ggtcompleto.jpg")


dfhoras<- ecv18 %>% 
  mutate(hola = case_when(horastrabajo %in% c(35:45) ~ "35-45",
                          horastrabajo <35 ~ "<35", 
                          horastrabajo >45 ~ ">45")) %>% 
  drop_na(hola) %>% 
  group_by(grupoedad, hola) %>% 
  count(hola) %>% 
  group_by(grupoedad) %>% 
  mutate(p=n/sum(n))

dfhoras %>% 
  ggplot(aes(x=grupoedad, y= p, color=hola)) +
  geom_point()+
  theme_minimal()

ecv18$mayoresmenores <- factor(ecv18$mayoresmenores, levels = c("35+","<35" ))


dfhoras2 <- ecv18 %>% 
  drop_na(horastrabajo) %>% 
  group_by(mayoresmenores) %>% 
  transmute(horastrabajo=horastrabajo,
            horitas1= ifelse(mayoresmenores=="<35",horastrabajo/sum(horastrabajo),0),
            horitas2 =ifelse(mayoresmenores=="35+",horastrabajo/sum(horastrabajo),0))

dfhoras3 <- ecv18 %>% 
  select(horastrabajo, mayoresmenores) %>% 
  drop_na(horastrabajo) %>% 
  mutate(horitasa= ifelse( mayoresmenores=="<35",horastrabajo,NA),
            horitasb=ifelse( mayoresmenores=="35+",horastrabajo,NA))


dfhoras2 %>% 
  ggplot(aes(x=horastrabajo))+
  geom_col(aes(y=horitas1, 
               alpha=0.2,
           fill="blue"))+
  geom_col(aes(y=horitas2), 
               alpha=0.2,
           fill="red")+
  theme_minimal()+
  xlim(20,75)
  scale_color_manual(values = c("darkturquoise","darkorchid2"))


dfhoras3 %>% 
  ggplot(aes(color= mayoresmenores))+
  geom_density(aes(x=horitasa, y=cumsum(..density..)), 
               alpha=0.4)+
  geom_density(aes(x=horitasb, y=cumsum(..density..)), 
               alpha=0.4)+
  theme_minimal()+
  xlim(0,75)+
  scale_color_manual(values = c("darkturquoise","darkorchid2"))+
  coord_cartesian(xlim=c(15, 65))
  
  
dfhoras2 %>%  
  ggplot(aes(x= horastrabajo, fill=mayoresmenores))+
  geom_col(aes(y=horitas1),
           alpha=0.7, 
           fill= "red")+
  geom_col(aes(y=horitas2), 
           alpha=0.7, 
           fill="green")+
  theme_minimal()

dfhoras2 %>% 
  filter(horitas1>0.000000000, horitas2>0.0000000000) %>% 
  ggplot()+
  geom_density(aes(x=horitas1), stat = "bin")



ecv18 %>% 
  mutate(count1= ifelse(horastrabajo %in% c(35:45), 1, 0),
         count2=ifelse(horastrabajo <35, 1 ,0),
         count3= ifelse(horastrabajo >45, 1, 0)) %>% 
  group_by(grupoedad) %>% 
  summarise(count(horastrabajo[ecv18$horastrabajo<30]))
  summarise(sum(hola, na.rm = T))

sum(ecv18$horastrabajo == "40", na.rm = T)

dfhorastrabajo<- ecv18 %>% 
  group_by(grupoedad, horastrabajo) %>% 
  count(horastrabajo)/length(ecv18$horastrabajo)

cc100 <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=100))

dfhorastrabajo %>% 
ggplot(aes(x=grupoedad, y=horastrabajo, size=n, color=horastrabajo)) +
  geom_point()+
  theme_minimal()+
  scale_color_gradient(low = "blue", high = "red")

##Trabajo < 30h

### Razones por las que se trabaja menos

#### 2006

trabamas <- ecv06 %>%
  select(grupoedad, trabajarmas) %>% 
  drop_na(trabajarmas) %>% 
  group_by(grupoedad, trabajarmas) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

trabamas$grupoedad <- factor(trabamas$grupoedad, levels = c("<23", "23-34", "35-44", "45-54", "55-64", "+65"))
trabamas$trabajarmas <- factor(trabamas$trabajarmas, levels= c("Otros", "No quiere", "Trabaja en el hogar", "Estudios","No encuentra" ))

ggplot(trabamas, aes(x = factor(grupoedad), y = perc*100, fill = factor(trabajarmas))) +
  geom_bar(stat="identity", width = 0.7)+
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank())

###Jóvenes que quieren trabajar más por edad

####2006
trabamas2 <- ecv06 %>%  
  select(grupoedad, trabajarmas) %>% 
  group_by(grupoedad) %>% 
  count(nope = trabajarmas== "No encuentra") %>% 
  drop_na(nope) %>% 
  pivot_wider(names_from = nope, values_from = n) %>%
  rename(c("Si" = "TRUE", "No"="FALSE"))

trabamas2$prob <- trabamas2$Si/(trabamas2$No+trabamas2$Si)


trabamas2$grupoedad <- factor(trabamas2$grupoedad, levels = c("<23", "23-34", "35-44", "45-54", "55-64", "+65"))


plottrabamas2 <- 
  ggplot(trabamas2, aes(x=grupoedad, y=prob, fill=grupoedad))+
  geom_col(alpha=0.85)+
  theme(legend.position = 0,
        plot.title = element_text(hjust = 0.5))+
  labs(title = "2006")+
  geom_text(aes(label=scales::percent(round(prob,2))), vjust=-0.5, family= "serif")+
  scale_fill_manual(values = c("#497c7d", "#9bc9be", "#b67182",  "#d0a5ae"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.75))

#### 2018

trabamas218 <- ecv18 %>%  
  select(grupoedad, trabajarmas) %>% 
  group_by(grupoedad) %>% 
  count(nope = trabajarmas== "No encuentra") %>% 
  drop_na(nope) %>% 
  pivot_wider(names_from = nope, values_from = n) %>% 
  rename(c("Si" = "TRUE", "No"="FALSE"))

trabamas218$prob <- trabamas218$Si/(trabamas218$No+trabamas218$Si)


trabamas218$grupoedad <- factor(trabamas218$grupoedad, levels = c("<23", "23-34", "35-44", "45-54", "55-64", "+65"))
plottrabamas218 <-
  ggplot(trabamas218, aes(x=grupoedad, y=prob, fill=grupoedad))+
  geom_bar(stat = "identity", alpha=0.85)+
  theme(legend.position = 0,
        plot.title = element_text(hjust = 0.5), 
        axis.text.y = element_blank())+
  ylim(0,0.75)+
  labs(title = "2018")+ 
  geom_text(aes(label=scales::percent(round(prob,2))), vjust=-0.5, family= "serif")+
  scale_fill_manual(values = c("#497c7d", "#9bc9be", "#b67182",  "#d0a5ae"))

plot_grid(plottrabamas2, plottrabamas218)

ggsave(plot=plot_grid(plottrabamas2, plottrabamas218),
                       filename= "../Imágenes finales/ggtrabamas.png", type= "cairo") #maese

###Jóvenes que quieren trabajar más por edad y nivel educativo

####2006

trabamas5 <- ecv06 %>%  
  select(grupoedad, trabajarmas, nivel_edu) %>% 
  group_by(grupoedad, nivel_edu) %>% 
  count(nope = trabajarmas== "No encuentra") %>% 
  drop_na(nope, nivel_edu) %>% 
  pivot_wider(names_from = nope, values_from = n) %>% 
  rename(c("Si" = "TRUE", "No"="FALSE"))

trabamas5$prob <- trabamas5$Si/(trabamas5$No+trabamas5$Si)

trabamas5 <- trabamas5 %>% 
  mutate(ahorasi = case_when(grupoedad == "23-34" ~ prob*0.3536977/ 1.030765,
                             grupoedad == "35-44" ~ prob*0.2420749/0.6941384,
                             grupoedad == "45-54" ~ prob*0.2889908/0.844295,
                             grupoedad == "55-64" ~ prob*0.1751825/0.418439))%>% 
  group_by(grupoedad) %>% 
  mutate(hola= prob/sum(prob))


#tmne<-
  ggplot(trabamas5, aes(x=grupoedad, y=ahorasi, fill=nivel_edu))+
  geom_col()+
    scale_fill_manual(values = c("#9bc9be", "#497c7d", "#11545a"))+
    theme(legend.position = 0,
        plot.title = element_text(hjust = 0.5))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.6))+
  labs(title = "2006")
  # geom_text(aes(label=ifelse(nivel_edu== "Bajo" & grupoedad!= "23-34", paste0(scales::percent(round(ahorasi,2))), "")), vjust= 3.4, color= "white", family= "serif")+
  # geom_text(aes(label=ifelse(nivel_edu== "Bajo" & grupoedad== "23-34", paste0(scales::percent(round(ahorasi,2))), "")), vjust= 5, color= "white", family= "serif")+
  # geom_text(aes(label= ifelse(nivel_edu == "medio", paste0(scales::percent(round(ahorasi,2))),"")), vjust= -1.5, color= "white", family= "serif")+
  # geom_text(aes(label= ifelse(nivel_edu == "alto", paste0(scales::percent(round(ahorasi,2))),"")), vjust= -6, color= "white", family= "serif")+
  # geom_text(aes(label=ifelse(nivel_edu== "alto" & grupoedad== "55-64", paste0(scales::percent(round(ahorasi,2))), "")), vjust= -3, color= "white", family= "serif")
  

#### 2018
trabamas518 <- 
  ecv18 %>%  
  select(grupoedad, trabajarmas, nivel_edu) %>% 
  group_by(grupoedad, nivel_edu) %>% 
  count(nope = trabajarmas== "No encuentra") %>% 
  drop_na(nope) %>% 
  pivot_wider(names_from = nope, values_from = n) %>% 
  rename(c("Si" = "TRUE", "No"="FALSE"))

trabamas518$prob <- trabamas518$Si/(trabamas518$No+trabamas518$Si)

trabamas518 <- trabamas518 %>% 
  mutate(ahorasi = case_when(grupoedad == "23-34" ~ prob*0.5743945/1.75253,
                   grupoedad == "35-44" ~ prob*0.3636364/1.189188,
                   grupoedad == "45-54" ~ prob*0.5191740/1.504517,
                   grupoedad == "55-64" ~ prob*0.4155844/1.168284)) %>% 
  group_by(grupoedad) %>% 
  mutate(hola= prob/sum(prob))

trabamas518$nivel_edu <- factor(trabamas518$nivel_edu, levels = c("alto", "medio", "bajo"))
tmne18<-
  ggplot(trabamas518, aes(x=grupoedad, y=ahorasi, fill=nivel_edu))+
  geom_col()+
    scale_fill_manual(values = c("#9bc9be", "#497c7d", "#11545a"), 
                      labels= c("Alto", "Medio", "Bajo"))+
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.6)) +
    labs(title = "2018")
  # geom_text(aes(label=ifelse(nivel_edu=="bajo", paste0(scales::percent(round(ahorasi,2))),"")), vjust=3, color= "white", family= "serif")+
  # geom_text(aes(label=ifelse(nivel_edu=="medio", paste0(scales::percent(round(ahorasi,2))),"")), vjust=-4, color= "white", family= "serif")+
  # geom_text(aes(label=ifelse(nivel_edu=="alto" & grupoedad != c("35-44", "55-64"), paste0(scales::percent(round(ahorasi,2))),"")), vjust=-13.5, color= "white", family= "serif")+
  # geom_text(aes(label=ifelse(nivel_edu=="alto" & grupoedad == "35-44", paste0(scales::percent(round(ahorasi,2))),"")), vjust=-10, color= "white", family= "serif")+
  # geom_text(aes(label=ifelse(nivel_edu=="alto" & grupoedad == "55-64", paste0(scales::percent(round(ahorasi,2))),"")), vjust=-10, color= "white", family= "serif")
  
plot_grid(tmne, tmne18) 

ggsave(plot_grid(tmne, tmne18, rel_widths = c(1,1.4)), 
       filename = "../Imágenes finales/ggtrabamasedu.png", type= "cairo" )

#GRÁFICOS CALIDAD DE VIDA

  ##Tabla

    ###Emancipación


dfemancipacion <- left_join(ecv06 %>% 
  filter(grupoedad == "23-34") %>%
    select(emancipado, grupoedad, pesos) %>% 
    mutate(casa=ifelse(emancipado==0, 1, 0)) %>% 
  summarise(id=1,
            "2006"=weighted.mean(casa, pesos)),
  ecv18 %>% 
    filter(grupoedad== "23-34") %>%
    select(emancipado, grupoedad, pesos) %>% 
    mutate(casa=ifelse(emancipado==0, 1, 0)) %>% 
    summarise(id=1,
              "2018"=weighted.mean(casa, pesos))
) %>% 
  mutate(cat= "...viven con sus padres")

    ####Vacaciones

dfvac<- left_join(ecv06 %>% 
        filter(grupoedad == "23-34", emancipado==1) %>%
          mutate(no=ifelse(vacaciones==0, 1, 0)) %>% 
        summarise(id=1,
                  "2006"=weighted.mean(no, pesos, na.rm=T)),
      ecv18 %>% 
        filter(grupoedad== "23-34", emancipado==1) %>%
        mutate(no=ifelse(vacaciones==0, 1, 0)) %>%
        summarise(id=1,
                  "2018"=weighted.mean(no, pesos, na.rm=T))
)%>% 
  mutate(cat= "...no pueden permitirse unas vacaciones")

    ###Retrasos

      ####Hipoteca/alquiler

dfhip<- left_join(
  ecv06 %>% 
    filter(grupoedad=="23-34", emancipado==1) %>% 
    summarise(id=1,
              "2006"=weighted.mean(retraso, pesos, na.rm = T)),
  ecv18 %>% 
    filter(grupoedad=="23-34", emancipado==1) %>% 
    summarise(id=1,
              "2018"=weighted.mean(retraso, pesos, na.rm = T)),
  by= "id"
  
)%>% 
  mutate(cat= "...se han retrasado en el pago de hipoteca/alquiler")

      ####Facturas

dffac<- left_join(
  ecv06 %>% 
    filter(grupoedad=="23-34", emancipado==1) %>% 
    summarise(id=1,
              "2006"=weighted.mean(retrasofacturas, pesos, na.rm = T)),
  ecv18 %>% 
    filter(grupoedad=="23-34", emancipado==1) %>% 
    summarise(id=1,
              "2018"=weighted.mean(retrasofacturas, pesos, na.rm = T)),
  by="id"
)%>% 
  mutate(cat= "...se han retrasado en otras facturas")
  
      ####Otros préstamos (deudas no relacionadas con la vivienda principal)
dfotros<- left_join(
  ecv06 %>% 
    filter(grupoedad=="23-34", emancipado==1) %>% 
    summarise(id=1,
              "2006"=weighted.mean(retrasootros, pesos, na.rm = T)),
  ecv18 %>% 
    filter(grupoedad=="23-34", emancipado==1) %>% 
    summarise(id=1,
              "2018"=weighted.mean(retrasootros, pesos, na.rm = T))
)%>% 
  mutate(cat= "...se han retrasado en otros préstamos")

    ###Estudiando y trabajando
dfestrab<- left_join(
  ecv06 %>% 
  filter(grupoedad=="23-34") %>% 
  summarise(id=1,
            "2006"=weighted.mean(estudiando, pesos, na.rm = T)),
  ecv18 %>% 
    filter(grupoedad=="23-34") %>% 
    summarise(id=1,
              "2018"=weighted.mean(estudiando, pesos, na.rm = T))
)%>% 
  mutate(cat= "...estudian y trabajan")

    ###Tenencia
dften<- left_join(
  ecv06 %>% 
    filter(grupoedad=="23-34", emancipado==1) %>% 
    mutate(no=ifelse(tenencia==0, 1, 0)) %>%
    summarise(id=1,
              "2006"=weighted.mean(no, pesos, na.rm = T)),
  ecv18 %>% 
    filter(grupoedad=="23-34", emancipado==1) %>%
    mutate(no=ifelse(tenencia==0, 1, 0)) %>%
    summarise(id=1,
              "2018"=weighted.mean(no, pesos, na.rm = T))
)%>% 
  mutate(cat= "...viven de alquiler")


    ###Tabla final

dftable2<- rbind(dfemancipacion, dfhip, dffac, dfotros, dften, dfvac) %>% 
  select(-id) %>% 
  mutate(Diferencia = `2018`-`2006`)


dftable2 %>% 
  gt(rowname_col = "cat") %>% 
  gt_theme_nytimes() %>% 
  tab_header(
    title = "Porcentaje de jóvenes que...",
    subtitle = "2006 vs 2018"
  ) %>% 
  fmt_percent(
    columns = c(`2006`, `2018`),
    decimals = 2,
    dec_mark = ".",
    sep_mark = " "
  ) %>% 
  fmt_percent(
    columns =Diferencia,
    decimals = 2,
    dec_mark = "."
  ) %>% 
  fmt_markdown(
    columns = cat,
  ) %>% 
  # gt_plt_bar_pct(
  #   column = Proporción,
  #   height = 16,
  #   scaled = TRUE, 
  #   fill = "#0e387a",
  #   background = "#9fafca"
  # ) %>% 
  gt_color_rows(Diferencia,
                palette = c("#90EE90", "#fbbf77", "#FF6863"), 
                domain = c(-0.03,0.03),
                use_paletteer = F) %>% 
  tab_style(style = cell_fill(color = "#ff4c46"),
            locations = cells_body(
              columns = Diferencia,
              rows = Diferencia>0.2
            )
  ) %>% 
  tab_footnote(footnote = "Sobre el total de emancipados", 
               locations = cells_stub(rows = c(2:6))) %>% 
  tab_options(table.font.names = "serif", 
              table.font.color = "black",
              footnotes.marks = c("*")) 

  # select(-edad) %>% 
  # select(cat, `2006`, `2018`) %>% 
  # mutate(Proporción = `2018`/`2006`*100,
  #        "-" = `2018`/`2006`) 
  # 

  ##Gráficos

    ###Ayuda Sólo está para 2018 
ecv18 %>% 
  filter(grupoedad=="23-34", emancipado==1) %>% 
  summarise(weighted.mean(ayuda, pesos)*100) #15.86% de los jóvenes piden ayuda 

ggayuda<- 
  ecv18 %>% 
  filter(emancipado==1) %>%
  group_by(grupoedad) %>% 
  summarise(p= weighted.mean(ayuda, pesos)) %>% 
  ggplot(aes(x=grupoedad,y=p, fill=grupoedad))+
  geom_col(alpha=0.85)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_text(aes(label=scales::percent(round(p,3)), 
                vjust=-0.3, 
                family= "serif"))+
  guides(fill="none")+
    scale_fill_manual(values = c("#497c7d", "#9bc9be", "#b67182",  "#d0a5ae"))
  
ggsave(plot = ggayuda, filename = "../Imágenes finales/ggayuda.jpg", type="cairo")

    ###Alquiler 

      ####Gráfico cambio alquileres
dfalquiler<- rbind(
  ecv06 %>% 
    filter(emancipado==1) %>% 
    group_by(grupoedad) %>% 
    summarise(año="2006",
              mean=weighted.mean(alquiler, pesos, na.rm=T)),
  ecv18 %>% 
    filter(emancipado==1) %>% 
    group_by(grupoedad) %>% 
    summarise(año="2018",
              mean=weighted.mean(alquiler, pesos, na.rm=T)))


ggvaralq<- 
   dfalquiler %>% 
  ggplot(aes(x=grupoedad, y=mean, fill=año))+ 
  geom_bar(stat= "identity", position = "dodge", alpha=0.85)+
  theme_minimal_hgrid()+
  theme(axis.title = element_blank(), 
        legend.title = element_blank(),
        legend.position = c(0.8,0.95), 
        text = element_text(family = "serif"), 
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))+
  guides(alpha="none")+
     scale_fill_manual(values = c("#497c7d", "#9bc9be", "#b67182",  "#d0a5ae"))
   
ggsave(plot = ggvaralq,  filename = "../Imágenes finales/ggvaralq.jpg", type="cairo" )


        ####Mapa variación alquiler

#https://github.com/aaumaitre/maps_Spain/blob/master/README.md Manual Ariane

sf_regional <- readOGR("ComunidadesAutonomas_ETRS89_30N/Comunidades_Autonomas_ETRS89_30N.shp")
regional_df <- tidy(sf_regional)
temp_df <- data.frame(sf_regional$Texto)
temp_df$id <- as.character(seq(0,nrow(temp_df)-1))
regional_df2 <- left_join(regional_df, temp_df, by="id")

dfalquiler2 <- left_join( 
  ecv06 %>% 
  filter(emancipado==1, grupoedad== "23-34") %>% 
  group_by(id) %>% 
  summarise(mean06= weighted.mean(alquiler, pesos, na.rm=T)),
  ecv18 %>% 
    filter(emancipado==1, grupoedad== "23-34") %>% 
    group_by(id) %>% 
    summarise(mean18= weighted.mean(alquiler, pesos, na.rm=T))
) %>% mutate(var= (mean18/mean06-1),
             id= as.character(id))

#dfalquiler2["mean06"][dfalquiler2["mean06"] > 849] <- 650

regional_plot <- regional_df2 %>% 
  left_join(dfalquiler2, by="id")
regional_plot <- regional_plot%>%
  mutate(lat_c = ifelse(lat <35e5, lat + 75e4, lat),
         long_c = ifelse(long <(-5e5), (long + 65e4), long))


#Creating separate df
canaries_line <- data.frame(long = c(-36e4, 15e4, 15e4),
                            lat = c(405e4, 405e4, 385e4))

theme_ari_maps <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
      plot.caption = element_text(size = 7.5, color = "grey40"),
      legend.title = element_text(color = "grey40", size = 8),
      legend.text = element_text(color = "grey40", size = 7, hjust = 0),
      legend.position = c(0.7, 0.07),
      legend.text.align = 0,
      plot.margin = unit(c(.5,.5,.2,.5), "cm"),
      panel.spacing = unit(c(2,0.2,.2,0.2), "cm"))
}


 
#ggalquileres <- 
   regional_plot%>%
  ggplot(aes(x=long_c, y = lat_c, group = group))+
  geom_polygon(aes(fill=var), color = "white", size = 0.3, alpha=0.9)+
  geom_path(data = canaries_line, aes(x=long, y = lat, group = NULL), color = "grey40")+
   scale_fill_gradient(low="#ffd89b", 
                       high = "#19547b",
                       #low="#b67182", 
                       #high = "#497c7d",
                       name= "Variación",
                       labels= scales::percent_format(accuracy = 1),
                       limits= c(-0.35, 0.21),
                       oob= scales::squish,
                       guide= guide_legend(
                         direction= "horizontal",
                         title.position = "top",
                         title.hjust = 0.5,
                         label.position = "bottom",
                         label.hjust = 0.5))+
  labs(title="Variación del precio del alquiler",
       subtitle="Jóvenes 23-34 años, 2006 vs 2018")+
  theme_ari_maps()+
  theme(plot.background = element_rect(colour = "white"),
        panel.background =element_rect(colour = "white"), 
        legend.background = element_rect(colour = "white"),
        text = element_text(family = "serif"),
        panel.grid.major = element_blank(),
        legend.position = c(0.85,0.2))

  ggsave(plot=ggalquileres, "../Imágenes finales/alquileres.jpg", type="cairo")
  
          ####Mapa alquileres 2006 y 2018

  # dfalquiler3<- ecv18 %>% 
  #   filter(emancipado==1, grupoedad== "23-34") %>% 
  #   group_by(id) %>% 
  #   summarise(mean18= mean(alquiler, na.rm=T),
  #             id= as.character(id))
  # 
  # regional_plot3 <- regional_df2 %>% 
  #   left_join(dfalquiler3, by="id")
  regional_plot3 <- regional_plot%>%
    mutate(lat_c = ifelse(lat <35e5, lat + 75e4, lat),
           long_c = ifelse(long <(-5e5), (long + 65e4), long)) 
  

  
ggalquileres18 <-   
  regional_plot3%>%
    ggplot(aes(x=long_c, y = lat_c, group = group))+
    geom_polygon(aes(fill=mean18), color = "white", size = 0.3, alpha=0.8)+
    geom_path(data = canaries_line, aes(x=long, y = lat, group = NULL),
              color = "grey40")+ #Separar las canarias
    scale_fill_gradient(low="#ffd89b", 
                        high = "#19547b",
                        name= "",
                        limits= c(163,657),
                        guide= guide_legend(
                          direction= "horizontal",
                          title.position = "top",
                          title.hjust = 0.5,
                          label.position = "bottom",
                          label.hjust = 0.5),
                        #breaks= c(300, 400, 500, 600, 700)
                        )+
    theme_ari_maps()+
    theme(plot.background = element_rect(colour = "white"),
          panel.background =element_rect(colour = "white"), 
          legend.background = element_rect(colour = "white"),
          text = element_text(family = "serif"),
          panel.grid.major = element_blank())
  
ggsave(plot=ggalquileres18, "alquileres18.jpg", type="cairo")
  
  
ggalquileres06 <- 
  regional_plot3%>%
  ggplot(aes(x=long_c, y = lat_c, group = group))+
  geom_polygon(aes(fill=mean06), color = "white", size = 0.3, alpha=0.8)+
  geom_path(data = canaries_line, aes(x=long, y = lat, group = NULL),
            color = "grey40")+ #Separar las canarias
  scale_fill_gradient(low="#ffd89b", 
                      high = "#19547b",
                      name= "",
                      limits= c(163, 657),
                      oob= scales::squish,
                      guide= guide_legend(
                        direction= "horizontal",
                        title.position = "top",
                        title.hjust = 0.5,
                        label.position = "bottom",
                        label.hjust = 0.5)
                      #breaks= c(300, 400, 500, 600, 700)
                      )+
  theme_ari_maps()+
  theme(plot.background = element_rect(colour = "white"),
        panel.background =element_rect(colour = "white"), 
        legend.background = element_rect(colour = "white"),
        text = element_text(family = "serif"),
        panel.grid.major = element_blank())

ggsalq<- plot_grid(ggalquileres06, ggalquileres18, labels = c("2006", "2018"))

ggsave(plot= ggsalq, "../Imágenes finales/ggsalq.jpg", width = 3000, units = "px", type= "cairo")



    ###Ingresos-gasto de vivienda
left_join(
rbind(ecv06 %>% 
  filter(edad %in% 19:29, emancipado==1) %>% 
  drop_na(gastovivienda) %>% 
  summarise(año=2006,
            viv=mean(gastovivienda),
            alq=mean(alquiler, na.rm=T)),
  ecv18 %>% 
  filter(edad %in% 19:29, emancipado==1) %>% 
  drop_na(gastovivienda) %>% 
  summarise(año=2018,
            viv=mean(gastovivienda),
            alq=mean(alquiler, na.rm=T))),

rbind(ees06 %>% 
  filter(edad== "19-29") %>% 
  summarise(año=2006,
            ing=mean(ingresos_anuales)),
  ees18 %>% 
  filter(edad=="19-29") %>% 
  summarise(año=2018,
            ing=mean(ingresos_anuales)))
) %>% 
  mutate(resta= ing/12-viv, 
         prop= viv/(ing/12)*100)


    ### Pensiones

dfpensiones <- rbind(ecv06 %>% 
  group_by(grupoedad) %>% 
  summarise(año="2006",
            prop=weighted.mean(dummypensiones, pesos, na.rm=T)
            ),
  ecv18 %>% 
    group_by(grupoedad) %>% 
    summarise(año="2018",
              prop=weighted.mean(dummypensiones, pesos, na.rm=T)
              ))

dfpensiones2 <- rbind(ecv06 %>% 
                       filter(pensiones>0) %>% #¿Dejar o no?
                group_by(grupoedad) %>% 
                summarise(año="2006",
                          mean=weighted.mean(pensiones,pesos, na.rm=T)),
              ecv18 %>% 
                filter(pensiones>0) %>% #¿Dejar o no?
                group_by(grupoedad) %>% 
                summarise(año="2018",
                          mean=weighted.mean(pensiones, pesos, na.rm=T)))

 ggpension <-   
   dfpensiones %>% 
  ggplot(aes(x=grupoedad, y=prop, fill=año))+
  geom_bar(position = "dodge", stat = "identity", alpha=0.85)+
  theme(legend.position = c(0.1,0.9))+
     scale_fill_manual(values = c( "#497c7d","#9bc9be"))+
   scale_y_continuous(labels = scales::percent_format(accuracy = 1))

ggpension2<-  
  dfpensiones2 %>% 
  ggplot(aes(x=grupoedad, y=mean, fill=año))+
  geom_bar(position = "dodge", stat = "identity", alpha=0.85)+
  theme(legend.position = c(0.1,0.9))+
    scale_fill_manual(values = c( "#497c7d","#9bc9be"))
    
ggsave(plot = ggpension, filename = "../Imágenes finales/ggpension.jpg", type= "cairo")
ggsave(plot = ggpension2, filename = "../Imágenes finales/ggpension2menorque0.jpg", type= "cairo")


    ###Gráfico gasto medio y tenencia
dfggingviv <- rbind(
ecv06 %>% 
  filter(emancipado==1) %>% 
  group_by(edad) %>% 
  summarise(año=2006, 
            mean=mean(gastovivienda), 
            ptenencia= sum(tenencia, na.rm = T)/length(tenencia)),
ecv18 %>% 
  filter(emancipado==1) %>% 
  group_by(edad) %>% 
  summarise(año=2018, 
            mean=mean(gastovivienda), 
            ptenencia=sum(tenencia, na.rm = T)/length(tenencia))
) 


dfggingviv %>% 
 # filter(DB040 %in% c("ES30", "ES51", "ES52", "ES61")) %>% 
  drop_na(mean) %>% 
  ggplot(aes(x=edad, y=mean, color=as.character(año)))+
  geom_jitter()+
  geom_line(aes(x=edad, y=ptenencia, color= as.character(año)))+
  theme(axis.title = element_blank(),
        legend.position = "")
  #scale_y_continuous(sec.axis =  sec_axis(~.*ptenencia))


#gginvviv<-   
  dfggingviv %>% 
  drop_na(mean) %>% 
  ggplot(aes(x=edad, y=ptenencia, size= mean, color=as.character(año), alpha=mean))+
  geom_point()+
  xlab("Edad")+
  ylab("Porcentaje de tenencia")+
  guides(color=guide_legend(title = element_blank(), title.position = "top"),
         size=guide_legend(title = "Gasto medio alquiler/hipoteca", 
                           title.position = "top"),
         alpha= guide_legend(title= "Gasto medio alquiler/hipoteca"))+
  scale_radius(range = c(1,6))+
  scale_color_manual(values = c("#384e78", "#5874dc"))+
  scale_x_continuous(breaks = round(seq(min(dfggingviv$edad), 
                                        max(dfggingviv$edad), by = 3),1))+
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0,1), 
                     breaks = seq(0, 1, by=0.2),
                     labels = scales::percent_format(accuracy = 1))+
  scale_alpha(range = c(1, 0.2))

ggsave(gginvviv, filename = "../Imágenes finales/gginvviv.jpeg", dpi = 300, type= "cairo")

#gini
library(ineq)
dfjoven06<- ecv06 %>% 
  filter(edad %in% 23:34, ingresos_anuales>0) %>% 
  select(ingresos_anuales)


dfjoven18<- ecv18 %>% 
  filter(edad %in% 23:34, ingresos_anuales>0) %>% 
  select(ingresos_anuales)

ineq(dfjoven06$ingresos_anuales, type= "Gini")
ineq(dfjoven18$ingresos_anuales, type= "Gini")


plot(Lc(dfjoven06$ingresos_anuales),col="darkred",lwd=2)

Distr1 <- Lc(dfjoven06$ingresos_anuales, n = rep(1,length(dfjoven06$ingresos_anuales)), plot =F)

p <- Distr1[1]
L <- Distr1[2]
Distr1_df <- data.frame(p,L)

x<- 
  ggplot(data=Distr1_df) +
  geom_line(aes(x=p, y=L), color="#9bc9be", size=0.6, alpha=0.85) +
  scale_x_continuous(name="Cumulative share of X", limits=c(0,1)) + 
  scale_y_continuous(name="Cumulative share of Y", limits=c(0,1)) +
  ggtitle("", subtitle = "2006")+
  geom_abline(color="#9bc9be", size=0.6)+
  theme(plot.subtitle = element_text(margin = margin(b=-30), vjust = 8, hjust = 0.1))


Distr2 <- Lc(dfjoven18$ingresos_anuales, n = rep(1,length(dfjoven18$ingresos_anuales)), plot =F)

p <- Distr2[1]
L <- Distr2[2]
Distr2_df <- data.frame(p,L)

#y<- 
  ggplot(data=Distr2_df) +
  geom_line(aes(x=p, y=L), color="#497c7d", size=0.6,alpha=0.85) +
  scale_x_continuous(name="Cumulative share of X", limits=c(0,1)) + 
  scale_y_continuous(name="Cumulative share of Y", limits=c(0,1)) +
  geom_abline(color="#497c7d", size=0.6)+
  ggtitle("", subtitle = "2018")+
  theme(axis.text.y = element_blank())+
    theme(plot.subtitle = element_text(margin = margin(b=-30), vjust = 8, hjust = 0.1))
  

ggsave(plot=plot_grid(x,y), width = 6, filename = "../Imágenes finales/gini.png", type= "cairo")

#Lorenz de edades

  #Por separado
dfgini06 <- ecv06%>% 
  filter(ingresos_anuales>0) %>% 
  group_by(edad) %>% 
  summarise(mean =weighted.mean(ingresos_anuales, pesos)) %>% 
  mutate(csum= cumsum(mean),
         equal= mean(mean), 
         lorenz=csum/sum(mean),
         gini=cumsum(equal/sum(mean)))

dfgini18 <- ecv18%>% 
  filter(ingresos_anuales>0) %>% 
  group_by(edad) %>% 
  summarise(mean =weighted.mean(ingresos_anuales, pesos)) %>% 
  mutate(csum= cumsum(mean),
         equal= mean(mean), 
         lorenz=csum/sum(mean),
         gini=cumsum(equal/sum(mean)))

  #Juntos

 dfgini<- rbind(ecv06%>% 
  filter(ingresos_anuales>0) %>% 
  group_by(edad) %>% 
  summarise(mean =weighted.mean(ingresos_anuales, pesos)) %>% 
  mutate(año="2006",
         csum= cumsum(mean),
         equal= mean(mean), 
         lorenz=csum/sum(mean),
         gini=cumsum(equal/sum(mean))),
ecv18%>% 
  filter(ingresos_anuales>0) %>% 
  group_by(edad) %>% 
  summarise(mean =weighted.mean(ingresos_anuales, pesos)) %>% 
  mutate(año="2018",
         csum= cumsum(mean),
         equal= mean(mean), 
         lorenz=csum/sum(mean),
         gini=cumsum(equal/sum(mean))))


gggini<-
  dfgini %>% 
ggplot(aes(x=edad, group=año)) + 
  geom_line(aes(y=lorenz, color=año), size=0.5)+
  geom_line(aes(y=gini), color="grey40", size=0.5)+
  theme(legend.position = c(.1,.9))+
    scale_color_manual(values = c( "#497c7d","#9bc9be"))
ggsave(plot = gggini, filename = "../Imágenes finales/gggini2.png", type="cairo")
  
  gggini06<-
  dfgini06 %>% 
    ggplot(aes(x=edad))+ 
    geom_line(aes(y=lorenz), color="#497c7d", size=0.5)+
    geom_line(aes(y=gini), color="#497c7d", size=0.5)+
    ggtitle("",subtitle = "2018")+
    theme(axis.text.y=element_blank(),
          plot.subtitle = element_text(margin = margin(b=-30), vjust = 8, hjust = 0.1))
  
gggini18<-
  dfgini18 %>% 
  ggplot(aes(x=edad))+ 
  geom_line(aes(y=lorenz), color="#497c7d", size=0.5)+
  geom_line(aes(y=gini), color="#497c7d", size=0.5)+
  ggtitle("",subtitle = "2018")+
  theme(axis.text.y=element_blank(),
        plot.subtitle = element_text(margin = margin(b=-30), vjust = 8, hjust = 0.1))
  
ggsave(plot=plot_grid(gggini06, gggini18, rel_widths = c(1.2,1)), filename = "../Imágenes finales/gggini.png", width = 6, type="cairo")

ineq(dfgini06$mean, type= "Gini")
ineq(dfgini18$mean, type= "Gini")

rbind(ecv06 %>% 
  drop_na(gastovivienda) %>% 
    filter(emancipado==1) %>% 
  group_by(grupoedad) %>% 
  summarise(año="2006",
            viv= weighted.mean(gastovivienda, pesos),
            ing=weighted.mean(ingresos_anuales, pesos, na.rm=T)),
  ecv18 %>% 
    drop_na(gastovivienda) %>%
    filter(emancipado==1) %>% 
    group_by(grupoedad) %>% 
    summarise(año="2018",
              viv= weighted.mean(gastovivienda, pesos),
              ing=weighted.mean(ingresos_anuales, pesos, na.rm=T))) %>% 
  mutate(rel=viv*12/ing*100)

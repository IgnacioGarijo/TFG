library(tidyverse)
library(reldist)


setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/5º/Segundo semestre/TFG eco/Ficheros y bases de datos")

#Construyendo ECV06


d06 <- read.csv("ECV/2006/datos_ecv2006/esudb06d.csv")
p06 <- read.csv("ECV/2006/datos_ecv2006/esudb06p.csv")
h06 <- read.csv("ECV/2006/datos_ecv2006/esudb06h.csv")
r06 <- read.csv("ECV/2006/datos_ecv2006/esudb06r.csv")

per06 <- left_join(r06,p06, by= c("RB030"="PB030")) %>%
mutate(id_hh= as.integer(str_sub(RB030, 1, -3)))
hog06 <- left_join(d06,h06, by= c("DB030"="HB030"))
ecv06 <- left_join(per06, hog06, by=c("id_hh" = "DB030"))

d18 <- read.csv("ECV/2018/disreg_ecv18/esudb18d.csv")
p18 <- read.csv("ECV/2018/disreg_ecv18/esudb18p.csv")
h18 <- read.csv("ECV/2018/disreg_ecv18/esudb18h.csv")
r18 <- read.csv("ECV/2018/disreg_ecv18/esudb18r.csv")

#Construyendo ECV18

per18 <- left_join(r18,p18, by= c("RB030"="PB030")) %>%
  mutate(id_hh= as.integer(str_sub(RB030, 1, -3)))
hog18 <- left_join(d18,h18, by= c("DB030"="HB030"))
ecv18 <- left_join(per18, hog18, by=c("id_hh" = "DB030"))

rm(d18, p18, h18, r18, d06, p06, h06, r06, hog06, hog18, per06, per18)

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
         ingcapital= (HY090N+HY040N),
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
         alquiler = HH060,
         gastovivienda = HH070,
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
         region= case_when(DB040=="ES11" ~  11,
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
         region= case_when(DB040=="ES11" ~  11,
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




ecv06 <- ecv06 %>% 
  mutate(ingresos_anuales= ifelse(ingresos_anuales==0, NA, ingresos_anuales))

df06 <- ecv06[!(is.na(ecv06$ingresos_anuales)), ]


wtd.quantile(ecv06$ingresos_anuales, q=c(0.2, 0.4, 0.6, 0.8, 1), na.rm = T, weight = ecv06$pesos)



q06 <- df06 %>% 
  mutate(Q1= ifelse(ingresos_anuales<=7653.54, 1, 0),
         Q2= ifelse(ingresos_anuales <= 11018 & Q1==0, 1, 0),
         Q3= ifelse(ingresos_anuales <=14000 & Q1==0 & Q2==0, 1, 0),
         Q4= ifelse(ingresos_anuales <=19200 & Q1==0 & Q2==0 & Q3==0, 1, 0),
         Q5= ifelse(ingresos_anuales >=19200 & Q1==0 & Q2==0 & Q3==0 & Q4==0, 1, 0)) 



dfq06<- plyr::join_all(list(q06 %>% 
                      group_by(grupoedad) %>% 
                      summarise(Q1=weighted.mean(Q1, pesos)),
                    
                    q06 %>% 
                      group_by(grupoedad) %>% 
                      summarise(Q2=weighted.mean(Q2, pesos)),
                    
                    q06 %>% 
                      group_by(grupoedad) %>% 
                      summarise(Q3=weighted.mean(Q3, pesos)),
                    
                    q06 %>% 
                      group_by(grupoedad) %>% 
                      summarise(Q4=weighted.mean(Q4, pesos)),
                    
                    q06 %>% 
                      group_by(grupoedad) %>% 
                      summarise(Q5=weighted.mean(Q5, pesos))
), by='grupoedad', type='left')


dfq06<- dfq06 %>%
  pivot_longer(!grupoedad, names_to = "cuantil", values_to = "value")


 gg06<- 
   dfq06 %>% 
  ggplot(aes(x=cuantil, y=value, group=grupoedad, color=grupoedad)) + 
  geom_line(alpha=0.85)+
  geom_point(alpha=0.85)+
  scale_color_manual(values = c("#b67182", "#9bc9be", "#66979b", "#7b5c96"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
     annotate("text", x = 0.8, y = 0.29, label = "2006", family= "serif")+
   theme(legend.margin=margin(5,5,5,5),
         legend.box.margin=margin(-10,-10,-10,-10))

ggsave(plot = gg06, filename = "../Imágenes finales/cuantiles06.png", width = 4.5, height = 2.2,
       type="cairo") 

#2018
  
  ecv18 <- ecv18 %>% 
    mutate(ingresos_anuales= ifelse(ingresos_anuales==0, NA, ingresos_anuales))
  
  df18 <- ecv18[!(is.na(ecv18$ingresos_anuales)), ]
  
  
  wtd.quantile(ecv18$ingresos_anuales, q=c(0.2, 0.4, 0.6, 0.8, 1), na.rm = T, weight = ecv18$pesos)
  
  
  
  q18 <- df18 %>% 
    mutate(Q1= ifelse(ingresos_anuales<=5618.22, 1, 0),
           Q2= ifelse(ingresos_anuales <= 11628.84 & Q1==0, 1, 0),
           Q3= ifelse(ingresos_anuales <=16396.36 & Q1==0 & Q2==0, 1, 0),
           Q4= ifelse(ingresos_anuales <=23667.10 & Q1==0 & Q2==0 & Q3==0, 1, 0),
           Q5= ifelse(ingresos_anuales >=23667.10 & Q1==0 & Q2==0 & Q3==0 & Q4==0, 1, 0)) 
  

  
  dfq18<- plyr::join_all(list(q18 %>% 
                                group_by(grupoedad) %>% 
                                summarise(Q1=weighted.mean(Q1, pesos)),
                              
                              q18 %>% 
                                group_by(grupoedad) %>% 
                                summarise(Q2=weighted.mean(Q2, pesos)),
                              
                              q18 %>% 
                                group_by(grupoedad) %>% 
                                summarise(Q3=weighted.mean(Q3, pesos)),
                              
                              q18 %>% 
                                group_by(grupoedad) %>% 
                                summarise(Q4=weighted.mean(Q4, pesos)),
                              
                              q18 %>% 
                                group_by(grupoedad) %>% 
                                summarise(Q5=weighted.mean(Q5, pesos))
  ), by='grupoedad', type='left')
  
  
  dfq18<- dfq18 %>%
    pivot_longer(!grupoedad, names_to = "cuantil", values_to = "value")
  
  
 gg18<- 
   dfq18 %>% 
    ggplot(aes(x=cuantil, y=value, group=grupoedad, color=grupoedad)) + 
    geom_line(alpha=0.85)+
    geom_point(alpha=0.85)+
    scale_color_manual(values = c("#b67182", "#9bc9be", "#66979b", "#7b5c96"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
     annotate("text", x = 0.8, y = 0.34, label = "2018", family= "serif")+
   theme(legend.margin=margin(5,5,5,5),
         legend.box.margin=margin(-10,-10,-10,-10))

ggsave(plot = gg18, filename = "../Imágenes finales/cuantiles18.png", width = 4.5, height = 2.2 ,
       type="cairo")  


left_join(dfq06, dfq18, by=c("grupoedad", "cuantil")) %>% mutate(dif=value.y-value.x)



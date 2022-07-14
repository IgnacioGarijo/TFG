library(tidyverse)
library(cowplot)
library(gt)
library(gtExtras)

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/5º/Segundo semestre/TFG eco/Ficheros y bases de datos")

epa06 <- read.csv("EPA/datos_4t06/CSV/EPA_2006T4.csv", sep= "")
epa18 <- read.csv("EPA/datos_4t18/CSV/EPA_2018T4.csv", sep="")

europe <- readxl::read_excel("../Ficheros y bases de datos/une_rt_a_h.xls")

europe$unemployment<- europe$unemployment/100


epa06 <- epa06 %>% 
  mutate(grupoedad= case_when(EDAD5 <=20 ~ "<24",
                              EDAD5 %in% 25:30 ~ "25-34",
                              EDAD5 %in% 35:40 ~ "35-44",
                              EDAD5 %in% 45:50 ~ "45-54",
                              EDAD5 %in% 55:60 ~ "55-64",
                              EDAD5 >= 65 ~ "+65"),
         mayoresmenores= ifelse(EDAD5<36, "menores", "mayores"),
         emancipado = ifelse(NPADRE == 0 & NMADRE ==0, 1, 0),
         women= ifelse(SEXO1==6, "women", "hombre"),
         nivel_edu = case_when(NFORMA == "AN" ~ "Analfabeto", 
                               NFORMA %in% c("P1", "P2", "S1") ~ "Bajo", 
                               NFORMA %in% c("SG", "SP") ~ "Medio", 
                               NFORMA == "SU" ~ "Alto"),
         estudiando = ifelse(CURSNR %in% c(1,2) | CURSR %in% c(1,2), 1, 0),
         edad_edu = ifelse(estudiando==0 & EDADEST!=0, EDADEST, NA),
         empleo= ifelse(TRAREM ==1 | AUSENT ==1, 1, 0),
         indefinido= ifelse(DUCON1==1, 1, 0),
         jcompleta= ifelse(PARCO1==1,1,0),
         manolo= ifelse(indefinido==1 & jcompleta==1 , 1, 0),
         hpactadas= ifelse(HORASP== 9999, NA, HORASP/100),
         grupohpactadas= case_when(hpactadas<21 ~ "<21",
                                   hpactadas %in% 21:34 ~ "21-34",
                                   hpactadas %in% 35:45 ~  "35-45", 
                                   hpactadas > 45 ~ ">45"),
         hhab= ifelse(HORASH==9999, NA, HORASH/100),
         htrab=  ifelse(HORASE== 9999, NA, HORASP/100),
         horasdeseadas= ifelse(HORDES==99, NA, HORDES),
         diftrab= htrab-hpactadas,
         diftrab1= hhab-hpactadas,
         difdes= horasdeseadas- hpactadas,
         enp= ifelse(EXTNPG == 9999 | EXTNPG == 0000, NA, EXTNPG/100),
         trabamas = ifelse(MASHOR==1, 1, 0),
         trabasatis= ifelse(difdes==0, 1,0),
         activo= case_when(empleo==1 | BUSCA==1 ~ 1,
                           empleo==0 & BUSCA==6 ~ 0,
                           empleo==0 & is.na(BUSCA) ~ 0),
         id= case_when(CCAA < 18 ~CCAA-1,
                       CCAA ==51 ~17,
                       CCAA ==52 ~ 18)) 


epa18 <- epa18 %>% 
  mutate(grupoedad= case_when(EDAD5 <=20 ~ "<24",
                              EDAD5 %in% 25:30 ~ "25-34",
                              EDAD5 %in% 35:40 ~ "35-44",
                              EDAD5 %in% 45:50 ~ "45-54",
                              EDAD5 %in% 55:60 ~ "55-64",
                              EDAD5 >= 65 ~ "+65"),
         mayoresmenores= ifelse(EDAD5<36, "menores", "mayores"),
         emancipado = ifelse(NPADRE == 0 & NMADRE ==0, 1, 0),
         women= ifelse(SEXO1==6, "women", "hombre"),
         nivel_edu = case_when(NFORMA == "AN" ~ "Analfabeto", 
                               NFORMA %in% c("P1", "P2", "S1") ~ "Bajo", 
                               NFORMA %in% c("SG", "SP") ~ "Medio", 
                               NFORMA == "SU" ~ "Alto"),
         estudiando = ifelse(CURSNR %in% c(1,2) | CURSR %in% c(1,2), 1, 0),
         edad_edu = ifelse(estudiando==0 & EDADEST!=0, EDADEST, NA),
         empleo= ifelse(TRAREM ==1 | AUSENT ==1, 1, 0),
         indefinido= ifelse(DUCON1==1, 1, 0),
         jcompleta= ifelse(PARCO1==1,1,0),
         manolo= ifelse(indefinido==1 & jcompleta==1 , 1, 0),
         hpactadas= ifelse(HORASP== 9999, NA, HORASP/100),
         grupohpactadas= case_when(hpactadas<21 ~ "<21",
                                   hpactadas %in% 21:34 ~ "21-34",
                                   hpactadas %in% 35:45 ~  "35-45", 
                                   hpactadas > 45 ~ ">45"),
         hhab= ifelse(HORASH==9999, NA, HORASH/100),
         htrab=  ifelse(HORASE== 9999, NA, HORASP/100),
         horasdeseadas= ifelse(HORDES==99, NA, HORDES),
         diftrab= htrab-hpactadas,
         diftrab1= hhab-hpactadas,
         difdes= horasdeseadas- hpactadas,
         enp= ifelse(EXTNPG == 9999 | EXTNPG == 0000, NA, EXTNPG/100),
         trabamas = ifelse(MASHOR==1, 1, 0),
         trabasatis= ifelse(difdes==0, 1,0),
         activo= case_when(empleo==1 | BUSCA==1 ~ 1,
                           empleo==0 & BUSCA==6 ~ 0,
                           empleo==0 & is.na(BUSCA) ~ 0),
         id= case_when(CCAA < 18 ~CCAA-1,
                       CCAA ==51 ~17,
                       CCAA ==52 ~ 18)
  ) 

epa06$grupoedad <- factor(epa06$grupoedad, levels = c("<24", "25-34", "35-44", "45-54", "55-64", "+65"))
epa18$grupoedad <- factor(epa18$grupoedad, levels = c("<24", "25-34", "35-44", "45-54", "55-64", "+65"))
epa06$nivel_edu <- factor(epa06$nivel_edu, levels = c("Alto", "Medio", "Bajo"))
epa18$nivel_edu <- factor(epa18$nivel_edu, levels = c("Alto", "Medio", "Bajo"))
epa18$grupohpactadas <- factor(epa18$grupohpactadas, levels = c("<21", "21-34", "35-45", ">45"))
epa06$mayoresmenores <- factor(epa06$mayoresmenores, levels = c("menores", "mayores"))
epa18$mayoresmenores <- factor(epa18$mayoresmenores, levels = c("menores", "mayores"))

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



#Estudios
dfestu<- left_join(
epa06 %>% 
  filter(nivel_edu!="NA", grupoedad=="25-34") %>% 
  count(nivel_edu) %>% 
  transmute(nivel_edu=nivel_edu,
            n06=n,
            `2006`=n/sum(n)),
epa18 %>% 
  filter(nivel_edu!="NA", grupoedad=="25-34") %>% 
  count(nivel_edu) %>% 
  transmute(nivel_edu=nivel_edu,
            n18= n,
            `2018`=n/sum(n))) %>% 
  mutate(var=`2018`-`2006`)

dfestu %>% 
  ggplot(aes(x=nivel_edu, y=var, fill=nivel_edu))+
  geom_col()+
  guides(fill="none")


dfestu2<- rbind(epa06 %>% 
    filter(nivel_edu!="NA", grupoedad=="25-34") %>% 
    count(nivel_edu) %>% 
    mutate(año="2006",
           p=n/sum(n)),
  epa18 %>% 
    filter(nivel_edu!="NA", grupoedad=="25-34") %>% 
    count(nivel_edu) %>% 
    mutate(año="2018",
           p=n/sum(n)))

ggestu<- 
  dfestu2 %>% 
  ggplot(aes(x=año, y=p, fill=nivel_edu))+
  geom_col(alpha=0.87)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values = c("#b67182", "#9bc9be", "#66979b"))
  
ggsave(plot = ggestu, filename = "../Imágenes finales/ggestudios.png",
       type="cairo")

#Tasa de paro

  ##General 

rbind(epa18 %>% 
        drop_na(empleo) %>%
        group_by(grupoedad) %>% 
        summarise(año="2018", 
                  tparo=(1-weighted.mean(empleo, FACTOREL))*100),
      epa06 %>% 
        drop_na(empleo) %>% 
        group_by(grupoedad) %>% 
        summarise(año="2006", 
                  tparo=(1-weighted.mean(empleo, FACTOREL))*100)
) 
  ##Género

dfparo<- rbind(epa18 %>% 
  drop_na(empleo, activo) %>% 
  group_by(grupoedad, women) %>% 
  summarise(año="2018", 
            tparo=1-sum(empleo)/sum(activo)),
  epa06 %>% 
    drop_na(empleo, activo) %>% 
    group_by(grupoedad, women) %>% 
    summarise(año="2006", 
              tparo=1-sum(empleo)/sum(activo))
  )


dfparo$grupoedad <- factor(dfparo$grupoedad, levels = c("<24", "25-34", "35-44", "45-54", "55-64", "+65"))
 ggparo <- 
   dfparo %>% 
  pivot_wider(names_from = "año", values_from = "tparo") %>% 
  mutate(var=`2018`-`2006`) %>% 
  ggplot(aes(x=grupoedad, y=var, fill=women))+
  geom_col(position = "dodge", alpha=0.8)+
  scale_fill_manual(values = c("#497c7d","#b67182"), labels= c("Hombres", "Mujeres"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


ggparo2<- 
  dfparo %>% 
  ggplot(aes(x=grupoedad, y=tparo,
             group=interaction(año, women), 
             color=interaction(año, women)))+ 
  geom_line(aes(linetype=año), size=1.0005)+
  geom_point(shape=19)+
  scale_linetype_manual(values = c("2006" = "dashed", "2018" = "solid"))+
  scale_color_manual(values = c("#497c7d", "#9bc9be", "#b67182",  "#d0a5ae"),
                     labels= c("Hombres-06", "Hombres-18", "Mujeres-06", "Mujeres-18")
                     )+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  guides(linetype="none")

ggsave(plot = plot_grid(ggparo2, ggparo, labels = "auto"), filename = "../Imágenes finales/ggparo.jpg", width=9.5, type= "cairo")


  #Nivel educativo

dfparoedu<- rbind(epa18 %>% 
                 drop_na(empleo, activo) %>%
                   filter(nivel_edu!= "Analfabeto", grupoedad!= "+65") %>% 
                 group_by(grupoedad, nivel_edu) %>% 
                 summarise(año="2018", 
                           tparo=1-sum(empleo)/sum(activo)),
               epa06 %>% 
                 drop_na(empleo, activo) %>% 
                 filter(nivel_edu!= "Analfabeto", grupoedad!= "+65") %>% 
                 group_by(grupoedad, nivel_edu) %>% 
                 summarise(año="2006", 
                           tparo=1-sum(empleo)/sum(activo))
) 

  
ggparoedu<- 
  dfparoedu %>% 
  pivot_wider(values_from = tparo, names_from = año) %>% 
  mutate(var=`2018`-`2006`) %>% 
  ggplot(aes(xmin = as.numeric(factor(grupoedad)) - .45, xmax = as.numeric(factor(grupoedad)) + .45,
             ymin= `2006`, ymax=`2018`, group=nivel_edu, fill=nivel_edu))+
  geom_rect(position = "dodge", alpha=0.85)+
  scale_x_continuous(breaks= c(1:5), labels = c("<24", "25-34", "35-44", "45-54", "55-64"))+
  scale_fill_manual(values = c("#66979b", "#9bc9be", "#b67182"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
ggsave(plot = ggparoedu, filename = "../Imágenes finales/ggparoedu.png", type="cairo")



 #Horas pactadas (trabajan un 4.62% menos que en 2006)
left_join(
epa06 %>% 
  filter(empleo==1) %>% 
  drop_na(hpactadas) %>% 
  group_by(grupoedad) %>% 
  summarise(a06= weighted.mean(hpactadas, FACTOREL)),
epa18 %>% 
  filter(empleo==1) %>% 
  drop_na(hpactadas) %>% 
  group_by(grupoedad) %>% 
  summarise(a18= weighted.mean(hpactadas, FACTOREL)))%>%
  mutate(prop= a18/a06-1) #Los jóvenes son los que más horas pactadas han reducido


#Diferencia horas deseadas y horas pactadas 

left_join(
epa06 %>%   
  drop_na(difdes) %>% 
  group_by(mayoresmenores) %>% 
  summarise(a06= weighted.mean(difdes, FACTOREL)),

epa18 %>%   
  drop_na(difdes) %>% 
  group_by(mayoresmenores) %>% 
  summarise(a18= weighted.mean(difdes, FACTOREL ))
) %>%
  mutate(prop= a18/a06) #han aumentado considerablemente, 
                         #más de lo que han disminuido las horas trabajadas. 
                         #Antes querían trabajar 7 horas más, ahora 12

#Horas deseadas
left_join(epa06 %>% 
            drop_na(horasdeseadas) %>% 
            group_by(grupoedad) %>% 
            summarise(a06=weighted.mean(horasdeseadas, FACTOREL)),
          epa18 %>% 
            drop_na(horasdeseadas) %>% 
            group_by(grupoedad) %>% 
            summarise(a18=weighted.mean(horasdeseadas, FACTOREL))) %>% 
  mutate(a18/a06-1) #La media de horas deseadas decrece con el grupo de edad
                    #Esto se mantiene en 2018 pero cada vez menos

#Edad de dejar de estudiar
left_join(epa06 %>% 
            filter(mayoresmenores=="menores") %>% 
  drop_na(edad_edu) %>% 
  group_by(mayoresmenores, nivel_edu) %>% 
  summarise(a06=weighted.mean(edad_edu, FACTOREL)),
epa18 %>% 
  filter(mayoresmenores=="menores") %>% 
  drop_na(edad_edu) %>% 
  group_by(mayoresmenores, nivel_edu) %>% 
  summarise(a18=weighted.mean(edad_edu, FACTOREL))
)%>% 
  mutate(a18-a06) #Las personas de entre 24 y 35 años que ya han dejado de estudiar
                    #de media dejan de estudiar 1.43 años más tarde (18.4 a 19.4).
                    #Mirar las diferencias por nivel de educación


#Menciones en texto junto con la tabla

 ##Estudiando
dfestud<- left_join(
epa06 %>% 
  filter(grupoedad=="25-34") %>% 
  summarise(id=1, 
            `2006`=weighted.mean(estudiando, FACTOREL, na.rm=T)),
epa18 %>% 
  filter(grupoedad=="25-34") %>% 
  summarise(id=1, 
            `2018`=weighted.mean(estudiando, FACTOREL, na.rm=T))) %>% 
  mutate(Proporción= `2018`/`2006`,
         Cat= "...estar estudiando")

    ##Trabajar más horas 

dftrabamas <- left_join(epa06 %>% 
                          drop_na(trabamas) %>% 
                          filter(grupoedad=="25-34")%>% 
                          summarise(id=1, 
                                    `2006`=weighted.mean(trabamas, FACTOREL)),
                        epa18 %>% 
                          drop_na(trabamas) %>% 
                          filter(grupoedad=="25-34")%>% 
                          summarise(id=1, 
                                    `2018`=weighted.mean(trabamas, FACTOREL)))%>% 
  mutate(Proporción= `2018`/`2006`,
         Cat= "...querer trabajar más horas")


  ##Actividad
dfactivos <- left_join(epa06 %>% 
                         group_by(grupoedad) %>% 
                       summarise(id=1, 
                                   `2006`=weighted.mean(activo, FACTOREL, na.rm=T)),
                       epa18 %>% 
                         mutate(cosa=ifelse(EDAD5<26, "joven", "viejo")) %>% 
                         group_by(grupoedad) %>% 
                         summarise(id=1, 
                                   `2018`=weighted.mean(activo, FACTOREL, na.rm=T))) %>% 
  mutate(Proporción= `2018`/`2006`-1,
         Cat= "...pertenecer a la población activa")


#TABLA PROBABILIDADES

  ##Estudiando y trabajando (sobre el total de los que trabajan)

dfestutrab<- left_join(epa06 %>% 
  filter(grupoedad=="25-34", empleo== 1) %>% 
  summarise(id=1, 
            `2006`=1- weighted.mean(estudiando, FACTOREL, na.rm=T)),
epa18 %>% 
  filter(grupoedad=="25-34", empleo== 1) %>% 
  summarise(id=1, 
            `2018`=1 - weighted.mean(estudiando, FACTOREL, na.rm=T))) %>% 
  mutate(Proporción= `2018`/`2006`,
         Cat= "... no estar estudiando a la vez que trabajando")



 

  ##Estar satisfecho con las horas que trabajas

dftrabasatis <- left_join(epa06 %>% 
                          drop_na(trabasatis) %>% 
                          filter(grupoedad=="25-34")%>% 
                          summarise(id=1, 
                                    `2006`=weighted.mean(trabasatis, FACTOREL)),
                        epa18 %>% 
                          drop_na(trabasatis) %>% 
                          filter(grupoedad=="25-34")%>% 
                          summarise(id=1, 
                                    `2018`=weighted.mean(trabasatis, FACTOREL)))%>% 
  mutate(Proporción= `2018`/`2006`,
         Cat= "...estar satisfecho con las horas que trabajas")





 ##Indefinido



dfindefinido<- left_join(epa06 %>% 
            filter(grupoedad=="25-34") %>% 
            summarise(id=1, 
                      `2006`=weighted.mean(indefinido, FACTOREL, na.rm=T)),
          epa18 %>% 
            filter(grupoedad=="25-34") %>% 
            summarise(id=1, 
                      `2018`=weighted.mean(indefinido, FACTOREL, na.rm=T))) %>% 
  mutate(Proporción= `2018`/`2006`,
         Cat= "...tener un contrato indefinido") ##Sobre el total de activos


left_join(epa06 %>% 
            filter(grupoedad=="25-34", empleo==1) %>% 
            summarise(id=1, 
                      `2006`=weighted.mean(indefinido, FACTOREL, na.rm=T)),
          epa18 %>% 
            filter(grupoedad=="25-34", empleo==1) %>% 
            summarise(id=1, 
                      `2018`=weighted.mean(indefinido, FACTOREL, na.rm=T))) %>% 
  mutate(Proporción= `2018`/`2006`) #Sobre el total de trabajadores

    ##Jornada completa

dfjcompleta<- left_join(epa06 %>% 
            filter(grupoedad=="25-34") %>% 
            summarise(id=1, 
                      `2006`=weighted.mean(jcompleta, FACTOREL, na.rm=T)),
          epa18 %>% 
            filter(grupoedad=="25-34") %>% 
            summarise(id=1, 
                      `2018`=weighted.mean(jcompleta, FACTOREL, na.rm=T))) %>% 
  mutate(Proporción= `2018`/`2006`,
         Cat="...tener un contrato a jornada completa") ##Sobre el total de activos

left_join(epa06 %>% 
            filter(grupoedad=="25-34", empleo==1) %>% 
            summarise(id=1, 
                      `2006`=weighted.mean(indefinido, FACTOREL, na.rm=T)),
          epa18 %>% 
            filter(grupoedad=="25-34", empleo==1) %>% 
            summarise(id=1, 
                      `2018`=weighted.mean(indefinido, FACTOREL, na.rm=T))) %>% 
  mutate(Proporción= `2018`/`2006`) #Sobre el total de trabajadores


#Tanto la probabilidad de trabajar a jornada completa como de ser indefinido han aumentado para
#los que ya tienen trabajo

  ##Manolo

dfmanolo<- left_join(epa06 %>% 
            group_by(grupoedad) %>% 
            summarise(id=1, 
                      `2006`=weighted.mean(manolo, FACTOREL, na.rm=T)),
          epa18 %>% 
            group_by(grupoedad) %>% 
            summarise(id=1, 
                      `2018`=weighted.mean(manolo, FACTOREL, na.rm=T))) %>% 
  mutate(Proporción= `2018`-`2006`,
         Cat= "...tener un contrato indefinido a jornada completa") ##Sobre el total de activos

left_join(epa06 %>% 
            filter(grupoedad== "25-34", empleo==1) %>% 
            summarise(id=1, 
                      `2006`=weighted.mean(manolo, FACTOREL, na.rm=T)),
          epa18 %>% 
            filter(grupoedad== "25-34", empleo==1) %>% 
            summarise(id=1, 
                      `2018`=weighted.mean(manolo, FACTOREL, na.rm=T))) %>% 
  mutate(Proporción= `2018`/`2006`) #Sobre el total de trabajadores

##No haber hecho horas extra no pagadas

dfhenp<-left_join(
 epa06 %>%  
   filter(grupoedad=="25-34") %>% 
  mutate(henp= case_when(enp>0 ~1,
                         empleo==1 & enp<=0 ~ 0,
                         empleo==1 & is.na(enp)~ 0)) %>% 
  summarise(id=1,
            `2006`=1-weighted.mean(henp, FACTOREL, na.rm=T)),
 epa18 %>%  
   filter(grupoedad=="25-34") %>% 
   mutate(henp= case_when(enp>0 ~1,
                          empleo==1 & enp<=0 ~ 0,
                          empleo==1 & is.na(enp)~ 0)) %>% 
   summarise(id=1,
             `2018`=1-weighted.mean(henp, FACTOREL, na.rm=T)))%>% 
  mutate(Proporción= `2018`/`2006`,
         Cat= "...no haber hecho horas extras no pagadas la semana anterior")

 ##Tabla final

dftable<- rbind(dfestutrab,
                dftrabasatis,
                dfhenp,
                dfindefinido, 
                dfjcompleta, 
                dfmanolo
                ) %>% mutate(Diferencia=`2018`-`2006`) %>% 
  select(-id, -Proporción)

#tablaepa <-
  dftable %>% 
  gt(rowname_col = "Cat") %>% 
  gt_theme_nytimes() %>% 
  tab_header(
    title = "Probabilidad de...",
    subtitle = "Jóvenes entre 25 y 34 años"
  ) %>% 
  fmt_percent(
    columns = c(`2006`, `2018`, Diferencia),
    decimals = 2,
    dec_mark = ".",
    sep_mark = " "
  ) %>% 
  fmt_markdown(
    columns = Cat,
  ) %>% 
  gt_color_rows(`Diferencia`,
                palette = c("#FF6863", "#fbbf77","#90EE90" ), 
                domain = c(-0.1, 0.04),
                use_paletteer = F) %>% 
  tab_style(style = cell_fill(color = "#ff4c46"),
            locations = cells_body(
              columns = `Diferencia`,
              rows = `Diferencia`< -0.31
            )
  ) %>% 
  tab_options(table.font.names = "serif", 
              table.font.color = "black",
              footnotes.marks = c("*")) 

# MAPA
  
  sf_regional <- readOGR("ComunidadesAutonomas_ETRS89_30N/Comunidades_Autonomas_ETRS89_30N.shp")
  regional_df <- tidy(sf_regional)
  temp_df <- data.frame(sf_regional$Texto)
  temp_df$id <- as.character(seq(0,nrow(temp_df)-1))
  regional_df2 <- left_join(regional_df, temp_df, by="id")
  
  dfepamapa <- left_join( 
    epa06 %>% 
      filter(grupoedad== "25-34") %>%
      drop_na(empleo, activo) %>% 
      group_by(id) %>% 
      summarise(mean06= 1- sum(empleo)/sum(activo)),
    epa18 %>% 
      drop_na(empleo, activo) %>% 
      filter(grupoedad== "25-34") %>% 
      group_by(id) %>% 
      summarise(mean18= 1- sum(empleo)/sum(activo))
  ) %>% mutate(var= (mean18/mean06-1),
               var2= mean18-mean06,
               id= as.character(id))

  ggregionesepa<- dfepamapa %>% 
    mutate(region=case_when(id==0 ~ "AN",
                     id==1 ~ "AR",
                     id==2 ~ "AS",
                     id==3 ~ "IB",
                     id==4 ~ "CN",
                     id==5 ~ "CB",
                     id==6 ~ "CM",
                     id==7 ~ "CL",
                     id==8 ~ "CT",
                     id==9 ~ "VC",
                     id==10 ~ "EX",
                     id==11~ "GA",
                     id==12 ~ "MD",
                     id==13 ~ "NC",
                     id==14 ~ "PV",
                     id==15 ~ "MC",
                     id==16 ~ "RI",
                     id==17 ~ "CE",
                     id==18 ~ "ML")) %>% 
    filter(region !="CE", region!="ML") %>% 
    ggplot(aes(x=mean06, y=var2))+
    geom_smooth(se=F, method="lm", color= "#9bc9be")+
    geom_text(aes(label=region), color= "#497c7d", family="serif")+
    scale_y_continuous(name= "Variación tasa de paro",
                       labels = scales::percent_format(accuracy = 1))+
    scale_x_continuous(name= "Tasa de paro 2006",
                       labels = scales::percent_format(accuracy = 1))+
    theme_minimal()+
    theme(plot.background = element_rect(colour = "white"),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(family="serif"),
          panel.grid.major = element_blank(),
          text = element_text(family = "serif", colour = "black"),
          axis.text = element_text(colour = "black"),
          axis.line.x = element_line(colour = "lightgrey"),
          axis.ticks.x = element_line(colour = "lightgrey", lineend = "round"))
    
ggsave(plot = ggregionesepa, filename = "../Imágenes finales/ggregionesepa.png", width = 5, type="cairo")

  regional_plot <- regional_df2 %>% 
    left_join(dfepamapa, by="id")
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
  
  
  
  mapaepa06 <- 
    regional_plot%>%
    ggplot(aes(x=long_c, y = lat_c, group = group))+
    geom_polygon(aes(fill=mean06), color = "white", size = 0.3, alpha=0.9)+
    geom_path(data = canaries_line, aes(x=long, y = lat, group = NULL), color = "grey40")+
    scale_fill_gradient(low="#ffd89b", 
                        high = "#19547b",
                        labels= scales::percent_format(accuracy = 0.1L),
                        limits= c(0.03,0.279),
                        breaks= c(seq(0,0.25, 0.05))
                        # oob= scales::squish,
                      #  breaks= c(0.125, 0.15, 0.175,0.2,0.225,0.25),
                      #  guide= guide_legend(
                       #   direction= "horizontal",
                          # title.position = "top",
                          # title.hjust = 0.5,
                          # label.position = "bottom",
                          # label.hjust = 0.5)
    )+
    labs(title="2006")+
    theme_ari_maps()+
    theme(plot.background = element_rect(colour = "white"),
          panel.background =element_rect(colour = "white"), 
          legend.background = element_blank(),
          text = element_text(family = "serif"),
          panel.grid.major = element_blank(),
          legend.position = c(0.1,0.6), 
          plot.title = element_text(hjust = 0.2, size=12), 
          legend.title = element_blank(),
          legend.key.size = unit(0.35, "cm"))
  
  
  
 mapaepa18 <- 
  regional_plot%>%
    ggplot(aes(x=long_c, y = lat_c, group = group))+
    geom_polygon(aes(fill=mean18), color = "white", size = 0.3, alpha=0.9)+
    geom_path(data = canaries_line, aes(x=long, y = lat, group = NULL), color = "grey40")+
    scale_fill_gradient(low="#ffd89b", 
                        high = "#19547b",
                        labels= scales::percent_format(accuracy = 0.1L),
                       limits= c(0.03,0.279),
                       breaks= c(seq(0,0.25, 0.05))
                       # oob= scales::squish,
                    # breaks= c(0.125, 0.15, 0.175,0.2,0.225,0.25),
                      #   guide= guide_legend(
                      # #   direction= "horizontal",
                      #      title.position = "top",
                      #      title.hjust = 0.5,
                      #      label.position = "bottom",
                      #      label.hjust = 0.5)
  )+
     labs(title="2018")+
    theme_ari_maps()+
    theme(plot.background = element_rect(colour = "white"),
          panel.background =element_rect(colour = "white"), 
          legend.background = element_blank(),
          text = element_text(family = "serif"),
          panel.grid.major = element_blank(),
          legend.position = "",
          plot.title = element_text(hjust = 0.2, size=12), 
          legend.title = element_blank())

ggsave(plot=plot_grid(mapaepa06, mapaepa18), filename = "../Imágenes finales/mapaepa.png", width = 7, type="cairo") 


#desempleo juvenil europeo

ggeurope <- europe %>%  
  group_by(year) %>% 
  ggplot(aes(x=year, y=unemployment, group=country, color=country)) +
  geom_line(alpha=0.85)+
  geom_point(alpha=0.85)+
  scale_color_manual(values = c("#b67182", "#9bc9be", "#66979b", "#7b5c96", "pink", "#AA151B"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(axis.text.x = element_text(angle = -45),
        legend.margin=margin(3,3,3,3),
        legend.box.margin=margin(-10,-10,-10,-10))
  
ggsave(plot = ggeurope, filename = "../Imágenes finales/ggeurope.png", width = 4.5, height = 2.2,
       type="cairo") 

  #scale_color_manual(values = c("#003399", "#002654","#FFCC00",  "#008C45", "#046A38", "#AA151B"))


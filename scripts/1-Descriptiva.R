rm(list=ls())
if(!require(pacman)) install.packages("pacman")
library(psych)
install.packages("modelsummary")
library(modelsummary)
require(pacman)
require(stargazer)
library(psych)
install.packages("modelsummary")
library(modelsummary)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       caret) # Classification And REgression Training

#Se cargan los datos disponibles en dropbox (dl=0 a dl=1)
tr_p <- read.csv("https://www.dropbox.com/scl/fi/vwfhvj05zbjh88ym0ywrf/train_personas.csv?dl=1&rlkey=zl6jxjvbzji2aqeaxsuqhrc2i")
tr_h <- read.csv("https://www.dropbox.com/scl/fi/mujk9xw7rerfg8efq22b5/train_hogares.csv?rlkey=2lp8la11mvsfz3jufn9fe21jk&dl=1")
skim(tr_p)
skim(tr_h)
glimpse(tr_p)
glimpse(tr_h)

#Renombrar las variables

tr_h <- tr_h %>% 
	rename(Tipo_vivienda= P5090) %>% 
	rename(Nper_h= Nper) %>% 
	rename(Nper_unid_gasto= Npersug) %>% 
 	rename(Ing_unid_gasto= Ingtotugarr) %>% 
	rename(Ing_percap_h= Ingpcug) %>% 
	rename(linea_indig= Li) %>% 
	rename(linea_pobre= Lp)

tr_p <- tr_p %>% 
	rename(Estrato= Estrato1) %>% 
	rename(Sexo= P6020) %>% 
	rename(Edad= P6040) %>% 
 	rename(Reg_seg_social= P6100) %>% 
	rename(PET= Pet) %>% 
	rename(Ocupado= Oc) %>% 
	rename(Desocupado= Des) %>% 
	rename(Inactivo= Ina) %>% 
	rename(Ing_ayuda_hogares= Iof3h)%>% 
	rename(Ing_ayuda_inst= Iof3i)%>% 
	rename(Ing_total= Ingtot )

table(tr_p$Reg_seg_social)

cols1 <- c("Estrato","Sexo","Reg_seg_social","PET","Ocupado",
          "Desocupado","Inactivo")
tr_p[cols1] <- lapply(tr_p[cols1],factor)
cols2 <- c("Tipo_vivienda","linea_indig", "linea_pobre")
tr_h[cols2] <- lapply(tr_h[cols2],factor)

tr_p_df <- tr_p %>% 
  select(Estrato,Sexo,Edad,Reg_seg_social,PET,Ocupado,Desocupado,Inactivo,Ing_ayuda_hogares,Ing_ayuda_inst,Ing_total)

tr_h_df <- tr_h %>% 
  select(Tipo_vivienda, Nper_h,Nper_unid_gasto,Ing_unid_gasto,Ing_percap_h,linea_indig,linea_pobre)

skim(tr_p_df)
skim(tr_h_df)
glimpse(tr_p_df)
glimpse(tr_h_df)
stargazer(tr_h_df,type="text",out="stat.txt")
stargazer(tr_p_df,type="text",out="stat.txt")
datasummary_skim(tr_p_df)
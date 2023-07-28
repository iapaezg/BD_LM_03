rm(list=ls())
if(!require(pacman)) install.packages("pacman")
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       caret, # Classification And REgression Training
       psych,
       modelsummary,
       stargazer)

#Se cargan los datos disponibles en dropbox (dl=0 a dl=1)
tr_p <- read_csv("https://www.dropbox.com/scl/fi/vwfhvj05zbjh88ym0ywrf/train_personas.csv?dl=1&rlkey=zl6jxjvbzji2aqeaxsuqhrc2i")
tr_h <- read_csv("https://www.dropbox.com/scl/fi/mujk9xw7rerfg8efq22b5/train_hogares.csv?rlkey=2lp8la11mvsfz3jufn9fe21jk&dl=1")
ts_p <- read_csv("https://www.dropbox.com/scl/fi/wm9e5hbg3pmgygax85pot/test_personas.csv?rlkey=l7iyjjvm9xqddme9fc79dbahp&dl=1")
ts_h <- read_csv("https://www.dropbox.com/scl/fi/sbdk0akubx1vrb5d5es7m/test_hogares.csv?rlkey=oic9yniarxkt3sffu97ipquod&dl=1")
ls(tr_h)
ls(ts_p)

skim(tr_p)
skim(tr_h)
glimpse(tr_p)
glimpse(ts_p)
skim(ts_p)

P6020 SEXO
P6040 EDAD EDAD2
P6090 AFILIACIÓN SALUD (AFILIADO SI O NO) MISSIN DATA O 9=0

P6210 NIVEL EDUCATIVO MEDIANA PARA MISSING + 9
P6210s1 GRADO ESCOLAR AñOS DE EDUCACIÓN 
¿PREDICIR AñOS DE TRABAJO POTENCIAL?

P6430 RELACION LABORAL TRABAJO MEDIANA MISSING + 9

MIssinga y 9 se lleva a 2 2=no
6510 HORAS EXTRAS
6545 RECIBIO PRIMAS
6580 RECIBIO BONIFICAIONES
6581S1 SUB ALIMENTACION
6581S2 SUB TRANSPORTE
6581S3 SUB FAMILIAR
6581S4 SUB EDUCATIVO
6590 ALIMENTOS (ESPECIE)
6600 VIVIENDA
6620 OTROS ESPECIE
6630S1-4 PRIMAS SS
1-P SS
2-P NAVIDAD
3-P VACACIONES
4-VIATICOS
6-BON ANUAL

6800 HOAS DE TRABAJO MISSIN DATA=MEDIA

6920 COTIZA PENSIONES

7045 HORAS OTRO TRABAJO DATA=MEDIA

TOTALLIZAN LAS HORAS TRABAJADAS 6800+7045, LUEGO SE SACA LA MEDIA PARA NA

DESOCUPDOS
7350 OCUP ÚLTIMO TRABAJO
7422 INGRESOS TRABJO RECIBIO
7472 INGRESOS TRABAJO



7495 RECIBIO PAGO ARREIENDO PENSION
7500S2 PENSION INVALIDEZ
7500S3 PENSION PATERNIDAD DIVORCIO
7505 RECIBIO DINERO EXTERNO OTROS HOGARES
7510S1 AYUDA HOGARES NACIONALES
7510S2 AYUDA HOGARES INTERNACIONALES
7510S3 AYUDA INSTITUCIONES NACIONALES
7510S5 INTERES RENDIMIENTOS INVERSION
7510S6 INTERES RENDIMIENTOS CESANTIAS
7510S7 OTRAS FUENTES

REEMPLAZAR POR 2=NO

INA




P5000 Cuartos en el hogar
P5010 Dormitorios
P5090 Tipo de vivienda
P5140 Arriendo
Nper Personas por hogar
Npersug Personas por unidad de gasto
Li Línea indigencia
Lp Línea de pobreza ingresos de un hogar


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

#HASTA AQUI
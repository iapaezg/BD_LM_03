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

tr_p <- tr_p %>% mutate(sample = "train")
tr_h <- tr_h  %>% mutate(sample = "train")
ts_p <- ts_p  %>% mutate(sample = "test")
ts_h <- ts_h  %>% mutate(sample = "test")


#Renombrar las variables

tr_p_d <- tr_p %>% 
	rename(Sexo= P6020) %>% 
	rename(Edad= P6040) %>% 
	rename(Afiliado_salud= P6090) %>% 
 	rename(Nivel_edu= P6210) %>%
	rename(Anos_edu= P6210sl) %>%
	rename(Rel_lab= P6430) %>%
	rename(H_extra= P6510) %>% 
	rename(Prima= P6545) %>% 
	rename(Bonos= P6580) %>% 
	rename(S_alim= P6581s1) %>% 
	rename(S_trans= P6581s2) %>% 
	rename(S_fam= P6581s3) %>% 
	rename(S_edu= P6581s4) %>% 
	rename(Aliment= P6590) %>% 
	rename(Vivienda= P6600) %>% 
	rename(S_edu= P6581s4) %>% 
	rename(Otros_esp= P6620) %>% 
	rename(Primas_ss= P6630s1) %>% 
	rename(Navidad = P6630s2) %>% 
	rename(Vacas = P6630s3) %>% 
	rename(Viaticos = P6630s4) %>% 
	rename(Bon_anual = P6630s6) %>% 
	rename(H_trabajo = P6800) %>%
	rename(C_pension = P69200) %>%
	rename(OH_trabajo = P7045) %>%
	rename(Arriendo= P7495)%>% 
	rename(Pension_inv= P7500s2)%>% 
	rename(Pension_pat= P7500s3)%>% 
	rename(Ing_ayuda_hogares= P7505)%>%
	rename(ayuda_hog_na= P7510s1)%>%
 	rename(ayuda_hog_int= P7510s2)%>%
	rename(ayuda_inst= P7510s3)%>% 
	rename(int_inv= P7510s5)%>% 
	rename(int_cesantias= P7510s6)%>% 
	rename(otras_fuentes= P7510s7)

ls(tr_p_d)

tr_h <- tr_h %>% 
	rename(Cuartos= P5000) %>% 
	rename(Dormitorio= P5010) %>% 
	rename(Tipo_vivienda= P5090) %>% 
	rename(Arriendos= P5140) %>% 
	rename(Nper_h= Nper) %>% 
	rename(Nper_unid_gasto= Npersug) %>% 
 	rename(linea_indig= Li) %>% 
	rename(linea_pobre= Lp)


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



table(tr_p$Reg_seg_social)

cols1 <- c("Estrato","Sexo","Reg_seg_social","PET","Ocupado",
          "Desocupado","Inactivo")
tr_p[cols1] <- lapply(tr_p[cols1],factor)
cols2 <- c("Tipo_vivienda","linea_indig", "linea_pobre")
tr_h[cols2] <- lapply(tr_h[cols2],factor)


tr_p_df <- tr_p %>% 
  select(ID, Orden, Clase, Dominio,P6020,P6040, P6090, P6210, P6210s1,P6430,


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
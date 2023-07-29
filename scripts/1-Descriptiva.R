rm(list=ls())
if(!require(pacman)) install.packages("pacman")
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       caret, # Classification And REgression Training
       psych,
       modelsummary,
       stargazer,
       foreach)

#Se cargan los datos disponibles en dropbox (dl=0 a dl=1)
tr_p <- read_csv("https://www.dropbox.com/scl/fi/vwfhvj05zbjh88ym0ywrf/train_personas.csv?dl=1&rlkey=zl6jxjvbzji2aqeaxsuqhrc2i")
tr_h <- read_csv("https://www.dropbox.com/scl/fi/mujk9xw7rerfg8efq22b5/train_hogares.csv?rlkey=2lp8la11mvsfz3jufn9fe21jk&dl=1")
ts_p <- read_csv("https://www.dropbox.com/scl/fi/wm9e5hbg3pmgygax85pot/test_personas.csv?rlkey=l7iyjjvm9xqddme9fc79dbahp&dl=1")
ts_h <- read_csv("https://www.dropbox.com/scl/fi/sbdk0akubx1vrb5d5es7m/test_hogares.csv?rlkey=oic9yniarxkt3sffu97ipquod&dl=1")

skim(tr_p)
skim(tr_h)
glimpse(tr_p)
glimpse(ts_p)
skim(ts_p)

tr_p <- tr_p %>% mutate(sample = "train")
tr_h <- tr_h  %>% mutate(sample = "train")
ts_p <- ts_p  %>% mutate(sample = "test")
ts_h <- ts_h  %>% mutate(sample = "test")

str(tr_p)

#Renombrar las variables
trp_m <- tr_p %>% 
  rename(sexo= P6020) %>% 
  rename(edad= P6040) %>% 
  rename(a_salud= P6090) %>% 
  rename(nivel_edu= P6210) %>%
  rename(anos_edu= P6210s1) %>%
  rename(rel_lab= P6430) %>%
  rename(h_extra= P6510) %>% 
  rename(prima= P6545) %>% 
  rename(bonos= P6580) %>% 
  rename(s_alim= P6585s1) %>% 
  rename(s_trans= P6585s2) %>% 
  rename(s_fam= P6585s3) %>% 
  rename(s_edu= P6585s4) %>% 
  rename(sal_alim= P6590) %>% 
  rename(sal_viv= P6600) %>% 
  rename(otros_esp= P6620) %>% 
  rename(prima_ss= P6630s1) %>% 
  rename(prima_nav= P6630s2) %>% 
  rename(prima_vac = P6630s3) %>% 
  rename(viaticos= P6630s4) %>% 
  rename(bon_anual = P6630s6) %>% 
  rename(h_tra1 = P6800) %>%
  rename(a_pension = P6920) %>%
  rename(h_tra2 = P7045) %>%
  rename(arriendo= P7495) %>% 
  rename(inv_pension= P7500s2) %>% 
  rename(pat_pension= P7500s3) %>% 
  rename(hog_na= P7510s1) %>%
  rename(hog_int= P7510s2) %>%
  rename(ayuda_inst= P7510s3) %>% 
  rename(int_inv= P7510s5) %>% 
  rename(int_cesantias= P7510s6) %>% 
  rename(otras_fuentes= P7510s7) %>% 
  rename(reg_salud=P6100) %>% 
  rename(q_hizo=P6240) %>% 
  rename(ing_des=P7422)

tsp_m <- ts_p %>% 
  rename(sexo= P6020) %>% 
  rename(edad= P6040) %>% 
  rename(a_salud= P6090) %>% 
  rename(nivel_edu= P6210) %>%
  rename(anos_edu= P6210s1) %>%
  rename(rel_lab= P6430) %>%
  rename(h_extra= P6510) %>% 
  rename(prima= P6545) %>% 
  rename(bonos= P6580) %>% 
  rename(s_alim= P6585s1) %>% 
  rename(s_trans= P6585s2) %>% 
  rename(s_fam= P6585s3) %>% 
  rename(s_edu= P6585s4) %>% 
  rename(sal_alim= P6590) %>% 
  rename(sal_viv= P6600) %>% 
  rename(otros_esp= P6620) %>% 
  rename(prima_ss= P6630s1) %>% 
  rename(prima_nav= P6630s2) %>% 
  rename(prima_vac = P6630s3) %>% 
  rename(viaticos= P6630s4) %>% 
  rename(bon_anual = P6630s6) %>% 
  rename(h_tra1 = P6800) %>%
  rename(a_pension = P6920) %>%
  rename(h_tra2 = P7045) %>%
  rename(arriendo= P7495) %>% 
  rename(inv_pension= P7500s2) %>% 
  rename(pat_pension= P7500s3) %>% 
  rename(hog_na= P7510s1) %>%
  rename(hog_int= P7510s2) %>%
  rename(ayuda_inst= P7510s3) %>% 
  rename(int_inv= P7510s5) %>% 
  rename(int_cesantias= P7510s6) %>% 
  rename(otras_fuentes= P7510s7) %>% 
  rename(reg_salud=P6100) %>% 
  rename(q_hizo=P6240) %>% 
  rename(ing_des=P7422)

str(tsp_m)

trp_m <- trp_m %>%
  select(id, Orden, Clase, Dominio, sexo, edad, a_salud, reg_salud, nivel_edu, anos_edu,
         q_hizo, rel_lab, h_extra, prima, bonos, s_alim, s_trans, s_fam, s_edu,
         sal_alim, sal_viv, otros_esp, prima_ss, prima_nav, prima_vac, viaticos, bon_anual,
         h_tra1, a_pension, h_tra2, ing_des, arriendo, inv_pension, pat_pension, hog_na,
         hog_int, ayuda_inst, int_inv, int_cesantias, otras_fuentes, Oc, Des, Ina)
  
tsp_m <- tsp_m %>%
  select(id, Orden, Clase, Dominio, sexo, edad, a_salud, reg_salud, nivel_edu, anos_edu,
         q_hizo, rel_lab, h_extra, prima, bonos, s_alim, s_trans, s_fam, s_edu,
         sal_alim, sal_viv, otros_esp, prima_ss, prima_nav, prima_vac, viaticos, bon_anual,
         h_tra1, a_pension, h_tra2, ing_des, arriendo, inv_pension, pat_pension, hog_na,
         hog_int, ayuda_inst, int_inv, int_cesantias, otras_fuentes, Oc, Des, Ina)

bd_p <- rbind(trp_m,tsp_m)
table(bd_p$reg_salud)
table(bd_p$nivel_edu)
median(bd_p$anos_edu[which(!is.na(bd_p$anos_edu))])
table(bd_p$q_hizo)
table(bd_p$rel_lab,bd_p$Oc)
# Tratamiento missing data
skim(bd_p)
bd_p <- bd_p %>% 
  mutate(reg_salud=ifelse(is.na(reg_salud)|reg_salud==9,3,reg_salud)) %>% # Moda=3
  mutate(nivel_edu=ifelse(is.na(nivel_edu)|nivel_edu==9,3,nivel_edu)) %>% # Moda=3
  mutate(anos_edu=ifelse(is.na(anos_edu)|anos_edu==99,median(bd_p$anos_edu[which(!is.na(bd_p$anos_edu))]),anos_edu)) %>% # Mediana
  mutate(q_hizo=ifelse(is.na(q_hizo),1,q_hizo)) %>% # Moda=1
  mutate(rel_lab=ifelse(is.na(rel_lab),9,rel_lab)) %>% # Se escoge 9=Otro para los missing
  mutate(h_tra2=ifelse(is.na(h_tra2)|h_tra2==98|h_tra2==99,0,h_tra2)) %>%  # 98 99 NA = 0
  mutate(h_tra1=ifelse(is.na(h_tra1),0,h_tra1)) %>% # NA=0
  mutate(h_trat=h_tra1+h_tra2) %>% 
  mutate(Oc=ifelse(is.na(Oc),0,Oc)) %>% 
  mutate(Des=ifelse(is.na(Des),0,Des)) %>% 
  mutate(Ina=ifelse(is.na(Ina),0,Ina)) %>% 
  mutate(a_salud=ifelse(is.na(a_salud)|a_salud==9|a_salud==2,0,a_salud)) %>% 
  mutate(h_extra=ifelse(is.na(h_extra)|h_extra==9|h_extra==2,0,h_extra)) %>% 
  mutate(prima=ifelse(is.na(prima)|prima==9|prima==2,0,prima)) %>% 
  mutate(bonos=ifelse(is.na(bonos)|bonos==9|bonos==2,0,bonos)) %>% 
  mutate(s_alim=ifelse(is.na(s_alim)|s_alim==9|s_alim==2,0,s_alim)) %>% 
  mutate(s_trans=ifelse(is.na(s_trans)|s_trans==9|s_trans==2,0,s_trans)) %>% 
  mutate(s_fam=ifelse(is.na(s_fam)|s_fam==9|s_fam==2,0,s_fam)) %>% 
  mutate(s_edu=ifelse(is.na(s_edu)|s_edu==9|s_edu==2,0,s_edu)) %>% 
  mutate(sal_alim=ifelse(is.na(sal_alim)|sal_alim==9|sal_alim==2,0,sal_alim)) %>% 
  mutate(sal_viv=ifelse(is.na(sal_viv)|sal_viv==9|sal_viv==2,0,sal_viv)) %>% 
  mutate(otros_esp=ifelse(is.na(otros_esp)|otros_esp==9|otros_esp==2,0,otros_esp)) %>% 
  mutate(prima_ss=ifelse(is.na(prima_ss)|prima_ss==9|prima_ss==2,0,prima_ss)) %>% 
  mutate(prima_nav=ifelse(is.na(prima_nav)|prima_nav==9|prima_nav==2,0,prima_nav)) %>% 
  mutate(prima_vac=ifelse(is.na(prima_vac)|prima_vac==9|prima_vac==2,0,prima_vac)) %>% 
  mutate(viaticos=ifelse(is.na(viaticos)|viaticos==9|viaticos==2,0,viaticos)) %>% 
  mutate(bon_anual=ifelse(is.na(bon_anual)|bon_anual==9|bon_anual==2,0,bon_anual)) %>% 
  mutate(a_pension=ifelse(is.na(a_pension)|a_pension==9|a_pension==2,0,a_pension)) %>% 
  mutate(ing_des=ifelse(is.na(ing_des)|ing_des==9|ing_des==2,0,ing_des)) %>% 
  mutate(arriendo=ifelse(is.na(arriendo)|arriendo==9|arriendo==2,0,arriendo)) %>% 
  mutate(inv_pension=ifelse(is.na(inv_pension)|inv_pension==9|inv_pension==2,0,inv_pension)) %>% 
  mutate(pat_pension=ifelse(is.na(pat_pension)|pat_pension==9|pat_pension==2,0,pat_pension)) %>% 
  mutate(hog_na=ifelse(is.na(hog_na)|hog_na==9|hog_na==2,0,hog_na)) %>% 
  mutate(hog_int=ifelse(is.na(hog_int)|hog_int==9|hog_int==2,0,hog_int)) %>% 
  mutate(ayuda_inst=ifelse(is.na(ayuda_inst)|ayuda_inst==9|ayuda_inst==2,0,ayuda_inst)) %>% 
  mutate(int_inv=ifelse(is.na(int_inv)|int_inv==9|int_inv==2,0,int_inv)) %>% 
  mutate(int_cesantias=ifelse(is.na(int_cesantias)|int_cesantias==9|int_cesantias==2,0,int_cesantias)) %>% 
  mutate(otras_fuentes=ifelse(is.na(otras_fuentes)|otras_fuentes==9|otras_fuentes==2,0,otras_fuentes)) %>% 
  mutate(edad2=edad^2)
  

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
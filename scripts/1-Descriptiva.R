rm(list=ls())
if(!require(pacman)) install.packages("pacman")
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       caret, # Classification And REgression Training
       psych,
       modelsummary,
       stargazer,
       foreach,
       ggplot2)


# Se cargan los datos disponibles en dropbox (dl=0 a dl=1) ----
tr_p <- read_csv("https://www.dropbox.com/scl/fi/vwfhvj05zbjh88ym0ywrf/train_personas.csv?dl=1&rlkey=zl6jxjvbzji2aqeaxsuqhrc2i")
tr_h <- read_csv("https://www.dropbox.com/scl/fi/mujk9xw7rerfg8efq22b5/train_hogares.csv?rlkey=2lp8la11mvsfz3jufn9fe21jk&dl=1")
ts_p <- read_csv("https://www.dropbox.com/scl/fi/wm9e5hbg3pmgygax85pot/test_personas.csv?rlkey=l7iyjjvm9xqddme9fc79dbahp&dl=1")
ts_h <- read_csv("https://www.dropbox.com/scl/fi/sbdk0akubx1vrb5d5es7m/test_hogares.csv?rlkey=oic9yniarxkt3sffu97ipquod&dl=1")

skim(tr_p)
skim(tr_h)
glimpse(tr_p)
glimpse(ts_p)
skim(ts_p)

# Se crea la variable sample ----
tr_p <- tr_p %>% mutate(sample = "train")
tr_h <- tr_h  %>% mutate(sample = "train")
ts_p <- ts_p  %>% mutate(sample = "test")
ts_h <- ts_h  %>% mutate(sample = "test")

str(tr_p)

# Tratamiento personas ------
## Renombrar las variables -----
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

## Seleccionamos variables de interes ----
trp_m <- trp_m %>%
  select(id, Orden, Clase, Dominio, sexo, edad, a_salud, reg_salud, nivel_edu, anos_edu,
         q_hizo, rel_lab, h_extra, prima, bonos, s_alim, s_trans, s_fam, s_edu,
         sal_alim, sal_viv, otros_esp, prima_ss, prima_nav, prima_vac, viaticos, bon_anual,
         h_tra1, a_pension, h_tra2, ing_des, arriendo, inv_pension, pat_pension, hog_na,
         hog_int, ayuda_inst, int_inv, int_cesantias, otras_fuentes, Oc, Des, Ina, sample)
  
tsp_m <- tsp_m %>%
  select(id, Orden, Clase, Dominio, sexo, edad, a_salud, reg_salud, nivel_edu, anos_edu,
         q_hizo, rel_lab, h_extra, prima, bonos, s_alim, s_trans, s_fam, s_edu,
         sal_alim, sal_viv, otros_esp, prima_ss, prima_nav, prima_vac, viaticos, bon_anual,
         h_tra1, a_pension, h_tra2, ing_des, arriendo, inv_pension, pat_pension, hog_na,
         hog_int, ayuda_inst, int_inv, int_cesantias, otras_fuentes, Oc, Des, Ina, sample)

bd_p <- rbind(trp_m,tsp_m)
table(bd_p$reg_salud)
table(bd_p$nivel_edu)
median(bd_p$anos_edu[which(!is.na(bd_p$anos_edu))])
table(bd_p$q_hizo)
table(bd_p$rel_lab,bd_p$Oc)

## Tratamiento missing data ----
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
  mutate(a_pension=ifelse(is.na(a_pension)|a_pension==9,0,a_pension)) %>% 
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
  mutate(edad2=edad^2) %>% 
  mutate(exp=edad-anos_edu-5) %>%
  mutate(exp=ifelse(exp<0,0,exp)) %>% 
  mutate(exp2=exp^2) %>% 
  mutate(Clase=ifelse(Clase==2,0,Clase)) %>% # 1: Urbano 0_2 Rural
  mutate(sexo=ifelse(sexo==2,0,sexo)) # 1:Hombre 0:Mujer
  
glimpse(bd_p)
skim(bd_p)
datasummary_skim(bd_p)

## Convertir variables categóricas en factores ----
f_des <- c("reg_salud","nivel_edu","q_hizo","rel_lab","a_pension")
bd_p[f_des] <- lapply(bd_p[f_des],factor)

ggplot(bd_p,aes(colour=sample,x=reg_salud)) +
  geom_bar(fill="grey") + 
  labs(x="Régimen de salud",y="Personas")+
  facet_wrap(~sample)

ggplot(bd_p,aes(colour=sample,x=nivel_edu)) +
  geom_bar(fill="grey") + 
  labs(x="Nivel educativo",y="Personas")+
  facet_wrap(~sample)

ggplot(bd_p,aes(colour=sample,x=rel_lab)) +
  geom_bar(fill="grey") + 
  labs(x="Relación laboral",y="Personas")+
  facet_wrap(~sample)

ggplot(bd_p,aes(colour=sample,x=q_hizo)) +
  geom_bar(fill="grey") + 
  labs(x="¿Qué hizo la semana anterior?",y="Personas")+
  facet_wrap(~sample)

ggplot(bd_p,aes(colour=sample,x=a_pension)) +
  geom_bar(fill="grey") + 
  labs(x="Tipo afiliación a pensión",y="Personas")+
  facet_wrap(~sample)

des_var <- bd_p %>% 
  dplyr::group_by(sample) %>% 
  skim()
write_csv(des_var,file="../views/descriptiva.csv")

skim(bd_p)

# Tratamiento hogares ----

## Renombrar variables ----
tr_hm <- tr_h %>% 
	rename(cuartos= P5000) %>% 
	rename(dormitorio= P5010) %>% 
	rename(tipo_vivienda= P5090)

ts_hm <- ts_h %>% 
  rename(cuartos= P5000) %>% 
  rename(dormitorio= P5010) %>% 
  rename(tipo_vivienda= P5090) 
  
ts_hm <- ts_hm %>% mutate(Pobre = (""))

str(ts_hm)

## Selección de variables de interes ----
tr_hm <- tr_hm %>%
  select(id, Clase, Dominio, cuartos, dormitorio, tipo_vivienda, 
         Nper, Npersug, Li, Lp, Pobre, sample)
ts_hm <- ts_hm %>%
  select(id, Clase, Dominio, cuartos, dormitorio, tipo_vivienda, 
         Nper, Npersug, Li, Lp,Pobre, sample)

bd_h <- rbind(tr_hm,ts_hm) # Unen bases de hogares

skim(bd_h)

## Tratamiento missing data ----

table(bd_h$cuartos)
table(bd_h$dormitorio)
table(bd_h$Npersug)
table(bd_h$Clase)

glimpse(bd_h)
bd_h <- bd_h %>% 
  mutate(cuartos=ifelse(is.na(cuartos)|cuartos==98,3,cuartos)) %>% # Moda=3 
  mutate(cuartos=ifelse(is.na(cuartos)|cuartos==43,3,cuartos))%>%  # Moda=3
  mutate(Clase=ifelse(Clase==2,0,Clase)) #0 es rural 1 es urbano

## Convertir variables categóricas en factores ----
  
cols2 <- c("Clase","tipo_vivienda", "Pobre")
bd_h[cols2] <- lapply(bd_h[cols2],factor)


## Estadisticas descriptivas de los hogares ----

skim(bd_h)
glimpse(bd_h)
datasummary_skim(bd_h)

ggplot(bd_h,aes(colour=sample,x=Dominio)) +
  geom_bar(fill="grey") + 
  labs(x="Ciudades (train/test)",y="Cantidad")+
  facet_wrap(~sample)+ 
  scale_x_discrete(guide = guide_axis(angle = 90))

ggplot(bd_h,aes(colour=sample,x=cuartos)) +
  geom_bar(fill="grey") + 
  labs(x="Número de cuartos (train/test)",y="Cantidad")+
  facet_wrap(~sample)

ggplot(bd_h,aes(colour=sample,x=dormitorio)) +
  geom_bar(fill="grey") + 
  labs(x="Número de dormitorios (train/test)",y="Cantidad")+
  facet_wrap(~sample)

## TIPO DE VIVIENDA. 1. Propia, totalmente pagada. 2. Propia, la están pagando
# 3. En arriendo o subarriendo. 4. En usufructo. 5. En posesión sin titulo. 6. Otra

ggplot(bd_h,aes(colour=sample,x=tipo_vivienda)) +
  geom_bar(fill="grey") + 
  labs(x="Tipo de vivienda (train/test)",y="Cantidad")+
  facet_wrap(~sample)
table(bd_h$tipo_vivienda)

ggplot(bd_h,aes(colour=sample,x=Nper)) +
  geom_bar(fill="grey") + 
  labs(x="Número personas por hogar (train/test)",y="Cantidad")+
  facet_wrap(~sample)
table(bd_h$Nper)

ggplot(bd_h,aes(colour=sample,x=Npersug)) +
  geom_bar(fill="grey") + 
  labs(x="Número personas por unidad de gasto (train/test)",y="Cantidad")+
  facet_wrap(~sample)

## Hogar pobre en la muestra de train. 1 = Pobre 0 = No pobre ----
table(tr_h$Pobre)

# Clasificación ------
set.seed(2023)
p_load(caret)

# se definen tres grupos de muestras
bd_h <- bd_h %>% 
  mutate(p_cuarto=Nper/cuartos)
db_trainh <- filter(bd_h,sample=="train")
db_testh <- filter(bd_h,sample=="test")
db_trainh$Pobre <- factor(db_trainh$Pobre,levels = c(0,1),
                          labels=c("no","si"))
levels(db_trainh$Pobre)
split1 <-createDataPartition(db_trainh$Pobre,p=0.7)[[1]]
length(split1)
training <- db_trainh[split1,]
other <- db_trainh[-split1,]
split2 <- createDataPartition(other$Pobre,p=1/3)[[1]]
evaluation <- other[split2,]
testing <- other[-split2,]
dim(training)
dim(testing)
dim(evaluation)

# Validar particiones
prop.table(table(db_trainh$Pobre))
prop.table(table(training$Pobre))
prop.table(table(testing$Pobre))
prop.table(table(evaluation$Pobre))

predict <- stats::predict

M1 <- as.formula("Pobre ~ Dominio + cuartos + Nper + tipo_vivienda")
M2 <- as.formula("Pobre ~ Dominio + p_cuarto + tipo_vivienda")

ffcv<-function(...)c(twoClassSummary(...), defaultSummary(...))
Control <- trainControl(method = "cv",
                        number = 5,
                        summaryFunction = ffcv,
                        classProbs = TRUE,
                        verbose=FALSE,
                        savePredictions = T)

# Modelo 1 - Logit
logitM1 <- train(
  M1,
  data = training,
  method = "glm",
  trControl = Control,
  family = "binomial",
  preProcess = c("center", "scale")
)

logitM1

# Modelo 2 - Logit
logitM2 <- train(
  M2,
  data = training,
  method = "glm",
  trControl = Control,
  family = "binomial",
  preProcess = c("center", "scale")
)

logitM2

# Lasso M1
grid <- 10^seq(-4, 0.01, length = 200)
lasso1 <- train(
  M1,
  data = training,
  method = "glmnet",
  trControl = Control,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=grid),
  preProcess = c("center", "scale")
)
lasso1

# Lasso M2
grid <- 10^seq(-4, 0.01, length = 200)
lasso2 <- train(
  M2,
  data = training,
  method = "glmnet",
  trControl = Control,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=grid),
  preProcess = c("center", "scale")
)
lasso2

# Lasso-ROC M1
lasso_roc1 <- train(
  M1, 
  data = training, 
  method = "glmnet",
  trControl = Control,
  family = "binomial", 
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=grid), 
  preProcess = c("center", "scale")
)
lasso_roc1

# Lasso-ROC M2
lasso_roc2 <- train(
  M2, 
  data = training, 
  method = "glmnet",
  trControl = Control,
  family = "binomial", 
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=grid), 
  preProcess = c("center", "scale")
)
lasso_roc2

# Puntos de corte para ROC
p_load(pROC)
resultados <- data.frame(Pobre=evaluation$Pobre)

# Para M1
resultados$R1 <- predict(lasso_roc1,
                         newdata=evaluation,
                         type="prob")[,1]
head(resultados)
# Roc para M1
rf_ROC1 <- roc(resultados$Pobre, resultados$R1, levels = rev(levels(resultados$Pobre)))
rf_ROC1

#Se calcula punto de corte
rf_Thresh1 <- coords(rf_ROC1, x = "best", best.method = "closest.topleft")
rf_Thresh1

#Se evalúan resultados
resultados<-resultados %>% mutate(lasso1hat05=ifelse(resultados$R1>0.5,"Si","No"),
                                    lasso1rf_Thresh=ifelse(resultados$R1>rf_Thresh1$threshold,"Si","No"))

# Caso en el que el threshold es = 0.5 (Bayes)
with(resultados,table(Pobre,lasso1hat05))

# Caso en el que el threshold se obtiene del ROC
with(resultados,table(Pobre,lasso1rf_Thresh))

# Para M2
resultados$R2 <- predict(lasso_roc2,
                         newdata=evaluation,
                         type="prob")[,1]
head(resultados)

# Roc para M2
rf_ROC2 <- roc(resultados$Pobre, resultados$R2, levels = rev(levels(resultados$Pobre)))
rf_ROC2

#Se calcula punto de corte
rf_Thresh2 <- coords(rf_ROC2, x = "best", best.method = "closest.topleft")
rf_Thresh2

#Se evalúan resultados
resultados<-resultados %>% mutate(lasso2hat05=ifelse(resultados$R2>0.5,"Si","No"),
                                  lasso2rf_Thresh=ifelse(resultados$R2>rf_Thresh2$threshold,"Si","No"))

# Caso en el que el threshold es = 0.5 (Bayes)
with(resultados,table(Pobre,lasso2hat05))

# Caso en el que el threshold se obtiene del ROC
with(resultados,table(Pobre,lasso2rf_Thresh))


# Elastic Net

#M1

elasticnet1 <- train(
  M1,
  data = training,
  method = "glmnet",
  trControl = Control,
  family = "binomial",
  metric = "Acc",
  preProcess = c("center", "scale")
)

elasticnet1

# M2

elasticnet2<- train(
  M2,
  data = training,
  method = "glmnet",
  trControl = Control,
  family = "binomial",
  metric = "Sens",
  preProcess = c("center", "scale")
)

elasticnet2

# Descriptiva de resultados M1 y M2, metodología (dentro del train)
testR <- data.frame(Pobre=testing$Pobre)
testR

testR$logitM1 <- predict(logitM1,
                         newdata = testing,
                         type= "prob")[,1]
testR$lasso1 <- predict(lasso1,
                         newdata = testing,
                         type= "prob")[,1]
testR$lasso_roc1 <- predict(lasso_roc1,
                         newdata = testing,
                         type= "prob")[,1]

testR$elasticnet1 <- predict(elasticnet1,
                                   newdata = testing,
                                   type = "prob")[,1]

testR <- testR %>% 
  mutate(
    logitM1=ifelse(logitM1>0.5,"si","no"),
    lasso1=ifelse(lasso1>0.5,"si","no"),
    lasso_roc1=ifelse(lasso_roc1>rf_Thresh1$threshold,"si","no")
    elasticnet1=ifelse(elasticnet1>0.5,"Si","No")
  )


with(testR,table(Pobre,logitM1))
with(testR,table(Pobre,lasso1))
with(testR,table(Pobre,lasso_roc1))
with(testR,table(Pobre,elasticnet1))


testR$logitM2 <- predict(logitM2,
                         newdata = testing,
                         type= "prob")[,1]
testR$lasso2 <- predict(lasso2,
                        newdata = testing,
                        type= "prob")[,1]
testR$lasso_roc2 <- predict(lasso_roc2,
                            newdata = testing,
                            type= "prob")[,1]

testR$elasticnet2 <- predict(elasticnet2,
                                   newdata = testing,
                                   type = "prob")[,1]
testR <- testR %>% 
  mutate(
    logitM2=ifelse(logitM2>0.5,"si","no"),
    lasso2=ifelse(lasso2>0.5,"si","no"),
    lasso_roc2=ifelse(lasso_roc2>rf_Thresh2$threshold,"si","no")
    elasticnet2=ifelse(elasticnet2>0.5,"Si","No")
  )

with(testR,table(Pobre,logitM2))
with(testR,table(Pobre,lasso2))
with(testR,table(Pobre,lasso_roc2))
with(testR,table(Pobre,elasticnet2))

logitM1
logitM2
lasso1
lasso2
lasso_roc1
lasso_roc2
elasticnet1
elasticnet2

# Evaluando en el test real
data <- db_testh


# Exportar resultados
d_submit <- db_testh
d_submit$predict <- predict(logitM1,d_submit,type = "prob")[,1]
submit <- d_submit %>% 
  mutate(Pobre=ifelse(predict>0.5,1,0)) %>% 
  select(id,Pobre)
prop.table(table(submit$Pobre))
write.csv(submit,file = "../stores/i1.csv",row.names = FALSE)
  





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

library(haven)
M3 <- read_sav("~/1 - ECONOMETRIA/MOD 3/737-Modulo03/Enaho01A-2020-300.sav")
M5 <- read_sav("~/1 - ECONOMETRIA/MOD 5/737-Modulo05/737-Modulo05/Enaho01A-2020-500.sav")

install.packages("dplyr")
library(dplyr)
M3Educ<-select(M3,CONGLOME, VIVIENDA, HOGAR, CODPERSO,P300A,P301A,P301B,P301C,P301D)

M5EmpIng<-select(M5, CONGLOME, VIVIENDA, HOGAR, CODPERSO, UBIGEO, DOMINIO, ESTRATO,
                 P207, P208A, OCU500, OCUPINF, EMPLPSEC, P512A,
                 P512B, P513T,
                 P513A1, P513A2, P518, P520,
                 I524A1, D529T, I530A, D536, I538A1, D540T, I541A, D543, D544T,
                 FAC500A)
rm(M3,M5)

M35EEI<- inner_join(x=M3Educ, y=M5EmpIng, by=c("CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO"))

install.packages("foreign")
library(foreign)

write.dta(M35EEI, "M35EEI.dta")

write.dta(M35EEI, "M35EEI.sav")


# Se crea el "df" para la Ecuación de Mincer #
tablaEM <- M35EEI
# Se crea la columna ubidpto a partir de ubigeo #
tablaEM$UBIDPTO <-substring(tablaEM$UBIGEO , 1, 2)
head(tablaEM$UBIDPTO)
# Se crea la columna región a partir de dominio #
tablaEM$REGION <-as.factor(tablaEM$DOMINIO)
head(tablaEM$REGION)
tablaEM$REGION<-ifelse(tablaEM$DOMINIO<=3, "Costa", ifelse(tablaEM$DOMINIO<=6, "Sierra",
                                                           ifelse(tablaEM$DOMINIO==7, "Selva","Lima Metropo
litana")))
head(tablaEM$REGION)

# Se crea la columna zona a partir de estrato #
tablaEM$ZONA<-ifelse(tablaEM$ESTRATO<=6, "Urbana", "Rural")
head(tablaEM$ZONA)
# Se crea la columna de ocupación partir de ocu500 #
tablaEM$OCU<-ifelse(tablaEM$OCU500 ==1, "Ocupado", "No Ocupado")
head(tablaEM$OCU)
# Se crea la columna de inform partir de ocupinf #
tablaEM$INFORM<-ifelse(tablaEM$OCUPINF ==1, "Informal", "Formal")
head(tablaEM$INFORM)
# Se crea la columna sexo o genero a partir de p207 #
tablaEM$SEXO<-ifelse(tablaEM$P207 ==1, "Masculino", "Femenino")
head(tablaEM$SEXO)
# Se crea la columna geduc y aeduc a partir de p301a, p301b, p301c #
tablaEM$GEDUC<-ifelse(tablaEM$P301A <=4, "Primaria", ifelse(tablaEM$P301A <=6, "Secundaria",
                                                           ifelse(tablaEM$P301A <=8, "Sup. No Univ.",
                                                                  ifelse(tablaEM$P301A <=11, "Sup. Univ.","EBE"))))
head(tablaEM$GEDUC)
# Asignar los valores según las condiciones especificadas
tablaEM$AEDUC <- ifelse(tablaEM$P301A <= 2, 0,
                        ifelse(tablaEM$P301A == 3 & tablaEM$P301B != 0, tablaEM$P301B ,
                               ifelse(tablaEM$P301A == 3 & !is.na(tablaEM$P301B), tablaEM$P301C ,
                                      ifelse(tablaEM$P301A == 4, 6,
                                             ifelse(tablaEM$P301A == 5, 6 + tablaEM$P301B ,
                                                    ifelse(tablaEM$P301A == 6, 11,
                                                           ifelse(tablaEM$P301A == 7 | tablaEM$P301A == 8 | tablaEM$P301A
                                                                  == 9 | tablaEM$P301A == 10, 11 + tablaEM$P301B ,
                                                                  ifelse(tablaEM$P301A == 11, 16 + tablaEM$P301B , NA))))))))
head(tablaEM$AEDUC)
summary(tablaEM$AEDUC)
# Se crea la columna aedad y aedad2 a partir de p208a #
tablaEM$AEDAD <-tablaEM$P208A
tablaEM$AEDAD2<-tablaEM$P208A^2
head(tablaEM$AEDAD2)
# Se crea la columna Ws (salario semanal) e LWs (ln) de las columnas "i524a1", "d529t", "i530a", "d536", "i538a1", "d540t","i541a", "d543", "d544t" #
tablaEM$WS<-(rowSums(tablaEM[c("I524A1", "D529T", "I530A", "D536", "I538A1", 
                               "D540T","I541A", "D543", "D544T")], na.rm = T))/52.1429
head(tablaEM$WS)
tablaEM$LWS<-log(tablaEM$WS)
head(tablaEM$LWS)


# Se filtra o se selecciona las variables de la ecuación de Mincer #
tablaREM<-select(tablaEM, UBIDPTO, REGION, ZONA, OCU, INFORM, SEXO, GEDUC, AEDUC, AEDAD, AEDAD2, WS, LWS)
head(tablaREM)
# se filtra los datos para un dpto en particular y para los ocupados.Ejemplo: Lima metropolitana#

tablaREM<-filter(tablaREM, UBIDPTO=="13" & OCU=="Ocupado" & WS>0)
head(tablaREM)
# Se graba la tabla de datos para las regresiones. formato: *.dt
a #
write_sav(tablaREM, "tablaREM.sav")
write.csv(tablaREM, "tablaREM.csv")
library(readr)

url <- "https://raw.githubusercontent.com/Joaprincs/Econometria/7f53036d57d454830d91a93480cf40f9a3807daf/tablaREM.csv"
tablaREM1 <- read_csv(url)
View(tablaREM1)
tablaREM1 <- tablaREM1[, -1]
head(tablaREM1)

# Histogroma Ws y LWs #
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
ggplot(data = tablaREM1, aes(x =WS, colour=SEXO, fill=SEXO))+
  geom_histogram( alpha=0.3, position="identity",bins = 60)+
  ggtitle("Distribución LWs para hombres y mujeres")
ggplot(data = tablaREM1, aes(x =LWS, colour=SEXO, fill=SEXO))+
  geom_histogram( alpha=0.3, position="identity",bins = 60)+
  ggtitle("Distribución LWs para hombres y mujeres")
ggplot(data = tablaREM1, aes(x =LWS, colour=SEXO, fill=SEXO))+
  geom_histogram( alpha=0.3, position="identity",bins = 60)+
  ggtitle("Distribución LWs para hombres y mujeres")
#CREAR TABLA BASE
base<-select(tablaREM1,LWS,AEDUC,AEDAD)
# mariz de correlación #
mcor<-round(cor(base),4)
upper<-mcor
upper[upper.tri(mcor)]<-""
upper<-as.data.frame(upper)
upper
# Diagrama de dispersión multiple y simples #
base<-select(tablaREM1, LWS, AEDUC, AEDAD, AEDAD2)
plot(base)
qplot(x=AEDUC,y=LWS,data=tablaREM1, color=factor(SEXO), geom=c("point","smooth"))
qplot(x=AEDAD,y=LWS,data=tablaREM1, color=factor(SEXO), geom=c("point","smooth"))
qplot(x=AEDAD2,y=LWS,data=tablaREM1, color=factor(SEXO), geom=c("point","smooth"))



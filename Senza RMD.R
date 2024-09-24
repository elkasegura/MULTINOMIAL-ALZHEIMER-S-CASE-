library(coin)
library(nnet)
library(glmnet)
library(VGAM)

#Se han realizado muchos estudios sobre la influencia del tabaquismo en el desarrollo 
#de la demencia y en el caso concreto del Alzheimer. La demencia es a menudo el resultado 
#de la interacción de factores genéticos, factores ambientales y factores de estilo de vida. 
#La demencia relacionada con el Alzheimer por lo general comienza de manera insidiosa y 
#progresa lenta y constantemente durante un período de varios años; la memoria episódica es 
#una de las primeras áreas cognitivas afectadas. Actualmente no existe ningún tratamiento que 
#pueda curar la demencia o alterar su progresión progresiva. Hasta que se encuentre un 
#tratamiento eficaz, las estrategias para reducir la frecuencia de la enfermedad, retrasar su 
#aparición y reducir su carga deben centrarse en identificar y actuar sobre los factores de riesgo 
#modificables. Los factores de riesgo modificables para la demencia incluyen fumar, un estilo de vida 
#sedentario, presión arterial alta, obesidad, diabetes, bajos niveles de cognición o baja educación. 
#Este trabajo consiste en determinar si el efecto del tabaquismo influye en el desarrollo del Alzheimer 
#y otras demencias. Como herramienta estadística se utiliza para el estudio la regresión multinomial 
#con el Software Rstudio.

#En este caso para simular la regresión multinomial se utiliza el conjunto de datos de Alzheimer de 
#la biblioteca "coin" del paquete R. Este conjunto de datos tiene 538 observaciones y tres variables: 
#"tabaquismo", "enfermedad" y "género". La referencia bibliográfica del caso de estudio es la siguiente:

#Salib, E. and Hillier, V. (1997). A case-control study of smoking and Alzheimer's disease. International 
#Journal of Geriatric Psychiatry 12(3), 295–300. doi: 10.1002/(SICI)1099-1166(199703)12:3<295::
#AID-GPS476>3.0.CO;2-3.

##Leer la base de datos de la libreria coin
data_alzhe<-data.frame(alzheimer)
summary(data_alzhe)
table(alzheimer)
# Interpretando la salida de acuerdo a la variable dependiente, 
#198 paciente tienen Alzheimer, 164 Other dementias y 176 Other diagnoses,
#338 son paciente feminas y 200 masculinas
#smoking               disease       gender   
#None :309   Alzheimer      :198   Female:338  
#<10  : 28   Other dementias:164   Male  :200  
#10-20:110   Other diagnoses:176               
#>20  : 91

#Tabla de frecuencias para cada categoria de cada de cada factor
tab_smok <- table(alzheimer$disease, alzheimer$smoking)
#, , gender = Female

#disease
#smoking Alzheimer Other dementias Other diagnoses
#None         91              55              80
#<10           7               7               3
#10-20        15              16              25
#>20          21               9               9

#, , gender = Male

#disease
#smoking Alzheimer Other dementias Other diagnoses
#None         35              24              24
#<10           8               1               2
#10-20        15              17              22
#>20  

barplot(tab_smok,                            
        main="Grafico a barre: Disease VS Smoking",               
        xlab="Smoking",                         
        ylab="Frequenze",                  
        legend = rownames(tab_smok),        
        ylim = c(0, 200),
        col=c("blue", "red", "cyan"),
        beside=TRUE)

tab_smok1 <- table(alzheimer$disease, alzheimer$gender)

barplot(tab_smok1,                            
        main="Grafico a barre: Disease VS Gender",               
        xlab="Gender",                         
        ylab="Frequenze",                  
        legend = rownames(tab_smok1),        
        ylim = c(0, 200),                    
        col=c("blue", "red", "cyan"),  
        beside=TRUE)

layout(matrix(1:2, ncol = 2))
spineplot(disease ~ smoking, data = alzheimer,
          subset = gender == "Male", main = "Male", col = c("blue", "red", "cyan"))
spineplot(disease ~ smoking, data = alzheimer,
          subset = gender == "Female", main = "Female", col = c("blue", "red", "cyan"))

attach(data_alzhe)
#Se reajusta el nivel de referencia para el dataset  con ref "Other dementias" 
data_alzhe$disease <- relevel(data_alzhe$disease, ref = "Other dementias")
options(contrasts=c("contr.treatment","contr.poly"))
alzhe_modS<-multinom(disease~gender*smoking, data=data_alzhe) #Modello Saturo
# weights:  27 (16 variable)
#initial  value 591.053411 
#iter  10 value 562.535131
#final  value 561.224741 
#converged
alzhe_mod0<-multinom(disease~1,data=data_alzhe) #Modelo nulo
# weights:  6 (2 variable)
#initial  value 591.053411 
#final  value 589.407756 
#converged
alzhe_mod1<-multinom(disease~gender, data=data_alzhe) #Modelo Gender
# weights:  9 (4 variable)
#initial  value 591.053411 
#final  value 584.617852 
#converged
alzhe_mod2<-multinom(disease~smoking, data=data_alzhe) #Modelo Smoking
# weights:  15 (8 variable)
#initial  value 591.053411 
#iter  10 value 575.814595
#final  value 575.783014 
#converged
alzhe_mod3<-multinom(disease~gender+smoking, data=data_alzhe) #Modelo Gender + Smoking
# weights:  18 (10 variable)
#initial  value 591.053411 
#iter  10 value 573.605715
#final  value 573.189760 
#converged

# Se restan las deviances de todos lo modelos predichos con la deviance del modelo Saturo, 
#se elige el que tenga la menor distancia con respecto al Modelo Saturo.
deviance(alzhe_mod1)-deviance(alzhe_modS) 
#[1] 46.78622
deviance(alzhe_mod2)-deviance(alzhe_modS)
#[1] 29.11655
deviance(alzhe_mod3)-deviance(alzhe_modS)
#[1] 23.93004
deviance(alzhe_mod0)-deviance(alzhe_modS)
#[1] 56.36603
summary(alzhe_mod3)

#Call:
#  multinom(formula = disease ~ gender + smoking, data = data_alzhe)

#Coefficients:
#                 (Intercept) genderMale smoking<10 smoking10-20 smoking>20
#Alzheimer         0.5988510 -0.4549438  0.2189701   -0.4602867 -0.8260256
#Other diagnoses   0.4079088 -0.4588215 -0.6872985    0.1813765 -0.9331809

#Std. Errors:
#                 (Intercept) genderMale smoking<10 smoking10-20 smoking>20
#Alzheimer         0.1591086  0.2271575  0.4638437    0.2954492  0.2911728
#Other diagnoses   0.1647381  0.2338396  0.5916437    0.2780938  0.3156220

#Residual Deviance: 1146.38 
#AIC: 1166.38

# Se elevan exponencialmente los coeficientes estimados del modelo elegido para deteminar el 
#riesgo relativo 
exp(coef(alzhe_mod3))

#                  (Intercept) genderMale smoking<10 smoking10-20 smoking>20
#Alzheimer          1.820026  0.6344836  1.2447940    0.6311027  0.4377858
#Other diagnoses    1.503670  0.6320281  0.5029329    1.1988665  0.3933007

alzhe<-vglm(disease~gender+smoking, multinomial(refLevel = 1),data=data_alzhe)
summary(alzhe)

#Call:
#  vglm(formula = disease ~ gender + smoking, family = multinomial(refLevel = 1), 
#       data = data_alzhe)

#Coefficients: 
#                Estimate Std. Error z value Pr(>|z|)    
#(Intercept):1    0.5989     0.1591   3.764 0.000167 ***
#(Intercept):2    0.4079     0.1647   2.476 0.013283 *  
#genderMale:1    -0.4549     0.2272  -2.003 0.045202 *  
#genderMale:2    -0.4588     0.2338  -1.962 0.049749 *  
#smoking<10:1     0.2190     0.4638   0.472 0.636873    
#smoking<10:2    -0.6873     0.5916  -1.162 0.245366    
#smoking10-20:1  -0.4603     0.2954  -1.558 0.119252    
#smoking10-20:2   0.1814     0.2781   0.652 0.514261    
#smoking>20:1    -0.8260     0.2912  -2.837 0.004556 ** 
#smoking>20:2    -0.9332     0.3156  -2.957 0.003110 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Names of linear predictors: log(mu[,2]/mu[,1]), log(mu[,3]/mu[,1])

#Residual deviance: 1146.38 on 1066 degrees of freedom

#Log-likelihood: -573.1898 on 1066 degrees of freedom

#Number of Fisher scoring iterations: 4 

#No Hauck-Donner effect found in any of the estimates


#Reference group is level  1  of the response

#Se realiza la prueba de Chi-Squared para determinar si se acepta el modelo logistico
#Se calcula los grados de libertad
#N(J-1)-#parametros, donde N è il numero de gruppo 
#N grupos*(J-1) = 2(gender)*4(smoking)*2(logit) -#parametros 
#para alzhe_mod3 es 16 - 10 = 6 p-value del chi square.

1-pchisq(deviance(alzhe_mod3)-deviance(alzhe_modS), 6) # no se acepta el modelo alzhe_mod3
#[1] 0.0005379607


smoking.labs<-factor(c("None", "<10", "10-20", ">20"),
                     levels=c("None", "<10", "10-20", ">20"))
gender.labs<-factor(c("Female","Male"),levels=c("Female","Male"))
disease.labs<-factor(c("Alzheimer", "Other dementias", "Other"),
                     levels=c("Alzheimer","Other dementias", "Other"))

predictions<-predict(alzhe_mod3, type = "probs", 
                     newdata = expand.grid(gender=gender.labs, smoking=smoking.labs))
cbind(expand.grid(gender = gender.labs, smoking = smoking.labs), predictions)

######ALTRA SOLUZIONE

smok<-ifelse(data_alzhe$smoking=="None","No","Yes")
#El factor Smoking se modifica de 4 categorias a dos categorias
alzheM<-cbind(data_alzhe, smok)
head(alzheM) #Visualizacion de las primera 6 observaciones
#  smoking   disease gender smok
#1    None Alzheimer Female   No
#2    None Alzheimer Female   No
#3    None Alzheimer Female   No
#4    None Alzheimer Female   No
#5    None Alzheimer Female   No
#6    None Alzheimer Female   No
tail(alzheM) #Visualizacion de las ultimas 6 observaciones
#    smoking         disease gender smok
#533     >20 Other diagnoses   Male  Yes
#534     >20 Other diagnoses   Male  Yes
#535     >20 Other diagnoses   Male  Yes
#536     >20 Other diagnoses   Male  Yes
#537     >20 Other diagnoses   Male  Yes
#538     >20 Other diagnoses   Male  Yes
table(alzheM$smok, alzheM$disease, alzheM$gender)

#, ,  = Female


#Other dementias Alzheimer Other diagnoses
#No               55        91              80
#Yes              32        43              37

#, ,  = Male


#Other dementias Alzheimer Other diagnoses
#No               24        35              24
#Yes              53        29              35

tab_smok2 <- table(alzheM$smok, alzheM$gender)

barplot(tab_smok2,                            
        main="Grafico a barre: Smok VS Gender",               
        xlab="Smoking",                         
        ylab="Frequenze",                  
        legend = rownames(tab_smok2),        
        ylim = c(0, 300
        ),                    
        col=c("blue", "red", "cyan"),  
        beside=TRUE)
#Se realizan nuevamente todos los pasos anteriores para la estimacion del nuevo modelo
attach(alzheM)
alzheM$disease <- relevel(alzheM$disease, ref = "Other dementias")
options(contrasts=c("contr.treatment","contr.poly"))
modS<-multinom(disease~gender*smok, data=alzheM) #Modello Saturo
# weights:  15 (8 variable)
#initial  value 591.053411 
#iter  10 value 580.298333
#final  value 580.260118 
#converged
mod0<-multinom(disease~1,data=alzheM) #Modelo nulo
# weights:  6 (2 variable)
#initial  value 591.053411 
#final  value 589.407756 
#converged
mod1<-multinom(disease~gender, data=alzheM) #Modelo Gender
# weights:  9 (4 variable)
#initial  value 591.053411 
#final  value 584.617852 
#converged
mod2<-multinom(disease~smok, data=alzheM) #Modelo Smoking
# weights:  9 (4 variable)
#initial  value 591.053411 
#final  value 584.884961 
#converged
mod3<-multinom(disease~gender+smok, data=alzheM) #Modelo Genero + Smoking
# weights:  12 (6 variable)
#initial  value 591.053411 
#iter  10 value 581.861183
#final  value 581.861035 
#converged
deviance(mod1)-deviance(modS)
#[1] 8.715468
deviance(mod2)-deviance(modS)
#[1] 9.249687
deviance(mod3)-deviance(modS)
#[1] 3.201834
deviance(mod0)-deviance(modS)
#[1] 18.29528
summary(mod3)

#Call:
#  multinom(formula = disease ~ gender + smok, data = alzheM)

#Coefficients:
#                 (Intercept) genderMale    smokYes
#Alzheimer         0.6098540 -0.4914707 -0.5172358
#Other diagnoses   0.4158041 -0.4825830 -0.3273280

#Std. Errors:
#                 (Intercept) genderMale   smokYes
#Alzheimer         0.1590710  0.2252614 0.2219751
#Other diagnoses   0.1646062  0.2303975 0.2261444

#Residual Deviance: 1163.722 
#AIC: 1175.722 

exp(coef(mod3))

#                (Intercept) genderMale   smokYes
#Alzheimer          1.840163  0.6117261 0.5961662
#Other diagnoses    1.515589  0.6171872 0.7208472

alzhe<-vglm(disease~gender+smok, multinomial(refLevel = 1),data=alzheM)
summary(alzhe)

#Call:
#  vglm(formula = disease ~ gender + smok, family = multinomial(refLevel = 1), 
#       data = alzheM)

#Coefficients: 
#                Estimate Std. Error z value Pr(>|z|)    
#(Intercept):1   0.6099     0.1591   3.834 0.000126 ***
#(Intercept):2   0.4158     0.1646   2.526 0.011535 *  
#genderMale:1   -0.4915     0.2253  -2.182 0.029126 *  
#genderMale:2   -0.4826     0.2304  -2.095 0.036210 *  
#smokYes:1      -0.5172     0.2220  -2.330 0.019798 *  
#smokYes:2      -0.3273     0.2261  -1.447 0.147780    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Names of linear predictors: log(mu[,2]/mu[,1]), log(mu[,3]/mu[,1])

#Residual deviance: 1163.722 on 1070 degrees of freedom

#Log-likelihood: -581.861 on 1070 degrees of freedom

#Number of Fisher scoring iterations: 4 

#No Hauck-Donner effect found in any of the estimates


#Reference group is level  1  of the response

#Se calcula los grados de libertad
#N(J-1)-#parametros, donde N è il numero de gruppo 
#N grupos*(J-1) = 2(gender)*2(smoking)*2(logit) -#parametros 
#para alzhe_mod3 es 8 - 6 = 2 p-value del chi square.
1-pchisq(deviance(mod3)-deviance(modS), 2) #  se acepta el modelo mod3
#[1] 0.2017115

#Se hayan las probabilidades estimadas para cada combinacion de cada nivel de los factores

smoking.labs<-factor(c("Yes", "No"),
                     levels=c("Yes", "No"))
gender.labs<-factor(c("Female","Male"),levels=c("Female","Male"))
disease.labs<-factor(c("Alzheimer", "Other dementias", "Other diagnoses"),
                     levels=c("Alzheimer","Other dementias", "Other diagnoses"))

predictions1<-predict(mod3, type = "probs", 
                      newdata = expand.grid(gender=gender.labs, smok=smoking.labs))
cbind(expand.grid(gender = gender.labs, smok = smoking.labs), predictions1)

#gender smok Other dementias Alzheimer Other diagnoses
#1 Female  Yes       0.3135238 0.3439490       0.3425273
#2   Male  Yes       0.4263717 0.2861336       0.2874947
#3 Female   No       0.2295815 0.4224673       0.3479512
#4   Male   No       0.3266824 0.3677383       0.3055793

predict_modalzhe<-predict(mod3)
head(predict_modalzhe)
table(predict_modalzhe)

#Se realiza la tabla de correcta clasificacion para determinar que porciento ha sido clasificado de forma 
#correcta de acuerdo al modelo estimado

predict_modalzhe<-predict(mod3, alzheM, type="class")
head(predict_modalzhe)
t<-table(predict_modalzhe, disease)
t

#                  disease
#predict_modalzhe  Other dementias Alzheimer Other diagnoses
#Other dementias              53        29              35
#Alzheimer                   111       169             141
#Other diagnoses               0         0               0

sum(diag(t))/sum(t)
#[1] 0.4126394

#El modelo solo clasifico bien el 41% de los pacientes, el modelo no presenta una acurateza buena, pero no es
#despreciable, se demostro que el habito de fumar influye en el desarrollo de las Demensias y el Alzheimer. 

freque.osse <- tapply(alzheM$disease, list(factor(alzheM$smok, levels = c("No","Yes")),
                                           factor(alzheM$gender,levels =c("Female", "Male"))), length)
freque.osse
#     Female Male
#No     226   83
#Yes    112  117
# la frecuencia teorica y de la probabilidad estimada
row.names2 <- rev(expand.grid(dimnames(freque.osse)))
freque.teoriche<-round(as.vector(freque.osse)* fitted(mod3)[!duplicated(as.data.frame(fitted(mod3))),
],1)
structure(.Data = as.data.frame(freque.teoriche), row.names = apply(row.names2, 1,paste, collapse = " ")) 

                                                                    
#            Other dementias Alzheimer Other diagnoses
#Female No             51.9      95.5            78.6
#Female Yes            35.1      38.5            38.4
#Male No               27.1      30.5            25.4
#Male Yes              49.9      33.5            33.6                                                                    
                                                                    
                                                                    
                                                                    
                                                                    
                                                                    
                                                                                                                                       

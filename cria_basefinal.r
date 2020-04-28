require(data.table)
require(shiny)
library(ggplot2)
require(plotly)
## Lendo os dados
setwd("C:\\Users\\EricaCastilho\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Calculo_KL_municipio_porano\\KL para todos os anos")

setwd("C:\\Users\\UFOP\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Calculo_KL_municipio_porano\\KL para todos os anos")

kl_tudo<-data.table(read.table("Kl_tudo.csv",dec=",",sep = ";",header = T))

## Calcula os hiatos
#Sexo = Meninas - meninos
kl_tudo$hiato_sexo_portugues<-kl_tudo$KL.Portugues_Feminino-kl_tudo$KL.Portugues_Masculino
kl_tudo$hiato_sexo_matematica<-kl_tudo$KL.Matematica_Feminino-kl_tudo$KL.Matematica_Masculino

#Raca = Pretos - Brancos
kl_tudo$hiato_raca_portugues<-kl_tudo$KL.Portugues_Preto-kl_tudo$KL.Portugues_Branco
kl_tudo$hiato_raca_matematica<-kl_tudo$KL.Matematica_Preto-kl_tudo$KL.Matematica_Branco

#Raca = Negro(preto+pardo) - Brancos
kl_tudo$hiato_raca02_portugues<-kl_tudo$KL.Portugues_Pardo_Preto-kl_tudo$KL.Portugues_Branco
kl_tudo$hiato_raca02_matematica<-kl_tudo$KL.Matematica_Pardo_Preto-kl_tudo$KL.Matematica_Branco



#NSE = Baixo(NSE1) - Alto(NSE5)
kl_tudo$hiato_nse_portugues<-kl_tudo$KL.Portugues_NSE_1-kl_tudo$KL.Portugues_NSE_5
kl_tudo$hiato_nse_matematica<-kl_tudo$KL.Matematica_NSE_1-kl_tudo$KL.Matematica_NSE_5


########################################
# Cria criterio de confiabilidade
########################################

amostra_minima=100
proporcao_minima=0.5

amostra_minima_grupo=75

## Portugues
# Sexo

aux<-apply(kl_tudo[,c('nportugues_Masculino','nportugues_Feminino')],1
           ,function(x) sum(x,na.rm=T))

aux<-aux/kl_tudo$nportugues

kl_tudo$confiavel_sexo_portugues<-kl_tudo$nportugues>amostra_minima & 
                                   aux>proporcao_minima & 
  kl_tudo$nportugues_Masculino>amostra_minima_grupo&
  kl_tudo$nportugues_Feminino>amostra_minima_grupo


# Raça

aux<-apply(kl_tudo[,c('nportugues_Branco','nportugues_Pardo','nportugues_Preto',
                      'nportugues_Amarelo','nportugues_Índigena')],1
           ,function(x) sum(x,na.rm=T))

aux<-aux/kl_tudo$nportugues

kl_tudo$confiavel_raca_portugues<-kl_tudo$nportugues>amostra_minima &
                                  aux>proporcao_minima &
                     kl_tudo$nportugues_Branco>amostra_minima_grupo&
                     kl_tudo$nportugues_Preto>amostra_minima_grupo


kl_tudo$confiavel_raca01_portugues<-kl_tudo$nportugues>amostra_minima &
  aux>proporcao_minima &
  kl_tudo$nportugues_Branco>amostra_minima_grupo&
  kl_tudo$nportugues_Pardo_Preto>amostra_minima_grupo


# NSE

aux<-apply(kl_tudo[,c('nportugues_NSE_1','nportugues_NSE_2','nportugues_NSE_3','nportugues_NSE_4','nportugues_NSE_5')],1
           ,function(x) sum(x,na.rm=T))

aux<-aux/kl_tudo$nportugues

kl_tudo$confiavel_nse_portugues<-kl_tudo$nportugues>amostra_minima &
                                 aux>proporcao_minima &
  kl_tudo$nportugues_NSE_1>amostra_minima_grupo&
  kl_tudo$nportugues_NSE_5>amostra_minima_grupo


## Matematica
# Sexo

aux<-apply(kl_tudo[,c('nmatematica_Masculino','nmatematica_Feminino')],1
           ,function(x) sum(x,na.rm=T))

aux<-aux/kl_tudo$nmatematica

kl_tudo$confiavel_sexo_matematica<-kl_tudo$nmatematica>amostra_minima & 
                                  aux>proporcao_minima & 
                      kl_tudo$nmatematica_Masculino>amostra_minima_grupo &
                      kl_tudo$nmatematica_Feminino>amostra_minima_grupo



# Raça

aux<-apply(kl_tudo[,c('nmatematica_Branco','nmatematica_Pardo','nmatematica_Preto',
                      'nmatematica_Amarelo','nmatematica_Índigena')],1
           ,function(x) sum(x,na.rm=T))

aux<-aux/kl_tudo$nmatematica

kl_tudo$confiavel_raca_matematica<-kl_tudo$nmatematica>amostra_minima &
                                  aux>proporcao_minima &
  kl_tudo$nmatematica_Branco>amostra_minima_grupo&
  kl_tudo$nmatematica_Preto>amostra_minima_grupo


kl_tudo$confiavel_raca01_matematica<-kl_tudo$nmatematica>amostra_minima &
  aux>proporcao_minima &
  kl_tudo$nmatematica_Branco>amostra_minima_grupo&
  kl_tudo$nmatematica_Pardo_Preto>amostra_minima_grupo


# NSE

aux<-apply(kl_tudo[,c('nmatematica_NSE_1','nmatematica_NSE_2','nmatematica_NSE_3','nmatematica_NSE_4','nmatematica_NSE_5')],1
           ,function(x) sum(x,na.rm=T))

aux<-aux/kl_tudo$nmatematica

kl_tudo$confiavel_nse_matematica<-kl_tudo$nmatematica>amostra_minima & 
                                aux>proporcao_minima &
                         kl_tudo$nmatematica_NSE_1>amostra_minima_grupo &
                         kl_tudo$nmatematica_NSE_5>amostra_minima_grupo





## Acrescenta os nomes dos municipios

setwd("C:\\Users\\UFOP\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\MalhaIBGE")
setwd("C:\\Users\\EricaCastilho\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\MalhaIBGE")

require(rgdal)

brasil_mapa = readOGR(".", "55mu2500gc")

aux<-match(kl_tudo$COD_MUNICIPIO,brasil_mapa$GEOCODIGO)


sum(brasil_mapa$GEOCODIGO==1504752)
kl_tudo$Nomes_municipios<-brasil_mapa$NOME[aux]
kl_tudo$Nomes_municipios
kl_tudo$Nomes_municipios<-iconv(kl_tudo$Nomes_municipios,to="ASCII//TRANSLIT")


## Acrescenta os estados

setwd("C:\\Users\\UFOP\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Analise dos resultados\\Analise Geral Mauricipio\\AppHiato - todos anos")
setwd("C:\\Users\\EricaCastilho\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Analise dos resultados\\Analise Geral Mauricipio\\AppHiato - todos anos")


nomes_estados<-fread("Nomes_municipios3.csv")

nomes_estados

aux<-match(kl_tudo$COD_MUNICIPIO,nomes_estados$codmun)

kl_tudo$UF<-nomes_estados$UF[aux]


## Identfica os 100 maiores cidades
setwd("C:\\Users\\UFOP\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Analise dos resultados")


setwd("C:\\Users\\EricaCastilho\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Analise dos resultados")

populacao_municipios<-fread("populacao_municipios.csv",sep = ';',dec=',',header=T)

populacao_municipios<-populacao_municipios[order(Pop2017,decreasing=TRUE),]

aux<-match(populacao_municipios$`CÓDIGO MUNICÍPIO`[1:100],kl_tudo[Serie==5,COD_MUNICIPIO])

kl_tudo$Cidade_grande<-factor(levels=c("Sim","Nao"))

# Quinto ano 
kl_tudo[Serie==5,]$Cidade_grande[aux]<-"Sim"

kl_tudo$Cidade_grande[is.na(kl_tudo$Cidade_grande)]<-"Nao"

# Nono ano
aux<-match(populacao_municipios$`CÓDIGO MUNICÍPIO`[1:100],kl_tudo[Serie==9,COD_MUNICIPIO])
kl_tudo[Serie==9,]$Cidade_grande[aux]<-"Sim"
kl_tudo[COD_MUNICIPIO==populacao_municipios$`CÓDIGO MUNICÍPIO`[17],Cidade_grande]

table(kl_tudo$Cidade_grande)
## Identifica as capitais

setwd("C:\\Users\\EricaCastilho\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Analise dos resultados\\Analise Geral Mauricipio")

setwd("C:\\Users\\UFOP\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Analise dos resultados\\Analise Geral Mauricipio")

codigo_capitais<-fread("Codigos_capitais.csv")

aux<-match(kl_tudo$COD_MUNICIPIO,codigo_capitais$IBGE7)
aux<-which(!is.na(aux))

kl_tudo$Capitais<-factor(levels=c("Sim","Nao"))  
kl_tudo$Capitais[aux]<-"Sim"
kl_tudo$Capitais[-aux]<-"Nao"
table(kl_tudo$Capitais)

# Cria indice de exclusao 
peso=0.5

# Portugues
kl_tudo$exclusao_sexo_portugues<-scale(-kl_tudo$KL.Portugues)*peso+scale(abs(kl_tudo$hiato_sexo_portugues))*(1-peso)
hist(kl_tudo[Serie==5 & confiavel_sexo_portugues,exclusao_nse_portugues])
hist(kl_tudo[Serie==9 & confiavel_sexo_portugues,exclusao_nse_portugues])
kl_tudo$exclusao_raca_portugues<-scale(-kl_tudo$KL.Portugues)*peso+scale(abs(kl_tudo$hiato_raca_portugues))*(1-peso)
kl_tudo$exclusao_raca02_portugues<-scale(-kl_tudo$KL.Portugues)*peso+scale(abs(kl_tudo$hiato_raca02_portugues))*(1-peso)

kl_tudo$exclusao_nse_portugues<-scale(-kl_tudo$KL.Portugues)*peso+scale(abs(kl_tudo$hiato_nse_portugues))*(1-peso)

# Matematica
kl_tudo$exclusao_sexo_matematica<-scale(-kl_tudo$KL.Matematica)*peso+scale(abs(kl_tudo$hiato_sexo_matematica))*(1-peso)
hist(kl_tudo[Serie==5 & confiavel_sexo_matematica,exclusao_sexo_matematica])
hist(kl_tudo[Serie==9 & confiavel_sexo_matematica,exclusao_sexo_matematica])
kl_tudo$exclusao_raca_matematica<-scale(-kl_tudo$KL.Matematica)*peso+scale(abs(kl_tudo$hiato_raca_matematica))*(1-peso)
kl_tudo$exclusao_raca02_matematica<-scale(-kl_tudo$KL.Matematica)*peso+scale(abs(kl_tudo$hiato_raca02_matematica))*(1-peso)

kl_tudo$exclusao_nse_matematica<-scale(-kl_tudo$KL.Matematica)*peso+scale(abs(kl_tudo$hiato_nse_matematica))*(1-peso)

## Identifica centro aglomeracaoes

setwd("C:\\Users\\EricaCastilho\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Analise dos resultados\\Analise Geral Mauricipio\\Arranjos Cidades")
setwd("C:\\Users\\UFOP\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Analise dos resultados\\Analise Geral Mauricipio\\Arranjos Cidades")


centros_aglomorecaoes<-fread("Centro_aglomerados.csv")


aux<-match(kl_tudo$COD_MUNICIPIO,centros_aglomorecaoes$codigos)
aux<-which(!is.na(aux))



kl_tudo$Centro_aglomeracoes<-"Nao"

kl_tudo$Centro_aglomeracoes[aux]<-"Sim"

table(kl_tudo[,Centro_aglomeracoes])

setwd("C:\\Users\\EricaCastilho\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Analise dos resultados\\Analise Geral Mauricipio\\AppHiato - todos anos")
setwd("C:\\Users\\UFOP\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Analise dos resultados\\Analise Geral Mauricipio\\AppHiato - todos anos")




save(kl_tudo,file="C:/Users/UFOP/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Analise dos resultados/Analise Geral Mauricipio/AppHiato_KL - todos anos/kl_tudo.Rdata")
save(kl_tudo,file="kl_tudo.Rdata")

load('kl_tudo.Rdata')
kl_tudo

fwrite(kl_tudo,"kl_tudo_todosanosjuntos.csv",sep=";",dec=",")

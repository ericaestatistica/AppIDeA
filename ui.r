library(shiny)
library(ggplot2)
require(googleVis)

fluidPage(
  tabsetPanel(type = "tabs",
              tabPanel("KL Leitura",

  sidebarLayout(
  sidebarPanel(
    textInput("municipio", "Digite o nome do municipio:"),
    
    selectInput("serie", "Serie", choices=c(5,9)),

    selectInput('x', 'X',c("KL Portugues"='KL.Portugues',
                           "Hiato Raca (Baixo-Alto) KL"='KL.Portugues_hiato_raca',
                "Hiato NSE (Baixo-Alto) KL"='KL.Portugues_hiato_NSE',
                "Hiato Sexo (Meninas-Meninos) KL"='KL.Portugues_hiato_sexo'), 
                selected='KL.Portugues_hiato_raca'),
    selectInput('y', 'Y', c("KL Portugues"='KL.Portugues',
                            "Hiato Raca (Baixo-Alto) KL"='KL.Portugues_hiato_raca',
                "Hiato NSE (Baixo-Alto) KL"='KL.Portugues_hiato_NSE',
                "Hiato Sexo (Meninas-Meninos) KL"='KL.Portugues_hiato_sexo'), 
                selected='KL.Portugues'),
    
      selectInput('subgrupo', 'Seleciona subgrupo:', c('None', 'Cidade_grande','Capitais',
                                                       'Arranjos Populacionais'='Centro_aglomeracoes',
                                                       'Regiao de Influencia'='ACP')),
   
    selectInput('regiao', 'Seleciona a Regiao Braileira:',
                c('None', 'Centro-Oeste','Nordeste','Norte','Sudeste','Sul')),
    
    selectInput('centralidade', 'Seleciona por tipo de centralidade:',
                c('None', names(table(kl_tudo$RIC10)))),
    
    selectInput("nomes", "Identificar nomes", choices=c('None','TRUE','FALSE'),
                selected="TRUE"), 
    selectInput('color', 'Cor (100 maiores cidades)', c('None', 'Cidade_grande')),
    selectInput('color_cap', 'Cor (Capitais)', c('None', 'Capitais')),
    selectInput('color_reg', 'Cor (Regioes)', c('None', 'Regiao')),
    selectInput('color_regIBGE', 'Cor (Tipos Centralidade )', c('None', Centralidade='RIC10')),
    

    
    selectInput('facet_row', 'Separa por tipo de cidade (100 maiores)', c(None='None', 'Cidade_grande')),
    selectInput('facet_row_cap', 'Separa por tipo de cidade (Capitais)', c(None='None', 'Capitais')),
    selectInput('facet_row_reg', 'Separa por tipo de cidade (Regioes)', c(None='None', 'Regiao')),
  
    
     #Input: Simple integer interval ----
    
    
    sliderInput("xlim_inf", "Limite Inferior de X:",
                min = -4, max = 1,
                value = limites_x_portugues[1],round=T,step=0.1),
    
    sliderInput("xlim_sup", "Limite Superior de X:",
                min =-4, max = 1,
                value = round(limites_x_portugues[2],2),round=T,step=0.1),
    
    sliderInput("ylim_inf", "Limite Inferior de Y:",
                min = -4, max = 1,
                value = round(limites_y_portugues[1],2),round=T,step=0.1),
    sliderInput("ylim_sup", "Limite Superior de Y:",
                min = -4, max = 1,
                value = round(limites_y_portugues[2],2),round=T,step=0.1)
    
  
    ),
  
  
  mainPanel(
 
    plotOutput('plot',
               click = "plot_click"),
    downloadButton(outputId = "down", label = "Download the plot (pdf)"),
    tableOutput("info"),
    htmlOutput("contingencia")
  
  )
    )),
  tabPanel("KL Matematica",
           
           sidebarLayout(
             sidebarPanel(
               
               textInput("municipio2", "Digite o nome do municipio:"),
               selectInput("serie2", "Serie", choices=c(5,9)),
              
              
          
               selectInput('x2', 'X',c("KL Matematica"='KL.Matematica',
                                       "Hiato Raca (Preto-Branco) KL"='KL.Matematica_hiato_raca',
                                       "Hiato NSE (Baixo-Alto) KL"='KL.Matematica_hiato_NSE',
                                       "Hiato Sexo (Meninas-Meninos) KL"='KL.Matematica_hiato_sexo'), 
                           selected='KL.Matematica_hiato_raca'),
               selectInput('y2', 'Y', c("KL Matematica"='KL.Matematica',
                                        "Hiato Raca (Baixo-Alto) KL"='KL.Matematica_hiato_raca',
                                        "Hiato NSE (Baixo-Alto) KL"='KL.Matematica_hiato_NSE',
                                        "Hiato Sexo (Meninas-Meninos) KL"='KL.Matematica_hiato_sexo'),
                           selected='KL.Matematica'),
               selectInput('subgrupo2', 'Seleciona subgrupo:', c('None', 'Cidade_grande','Capitais','Arranjos Populacionais'='Centro_aglomeracoes')),
               
               selectInput('regiao2', 'Seleciona a Regiao Braileira:',
                           c('None', 'Centro-Oeste','Nordeste','Norte','Sudeste','Sul')),
               
               selectInput('centralidade2', 'Seleciona por tipo de centralidade:',
                           c('None', names(table(kl_tudo$RIC10)))),
               
               selectInput("nomes2", "Identificar nomes", choices=c('None','TRUE','FALSE'),
                           selected="TRUE"),
              
               
               selectInput('color2', 'Cor (100 maiores cidades)', c('None', 'Cidade_grande')),
               selectInput('color2_cap', 'Cor (Capitais)', c('None', 'Capitais')),
               selectInput('color2_reg', 'Cor (Regioes)', c('None', 'Regiao')),
               selectInput('color2_regIBGE', 'Cor (Tipos Centralidade )', c('None', Centralidade='RIC10')),
               
               
               
               selectInput('facet_row2', 'Separa por tipo de cidade (100 maiores)', c(None='None', 'Cidade_grande')),
               selectInput('facet_row2_cap', 'Separa por tipo de cidade (Capitais)', c(None='None', 'Capitais')),
              
               selectInput('facet_row2_reg', 'Separa por tipo de cidade (Regioes)', c(None='None', 'Regiao')),
               
               # Input: Simple integer interval ----
               sliderInput("xlim_inf2", "Limite Inferior de X:",
                           min = -4, max = 1,
                           value = round(limites_x_portugues[1],2),round=T,step=0.1),
               sliderInput("xlim_sup2", "Limite Superior de X:",
                           min = -4, max = 1,
                           value = round(limites_x_portugues[2],2),round=T,step=0.1),
               sliderInput("ylim_inf2", "Limite Inferior de Y:",
                           min = -4, max = 1,
                           value = round(limites_y_portugues[1],2),round=T,step=0.1),
               sliderInput("ylim_sup2", "Limite Superior de Y:",
                           min =-4, max = 1,
                           value = round(limites_y_portugues[2],2),round=T,step=0.1)
               
             ),
             
             mainPanel(
               
               
               plotOutput('plot2',
                          click = "plot2_click"),
               downloadButton(outputId = "down2", label = "Download the plot (pdf)"),
               tableOutput("info2"),
               htmlOutput("contingencia2")
               
             )
           ))
  
  )
  )
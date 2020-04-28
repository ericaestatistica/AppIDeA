#devtools::install_github("AliciaSchep/gglabeller") 

# Para o nearpoint funcionar nao pode dar print no grafico
library(ggrepel)
require(data.table)
require(shiny)
library(ggplot2)
require(plotly)



## Lendo os dados
#setwd("C:\\Users\\EricaCastilho\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Analise dos resultados\\Analise Geral Mauricipio\\AppHiato_KL_nova")
#setwd("C:\\Users\\UFOP\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Analise dos resultados\\Analise Geral Mauricipio\\AppHiato_KL_nova")



load('kl_tudo.Rdata')

kl_tudo$RIC10<-factor(kl_tudo$RIC10)


levels(kl_tudo$RIC10)<-c(rep('Capital Regional',3),rep('Centro de Zona',2),
                        'Centro Local',rep('Centro Subregional',2),rep('Metropole',3))

kl_tudo$RIC10 = factor(kl_tudo$RIC10,levels(kl_tudo$RIC10)[c(5,1,4,2,3)])

amostra_minima=1000



limites_x_portugues<-range(kl_tudo[nportugues>amostra_minima,KL.Portugues],na.rm=T)
limites_y_portugues<-range(kl_tudo[nportugues>amostra_minima,c('hiato_sexo_portugues','hiato_raca_portugues','hiato_nse_portugues')],na.rm=T)


limites_x_matematica<-range(kl_tudo[nmatematica>amostra_minima,KL.Matematica],na.rm=T)
limites_y_matematica<-range(kl_tudo[nmatematica>amostra_minima,c('hiato_sexo_matematica','hiato_raca_matematica','hiato_nse_matematica')],na.rm=T)

limites_y<-range(kl_tudo[nportugues>amostra_minima &
                           nmatematica>amostra_minima,c(KL.Portugues,KL.Matematica)],na.rm=T)

limites_x<-range(kl_tudo[nportugues>amostra_minima &
                           nmatematica>amostra_minima,
                         c(KL.Portugues_hiato_raca,KL.Portugues_hiato_NSE,KL.Portugues_hiato_sexo,
                           KL.Matematica_hiato_raca,KL.Matematica_hiato_NSE,KL.Matematica_hiato_sexo)],
                 na.rm=T)



pontos_corte_kl<-c(-1.7,-1.1,-0.7,-0.4)

kl_tudo$Regiao<-factor(kl_tudo$Regiao)

levels(kl_tudo$Regiao)



function(input, output) {
  
  limites_portugues<-reactive({range(c(kl_tudo[[input$x]][kl_tudo$nportugues>amostra_minima]
                                                          ,kl_tudo[[input$y]][kl_tudo$nportugues>amostra_minima]),na.rm=T)
    
    })

 
  
  df_subset_portugues <- reactive({
    a <- subset(kl_tudo, Serie == input$serie )
    
    if(input$regiao != 'None')
      a <- subset(a, Regiao == input$regiao )
    
    
    if(input$centralidade != 'None')
      a <- subset(a, RIC10 == input$centralidade )
    
    
    
    if(input$subgrupo != 'None')
      a <- a[which(a[[input$subgrupo]]=="Sim"),]
    
    
    if (input$x == 'KL.Portugues_hiato_sexo')
     a <- subset(a, confiavel_sexo_portugues == TRUE)
    
    if (input$x == 'KL.Portugues_hiato_raca')
      a <- subset(a, confiavel_raca_portugues == TRUE)
    

    
    if (input$x == 'KL.Portugues_hiato_NSE')
      a <- subset(a, confiavel_nse_portugues == TRUE)
    
    return(a)
  })
  
  
  
  df_subset_matematica <- reactive({
    a <- subset(kl_tudo, Serie == input$serie2)
    
    
    
    if(input$regiao2 != 'None')
      a <- subset(a, Regiao == input$regiao2 )
    
    
    if(input$centralidade2 != 'None')
      a <- subset(a, RIC10 == input$centralidade2 )
    
    
    
    if(input$subgrupo2 != 'None')
      a <- a[which(a[[input$subgrupo2]]=="Sim"),]
    
    if (input$x2 == 'KL.Matematica_hiato_sexo')
      a <- subset(a, confiavel_sexo_matematica == TRUE)
    
    if (input$x2 == 'KL.Matematica_hiato_raca')
      a <- subset(a, confiavel_raca_matematica == TRUE)
    
    
    if (input$x2 == 'KL.Matematica_hiato_raca')
      a <- subset(a, confiavel_raca01_matematica == TRUE)
    
    
    if (input$x2 == 'KL.Matematica_hiato_NSE')
      a <- subset(a, confiavel_nse_matematica == TRUE)
    
    return(a)
  })
  
  
   #limites_x_portugues<-limites_portugues
  #limites_y_portugues<-limites_portugues

  
    
  
    
  
  myPlot <- function(){   
    media<-mean(df_subset_portugues()[[input$x]],na.rm=T)
  
    
    if (input$subgrupo != 'None'){
       if(input$nomes==TRUE) 
     p <- ggplot(df_subset_portugues(),
                aes_string(x=input$x, y=input$y,label="Nomes_municipios")) + 
           geom_point(colour="black")+geom_text_repel()+
           xlim(range(df_subset_portugues()[[input$x]]))+ylim(range(df_subset_portugues()[[input$y]]))
       
       else 
         p <- ggplot(df_subset_portugues(),
                     aes_string(x=input$x, y=input$y,size=input$z)) + geom_point(colour="black")+
           xlim(c(input$xlim_inf,input$xlim_sup))+ylim(c(input$ylim_inf,input$ylim_sup))
       
       
       
    }
    else
    {
      p <- ggplot(df_subset_portugues(),
                  aes_string(x=input$x, y=input$y)) + geom_point()+  
        #xlim(c(input$xlim_inf,input$xlim_sup))+ylim(c(input$ylim_inf,input$ylim_sup))
        xlim(limites_x)+
               ylim(limites_y)
      
    }  
     p<-p+ 
      geom_abline(intercept = 0, slope = 0,col='blue',size=1.5)+geom_vline(xintercept=0,col='blue',size=1.5)+
    
       
       geom_hline(yintercept=pontos_corte_kl[1],linetype="dashed",col='blue')+
       geom_hline(yintercept=pontos_corte_kl[2],linetype="dashed",col='blue')+
       geom_hline(yintercept=pontos_corte_kl[3],linetype="dashed",col='blue')+
       geom_hline(yintercept=pontos_corte_kl[4],linetype="dashed",col='blue')+
      annotate("text", label = "Baixa", y = pontos_corte_kl[1]-.15, x = limites_portugues()[1]*4/5, size=4, colour = "red")+
      annotate("text", label = "Media \n Baixa", y = mean(c(pontos_corte_kl[1],pontos_corte_kl[2])), x = limites_portugues()[1]*4/5, size=4, colour = "red")+
      annotate("text", label = "Media", y = mean(c(pontos_corte_kl[2],pontos_corte_kl[3])), x = limites_portugues()[1]*4/5, size=4, colour = "red")+
      annotate("text", label = "Media \n Alta", y = mean(c(pontos_corte_kl[3],pontos_corte_kl[4])), x = limites_portugues()[1]*4/5, size=4, colour = "red")+
      annotate("text", label = "Alta", y = pontos_corte_kl[4]+.75, x = limites_portugues()[1]*4/5, size=4, colour = "red")
     
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    if (input$color_cap != 'None')
      p <- p + aes_string(color=input$color_cap)
     
     if (input$color_reg != 'None')
       p <- p + aes_string(color=input$color_reg)
     
    
     if (input$color_regIBGE != 'None')
       p <- p + aes_string(color=input$color_regIBGE)+scale_color_manual(values=c('black','blue','green',
                                                                                  'red','yellow'))
     
     
     
    facets <- paste(input$facet_row, '~.')
    if (input$facet_row != "None")
      p <- p + facet_grid(facets)
    
    facets_cap <- paste(input$facet_row_cap, '~.')
    if (input$facet_row_cap != "None")
      p <- p + facet_grid(facets_cap)


    facets_reg <- paste('~',input$facet_row_reg)
    if (input$facet_row_reg != "None")
      p <- p + facet_wrap(facets_reg)
    
  
                            
    
    
    
    
   if(input$municipio != ""){
     aux<-df_subset_portugues()[Nomes_municipios==input$municipio]
     p<-p+geom_label_repel(data=aux,
                          label=aux[['Nomes_municipios']],
                          fontface = 'bold', color = 'black',
                          box.padding = 0.35, point.padding = 0.5,
                          segment.color = 'red')+
       geom_point(data=aux, size = 3, color = 'red')
   }
    
    p
    

    
  }
  
  output$plot <- renderPlot({
    myPlot()
  })
  

  output$info <- renderTable({
    # With ggplot2, no need to tell it what the x and y variables are.
    # threshold: set max distance, in pixels
    # maxpoints: maximum number of rows to return
    # addDist: add column with distance, in pixels
    #aux<-which(nearPoints(df_subset_portugues(),input$plot_click, threshold = 5, maxpoints = 1,
               #addDist = TRUE,allRows = T)$selected_)
    if(input$municipio == ""){

    aux<-data.table(nearPoints(df_subset_portugues(), input$plot_click,input$x,input$y,threshold = 5,maxpoints = 1))


  
    
    if(input$x == 'KL.Portugues_hiato_raca'){
      
      if (input$color_regIBGE == 'None'){
        
        aux2<-cbind(aux[,c('Nomes_municipios','UF')],
          aux[[input$x]], aux[[input$y]],
          aux[,c('nportugues','nPreto_Portugues','nBranco_Portugues')])
             
      #aux2<-cbind(df_subset_portugues()[aux,c('Nomes_municipios','UF')],
      #           df_subset_portugues()[[input$x]][aux],
      ##           df_subset_portugues()[[input$y]][aux],
      #           df_subset_portugues()[aux,c('nportugues')],
      #           df_subset_portugues()[aux,c('nPreto_Portugues')],
      #           df_subset_portugues()[aux,c('nBranco_Portugues')]
       #          )
    
      colnames(aux2)<-c("Nome do Municipio","UF",input$x,input$y,"Numero de alunos",
                       'nPreto','nBranco')
        }
      
    else{
        
      aux2<-cbind(aux[,c('Nomes_municipios','UF','RIC10')],
                  aux[[input$x]], aux[[input$y]],
                  aux[,c('nportugues','nPreto_Portugues','nBranco_Portugues')])
      
          
        
        colnames(aux2)<-c("Nome do Municipio","UF","Centralidade",input$x,input$y,"Numero de alunos",
                         'nPreto','nBranco')
      }
      
     
      
    }
    
    if(input$x == 'KL.Portugues_hiato_sexo'){
      
      if (input$color_regIBGE == 'None'){
        
        aux2<-cbind(aux[,c('Nomes_municipios','UF')],
                    aux[[input$x]], aux[[input$y]],
                    aux[,c('nportugues','nFeminino_Portugues','nMasculino_Portugues')])
        
      colnames(aux2)<-c("Nome do Municipio","UF",input$x,input$y,"Numero de alunos",
                       'nMeninas','nMeninos')
      
      }
      
    else{
      
      aux2<-cbind(aux[,c('Nomes_municipios','UF')],
                  aux[[input$x]], aux[[input$y]],
                  aux[,c('nportugues','nFeminino_Portugues','nMasculino_Portugues')])
      
      colnames(aux2)<-c("Nome do Municipio","UF","Centralidade",input$x,input$y,"Numero de alunos",
                       'nMeninas','nMeninos')
      
      
    }  
    }
    
    
    
    if(input$x == 'KL.Portugues_hiato_NSE'){
      
      
      if (input$color_regIBGE == 'None'){
        aux2<-cbind(aux[,c('Nomes_municipios','UF')],
                    aux[[input$x]], aux[[input$y]],
                    aux[,c('nportugues','nNSE1_Portugues','nNSE5_Portugues')])
        
      colnames(aux2)<-c("Nome do Municipio","UF",input$x,input$y,"Numero de alunos",
                       'nNSE1','nNSE5')
      }
      else{
        
        aux2<-cbind(aux[,c('Nomes_municipios','UF','RIC10')],
                    aux[[input$x]], aux[[input$y]],
                    aux[,c('nportugues','nNSE1_Portugues','nNSE5_Portugues')])
        
        colnames(aux2)<-c("Nome do Municipio","UF","Centralidade",input$x,input$y,"Numero de alunos",
                         'nNSE1','nNSE5')
        
      }
      
    }
    }
    else{
      aux<-df_subset_portugues()[Nomes_municipios==input$municipio]
      aux2<-cbind(aux[,c('Nomes_municipios','UF')],
                 aux[[input$x]],
                 aux[[input$y]],
                 aux[,c('nportugues')]
      )
      
      if(input$x=='KL.Portugues_hiato_raca'){
        aux2<-cbind(aux2,aux[,c('nPreto_Portugues','nBranco_Portugues')]) 
        colnames(aux2)<-c("Municipio","UF",input$x,input$y,
                          "Numero de alunos",'nPreto','nBranco')
      }
      
      if(input$x=='KL.Portugues_hiato_NSE'){
        aux2<-cbind(aux2,aux[,c('nNSE1_Portugues','nNSE5_Portugues')]) 
        colnames(aux2)<-c("Municipio","UF",input$x,input$y,
                          "Numero de alunos",'nNSE1','nNSE5')
      }
      
      
      if(input$x=='KL.Portugues_hiato_sexo'){
        aux2<-cbind(aux2,aux[,c('nFeminino_Portugues','nMasculino_Portugues')]) 
        colnames(aux2)<-c("Municipio","UF",input$x,input$y,
                          "Numero de alunos",'nFeminino','nMasculino')
      }
      
    
   
      
      
    }
    
    
    return(aux2)
    })
  
  output$contingencia <- renderTable({
    indice_KL<-factor(findInterval(df_subset_portugues()[[input$y]],c(-100,pontos_corte_kl,100)),
                      levels=1:5)
    levels(indice_KL)<-c("Baixa","Media Baixa","Media","Media Alta","Alta")
    
    indice_hiato<-factor(findInterval(df_subset_portugues()[[input$x]],c(-100,pontos_corte_kl,100)),
                         levels=1:5)
    levels(indice_hiato)<-c("Baixa","Media Baixa","Media","Media Alta","Alta")
    
    #contigencia<-table(indice_hiato)
    contigencia<-addmargins(table(indice_KL,indice_hiato))
    colnames(contigencia)<-c("Baixa (Hiato)","Media Baixa","Media","Media Alta","Alta","Total")
    rownames(contigencia)<-c("Baixa","Media Baixa","Media","Media Alta","Alta (KL) ","Total")
    contigencia<-contigencia[c(5,4,3,2,1),]
    #contigencia<-contigencia[,c(5,4,3,2,1)]
    as.data.frame.matrix(contigencia)
    
  }, include.rownames=TRUE)
  
  output$down <- downloadHandler(
    filename = function() {
      paste(input$y,input$x,".pdf",sep="")
    },
    content = function(file) {
       pdf(file)
      print(myPlot())
      dev.off()
    }
  )
  
  
  
  
  myPlot2 <- function(){   
    
    media2<-mean(df_subset_matematica()[[input$x2]],na.rm=T)
    
    limites_matematica<-reactive({range(c(kl_tudo[[input$x2]][kl_tudo$nmatematica>amostra_minima]
                                         ,kl_tudo[[input$y2]][kl_tudo$nmatematica>amostra_minima]),na.rm=T)
      
    })
    
    
    
    if (input$subgrupo2 != 'None'){
      if(input$nomes2==TRUE) 
        p2 <- ggplot(df_subset_matematica(),
                    aes_string(x=input$x2, y=input$y2,label="Nomes_municipios")) + geom_point(colour="black")+geom_text_repel()+
          xlim(range(df_subset_matematica()[[input$x2]]))+ylim(range(df_subset_matematica()[[input$y2]]))
      else 
        p2 <- ggplot(df_subset_matematica(),
                    aes_string(x=input$x2, y=input$y2,size=input$z2)) + geom_point(colour="black")+
          xlim(c(input$xlim_inf2,input$xlim_sup2))+ylim(c(input$ylim_inf2,input$ylim_sup2))
    }
    else
    {
      p2 <- ggplot(df_subset_matematica(),
                  aes_string(x=input$x2, y=input$y2)) + geom_point()+
        xlim(limites_x)+
        ylim(limites_y)
      
        #xlim(limites_matematica())+ylim(limites_matematica())
        #xlim(c(input$xlim_inf2,input$xlim_sup2))+ylim(c(input$ylim_inf2,input$ylim_sup2))
      
    }  
    
    p2 <-p2+
      geom_abline(intercept = 0, slope = 0,col='blue',size=1.5)+
      geom_vline(xintercept=0,col='blue',size=1.5)+
    
      geom_hline(yintercept=pontos_corte_kl[1],linetype="dashed",col='blue')+
      geom_hline(yintercept=pontos_corte_kl[2],linetype="dashed",col='blue')+
      geom_hline(yintercept=pontos_corte_kl[3],linetype="dashed",col='blue')+
      geom_hline(yintercept=pontos_corte_kl[4],linetype="dashed",col='blue')+
      annotate("text", label = "Baixa", y = pontos_corte_kl[1]-.15, x = limites_matematica()[1]*4/5, size=4, colour = "red")+
      annotate("text", label = "Media \n Baixa", y = mean(c(pontos_corte_kl[1],pontos_corte_kl[2])), x = limites_matematica()[1]*4/5, size=4, colour = "red")+
      annotate("text", label = "Media", y = mean(c(pontos_corte_kl[2],pontos_corte_kl[3])), x = limites_matematica()[1]*4/5, size=4, colour = "red")+
      annotate("text", label = "Media \n Alta", y = mean(c(pontos_corte_kl[3],pontos_corte_kl[4])), x = limites_matematica()[1]*4/5, size=4, colour = "red")+
      annotate("text", label = "Alta", y = pontos_corte_kl[4]+.75, x = limites_matematica()[1]*4/5, size=4, colour = "red")
    
    
      
      if (input$color2 != 'None')
      p2 <- p2 + aes_string(color=input$color2)
    
    if (input$color2_cap != 'None')
      p2 <- p2 + aes_string(color=input$color2_cap)
    
    
    if (input$color2_reg != 'None')
      p2 <- p2 + aes_string(color=input$color2_reg)
    
    
    if (input$color2_regIBGE != 'None')
      p2 <- p2 + aes_string(color=input$color2_regIBGE)+scale_color_manual(values=c('black','blue','green',
                                                                                     'red','yellow'))
    
    
    facets2 <- paste(input$facet_row2, '~.')
    if (input$facet_row2 != "None")
      p2 <- p2 + facet_grid(facets2)
    
    facets_cap2 <- paste(input$facet_row2_cap, '~.')
    if (input$facet_row2_cap != "None")
      p2 <- p2 + facet_grid(facets_cap2)
    
    
    facets_reg2 <- paste('~',input$facet_row2_reg)
    if (input$facet_row2_reg != "None")
      p2 <- p2 + facet_wrap(facets_reg2)
    
    
   
    
   
    if(input$municipio2 != ""){
      aux<-df_subset_matematica()[Nomes_municipios==input$municipio2]
      p2<-p2+geom_label_repel(data=aux,
                            label=aux[['Nomes_municipios']],
                            fontface = 'bold', color = 'black',
                            box.padding = 0.35, point.padding = 0.5,
                            segment.color = 'red')+
        geom_point(data=aux, size = 3, color = 'red')
    }
    
    
    p2
    
    
  }
  
  
  output$plot2 <- renderPlot({
    myPlot2()
  })
  
  
  
  output$info2 <- renderTable({
    if(input$municipio2 == ""){
      
      aux<-data.table(nearPoints(df_subset_matematica(), input$plot2_click,input$x2,input$y2,threshold = 5,maxpoints = 1))
      
      
      
      
      if(input$x2 == 'KL.Matematica_hiato_raca'){
        
        if (input$color2_regIBGE == 'None'){
          
          aux2<-cbind(aux[,c('Nomes_municipios','UF')],
                      aux[[input$x2]], aux[[input$y2]],
                      aux[,c('nmatematica','nPreto_Matematica','nBranco_Matematica')])
           colnames(aux2)<-c("Nome do Municipio","UF",input$x2,input$y2,"Numero de alunos",
                            'nPreto','nBranco')
        }
        
        else{
          
          aux2<-cbind(aux[,c('Nomes_municipios','UF','RIC10')],
                      aux[[input$x]], aux[[input$y]],
                      aux[,c('nmatematica','nPreto_Matematica','nBranco_Matematica')])
          
          
          
          colnames(aux2)<-c("Nome do Municipio","UF","Centralidade",input$x2,input$y2,"Numero de alunos",
                            'nPreto','nBranco')
        }
        
        
        
      }
      
      if(input$x2 == 'KL.Matematica_hiato_sexo'){
        
        if (input$color2_regIBGE == 'None'){
          
          aux2<-cbind(aux[,c('Nomes_municipios','UF')],
                      aux[[input$x2]], aux[[input$y2]],
                      aux[,c('nmatematica','nFeminino_Matematica','nMasculino_Matematica')])
          
          colnames(aux2)<-c("Nome do Municipio","UF",input$x2,input$y2,"Numero de alunos",
                            'nMeninas','nMeninos')
          
        }
        
        else{
          
          aux2<-cbind(aux[,c('Nomes_municipios','UF')],
                      aux[[input$x2]], aux[[input$y2]],
                      aux[,c('nmatematica','nFeminino_Matematica','nMasculino_Matematica')])
          
          colnames(aux2)<-c("Nome do Municipio","UF","Centralidade",input$x2,input$y2,"Numero de alunos",
                            'nMeninas','nMeninos')
          
          
        }  
      }
      
      
      
      if(input$x2 == 'KL.Matematica_hiato_NSE'){
        
        
        if (input$color2_regIBGE == 'None'){
          aux2<-cbind(aux[,c('Nomes_municipios','UF')],
                      aux[[input$x2]], aux[[input$y2]],
                      aux[,c('nmatematica','nNSE1_Matematica','nNSE5_Matematica')])
          
          colnames(aux2)<-c("Nome do Municipio","UF",input$x2,input$y2,"Numero de alunos",
                            'nNSE1','nNSE5')
        }
        else{
          
          aux2<-cbind(aux[,c('Nomes_municipios','UF','RIC10')],
                      aux[[input$x]], aux[[input$y]],
                      aux[,c('nmatematica','nNSE1_Matematica','nNSE5_Matematica')])
          
          colnames(aux2)<-c("Nome do Municipio","UF","Centralidade",input$x2,input$y2,"Numero de alunos",
                            'nNSE1','nNSE5')
          
        }
        
      }
    }
    else{
      aux<-df_subset_matematica()[Nomes_municipios==input$municipio2]
      aux2<-cbind(aux[,c('Nomes_municipios','UF')],
                  aux[[input$x2]],
                  aux[[input$y2]],
                  aux[,c('nmatematica')]
      )
      
      if(input$x2=='KL.Matematica_hiato_raca'){
        aux2<-cbind(aux2,aux[,c('nPreto_Matematica','nBranco_Matematica')]) 
        colnames(aux2)<-c("Municipio","UF",input$x2,input$y2,
                          "Numero de alunos",'nPreto','nBranco')
      }
      
      if(input$x2=='KL.Matematica_hiato_NSE'){
        aux2<-cbind(aux2,aux[,c('nNSE1_Matematica','nNSE5_Matematica')]) 
        colnames(aux2)<-c("Municipio","UF",input$x2,input$y2,
                          "Numero de alunos",'nNSE1','nNSE5')
      }
      
      
      if(input$x2=='KL.Matematica_hiato_sexo'){
        aux2<-cbind(aux2,aux[,c('nFeminino_Matematica','nMasculino_Matematica')]) 
        colnames(aux2)<-c("Municipio","UF",input$x2,input$y2,
                          "Numero de alunos",'nFeminino','nMasculino')
      }
      
      
      
      
      
    }
    
    
    return(aux2)
  })
  
 
  output$contingencia2 <- renderTable({
    indice_KL<-factor(findInterval(df_subset_matematica()[[input$y]],c(-100,pontos_corte_kl,100)),
                      levels=1:5)
    levels(indice_KL)<-c("Baixa","Media Baixa","Media","Media Alta","Alta")
    
    indice_hiato<-factor(findInterval(df_subset_matematica()[[input$x]],c(-100,pontos_corte_kl,100)),
                         levels=1:5)
    levels(indice_hiato)<-c("Baixa","Media Baixa","Media","Media Alta","Alta")
    
    #contigencia<-table(indice_hiato)
    contigencia2<-addmargins(table(indice_KL,indice_hiato))
    colnames(contigencia2)<-c("Baixa (Hiato)","Media Baixa","Media","Media Alta","Alta","Total")
    rownames(contigencia2)<-c("Baixa","Media Baixa","Media","Media Alta","Alta (KL) ","Total")
    contigencia2<-contigencia2[c(5,4,3,2,1),]
    #contigencia<-contigencia[,c(5,4,3,2,1)]
    as.data.frame.matrix(contigencia2)
    
  }, include.rownames=TRUE)
  
  ## call the plot function when downloading the image
  output$down2 <- downloadHandler(
    filename = function() {
      paste(input$y2,input$x2,".pdf",sep="")
    },
    content = function(file) {
      pdf(file)
      print(myPlot2())
      dev.off()
    }
  )  
  
  
}



library(httr)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load R packages
library(shiny)
library(shinythemes)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(

                   "Visualizador de Dados da HIDROWEB v3.2.0",
                   tabPanel("Série Histórica",
                            sidebarPanel(
                               tags$h3("Dados:"),
                               textInput("txt1", "Estação:", ""),
                               actionButton("serie_historica", "Gerar Série Histórica", icon=icon("chart-area"))
                            ), # sidebarPanel
                            mainPanel(
                               h1("Série Histórica"),
                               plotOutput("plot1")
                            ) # mainPanel

                   ), # Navbar 1, tabPanel
                   tabPanel("Navbar 2", "This panel is intentionally left blank"),
                   tabPanel("Navbar 3", "This panel is intentionally left blank")
                   #
                ) # navbarPage
) # fluidPage

# Define server function
server <- function(input, output) {

  observeEvent(input$serie_historica, {

     # setwd("download")
     download_folder = "download"

     if (file.exists(download_folder)){
       setwd(download_folder)
     }

     estacao = input$txt1
     arquivo_estacao = paste0(estacao, ".zip")
     data_arquivo_esacao = as.Date(as.POSIXct(file.mtime(arquivo_estacao), 'GMT'))

     if(!file.exists(arquivo_estacao) || Sys.Date() != data_arquivo_esacao){
       baseurl = "https://www.snirh.gov.br/hidroweb/rest/api/documento/convencionais?tipo=&documentos="

       # tipo=1 arquivo access *.mdb
       # tipo=2 arquivo texto  *.txt
       # tipo=3 arquivo excel  *.csv
       tipo = 3

       #substituindo o tipo
       baseurl = gsub("tipo=",paste0("tipo=",tipo),baseurl)
       baseurl_est = paste0(baseurl,estacao[1])

       # value="8" para Cotas (cm)
       # value="9" para VazÃµes (mÂ³/s)
       # value="10" para Chuva
       # value="12" para Qualidade da Ãgua
       # value="13" para Resumo de Descarga
       # value="16" para Perfil Transversal

       # #Conexao
       r = POST(url = baseurl_est, body = list(cboTipoReg = "8"), encode = "form")
       if (r$status_code == 405) {
         cont = content(r, as = "text", encoding="ISO-8859-1")
         download.file(baseurl_est, paste0(estacao[1], ".zip"), mode = "wb")
       }
     }

      unzip(paste0(estacao, ".zip"))
      unzip(paste0("cotas_C_", estacao, ".zip"))

      tabela_original <-
         read.csv2(paste0("cotas_C_", estacao, ".csv"), header=FALSE, sep = ";", skip = 14, skipNul=TRUE)


      serie_anual <- setDT(tabela_original)[, .(
         data = dmy(V3),
         maxima_mensal = V7,
         minima_mensal = V8,
         media_mensal = V9
      )]

      serie_anual <- distinct(serie_anual, data, .keep_all= TRUE)

      output$plot1 <- renderPlot({
        ggplot(data = serie_anual, aes(x=data, y=media_mensal))+
          # geom_point()+
          geom_area(fill = "#73b8f5")+
          geom_line(size= 0.5)+
          scale_x_date(date_labels = "%Y", date_breaks = "1 year",expand = c(0, 0))+
                     # limits = as.Date(c("1985-01-01","2020-01-01")),
          scale_y_continuous(expand = c(0, 0))+

          theme_classic(
            base_size = 14,
          )+
          ggtitle(paste0("Estação: ", estacao))+
          theme(
            axis.text.x=element_text(angle=60, hjust=1),
            axis.title.y.right  = element_blank(),
            panel.grid.major.y = element_line(),
            plot.title = element_text(size = 16, hjust = 0.5)
          )+
          # geom_area(mapping = aes(dist = ifelse(dist>20000 & dist< 30000 , dist, 0)), fill = "red") +
          # scale_x_discrete(breaks=c(10000, 20000, 40000))+
          # scale_color_manual(v0alues=palette$hex, breaks = waiver(), na.value = "blank")+
          labs(x = "Data", y = "Nível da Água (mm)")
      })
  })

}

# Create Shiny object
shinyApp(ui = ui, server = server)






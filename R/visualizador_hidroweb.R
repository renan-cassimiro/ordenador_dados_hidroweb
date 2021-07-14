library(httr)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

baseurl = "https://www.snirh.gov.br/hidroweb/rest/api/documento/convencionais?tipo=&documentos="
estacoes = c(13700000)

setwd("download")
destino = getwd()

# tipo=1 arquivo access *.mdb
# tipo=2 arquivo texto  *.txt
# tipo=3 arquivo excel  *.csv
tipo = 3

#substituindo o tipo
baseurl = gsub("tipo=",paste0("tipo=",tipo),baseurl)

baseurl_est = paste0(baseurl,estacoes[1])

# value="8" para Cotas (cm)
# value="9" para Vazões (m³/s)
# value="10" para Chuva
# value="12" para Qualidade da Água
# value="13" para Resumo de Descarga
# value="16" para Perfil Transversal

# #Conexao
r = POST(url = baseurl_est, body = list(cboTipoReg = "8"), encode = "form")
if (r$status_code == 405) {
   cont = content(r, as = "text", encoding="ISO-8859-1")
   download.file(baseurl_est, paste0(estacoes[1], ".zip"), mode = "wb")
}

unzip(paste0(estacoes[1], ".zip"))
unzip(paste0("cotas_C_", estacoes[1], ".zip"))

tabela_original <- 
   read.csv2(paste0("cotas_C_", estacoes[1], ".csv"), header=FALSE, sep = ";", skip = 14, skipNul=TRUE)


serie_anual <- setDT(tabela_original)[, .(
      data = dmy(V3),
      maxima_mensal = V7,
      minima_mensal = V8,
      media_mensal = V9  
   )]

serie_anual <- distinct(serie_anual, data, .keep_all= TRUE)


ggplot(data = serie_anual, aes(x=data, y=media_mensal))+
   # geom_point()+
   geom_area(fill = "#73b8f5")+
   geom_line(size= 0.5)+
   theme_classic(
      base_size = 14
   )+
   ggtitle(paste0("Estação: ", estacoes[1]))+
   # theme(
   #    axis.title.y.right  = element_blank(),
   #    panel.grid.major.y = element_line(),
   #    plot.title = element_text(size = 16, hjust = 0.5)
   # )+
   # geom_area(mapping = aes(dist = ifelse(dist>20000 & dist< 30000 , dist, 0)), fill = "red") +   
   # scale_x_discrete(breaks=c(10000, 20000, 40000))+
   # scale_color_manual(v0alues=palette$hex, breaks = waiver(), na.value = "blank")+
   labs(x = "Data", y = "Nível da Água (mm)")





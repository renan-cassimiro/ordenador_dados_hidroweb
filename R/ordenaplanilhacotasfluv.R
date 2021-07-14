'#' Ordernar planilha de cotas fluviométricas
#' Função ordernar planilha de cotas fluviométricas por dia
#' Para utilizar: ordenaplanilhacotasfluv()


tabela_original <- read.csv2("mock.csv", header=FALSE, sep = ";")
datas <- vector()

for(cotas in tabela_original){
  print(cotas[1])

}

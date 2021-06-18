
getDados <- function(){
  progress <- shiny::Progress$new(); progress$set(message = "Criando conexão", value = 1)
  getSKUS_MARA(); getFiliais(); getProdutos()
 
  progress$set(message = "Buscando informações da decisão S&OP", value = 1)
  rateio_filial <<- getDecisaoRateio(getDecisao()); 

  progress$set(message = "Buscando informações da previsão semanal", value = 1)
  previsao = getDemandaSemanal(); previsao_semanal <<- getPrevisaoSemanal(previsao)
  
  progress$set(message = "Buscando informações do volume faturado", value = 1)
  getFaturadoLiquido();
  
  progress$set(message = "Buscando informações da carteira", value = 1)
  getCarteira()
  
  progress$set(message = "Buscando informações do vendido", value = 1)
  getVendido()
  
  progress$set(message = "Buscando informações do volume planejado e produzido", value = 1)
  getProducao()
    
  progress$set(message = "Buscando informações do estoque", value = 1)
  getEstoqueTransferencia();

  progress$close()
}

    
convertTo <- function(dados, convertTo){
  if (convertTo == "Ton") dados[sapply(dados, is.numeric)] = dados[sapply(dados, is.numeric)]/1000
  return(dados)
}

convertDataToTurn <-function(producao){
  producao$DATE = as.Date(as.character(producao$MFGORDERCONFIRMATIONENTRYDATE), format = "%Y%m%d")
  producao$DATA_TURNO = producao$DATE
  for (i in 1:nrow(producao)){
    if (producao[i,]$MFGORDERCONFIRMATIONENTRYTIME < 61500) producao[i,]$DATA_TURNO =  producao[i,]$DATE - days(1)
  }
  producao = producao[month(producao$DATA_TURNO) %in% month(periodo),]
  return(producao)
}

getOrderColumns <- function(dados){
  dados = (data.frame(dados[,colnames(dados) %in% c('COD', 'SKU', 'FAMILIA', 'FAMILIA_SOP', 'MERCADO', 'EMBALAGEM', 'MARCA', 'FILIAL', 'FILIAL_DESC')],
                     dados[,grepl('DECISAO', colnames(dados))],                             
                     dados[,colnames(dados) %in% c("FATURADO LÍQUIDO", "VENDIDO", "CARTEIRA", "% ATINGIMENTO CONSENSO", "FATURADO ESTIMADO", "FATURADO ESTIMADO VS CONSENSO")],
                     dados[,colnames(dados) %in% c("ESTOQUE SUFICIENTE", "ESTOQUE TRANSITO", "ESTOQUE", "ESTOQUE QUALIDADE", "ESTOQUE LÍQUIDO", "DIAS COBERTURA ESTOQUE")],
                     dados[,grepl('PROD', colnames(dados))], check.names = FALSE))
  colnames(dados)[grepl("DECISAO", colnames(dados))] =  "S&OP"
  return(dados)
}

getCalcColumns <- function(dados){
  dados = getEstoqueLiquido(dados); 
  dados = getDiasCoberturaEstoque(dados);
  dados = getAtingimentoFaturado(dados);
  dados = getFaturadoEstimado(dados);
  return(getOrderColumns(dados))
}

getCalcTotais <- function(dados){
  dados = dados %>% adorn_totals("row");
  dados[nrow(dados),]$`% ATINGIMENTO CONSENSO` = ((sum(dados$`FATURADO LÍQUIDO`, na.rm = TRUE) + sum(dados$CARTEIRA, na.rm = TRUE))*100)/sum(dados$`S&OP`, na.rm = TRUE)
  dados[nrow(dados),]$`DIAS COBERTURA ESTOQUE` = ((sum(dados$ESTOQUE, na.rm = TRUE) + sum(dados$`ESTOQUE TRANSITO`, na.rm = TRUE))*as.numeric(days_in_month(today())))/sum(dados$`S&OP`, na.rm = TRUE)
  return(dados)
}

getFaturadoEstimado <- function(dados){
  dados$`FATURADO ESTIMADO` = (dados$`FATURADO LÍQUIDO`*100)/((sum(dados$`FATURADO LÍQUIDO`)*100)/sum(dados$`DECISAO S&OP`))
  dados$`FATURADO ESTIMADO VS CONSENSO` = dados$`FATURADO ESTIMADO` - dados$`DECISAO S&OP`
  dados$`ESTOQUE SUFICIENTE` = (dados$ESTOQUE + dados$`ESTOQUE TRANSITO`) - (dados$`FATURADO ESTIMADO` - dados$`FATURADO LÍQUIDO`)
  return(dados)
}

getAtingimentoFaturado <- function(dados){
  dados$`% ATINGIMENTO CONSENSO` = ((dados$`FATURADO LÍQUIDO` + dados$CARTEIRA)*100)/dados$`DECISAO S&OP`
  return(dados)
}


getDiasCoberturaEstoque <- function(dados){
  dados$`DIAS COBERTURA ESTOQUE` = ((dados$ESTOQUE + dados$`ESTOQUE TRANSITO`)*as.numeric(days_in_month(today())))/dados$`DECISAO S&OP`
  return(dados)
}

getEstoqueLiquido <- function(dados){
  dados$`ESTOQUE LÍQUIDO` = (dados$ESTOQUE + dados$`ESTOQUE TRANSITO`) - dados$CARTEIRA
  return(dados)
}

convertFilial <- function(data){
  
  if (nrow(data[is.na(data$FILIAL),]) != 0){
    data[is.na(data$FILIAL),]$FILIAL = 1101; data[is.na(data$FILIAL_DESC),]$FILIAL_DESC = "1101 - MARILAN MATRIZ"
  }
  
  if (nrow(data[data$FILIAL %in% 1116,]) != 0){
    data[data$FILIAL %in% 1116,]$FILIAL = 1101; data[data$FILIAL_DESC %in% "1116 - MARILAN RIO GRANDE DO SUL",]$FILIAL_DESC = "1101 - MARILAN MATRIZ"
  }
  
  data = data %>% group_by(COD, SKU, FILIAL, FILIAL_DESC, FAMILIA, FAMILIA_SOP, MERCADO, EMBALAGEM, MARCA) %>% summarise_if(is.numeric, sum, na.rm = TRUE)
  return(data)
}

formatData <- function(table){
  table = formatCOD(table)
  table = formatSKU(table)
  return(table)
}

formatCOD <- function(table){
  table$COD = str_replace(table$COD, "0000000000000000", "")
  table$COD = str_replace(table$COD, "000000000000000", "")
  table$COD = str_replace(table$COD, "00000000000000", "")
  table$COD = str_replace(table$COD, "0000000000000", "")
  table$COD = str_replace(table$COD, "NE", "")
  return(table)
}

formatSKU <- function(table){
  table$SKU = ""; table$FAMILIA = "" ; table$FAMILIA_SOP = ""; table$MERCADO = ""; table$EMBALAGEM = ""; table$MARCA = ""; table$FILIAL_DESC = "";
  for (i in 1:nrow(table)){
    if (nrow(produtos[produtos$id_produto %in% table[i,]$COD,])!=0){
      table[i,]$SKU = produtos[produtos$id_produto %in% table[i,]$COD,]$sku;
      table[i,]$FAMILIA = produtos[produtos$id_produto %in% table[i,]$COD,]$familia;
      table[i,]$FAMILIA_SOP = produtos[produtos$id_produto %in% table[i,]$COD,]$familia_sop;
      table[i,]$MERCADO = produtos[produtos$id_produto %in% table[i,]$COD,]$mercado;
      table[i,]$EMBALAGEM = produtos[produtos$id_produto %in% table[i,]$COD,]$embalagem;
      table[i,]$MARCA = produtos[produtos$id_produto %in% table[i,]$COD,]$marca;
      if (nrow(filiais[filiais$id_filial %in% table[i,]$FILIAL,])!=0) table[i,]$FILIAL_DESC = filiais[filiais$id_filial %in% table[i,]$FILIAL,]$filial_desc
    }
  }
  return(table)
}

trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)
}

getDecisaoRateio <- function(rateio_filial){
  if(TRUE %in% is.na(rateio_filial$PARTICIPACAO)) rateio_filial[is.na(rateio_filial$PARTICIPACAO),]$PARTICIPACAO = 1
  if(TRUE %in% is.na(rateio_filial$CONSENSO)) rateio_filial[is.na(rateio_filial$CONSENSO),]$CONSENSO = 0
  rateio_filial$`DECISAO S&OP`  = rateio_filial$`CONSENSO` * rateio_filial$PARTICIPACAO 
  rateio_filial = rateio_filial[,c('COD','SKU', 'FAMILIA', 'FAMILIA_SOP', 'MERCADO', 'EMBALAGEM', 'MARCA', 'FILIAL', 'FILIAL_DESC', 'DECISAO S&OP')]
  rateio_filial$`DECISAO S&OP` = rateio_filial$`DECISAO S&OP`; 
  return(rateio_filial)
}

#PREVISAO SEMANAL
getPrevisaoSemanal<- function(previsao_semanal){
  previsao_semanal$VALOR = previsao_semanal$VALOR; 
  previsao_semanal$DATA = format(previsao_semanal$DATA, "%d/%m"); 
  previsao_semanal = reshape(data.frame(previsao_semanal), timevar = "DATA", idvar = c("COD", "SKU", "FAMILIA", "FAMILIA_SOP", "MERCADO",  'EMBALAGEM', 'MARCA', 'FILIAL', 'FILIAL_DESC'), direction = "wide")
  colnames(previsao_semanal) = gsub("VALOR.", "DECISAO SEMANAL ", colnames(previsao_semanal))
  return(previsao_semanal)
}

formatCargaMRP <- function(producao_planejado){
  colnames(producao_planejado)[1] = "COD"
  producao_planejado$COD = toupper(producao_planejado$COD)
  producao_planejado = producao_planejado %>% gather(data, valor,colnames(producao_planejado)[3:length(colnames(producao_planejado))])
  producao_planejado$data = excel_numeric_to_date(as.numeric(producao_planejado$data))
  producao_planejado[is.na(producao_planejado)] = 0; 
  colnames(producao_planejado) = toupper(colnames(producao_planejado))
  return(producao_planejado[,c("COD", "DATA", "VALOR")])
}

formatCargaDemanda <- function(demanda_parcial){
  rateio = checkLastDemanda(demanda_parcial); colnames(demanda_parcial) = toupper(colnames(demanda_parcial)); 
  colnames(demanda_parcial)[length(colnames(demanda_parcial))] = 'VALOR'; demanda_parcial$COD = toupper(demanda_parcial$COD)
  
  if (TRUE %in% grepl(pattern = 'FILIAL|CENTRO', x = colnames(demanda_parcial))){
    cod_alter = paste0(rateio$COD, "|", rateio$FILIAL) %in% paste0(demanda_parcial$COD, "|", demanda_parcial$FILIAL)
    for (i in (1:nrow(rateio))[cod_alter]){
      rateio[i,]$VALOR = demanda_parcial[(demanda_parcial$COD %in% rateio[i,]$COD) & (demanda_parcial$FILIAL %in% rateio[i,]$FILIAL),]$VALOR
    }
  }else{
    cod_alter = rateio$COD %in% demanda_parcial$COD
    for (i in (1:nrow(rateio))[cod_alter]){
      rateio[i,]$VALOR = rateio[i,]$PARTICIPACAO *  demanda_parcial[demanda_parcial$COD %in% rateio[i,]$COD,]$VALOR
    }
  }
  return(rateio[,c("COD", "DATA", 'FILIAL', "VALOR")])
}

checkLastDemanda <- function(demanda_parcial){
  rateio = getDemandaSemanal();
  if (nrow(rateio) == 0 ){ rateio = getDecisao(); rateio = getRateio(rateio); rateio$VALOR = rateio$`DECISAO RATEIO`; } 
  else{ 
    rateio$PARTICIPACAO = 0;
    for (i in (1:nrow(rateio))){
      if (sum(rateio[rateio$COD %in% rateio[i,]$COD,]$VALOR)!=0) rateio[i,]$PARTICIPACAO = rateio[i,]$VALOR/sum(rateio[rateio$COD %in% rateio[i,]$COD,]$VALOR) 
    }
  }
  colnames(rateio) = toupper(colnames(rateio)); rateio$DATA = unique(demanda_parcial$DATA);
  return(rateio)
}

getSKUS_MARA <- function(){
  con = getConnection()
  parms <- list('DELIMITER' = ';', 'FIELDS' = list(FIELDNAME = list('MATNR')), 'OPTIONS' = list(TEXT = list("MTART = 'FERT'")), 'QUERY_TABLE' = 'MARA')
  SKU_MARA <<-  strsplit(RSAPInvoke(con, "RFC_READ_TABLE", parms)$DATA$WA, "\n")
  RSAPClose(con)
}

getFiliais <- function(){
  con <- tryCatch({ dbConnect(RMariaDB::MariaDB(), host = "192.168.235.20", user = "server", password="R*proj2018", dbname='consenso')   }, error = function(err) { return("") })
  res <- dbSendQuery(con, paste0("SELECT * from filial;"))
  filiais <<- dbFetch(res); 
}

getProdutos <- function(){
  con <- tryCatch({ dbConnect(RMariaDB::MariaDB(), host = "192.168.235.20", user = "server", password="R*proj2018", dbname='consenso')   }, error = function(err) { return("") })
  res <- dbSendQuery(con, paste0("SELECT * from produto;"))
  produtos <<- dbFetch(res); 
}

getConnection <- function(){
  return(RSAPConnect(ashost="s4prd.marilan.com.br", sysnr="06", client="300", user="S&PSERVICO", passwd="lgkmvjnhdgcflo128.*", lang="EN",  trace="0", lcheck="0"))
}

getEstoqueDepositos <- function(){
  con = getConnection()
  parms <- list('I_MCHB_KEY' = list(MATNR = list(), WERKS = list(), LGORT = list()))
  parms$I_MCHB_KEY$MATNR = rep(SKU_MARA, 11)
  parms$I_MCHB_KEY$WERKS = c(rep(list("1101"), length(SKU_MARA)), rep(list("1103"), length(SKU_MARA)), rep(list("1107"), length(SKU_MARA)), rep(list("1108"), length(SKU_MARA)), rep(list("1109"), length(SKU_MARA)), rep(list("1110"), length(SKU_MARA)), rep(list("1116"), length(SKU_MARA)), rep(list("1117"), length(SKU_MARA)), rep(list("1118"), length(SKU_MARA)), rep(list("1119"), length(SKU_MARA)), rep(list("1201"), length(SKU_MARA)))
  parms$I_MCHB_KEY$LGORT = rep(list("2001"), length(SKU_MARA)*11)
  estoque <<- RSAPInvoke(con, "ZBF_READ_V_STOCK", parms)$C_MDBF
  RSAPClose(con)
  estoque <<- estoque %>% group_by('COD' = trim(MATNR), 'FILIAL' = WERKS) %>% summarise('ESTOQUE' = sum(as.numeric(LABST)), "QUALIDADE" =  sum(as.numeric(INSME)))
  estoque <<- formatData(estoque); estoque <<- estoque[estoque$ESTOQUE!=0,]
  estoque <<- estoque %>% group_by(COD,  SKU, FAMILIA, FAMILIA_SOP, MERCADO, EMBALAGEM, MARCA, FILIAL,FILIAL_DESC) %>% summarise('ESTOQUE' = sum(as.numeric(ESTOQUE))/1000, 'QUALIDADE' = sum(as.numeric(QUALIDADE))/1000)
}

getFaturadoLiquido <- function(){
   con = getConnection()
   parms <- list('DELIMITER' = ';', 'FIELDS' = list(), 'OPTIONS' = list(TEXT = list(paste0("Perio = '", paste0(format(periodo,"%Y"), '0', format(periodo,"%m")), "'"))), 'QUERY_TABLE' = 'ZCSDCE11000')
   res <- RSAPInvoke(con, "RFC_READ_TABLE", parms); RSAPClose(con)
   
   faturado_f = data.frame(res$DATA, colsplit(res$DATA$WA, split = ";", names = sub("\\s+$", "", res$FIELDS$FIELDNAME)), stringsAsFactors = FALSE)
   if (typeof(faturado_f$VV002) == 'factor') faturado_f$VV002 =  unfactor(faturado_f$VV002)
   
   for (i in 1:nrow(faturado_f)){
     if (grepl("-", faturado_f[i,]$VV002)){
       faturado_f[i,]$VV002 = (paste0("-", trim(stri_replace_last_fixed(as.character(faturado_f[i,]$VV002)[grepl("-", as.character(faturado_f[i,]$VV002))], '-', ''))))
     }
   }
   faturado <<- faturado_f %>% group_by("COD" = trim(ARTNR), 'FILIAL' = WERKS) %>% summarise('FATURADO' = sum(as.numeric(VV002)))
   faturado <<- faturado[!(faturado$COD %in% ''),]; faturado <<- faturado[faturado$FILIAL != '1201',]
   faturado <<- formatData(faturado);
   faturado <<- faturado %>% group_by(COD, FILIAL, SKU, FAMILIA, FAMILIA_SOP, MERCADO, EMBALAGEM, MARCA, FILIAL_DESC) %>% summarise('FATURADO LÍQUIDO' = sum(FATURADO))
 }

getCarteira <- function(){
  con = getConnection()
  parms <- list(P_BUKRS = list('1000'), P_MANDT = list('300'), P_VKORG = list('1100'))
  faturado_f = RSAPInvoke(con, "ZFSD_CARTEIRA", parms)$P_RESULT; 
  
  parms <- list(P_BUKRS = list('1000'), P_MANDT = list('300'), P_VKORG = list('1101'))
  faturado_f = rbind(faturado_f, RSAPInvoke(con, "ZFSD_CARTEIRA", parms)$P_RESULT); RSAPClose(con);
  
  carteira <<- faturado_f %>% group_by("COD" = trim(MATNR), 'FILIAL' = WERKS) %>% summarise('CARTEIRA' = sum(as.numeric(VV003)))
  carteira <<- carteira[!(carteira$COD %in% ''),]; carteira <<- formatData(carteira);
}

getVendido <- function(){
  con = getConnection()
  parms <- list('DELIMITER' = ';', 'FIELDS' = list(FIELDNAME = list("MATERIAL", "PRODUCTIONPLANT", "VOLUME_VENDIDO_KG")), 
                'OPTIONS' = list(TEXT = list(paste0("YEARMONTH = '", paste0(format(periodo,"%Y"), format(periodo,"%m")), "'"))), 'QUERY_TABLE' = 'ZSDCUBINDVENDAS')

  res <- RSAPInvoke(con, "RFC_READ_TABLE", parms); RSAPClose(con);
  
  if (is.null(nrow(res$DATA))){
    vendido <<- formatData(data.frame("COD" = 258, 'FILIAL' = 1101, 'VENDIDO' = 0))
  }else{
    vendido_v = data.frame(res$DATA, colsplit(res$DATA$WA, ";", names = sub("\\s+$", "", res$FIELDS$FIELDNAME)))
    vendido <<- vendido_v %>% group_by("COD" = trim(MATERIAL), 'FILIAL' = PRODUCTIONPLANT) %>% summarise('VENDIDO' = sum(as.numeric(VOLUME_VENDIDO_KG)))
    vendido <<- vendido[!(vendido$COD %in% ''),]; vendido <<- formatData(vendido);vendido <<- vendido[!(vendido$FILIAL %in% c('1201')),]
  }
}


getEstoqueTransferencia <- function(){
  con = getConnection()
  estoque_transito <<- data.frame("COD" = as.character(), "FILIAL" = as.double(), "ESTOQUE" = as.double(), "TRANSITO" = as.double())
  parms <- list('P_MATNR' = list(SIGN = list(), OPTION = list(), LOW = list(), HIGH = list()), 'P_WERKS' = list(SIGN = list(), OPTION = list(), LOW = list(), HIGH = list()))
  parms$P_MATNR$SIGN =  rep(list("I"), length(SKU_MARA)); parms$P_MATNR$OPTION = rep(list("EQ"), length(SKU_MARA)); parms$P_MATNR$LOW = (SKU_MARA); parms$P_MATNR$HIGH = (SKU_MARA)
  parms$P_WERKS$SIGN = list('I'); parms$P_WERKS$OPTION = list('BT'); parms$P_WERKS$LOW = list('1101'); parms$P_WERKS$HIGH = list('1201')
  
  res <- RSAPInvoke(con, "ZFMMM_CMATSTOCKACT", parms)$L_STOCK; RSAPClose(con)
  estoque = res %>% group_by("COD" = trim(MATERIAL), "FILIAL" = PLANT, "TIPO" = INVENTORYSTOCKTYPE) %>% summarise(valor = sum(as.numeric(MATLWRHSSTKQTYINMATLBASEUNIT)))
  estoque_transito <<- spread(estoque, key = "TIPO", value = "valor");  estoque_transito <<- formatData(estoque_transito); 
  estoque_transito <<- estoque_transito[!(estoque_transito$COD %in% c('549', '7003', '7004', '3728')),]; 
  estoque_transito <<- estoque_transito[!(estoque_transito$FILIAL %in% c('1201')),]
  estoque_transito$FILIAL <<- as.numeric(estoque_transito$FILIAL)
  estoque_transito <<- estoque_transito %>% group_by(COD, FILIAL, SKU, FAMILIA, FAMILIA_SOP, MERCADO, EMBALAGEM, MARCA, FILIAL_DESC) %>% summarise('ESTOQUE TRANSITO' = sum(as.numeric(`06`)), 
                                                                                                                                                   'ESTOQUE' = sum(as.numeric(`01`)),
                                                                                                                                                   'ESTOQUE QUALIDADE' = sum(as.numeric(`02`)))
}

getEstoqueTransferenciaAux <- function(){
  con = getConnection()
  FILIAIS = c("1103", "1107", "1108", "1109", "1110", "1116", "1117", "1118", "1119")
  estoque_transito <<- data.frame("COD" = as.character(), "FILIAL" = as.double(), "TRANSITO" = as.double())
  cl <- makeCluster(4); registerDoParallel(cl)
  
  estoque_transito <<-
    foreach(sku=SKU_MARA, .combine='rbind') %:%
    foreach(filial=FILIAIS, .combine='rbind') %do% {
      parms <- list(MATERIAL  = list(sku), PLANT = list(filial))
      estoque_t = RSAPInvoke(con, "BAPI_MATERIAL_STOCK_REQ_LIST", parms)
      data.frame("COD" = trim(sku), "FILIAL" = filial, "TRANSITO" = estoque_t[["MRP_STOCK_DETAIL"]][["STCK_IN_TRANSIT"]])
    }
  stopImplicitCluster(); RSAPClose(con)
  estoque_transito <<- formatData(estoque_transito); estoque_transito <<- estoque_transito[estoque_transito$TRANSITO!=0,]
  estoque_transito <<- estoque_transito %>% group_by(COD, FILIAL, SKU, FAMILIA, FAMILIA_SOP, MERCADO, EMBALAGEM, MARCA, FILIAL_DESC) %>% summarise('TRANSITO' = sum(as.numeric(TRANSITO))/1000)
}

getProducao <- function(){
  con = getConnection()

  parms <- list('DELIMITER' = ';', 'FIELDS' = list(FIELDNAME = list('MATERIAL', 'CONFIRMATIONTOTALQUANTITY', 'MFGORDERCONFIRMATIONENTRYDATE', 'MFGORDERCONFIRMATIONENTRYTIME')), 'OPTIONS' = list(TEXT = list(
               paste0("MFGORDERCONFIRMATIONENTRYDATE BETWEEN '", paste0(format(periodo,"%Y"), format(periodo,"%m"), '01'), "' AND '", paste0(format(periodo,"%Y"), format(periodo,"%m"), '31'), "'"))),
               'QUERY_TABLE' = 'IPPMFGORDOPCONFC')

  res <- RSAPInvoke(con, "RFC_READ_TABLE", parms); RSAPClose(con)
  producao_p = data.frame(res$DATA, colsplit(res$DATA$WA, ";", names = sub("\\s+$", "", res$FIELDS$FIELDNAME)))
  producao_p = convertDataToTurn((producao_p))
  producao_real <<- producao_p %>% group_by("COD" = trim(MATERIAL), 'FILIAL' = '1101', 'DATA' = ymd(producao_p$DATA_TURNO)) %>% summarise('REAL' = sum(as.numeric(CONFIRMATIONTOTALQUANTITY)))
  
  producao_real <<- formatData(producao_real); producao_real <<- producao_real[producao_real$REAL !=0,]; 
  producao_real <<- producao_real[producao_real$COD %in% produtos$id_produto,]
  producao_real <<- producao_real %>% group_by(COD, FILIAL, SKU, FAMILIA, FAMILIA_SOP, MERCADO, EMBALAGEM, MARCA, FILIAL_DESC, DATA) %>% summarise('REAL' = sum(as.numeric(REAL)))
  
  for (i in 1:nrow(producao_real)) {
     producao_real[i,]$REAL <<- producao_real[i,]$REAL * produtos[produtos$id_produto %in% producao_real[i,]$COD,]$volume_caixa
  }
  
  producao_real$REAL <<- producao_real$REAL; producao_real$DATA <<- format(producao_real$DATA, "%d/%m")
  #PRODUCAO PLANEJADO
  con <- tryCatch({ dbConnect(RMariaDB::MariaDB(), host = "192.168.235.20", user = "server", password="R*proj2018", dbname='consenso')  }, error = function(err) { return("") })
  data = date(periodo); day(data) = 1; 
  res <- dbSendQuery(con, paste0("SELECT produto.id_produto as COD, produto.sku as SKU, produto.familia as FAMILIA, produto.FAMILIA_SOP as FAMILIA_SOP, produto.mercado as MERCADO,
                                  produto.embalagem as EMBALAGEM, produto.marca as MARCA,
                                  data_producao as DATA, valor as PLAN FROM producao_planejado INNER JOIN produto ON producao_planejado.produto = produto.id_produto
                                 WHERE data_producao BETWEEN '", data, "' AND '",  (data+months(1))-1 ,"';"))
  producao_MRP <<- dbFetch(res); dbClearResult(res);  dbDisconnect(con);
  
  if (nrow(producao_MRP) == 0){
    producao_real = reshape(data.frame(producao_real), timevar = "DATA", idvar = c("COD", "SKU", "FAMILIA", "FAMILIA_SOP", "MERCADO", 'EMBALAGEM', 'MARCA', 'FILIAL', 'FILIAL_DESC'), direction = "wide")
    producao_real$`TOTAL PROD. REAL` = rowSums(producao_real[10:ncol(producao_real)], na.rm = TRUE)
    producao_real = producao_real[,c("COD", "SKU", "FAMILIA", "FAMILIA_SOP", "MERCADO",'EMBALAGEM', 'MARCA', 'FILIAL', 'FILIAL_DESC', 'TOTAL PROD. REAL', sort(names(producao_real)[11:length(names(producao_real))-1]))]
    producao <<- producao_real[,colnames(producao_real) %in% c("COD", "SKU", "FAMILIA", "FAMILIA_SOP", "MERCADO", 'EMBALAGEM', 'MARCA', 'FILIAL', 'FILIAL_DESC', 'TOTAL PROD. REAL') | grepl('REAL.', colnames(producao_real))]
    producao[is.na(producao)] <<- 0;colnames(producao) <<- gsub("REAL.", "PROD. REAL. ", colnames(producao));
  }else{
    producao_MRP$PLAN <<- producao_MRP$PLAN; producao_MRP$DATA <<- format(producao_MRP$DATA, "%d/%m"); producao_MRP$FILIAL <<- '1101'; producao_MRP$FILIAL_DESC <<- '1101 - MARILAN MATRIZ'; 
    producao_diferenca = merge(producao_real, producao_MRP, by = c('COD','SKU', 'FILIAL', 'FILIAL_DESC', 'FAMILIA', 'FAMILIA_SOP', 'MERCADO', 'DATA', 'EMBALAGEM', 'MARCA'), all=TRUE)
    producao_diferenca[is.na(producao_diferenca)] = 0; 
    producao_diferenca$ZDIF = producao_diferenca$REAL - producao_diferenca$PLAN
    producao_diferenca = reshape(data.frame(producao_diferenca), timevar = "DATA", idvar = c("COD", "SKU", "FAMILIA", "FAMILIA_SOP", "MERCADO", 'EMBALAGEM', 'MARCA', 'FILIAL', 'FILIAL_DESC'), direction = "wide")
    producao_diferenca = producao_diferenca[,c("COD", "SKU", "FAMILIA", "FAMILIA_SOP", "MERCADO",'EMBALAGEM', 'MARCA', 'FILIAL', 'FILIAL_DESC', sort(names(producao_diferenca)[8:length(names(producao_diferenca))]))]
    producao_diferenca$`TOTAL REAL.` = rowSums(producao_diferenca[,grepl('REAL', colnames(producao_diferenca))], na.rm = TRUE)
    producao_diferenca$`TOTAL PLAN.` = rowSums(producao_diferenca[,grepl('PLAN', colnames(producao_diferenca))], na.rm = TRUE)
    producao_diferenca$`TOTAL ZDIF.` = producao_diferenca$`TOTAL REAL.` - producao_diferenca$`TOTAL PLAN.`
    producao_diferenca = producao_diferenca[,order(gsub("ZDIF.|PLAN.|REAL.", "", colnames(producao_diferenca)))]
    
    producao <<- producao_diferenca[,c("COD", "SKU", "FAMILIA", "FAMILIA_SOP", "MERCADO", 'EMBALAGEM', 'MARCA', 'FILIAL', 'FILIAL_DESC',
                           colnames(producao_diferenca)[grep('TOTAL', colnames(producao_diferenca))],
                           colnames(producao_diferenca)[grepl("ZDIF.|PLAN.|REAL.", colnames(producao_diferenca)) & !grepl("TOTAL", colnames(producao_diferenca))])]

    colnames(producao) <<- gsub("PLAN.", "PROD. PLAN. ", colnames(producao)); 
    colnames(producao) <<- gsub("REAL.", "PROD. REAL. ", colnames(producao)); 
    colnames(producao) <<- gsub("ZDIF.", "PROD. DIF. ", colnames(producao)); 
  }
}

getDecisao <- function(){
  con <- tryCatch({ dbConnect(RMariaDB::MariaDB(), host = "192.168.235.20", user = "server", password="R*proj2018", dbname='consenso')   }, error = function(err) { return("") })
  
  res <- dbSendQuery(con, "SELECT produto.id_produto as COD, produto.sku as SKU, produto.familia as FAMILIA, produto.familia_sop as FAMILIA_SOP,
                     filial.id_filial as FILIAL, filial.filial_desc as FILIAL_DESC, mercado as MERCADO, embalagem as EMBALAGEM, marca as MARCA,
                     (sum(faturado.valor) OVER (PARTITION BY filial, produto)/sum(faturado.valor) OVER (PARTITION BY produto)) as PARTICIPACAO
                     FROM faturado INNER JOIN produto ON faturado.produto = produto.id_produto 
                     INNER JOIN filial ON faturado.filial = filial.id_filial 
                     WHERE data between (SELECT MAX(data) - interval 2 month from faturado) and (SELECT MAX(data) from faturado)");
  rateio_filial = dbFetch(res); 
  rateio_filial = rateio_filial[!(duplicated(rateio_filial) | is.na(rateio_filial$PARTICIPACAO)),]
  
  data = date(periodo); day(data) = 1; 
  res <- dbSendQuery(con, paste0("SELECT produto.id_produto as COD, produto.sku as SKU, produto.familia as FAMILIA, produto.familia_sop as FAMILIA_SOP,
                                  mercado as MERCADO, embalagem as EMBALAGEM, marca as MARCA, sum(decisao.valor) AS CONSENSO
                                  FROM decisao INNER JOIN produto ON decisao.produto = produto.id_produto 
                                  WHERE data = '", data, "' GROUP BY id_produto"))
  consenso = dbFetch(res);
  rateio_filial = merge(rateio_filial, consenso, by = c('COD', 'SKU', 'FAMILIA', 'FAMILIA_SOP', 'MERCADO', 'EMBALAGEM', 'MARCA'), all = TRUE)
  dbClearResult(res);  dbDisconnect(con)
  return(rateio_filial)
}

getDemandaSemanal <- function(){
  con <- tryCatch({ dbConnect(RMariaDB::MariaDB(), host = "192.168.235.20", user = "server", password="R*proj2018", dbname='consenso')   }, error = function(err) { return("") })
  data = date(periodo); day(data) = 1; 

  res <- dbSendQuery(con, paste0("SELECT produto.id_produto as COD, produto.sku as SKU, produto.familia as FAMILIA, produto.FAMILIA_SOP as FAMILIA_SOP, produto.mercado as MERCADO,
                                 produto.embalagem as EMBALAGEM, produto.marca as MARCA, data as DATA, filial.id_filial as FILIAL,  filial.filial_desc as FILIAL_DESC, valor as VALOR FROM previsao_semanal INNER JOIN 
                                 produto ON previsao_semanal.produto = produto.id_produto INNER JOIN filial ON previsao_semanal.filial = filial.id_filial 
                                 WHERE data BETWEEN '", data, "' AND '",  (data+months(1))-1 ,"';"))
  previsao_semanal = dbFetch(res); 
  dbClearResult(res);  dbDisconnect(con)
  return(previsao_semanal)
}

getProducao_planejado <- function(){
  con <- tryCatch({ dbConnect(RMariaDB::MariaDB(), host = "192.168.235.20", user = "server", password="R*proj2018", dbname='consenso')   }, error = function(err) { return("") })
  data = date(periodo); day(data) = 1; 
  res <- dbSendQuery(con, paste0("SELECT produto.id_produto as COD, produto.sku as SKU, produto.familia as FAMILIA, produto.FAMILIA_SOP as FAMILIA_SOP, produto.mercado as MERCADO,
                                  data_producao as DATA, valor FROM producao_planejado INNER JOIN produto ON producao_planejado.produto = produto.id_produto
                                 WHERE data_producao BETWEEN '", data, "' AND '",  (data+months(1))-1 ,"';"))
  producao_MRP <<- dbFetch(res); dbClearResult(res);  dbDisconnect(con);
  producao_MRP$valor <<- producao_MRP$valor/1000; producao_MRP$DATA <<- format(producao_MRP$DATA, "%d/%m"); producao_MRP$FILIAL <<- '1101'; producao_MRP$FILIAL_DESC <<- '1101 - MARILAN MATRIZ'; 
  producao_MRP <<- reshape(data.frame(producao_MRP), timevar = "DATA", idvar = c("COD", "SKU", "FAMILIA", "FAMILIA_SOP", "MERCADO", 'FILIAL', 'FILIAL_DESC'), direction = "wide")
  colnames(producao_MRP) <<- gsub("valor.", "PLAN. ", colnames(producao_MRP))
  producao_MRP <<- producao_MRP[,c("COD", "SKU", "FAMILIA", "FAMILIA_SOP", "MERCADO", 'FILIAL', 'FILIAL_DESC', sort(names(producao_MRP)[8:length(names(producao_MRP))]))]
}

getDescriptionMara <- function(){
  parms <- list('DELIMITER' = ';', 'FIELDS' = list(FIELDNAME = list('MAKTX')), 'OPTIONS' = list(TEXT = list("MATNR = 'Y06'")), 'QUERY_TABLE' = 'MAKT')
}

#ENTRADA DE DADOS
addDados <- function(tipo_entrada, dados){
  con <- tryCatch({ dbConnect(RMariaDB::MariaDB(), host = "192.168.235.20", user = "server",
                              password="R*proj2018", dbname='consenso')   }, error = function(err) { return("") })
  
  switch(tipo_entrada, 
         "MRP"={
           query =  paste0("INSERT INTO producao_planejado (produto, data_producao, valor) VALUES ")
           query = paste0(query, paste0(paste0("('", dados[1:nrow(dados),]$COD, "', '",  dados[1:nrow(dados),]$DATA , "', ", dados[1:nrow(dados),]$VALOR ,"), "), collapse = ""))
           query = stri_replace_last(query, fixed = ",", " ON DUPLICATE KEY UPDATE valor=values(valor);")
         },
         "DEMANDA"={
           query =  paste0("REPLACE INTO previsao_semanal (produto, data, filial, valor) VALUES ")
           query = paste0(query, paste0(paste0("('", dados[1:nrow(dados),]$COD, "', '",  dados[1:nrow(dados),]$DATA , "', ", dados[1:nrow(dados),]$FILIAL ,", ", dados[1:nrow(dados),]$VALOR ,"), "), collapse = ""))
           query = stri_replace_last(query, fixed = ",", " ON DUPLICATE KEY UPDATE valor=values(valor);")
         })
  
  res <- tryCatch({
    dbGetQuery(con, query);
    shinyalert("Dados salvos com sucesso", '', type = "success");
  }, error = function(err) {  
    produtos = dbGetQuery(con, "select id_produto from produto");
    prod_n_cadastrado = unique(dados[!(dados$COD %in% produtos$id_produto),]$COD)
    if (length(prod_n_cadastrado) == 0) 
      shinyalert("Não foi possível salvar os dados", paste0(err), type = "error")
    else 
      shinyalert(paste0("Os seguintes SKUs precisam se cadastrados no Planner: ", paste(prod_n_cadastrado, collapse = ", "), ". Entre em contado com Adm de Vendas."))
  })
  dbDisconnect(con)
}



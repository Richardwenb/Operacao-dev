
 con = RSAPConnect(ashost="s4prd.marilan.com.br", sysnr="06", client="300", user="GVLL67763", passwd="Marilan20*", lang="EN",  trace="0", lcheck="0")
 
 
 parms <- list('DELIMITER' = ';', 'FIELDS' = list(FIELDNAME = list(
                                                       #   "ORGNIAZACAO_VENDAS",
                                                        #  "SALESOFFICE",
                                                          "VOLUME_KG_FATURADO_LIQUIDO" )), 
                       'OPTIONS' = list(TEXT = list("CALENDARYEAR = '2020' and CALENDARMONTH = '01'")), 
               'QUERY_TABLE' = 'ZSDCUBEFATURADO')
 
 
 res <- RSAPInvoke(con, "RFC_READ_TABLE", parms); RSAPClose(con);

 info = data.frame(res$DATA, colsplit(res$DATA$WA, ";", names = sub("\\s+$", "", res$FIELDS$FIELDNAME)))
 
 
 

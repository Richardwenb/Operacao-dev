
server <- function(input, output, session) {

  #start_time <- Sys.time()
  source('dados.R', local = TRUE)
  source('utils.R', local = TRUE)
  
  getInformation <- function(){
    allDados <<- merge(rateio_filial, previsao_semanal, by = c('COD','SKU', 'FILIAL', 'FILIAL_DESC', 'FAMILIA', 'FAMILIA_SOP', 'MERCADO', 'EMBALAGEM', 'MARCA'), all=TRUE)
    allDados <<- merge(allDados, faturado, by = c('COD','SKU', 'FILIAL', 'FILIAL_DESC', 'FAMILIA', 'FAMILIA_SOP', 'MERCADO', 'EMBALAGEM', 'MARCA'), all=TRUE)
    allDados <<- merge(allDados, vendido, by = c('COD','SKU', 'FILIAL', 'FILIAL_DESC', 'FAMILIA', 'FAMILIA_SOP', 'MERCADO', 'EMBALAGEM', 'MARCA'), all=TRUE)
    allDados <<- merge(allDados, carteira, by = c('COD','SKU', 'FILIAL', 'FILIAL_DESC', 'FAMILIA', 'FAMILIA_SOP', 'MERCADO', 'EMBALAGEM', 'MARCA'), all=TRUE)
    allDados <<- merge(allDados, estoque_transito, by = c('COD','SKU', 'FILIAL', 'FILIAL_DESC', 'FAMILIA', 'FAMILIA_SOP', 'MERCADO', 'EMBALAGEM', 'MARCA'), all=TRUE)
    allDados <<- merge(allDados, producao, by = c('COD','SKU', 'FILIAL', 'FILIAL_DESC', 'FAMILIA', 'FAMILIA_SOP', 'MERCADO', 'EMBALAGEM', 'MARCA'), all=TRUE)
    allDados[is.na(allDados)] <<- 0; #allDados[rowSums(allDados[,10:ncol(allDados)]) == 0,] <<- NA;
    allDados <<-convertFilial(allDados)
    dados = allDados %>% group_by_at(c('FAMILIA_SOP', 'MERCADO', 'EMBALAGEM', 'MARCA')) %>% summarise_if(is.numeric, sum, na.rm = TRUE)
    outTable(getCalcColumns(dados))
  }
  
  outTable <- function(dados){
    dados = getCalcTotais(dados) %>% mutate_if(is.numeric, ~round(., 2))

   output$table <-  DT::renderDataTable(datatable(dados, filter = 'top', class = 'cell-border stripe', width = '100%', height = '100%', extensions = c('FixedHeader', 'Buttons'),
                                        options = list(fixedHeader = TRUE, pageLength = nrow(dados), dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf',  I('colvis'))), rownames = FALSE) %>%
                                        formatRound(colnames(dplyr::select_if(dados, is.numeric)), 2, mark = ",", dec.mark = ".") %>%
                                        formatStyle(c("FATURADO ESTIMADO", "FATURADO ESTIMADO VS CONSENSO", "ESTOQUE SUFICIENTE", "DIAS COBERTURA ESTOQUE", "ESTOQUE LÍQUIDO"), backgroundColor = '#dce4f7') %>%
                                        formatStyle("% ATINGIMENTO CONSENSO", backgroundColor = styleInterval(80, c('#dce4f7', '#ff8f8f'))) %>%
                                        formatStyle(colnames(dados)[1], target = 'row',  color = styleEqual("Total", 'black'), backgroundColor = styleEqual("Total", '#bfbfbf')))
  }
    
  observeEvent({input$selecao
                input$mercado
                input$porfilial
                input$dias_producao
                input$volume_caixa},{
                  
      if (exists("allDados")){
        if (input$mercado == 'RESUMO') dados = allDados
        else dados = allDados[allDados$MERCADO %in% input$mercado,]
          
        if (input$selecao == 'FAMILIA' || input$selecao == 'FAMILIA_SOP') 
          dados = dados %>% group_by_at(colnames(dados)[colnames(dados) %in% c(input$selecao, 'FILIAL', 'FILIAL_DESC', 'MERCADO', 'EMBALAGEM', 'MARCA')]) %>% summarise_if(is.numeric, sum, na.rm = TRUE)
        else if (input$selecao != 'TODOS SKUS') dados = dados[dados$FAMILIA_SOP %in% input$selecao,]
        
        if (input$porfilial != 'TODAS FILIAIS'){
          if (input$porfilial == 'SEM FILIAL') dados = dados %>% group_by_at(colnames(dados)[colnames(dados) %in% c(input$selecao, "COD", "SKU", 'MERCADO', 'EMBALAGEM', 'MARCA')]) %>% summarise_if(is.numeric, sum, na.rm = TRUE)
          else dados = dados[dados$FILIAL_DESC == input$porfilial,]       
        }
        
        if (input$dias_producao == 'SEM DIAS DE PRODUCAO'){
          dados = dados[,colnames(dados)[!grepl("PLAN.|REAL.|DIF. ", colnames(dados)) 
                                         | (colnames(dados) %in% c('TOTAL PROD. PLAN. ', 'TOTAL PROD. REAL. ', 'TOTAL PROD. DIF. '))]]
        }
        
        outTable(convertTo(getCalcColumns(dados), input$volume_caixa))
      }   
   })
   
  getDadosUpdate <- function(){ getDados(); updateScreen(); lastUpdate <<- Sys.time() - (3*3600) }
  
  updateScreen <- function(){ getInformation(); updateTextInput(session, "update", value = paste("Ultima atualização em ", format(lastUpdate, format = '%d/%m/%Y - %H:%M'))) }
  
  observeEvent(input$btn_atualizar, { getDadosUpdate(); updateScreen(); })
  
  observeEvent(input$file_carga_mrp, {
    producao = tryCatch({  
      read_excel((input$file_carga_mrp$datapath))
    }, error = function(err) {
      shinyalert("Erro!", "Arquivo invalido", type = "error"); return("")
    })
    
    if (TRUE %in% grepl('LINHA', colnames(producao))){ 
      producao_planejado <<- formatCargaMRP(producao); 
      output$table_cargas <-  DT::renderDataTable(datatable(producao_planejado, class = 'cell-border stripe', width = '100%', height = '100%', options = list(pageLength = 1000, lengthMenu = c(25, 50, 1000)), rownames = FALSE)%>%
                                                             formatRound(colnames(dplyr::select_if(producao_planejado, is.numeric)), 2, mark = ".", dec.mark = ","))
    }
    else{ shinyalert("Erro!", "Arquivo invalido", type = "error"); return("")}
  })
  
  observeEvent(input$btn_carga_mrp, { addDados("MRP", producao_planejado); output$table_cargas <-  DT::renderDataTable(NULL); producao_planejado<<- NULL;})
  
  observeEvent(input$file_carga_previsao_parcial, {
    demanda = tryCatch({  
      read_excel((input$file_carga_previsao_parcial$datapath))
    }, error = function(err) {
      shinyalert("Erro!", "Arquivo invalido", type = "error"); return("")
    })
    
    progress <- shiny::Progress$new(); progress$set(message = "Verificando", value = 1);
    demanda_parcial <<- formatCargaDemanda(demanda);  progress$close()
    output$table_cargas <-  DT::renderDataTable(datatable(demanda_parcial, class = 'cell-border stripe', width = '100%', height = '100%', options = list(pageLength = 1000, lengthMenu = c(25, 50, 1000)), rownames = FALSE)%>%
                                                formatRound('VALOR', 2, mark = ".", dec.mark = ","))
  })
  
  observeEvent(input$btn_carga_demanda, { addDados("DEMANDA", demanda_parcial); output$table_cargas <-  DT::renderDataTable(NULL);  demanda_parcial <<- NULL; })
  
  getDadosUpdate(); 
  updateScreen();
  #end_time <- Sys.time(); print(end_time - start_time)
}


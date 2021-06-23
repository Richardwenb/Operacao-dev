ui <- shinyUI(navbarPage("",
  tabPanel("Visão S&OE",  
    useShinyalert(),
    mainPanel(width = "95%", height = "95%",  
      column(width = 1, height = "200px", br(), actionButton("btn_atualizar", "Atualizar")),
      column(width = 10, height = "200px",  textInput("update", "")),
      br(),br(),
      fluidRow(
        column(width = 2, height = "150px",
               selectInput("selecao", "", c("FAMILIA", "FAMILIAS S&OP" = "FAMILIA_SOP", 'TODOS SKUS', 'AMANTEIGADOS', 'CHOCOLATE COBERTO', 'COOKIES INDULGENCIA', 'COOKIES INFANTIS',
                                            'COOKIES SAUDABILIDADE', 'CRACKERS', 'LEV CRACKERS', 'DISTRACAO', 'LAMINADO', 'MINI MAISENA', 'MOLDADOS CAIXINHA', 'MOLDADOS TUBETE',
                                            'PERSONAL CRACKER', 'PERSONAL CRACKER RECHEADO', 'RECHEADO DOCE', 'ROSQUINHAS', 'SALGADOS SORTIDOS', 'SNACK DOCE', 'SNACK LEV',
                                            'SNACK SALGADO', 'TORRADA EXTRUSADA', 'TORTINHA', 'WAFER','BOLOS'))
        ),
        column(width = 2,  height = "150px", style = "margin-top: 19px;",
               radioGroupButtons(("volume_caixa"), justified = TRUE, choices = c("Ton", "KG"))),
        column(width = 2, height = "150px", selectInput("mercado", "", c('INTERNO' =  'NACIONAL', 'EXTERNO' =  'COMEX', 'VIVALE', 'RESUMO'))),
        column(width = 3, height = "150px", selectInput("porfilial", "", choices = c('SEM FILIAL', 'TODAS FILIAIS', "1101 - MARILAN MATRIZ", "1103 - MARILAN VITORIA DA CONQUISTA", "1107 - MARILAN PARA",
                                                                                     "1108 - MARILAN PERNAMBUCO", "1109 - MARILAN BA SIMOES FILHO", "1110 - MARILAN GOIAS", 
                                                                                     "1117 - MARILAN MINAS GERAIS", "1118 - MARILAN MARILIA SP", "1119 - MARILAN RIO DE JANEIRO","1201 - MARILAN NORDESTE"))),
        column(width = 3, height = "150px", selectInput("dias_producao", "", c('SEM DIAS DE PRODUCAO', 'COM DIAS DE PRODUCAO')))
        
    ),
    
      br(),br(),
      DTOutput('table')
    )
  ),
  tabPanel("Cargas", 
    sidebarPanel(
      fileInput("file_carga_mrp", "MRP", accept = '.xlsx' ),
      actionButton("btn_carga_mrp", label = h5("Realizar Carga"))
    ,width = 2),
    sidebarPanel(
      fileInput("file_carga_previsao_parcial", "Previsão demanda", accept = '.xlsx' ),
      actionButton("btn_carga_demanda", label = h5("Realizar Carga"))
      ,width = 2)
    ), 
    br(),br(),
    DTOutput('table_cargas')
))

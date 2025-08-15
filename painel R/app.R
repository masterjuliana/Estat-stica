# Pacotes necessários
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(plotly)
library(DT)
library(collapsibleTree)
library(tidyr)

# =====================
# CORES IGUAIS AO BI
# =====================
cores_situacao <- c(
  "Admitida" = "#4CAF50",
  "Parcialmente Admitida" = "#FFC107",
  "Não Admitida" = "#F44336",
  "Dúvida / Discutir" = "#2196F3",
  "Repetida" = "#FF9800"
)

# =====================
# UI
# =====================
ui <- dashboardPage(
  dashboardHeader(title = "Consulta Pública - Decreto 7217/2010"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Carregar Dados", icon = icon("upload"),
               fileInput("file1", "Escolha o arquivo Excel",
                         accept = c(".xlsx")
               )
      ),
      menuItem("Progresso", tabName = "progresso", icon = icon("chart-pie")),
      menuItem("Capítulos", tabName = "capitulos", icon = icon("chart-bar")),
      menuItem("Treemap", tabName = "treemap", icon = icon("th")),
      menuItem("Tabela", tabName = "tabela", icon = icon("table")),
      menuItem("Hierarquia", tabName = "hierarquia", icon = icon("project-diagram"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "progresso",
              fluidRow(
                box(width = 12, title = "Situação Geral", status = "primary", solidHeader = TRUE,
                    plotlyOutput("grafico_rosca", height = 400))
              )
      ),
      tabItem(tabName = "capitulos",
              fluidRow(
                box(width = 12, title = "Capítulo x Situação", status = "primary", solidHeader = TRUE,
                    plotlyOutput("grafico_barras", height = 600))
              )
      ),
      tabItem(tabName = "treemap",
              fluidRow(
                box(width = 12, title = "Treemap de Situação", status = "primary", solidHeader = TRUE,
                    plotlyOutput("grafico_treemap", height = 500))
              )
      ),
      tabItem(tabName = "tabela",
              fluidRow(
                box(width = 12, title = "Tabela Detalhada", status = "primary", solidHeader = TRUE,
                    DTOutput("tabela_dados"))
              )
      ),
      tabItem(tabName = "hierarquia",
              fluidRow(
                box(width = 12, title = "Distribuição Hierárquica", status = "primary", solidHeader = TRUE,
                    collapsibleTreeOutput("grafico_hierarquia", height = 600))
              )
      )
    )
  )
)

# =====================
# SERVER
# =====================
server <- function(input, output, session) {
  
  # Objeto reativo para ler e processar os dados do arquivo carregado
  dados_cp_react <- reactive({
    req(input$file1) # Garante que o código só é executado se um arquivo for carregado
    
    # Lê o arquivo Excel carregado pelo utilizador
    dados <- read_excel(input$file1$datapath)
    
    # Aplica o pré-processamento original
    dados <- dados %>%
      rename_with(~ gsub("\n", "_", .x)) %>%
      rename(Situacao = Situação) %>%
      mutate(Situacao = as.factor(Situacao)) %>%
      mutate(
        Capitulo_Abreviado = ifelse(
          is.na(Capitulo) | Capitulo == "",
          "Sem Capítulo",
          sub("^(CAPÍTULO [IVX]+).*", "\\1", Capitulo)
        )
      )
    
    return(dados)
  })
  
  # Agrupa os dados para o gráfico de rosca, agora dependendo dos dados reativos
  dados_agrupados_situacao <- reactive({
    dados_cp_react() %>%
      group_by(Situacao) %>%
      summarise(Contagem = n(), .groups = 'drop') %>%
      arrange(desc(Contagem))
  })
  
  # Agrupa os dados para o gráfico de barras, também dependendo dos dados reativos
  dados_agrupados_capitulo <- reactive({
    dados_cp_react() %>%
      group_by(Capitulo_Abreviado, Situacao) %>%
      summarise(Contagem = n(), .groups = 'drop') %>%
      tidyr::complete(Capitulo_Abreviado, Situacao, fill = list(Contagem = 0))
  })
  
  # --- Geração dos Outputs (gráficos e tabela) ---
  
  # Gráfico de Rosca (Aba "Progresso")
  output$grafico_rosca <- renderPlotly({
    dados_agrupados <- dados_agrupados_situacao()
    if (nrow(dados_agrupados) == 0) return(NULL)
    plot_ly(
      data = dados_agrupados,
      labels = ~Situacao,
      values = ~Contagem,
      type = "pie",
      hole = 0.6,
      textinfo = "label+percent",
      insidetextorientation = "radial",
      marker = list(colors = cores_situacao)
    ) %>%
      layout(
        title = "Distribuição Geral por Situação",
        showlegend = TRUE,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
  })
  
  # Gráfico de Barras (Aba "Capítulos")
  output$grafico_barras <- renderPlotly({
    dados_agrupados <- dados_agrupados_capitulo()
    if (nrow(dados_agrupados) == 0) return(NULL)
    plot_ly(
      data = dados_agrupados,
      x = ~Capitulo_Abreviado,
      y = ~Contagem,
      color = ~Situacao,
      colors = cores_situacao,
      type = "bar"
    ) %>%
      layout(
        title = "Situação por Capítulo",
        barmode = "stack",
        xaxis = list(
          title = "Capítulo",
          automargin = TRUE,
          tickangle = -45,
          tickfont = list(
            size = 12,
            family = "Arial",
            color = "#333333"
          )
        ),
        yaxis = list(title = "Número de Contribuições")
      )
  })
  
  # Treemap (Aba "Treemap")
  output$grafico_treemap <- renderPlotly({
    dados_treemap <- dados_cp_react() %>%
      group_by(Capitulo, Situacao) %>%
      summarise(Contagem = n(), .groups = 'drop')
    if (nrow(dados_treemap) == 0) return(NULL)
    plot_ly(
      data = dados_treemap,
      type = "treemap",
      labels = ~Capitulo,
      parents = ~Situacao,
      values = ~Contagem,
      marker = list(colors = cores_situacao)
    )
  })
  
  # Tabela (Aba "Tabela")
  output$tabela_dados <- renderDT({
    dados_tabela <- dados_cp_react()
    if (nrow(dados_tabela) == 0) return(NULL)
    datatable(
      dados_tabela,
      options = list(
        pageLength = 10,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')
      )
    )
  })
  
  # Gráfico de Hierarquia (Aba "Hierarquia")
  output$grafico_hierarquia <- renderCollapsibleTree({
    dados_hierarquia <- dados_cp_react()
    if (nrow(dados_hierarquia) == 0) return(NULL)
    collapsibleTree(
      dados_hierarquia,
      hierarchy = c("Capitulo", "Artigo", "Parágrafo"),
      root = "Estrutura",
      inputId = "node_id",
      zoomable = TRUE
    )
  })
}

# Juntando tudo para rodar o app
shinyApp(ui = ui, server = server)
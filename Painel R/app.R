# Pacotes necessários
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(plotly)
library(DT)
library(collapsibleTree)
library(tidyr)
library(httr)

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
# UI (Interface do Usuário)
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
      menuItem("Minuta do Decreto", tabName = "minuta", icon = icon("file-alt")),
      menuItem("Progresso", tabName = "progresso", icon = icon("chart-pie")),
      menuItem("Capítulos", tabName = "capitulos", icon = icon("chart-bar")),
      menuItem("Treemap", tabName = "treemap", icon = icon("th")),
      menuItem("Tabela", tabName = "tabela", icon = icon("table")),
      menuItem("Hierarquia", tabName = "hierarquia", icon = icon("project-diagram"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Aba: Minuta do Decreto
      tabItem(tabName = "minuta",
              fluidRow(
                box(width = 12, title = "Resumo da Minuta", status = "info", solidHeader = TRUE,
                    HTML("
          <h4>📄 Consulta Pública nº 003/2025</h4>
          <p><strong>Período:</strong> 19/03/2025 a 03/05/2025<br>
          <strong>Órgão:</strong> Ministério das Cidades<br>
          <strong>Objetivo:</strong> Atualizar o Decreto nº 7.217/2010 conforme a Lei nº 14.026/2020</p>
          <a href='https://www.gov.br/participamaisbrasil/rev7217-2010' target='_blank' class='btn btn-primary'>🔗 Acessar Minuta Completa</a>
        ")
                )
              ),
              fluidRow(
                box(width = 6, title = "📆 Linha do Tempo", status = "warning", solidHeader = TRUE,
                    HTML("
          <ul>
            <li><strong>19/03/2025:</strong> Início da consulta</li>
            <li><strong>03/05/2025:</strong> Encerramento da consulta</li>
            <li><strong>Maio/2025:</strong> Consolidação das contribuições</li>
            <li><strong>Dezemb/2025:</strong> Publicação estimada do decreto</li>
          </ul>
        ")
                ),
                box(width = 6, title = "📚 Destaques Temáticos", status = "success", solidHeader = TRUE,
                    HTML("
          <table class='table table-bordered'>
            <thead>
              <tr><th>Tema</th><th>Conteúdo-chave</th></tr>
            </thead>
            <tbody>
              <tr><td>Universalização</td><td>Metas para cobertura total de água e esgoto até 2033</td></tr>
              <tr><td>Áreas rurais e vulneráveis</td><td>Regras específicas para atendimento em regiões de difícil acesso</td></tr>
              <tr><td>Planejamento</td><td>Exigência de planos municipais e regionais de saneamento</td></tr>
              <tr><td>Fiscalização</td><td>Fortalecimento da regulação por agências independentes</td></tr>
              <tr><td>Participação social</td><td>Mecanismos de consulta e controle social nos serviços</td></tr>
            </tbody>
          </table>
        ")
                )
              )
      ),
      
      # Aba: Progresso (Gráfico de Rosca)
      tabItem(tabName = "progresso",
              fluidRow(
                box(width = 12, title = "Situação Geral", status = "primary", solidHeader = TRUE,
                    plotlyOutput("grafico_rosca", height = 400))
              )
      ),
      
      # Aba: Capítulos (Gráfico de Barras)
      tabItem(tabName = "capitulos",
              fluidRow(
                box(width = 12, title = "Capítulo x Situação", status = "primary", solidHeader = TRUE,
                    plotlyOutput("grafico_barras", height = 600))
              )
      ),
      
      # Aba: Treemap
      tabItem(tabName = "treemap",
              fluidRow(
                box(width = 12, title = "Treemap de Situação", status = "primary", solidHeader = TRUE,
                    plotlyOutput("grafico_treemap", height = 500))
              )
      ),
      
      # Aba: Tabela
      tabItem(tabName = "tabela",
              fluidRow(
                box(width = 12, title = "Tabela Detalhada", status = "primary", solidHeader = TRUE,
                    DTOutput("tabela_dados"))
              )
      ),
      
      # Aba: Hierarquia
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
# SERVER (Lógica do Aplicativo)
# =====================
server <- function(input, output, session) {
  
  # 1. CARREGAMENTO E PRÉ-PROCESSAMENTO DOS DADOS (Objeto Reativo)
  # Este objeto reativo carrega os dados ou do arquivo local ou do GitHub,
  # garantindo que a lógica de processamento seja executada apenas uma vez.
  dados_cp_react <- reactive({
    
    # Define a fonte dos dados: arquivo local ou URL do GitHub
    if (!is.null(input$file1)) {
      dados <- read_excel(input$file1$datapath)
    } else {
      url <- "https://raw.githubusercontent.com/masterjuliana/Estat-stica/main/painel%20R/Consulta-testeR.xlsx"
      destino_temp <- tempfile(fileext = ".xlsx")
      httr::GET(url, write_disk(destino_temp, overwrite = TRUE))
      dados <- read_excel(destino_temp)
    }
    
    # Pré-processamento: renomear colunas, converter para fator, criar coluna de capítulo abreviado
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
  
  # 2. AGREGAÇÃO DOS DADOS PARA GRÁFICOS (Objetos Reativos)
  # Esses objetos reativos preparam os dados específicos para cada visualização.
  
  # Dados agrupados por Situação (para o gráfico de rosca)
  dados_agrupados_situacao <- reactive({
    dados_cp_react() %>%
      group_by(Situacao) %>%
      summarise(Contagem = n(), .groups = 'drop') %>%
      arrange(desc(Contagem))
  })
  
  # Dados agrupados por Capítulo e Situação (para o gráfico de barras)
  dados_agrupados_capitulo <- reactive({
    dados_cp_react() %>%
      group_by(Capitulo_Abreviado, Situacao) %>%
      summarise(Contagem = n(), .groups = 'drop') %>%
      tidyr::complete(Capitulo_Abreviado, Situacao, fill = list(Contagem = 0))
  })
  
  # Dados para o Treemap (agrupamento por Capítulo e Situação)
  dados_treemap <- reactive({
    dados_cp_react() %>%
      group_by(Capitulo, Situacao) %>%
      summarise(Contagem = n(), .groups = 'drop')
  })
  
  # 3. GERAÇÃO DOS OUTPUTS (Gráficos e Tabela)
  
  # Gráfico de Rosca (Aba "Progresso")
  output$grafico_rosca <- renderPlotly({
    dados <- dados_agrupados_situacao()
    if (nrow(dados) == 0) return(NULL)
    plot_ly(
      data = dados,
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
    dados <- dados_agrupados_capitulo()
    if (nrow(dados) == 0) return(NULL)
    plot_ly(
      data = dados,
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
    dados <- dados_treemap()
    if (nrow(dados) == 0) return(NULL)
    plot_ly(
      data = dados,
      type = "treemap",
      labels = ~Capitulo,
      parents = ~Situacao,
      values = ~Contagem,
      marker = list(colors = cores_situacao)
    )
  })
  
  # Tabela (Aba "Tabela")
  output$tabela_dados <- renderDT({
    dados <- dados_cp_react()
    if (nrow(dados) == 0) return(NULL)
    datatable(
      dados,
      options = list(
        pageLength = 10,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')
      )
    )
  })
  
  # Gráfico de Hierarquia (Aba "Hierarquia")
  output$grafico_hierarquia <- renderCollapsibleTree({
    dados <- dados_cp_react()
    if (nrow(dados) == 0) return(NULL)
    collapsibleTree(
      dados,
      hierarchy = c("Capitulo", "Artigo", "Parágrafo"),
      root = "Estrutura",
      inputId = "node_id",
      zoomable = TRUE
    )
  })
}

# Juntando tudo para rodar o app
shinyApp(ui = ui, server = server)
# 📊 Painel Interativo - Situação por Capítulo

Este projeto apresenta um painel interativo desenvolvido em **R** com `flexdashboard`, `shiny` e `plotly`, que permite visualizar a situação de contribuições por capítulo do Decreto nº 7.217.

## 🔧 Tecnologias Utilizadas

- R
- flexdashboard
- shiny
- plotly
- dplyr, tidyr, janitor
- readxl, httr

## 📁 Estrutura do Repositório

Estat-stica/ └── painel R/ └── painel_interativo.Rmd

Code

## 📈 Funcionalidades

- Filtro dinâmico por situação (Concluído, Em andamento, Pendente, Cancelado)
- Gráfico de barras empilhadas por capítulo
- Visualização interativa com plotly
- Layout responsivo com flexdashboard

## 🚀 Como Executar

1. Instale os pacotes necessários:

```r
install.packages(c("flexdashboard", "shiny", "httr", "readxl", "dplyr", "tidyr", "plotly", "janitor"))
Execute o painel no RStudio:

Abra o arquivo .Rmd dentro da pasta painel R

Clique em "Run Document" ou use rmarkdown::run("painel_interativo.Rmd")

⚠️ O painel depende de conexão com a internet para carregar os dados via GitHub.

📊 Fonte dos Dados
Os dados são carregados diretamente do arquivo Excel hospedado neste link.

👩‍💻 Autora
Juliana Farias Rio de Janeiro, Brasil 🔗 LinkedIn 📄 Currículo Lattes 🐙 GitHub

🇬🇧 English
Interactive Dashboard – Status by Chapter
This project presents an interactive dashboard built in R using flexdashboard, shiny, and plotly, designed to visualize the status of contributions by chapter of Decree No. 7.217.

Technologies Used: R, flexdashboard, shiny, plotly, dplyr, tidyr, janitor, readxl, httr

Repository Structure:

Code
Estat-stica/
└── painel R/
    └── painel_interativo.Rmd
Features:

Dynamic filter by status (Completed, In Progress, Pending, Canceled)

Stacked bar chart by chapter

Interactive visualization with plotly

Responsive layout with flexdashboard

How to Run:

Install required packages and run the .Rmd file in RStudio using rmarkdown::run().

Data Source: Excel file hosted on GitHub

Author: Juliana Farias – LinkedIn | Lattes CV | GitHub

🇪🇸 Español
Panel Interactivo – Situación por Capítulo
Este proyecto presenta un panel interactivo desarrollado en R usando flexdashboard, shiny y plotly, diseñado para visualizar el estado de las contribuciones por capítulo del Decreto Nº 7.217.

Tecnologías Utilizadas: R, flexdashboard, shiny, plotly, dplyr, tidyr, janitor, readxl, httr

Estructura del Repositorio:

Code
Estat-stica/
└── painel R/
    └── painel_interativo.Rmd
Funcionalidades:

Filtro dinámico por situación (Concluido, En progreso, Pendiente, Cancelado)

Gráfico de barras apiladas por capítulo

Visualización interactiva con plotly

Diseño responsivo con flexdashboard

Cómo Ejecutar:

Instala los paquetes necesarios y ejecuta el archivo .Rmd en RStudio usando rmarkdown::run().

Fuente de Datos: Archivo Excel alojado en GitHub

Autora: Juliana Farias – LinkedIn | Currículum Lattes | GitHub

🇩🇪 Deutsch
Interaktives Dashboard – Status nach Kapitel
Dieses Projekt zeigt ein interaktives Dashboard, das in R mit flexdashboard, shiny und plotly erstellt wurde. Es dient zur Visualisierung des Beitragsstatus nach Kapitel des Dekrets Nr. 7.217.

Verwendete Technologien: R, flexdashboard, shiny, plotly, dplyr, tidyr, janitor, readxl, httr

Projektstruktur:

Code
Estat-stica/
└── painel R/
    └── painel_interativo.Rmd
Funktionen:

Dynamischer Filter nach Status (Abgeschlossen, In Bearbeitung, Ausstehend, Abgebrochen)

Gestapeltes Balkendiagramm nach Kapitel

Interaktive Visualisierung mit plotly

Responsives Layout mit flexdashboard

Ausführung:

Installiere die benötigten Pakete und führe die .Rmd-Datei in RStudio mit rmarkdown::run() aus.

Datenquelle: Excel-Datei auf GitHub gehostet

Autorin: Juliana Farias – LinkedIn | Lattes-Lebenslauf | GitHub

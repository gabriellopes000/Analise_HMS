################################################################################
# GERADOR DE ARQUIVOS DO HMS - VERSÃO 1 - 09/2026
################################################################################








# PARA EXECUTAR APERTE CTRL+A SEGUIDO DE CTRL+ENTER

# NÃO É NECESSÁRIA NENHUMA ALTERAÇÃO MANUAL NO CÓDIGO

# O PACOTE DSSRIP PRECISA ESTAR CORRETAMENTE INSTALADO E CONFIGURADO NESTA MÁQUINA PARA QUE A GERAÇÃO DOS ARQUIVOS FUNCIONE.

# EM CASO DE DÚVIDAS, PROCURE GABRIEL DE PAULA OU VINÍCIUS MOREIRA.















################################################################################
################################################################################

if(R.Version()$arch=="x86_64"){
  # use 64-bit .jar and .dll
  options(dss_override_location="C:\\Program Files\\HEC\\HEC-DSSVue\\")
  Sys.setenv(JAVA_HOME=paste0(options("dss_override_location"), "java"))
} else {
  # use 32-bit .jar and .dll (old dssrip, no longer needed)
}


# 1) onde estão os jars e a DLL extraída
options(dss_override_location = "C:/projects/dssrip/monolith")

# 2) onde está o seu arquivo de configuração recém-salvo
options(dss_config_filename  = "C:/projects/dssrip/monolith/dssrip.config")

# 3) forçar uso desta configuração
options(dss_default_config   = "monolith-win-x86_64")
options(dss_allowed_states   = "tested")
options(dssrip_debug         = TRUE)

pacotes_necessarios <- c("shiny", "shinyWidgets", "shinyFiles", "shinyFeedback",
                         "rhandsontable", "plotly", "DT", "fs",
                         "dssrip", "foreach", "stringr", "xts", "ggplot2",
                         "dplyr", "matlib", "data.table", "readxl", "splines", "beepr")
for (pkg in pacotes_necessarios) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)
}

options(shiny.launch.browser = TRUE)
rm(list = ls())
# ==============================================================================
# FUNÇÕES AUXILIARES
# ==============================================================================

logistic <- function(x, L, k, x0) L / (1 + exp(-k * (x - x0)))

dados_huff_default <- data.frame(
  Tempo = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100),
  Q1 = c(0, 16, 33, 43, 52, 60, 66, 71, 75, 79, 82, 84, 86, 88, 90, 92, 94, 96, 97, 98, 100),
  Q2 = c(0, 3, 8, 12, 16, 22, 29, 39, 51, 62, 70, 76, 81, 85, 88, 91, 93, 95, 97, 98, 100),
  Q3 = c(0, 3, 6, 9, 12, 15, 19, 23, 27, 32, 38, 45, 57, 70, 79, 85, 89, 92, 95, 97, 100),
  Q4 = c(0, 2, 5, 8, 10, 13, 16, 19, 22, 25, 28, 32, 35, 39, 45, 51, 59, 72, 84, 92, 100)
)

ajustar_curvas_huff <- function(dados) {
  modelos <- list()
  modelos$Q1 <- lm(Q1 ~ poly(Tempo, 4, raw = TRUE), data = dados)
  modelos$Q2 <- nls(Q2 ~ logistic(Tempo, L, k, x0), data = dados,
                    start = list(L = max(dados$Q2), k = 0.1, x0 = 50))
  modelos$Q3 <- nls(Q3 ~ logistic(Tempo, L, k, x0), data = dados,
                    start = list(L = max(dados$Q3), k = 0.1, x0 = 50))
  modelos$Q4 <- lm(Q4 ~ bs(Tempo, degree = 3, knots = c(0, 40, 45, 85, 100)), data = dados)
  modelos
}

converter_duracao_minutos <- function(duracao_texto) {
  duracao_texto <- trimws(as.character(duracao_texto))
  partes <- str_match(duracao_texto, "^([0-9]+[.,]?[0-9]*)\\s*(min|m|h|d)$")
  if (any(is.na(partes[, 1]))) {
    idx_invalidos <- which(is.na(partes[, 1]))
    stop("Duração(ões) não reconhecida(s): ",
         paste(duracao_texto[idx_invalidos], collapse = ", "),
         ". Use 'N min', 'N h' ou 'N d' (ex.: '5 min', '1 h', '2 d').")
  }
  valor <- as.numeric(gsub(",", ".", partes[, 2]))
  unidade <- tolower(partes[, 3])
  fator <- dplyr::case_when(
    unidade %in% c("min", "m") ~ 1,
    unidade == "h" ~ 60,
    unidade == "d" ~ 1440,
    TRUE ~ NA_real_
  )
  valor * fator
}

determinar_quartil <- function(duracao_minutos) {
  if (duracao_minutos <= 12 * 60) return("Q2")
  if (duracao_minutos < 24 * 60) return("Q3")
  return("Q4")
}

calcular_fatores_correcao <- function(modelo, NB, intensidade) {
  df_primeiro <- data.frame(Tempo = (1 / NB) * 100)
  fator_translacao <- predict(modelo, newdata = df_primeiro) * intensidade / 100
  df_total <- data.frame(Tempo = 100)
  precip_total_sem_correcao <- predict(modelo, newdata = df_total) * intensidade / 100
  fator_amplificacao <- intensidade / (precip_total_sem_correcao - fator_translacao)
  list(translacao = as.numeric(fator_translacao),
       amplificacao = as.numeric(fator_amplificacao))
}

#' Gera um único hietograma (vetor de precipitação acumulada)
gerar_hietograma <- function(modelo_escolhido, NB, intensidade) {
  precip_acumulada <- numeric(NB + 1)
  precip_acumulada[1] <- 0
  fatores <- calcular_fatores_correcao(modelo_escolhido, NB, intensidade)
  for (j in 1:NB) {
    percentual_tempo <- (j / NB) * 100
    if (j == NB) {
      precip_acumulada[j + 1] <- intensidade
    } else if (percentual_tempo > 0 && percentual_tempo < 100) {
      novos_dados <- data.frame(Tempo = percentual_tempo)
      precip_bruta <- predict(modelo_escolhido, newdata = novos_dados) * intensidade / 100
      precip_acumulada[j + 1] <- (as.numeric(precip_bruta) - fatores$translacao) * fatores$amplificacao
    }
  }
  precip_acumulada[precip_acumulada < 0] <- 0
  precip_acumulada
}

#' Formata o nome de um TR para uso no nome do evento.
#' TRs numéricos (anos) recebem o sufixo "A" (ex.: "100A").
#' TRs textuais como "PMP"/"PMP majorada" são usados como estão.
formatar_tr_nome <- function(tr) {
  if (grepl("^[0-9]+$", tr)) {
    paste0(tr, "A")
  } else {
    tr
  }
}

#' Gera os arquivos completos do HEC-HMS (.dss, .gage, .met, .hms, .run)
#' a partir dos quantis, TRs e sub-bacias definidos no app.
#' Sobrescreve os arquivos existentes na pasta do projeto a cada execução.
gerar_arquivos_hms <- function(quantis, trs, basin, subbasins, projectName, projectFolder,
                               modelos_huff, progress_callback = NULL) {
  
  meses <- c("January","February","March","April","May","June",
             "July","August","September","October","November","December")
  
  total_linhas <- length(trs) * nrow(quantis)
  passo <- 0
  contador_eventos <- 0
  
  txt_gage <- ""
  txt_met <- ""
  txt_run <- ""
  
  # Garantir que a pasta do projeto existe
  if (!dir.exists(projectFolder)) dir.create(projectFolder, recursive = TRUE)
  
  # Abrir arquivo DSS (recriado do zero: remove o antigo se existir)
  projectFileDss <- file.path(projectFolder, paste0(projectName, ".dss"))
  if (file.exists(projectFileDss)) file.remove(projectFileDss)
  
  dssFile <- .jcall("hec/heclib/dss/HecDss", "Lhec/heclib/dss/HecDss;",
                    method = "open", projectFileDss)
  
  for (tr in trs) {
    col <- as.character(tr)
    
    for (i in seq_len(nrow(quantis))) {
      
      bloco <- quantis$Bloco[i]
      intensidade <- quantis[[col]][i]
      duracao <- quantis$Duracao[i]           # já em minutos (quantis_processados())
      NB <- as.integer(duracao / bloco)
      dias <- max(1, as.integer(duracao / (24 * 60)))
      
      quartil <- determinar_quartil(duracao)
      nome_evento <- paste0("TT - ", formatar_tr_nome(tr), " ", quantis$Sufix[i], " - ", quartil)
      
      modelo_escolhido <- modelos_huff[[quartil]]
      precip_acumulada <- gerar_hietograma(modelo_escolhido, NB, intensidade)
      
      # ---- Gravação no DSS ---------------------------------------------------
      tsc <- .jnew("hec/io/TimeSeriesContainer")
      
      dss_path <- paste0("//", nome_evento, "/PRECIP-CUM/31Dec1999 - ", dias,
                         "Jan2000/", bloco, "MIN/GAGE/")
      tsc$fullName <- dss_path
      
      start <- .jnew("hec/heclib/util/HecTime", "01Jan2000", "0000")
      tsc$interval <- as.integer(bloco)
      
      timec <- start$value()
      times <- .jarray(1:(NB + 1))
      times[[1]] <- start$value()
      
      for (j in 1:NB) {
        timec <- timec + bloco
        times[[j + 1]] <- as.integer(timec)
      }
      
      time1 <- as.POSIXct("2000-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
      time1 <- time1 + bloco * 60 * NB
      timef <- paste0(strftime(time1, format = "%d "),
                      meses[as.integer(strftime(time1, format = "%m"))],
                      strftime(time1, format = " %Y, %H:%M"))
      
      values <- .jarray(precip_acumulada)
      tsc$times <- times
      tsc$values <- values
      tsc$numberValues <- length(values)
      tsc$units <- "MM"
      tsc$type <- "INST-CUM"
      
      dssFile$put(tsc)
      
      # ---- Texto do .gage ------------------------------------------------------
      txt_gage <- paste0(txt_gage,
                         "Gage: ", nome_evento, "\n",
                         "Latitude:0\n",
                         "Longitude:0\n",
                         "Gage Type: Precipitation\n",
                         "Precipitation Type: Cumulative\n",
                         "Units: MM\n",
                         "Local to Project: YES\n",
                         "Start Time: 1 January 2000, 00:00\n",
                         "End Time: ", timef, "\n",
                         "Pathname: ", dss_path, "\n",
                         "Units System: SI\n",
                         "Data Type: PRECIP-CUM\n",
                         "dss File: ", projectFileDss, "\n",
                         "End:\n\n")
      
      # ---- Arquivo .met individual -----------------------------------------
      metFile <- file.path(projectFolder, paste0(nome_evento, ".met"))
      metTxt <- paste0("Meteorology: ", nome_evento, "\n",
                       "     Version: 4.12\n",
                       "     Unit System: Metric\n",
                       "     Set Missing Data to Default: Yes\n",
                       "     Precipitation Method: Specified Average\n",
                       "     Short-Wave Radiation Method: None\n",
                       "     Long-Wave Radiation Method: None\n",
                       "     Snowmelt Method: None\n",
                       "     Evapotranspiration Method: No Evapotranspiration\n",
                       "     Use Basin Model: ", basin, "\n",
                       "End:")
      
      for (sb in subbasins) {
        metTxt <- paste0(metTxt, "\nSubbasin: ", sb, "\n",
                         "     Gage:", nome_evento, "\n",
                         "End:\n")
      }
      
      write(metTxt, file = metFile, append = FALSE)
      
      txt_met <- paste0(txt_met,
                        "Precipitation: ", nome_evento, "\n",
                        "     FileName: ", nome_evento, ".met\n",
                        "     Description: \n",
                        "End:\n\n")
      
      # ---- Texto do .run ---------------------------------------------------
      control <- ifelse(dias <= 5, "dp<=5d", "dp>5d")
      txt_run <- paste0(txt_run, "\n\nRun: ", nome_evento, "\n",
                        "    Basin: ", basin, "\n",
                        "    Precip: ", nome_evento, "\n",
                        "    Control: ", control, "\n",
                        "End:")
      
      contador_eventos <- contador_eventos + 1
      passo <- passo + 1
      if (!is.null(progress_callback)) {
        progress_callback(passo / total_linhas, nome_evento)
      }
    }
  }
  
  # ---- Fechamento do DSS ------------------------------------------------------
  dssFile$close()
  
  # ---- Arquivo .gage ------------------------------------------------------
  gageFile <- file.path(projectFolder, paste0(projectName, ".gage"))
  gageHeader <- paste0("Gage Manager: ", projectName, "\n",
                       "Version: 4.12\n",
                       "Filepath Separator: \\\n",
                       "End:\n")
  write(gageHeader, file = gageFile, append = FALSE)
  write(txt_gage, file = gageFile, append = TRUE)
  
  # ---- Arquivo .hms ------------------------------------------------------
  txt_hms <- paste0("Project: ", projectName, "\n",
                    "     Description: \n",
                    "     Version: 4.12\n",
                    "     Filepath Separator: \\\n",
                    "     DSS File Name: ", projectName, ".dss\n",
                    "     Time Zone ID: America/Sao_Paulo\n",
                    "End:\n\n",
                    "Basin: ", basin, "\n",
                    "     Filename: ", gsub(" ", "_", basin), ".basin\n",
                    "     Description: \n",
                    "End:\n\n",
                    txt_met, "\n\n",
                    "Control: dp<=5d\n",
                    "     FileName: dp__5d.control\n",
                    "     Description: \n",
                    "End:\n\n",
                    "Control: dp>5d\n",
                    "     FileName: dp_5d.control\n",
                    "     Description: \n",
                    "End:")
  
  hmsFile <- file.path(projectFolder, paste0(projectName, ".hms"))
  write(txt_hms, file = hmsFile, append = FALSE)
  
  # ---- Arquivo .run ------------------------------------------------------
  txt_run_final <- paste0(txt_run, "\n\n",
                          "Control: dp>5d\n",
                          "     FileName: dp_5d.control\n",
                          "     Description: \n",
                          "End:\n\n",
                          "Control: dp<=5d\n",
                          "     FileName: dp__5d.control\n",
                          "     Description: \n",
                          "End:")
  
  runFile <- file.path(projectFolder, paste0(projectName, ".run"))
  write(txt_run_final, file = runFile, append = FALSE)
  
  list(
    total_eventos = contador_eventos,
    arquivos = c(gageFile, hmsFile, runFile, projectFileDss)
  )
}

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  useShinyFeedback(),
  titlePanel(
    div(
      "Gerador de Arquivos do HMS",
      tags$div(
        style = "font-size: 16px; font-weight: normal; color: #555; margin-top: 4px;",
        "Distribuição de Huff - Versão 1 - 09/2026"
      )
    )
  ),
  
  tabsetPanel(
    id = "tabs",
    
    # ---- ABA 0: CONFIGURAÇÃO ------------------------------------------------
    tabPanel("0. Configuração",
             br(),
             tags$div(
               style = "background-color:#f8d7da; padding:10px 14px; border-radius:4px; margin-bottom:15px;",
               strong("Antes de usar este aplicativo, verifique se o projeto HEC-HMS já possui:"),
               tags$ul(
                 tags$li(strong("Basin Model"), " — com o nome exato que será informado na aba '2. Parâmetros do Projeto' (ex.: 'Basin 1'), já contendo, no mínimo, as sub-bacias."),
                 tags$li(strong("Control Specifications"), " — dois controles nomeados exatamente ",
                         code("dp<=5d"), " (para eventos com duração ≤ 5 dias) e ",
                         code("dp>5d"), " (para eventos com duração > 5 dias), com os períodos de ",
                         "simulação compatíveis com as durações que serão processadas. ",
                         "Esses são os nomes das Control Specifications dentro do HEC-HMS — ",
                         "o próprio programa converte para os nomes de arquivo ",
                         code("dp__5d.control"), " e ", code("dp_5d.control"), " ao salvar.")
               ),
               "O aplicativo ", strong("gera automaticamente"), " os Meteorologic Models (.met), o Gage Manager (.gage), as séries de precipitação (.dss) e o Run Manager (.run) — mas ", strong("não cria"), " o Basin Model nem as Control Specifications. Se esses arquivos não existirem na pasta do projeto com os nomes corretos, o HEC-HMS não conseguirá abrir ou rodar as simulações geradas.",
               tags$br(), tags$br(),
               strong("Requisito técnico: "), "o pacote ", code("dssrip"), " precisa estar corretamente instalado e configurado nesta máquina para que a geração dos arquivos funcione. Em caso de dúvidas, procure ",
               strong("Gabriel de Paula"), " ou ", strong("Vinícius Moreira"), ".",
               tags$br(), tags$br(),
               strong("Antes de rodar o aplicativo: "), "feche o HEC-HMS, caso esteja aberto, para evitar conflitos de acesso aos arquivos do projeto.",
               tags$br(),
               strong("Para rodar novamente: "), "se precisar executar o aplicativo mais de uma vez, feche a janela do navegador referente à execução anterior antes de iniciar uma nova."
             ),
             br(),
             fluidRow(
               column(6,
                      h4("Tempos de retorno (TR)"),
                      helpText("Selecione os TRs que serão colunas na tabela de quantis. ",
                               "PMP = Precipitação Máxima Provável; ",
                               "PMP majorada = PMP majorada (quando aplicável)."),
                      checkboxGroupInput("trs_config", NULL,
                                         choices = c("2", "5", "10", "20", "25", "50", "100", "200",
                                                     "500", "1000", "10000", "PMP", "PMP majorada"),
                                         selected = c("2", "5", "10", "20", "25", "50", "100", "200",
                                                      "500", "1000", "10000", "PMP", "PMP majorada"))
               ),
               column(6,
                      h4("Durações e blocos"),
                      helpText("Uma linha por duração. 'Bloco' é o intervalo de discretização (min)."),
                      rHandsontableOutput("tabela_duracoes_config"),
                      br(),
                      actionButton("add_duracao", "Adicionar duração", icon = icon("plus")),
                      actionButton("rm_duracao", "Remover última duração", icon = icon("minus"))
               )
             ),
             br(),
             actionButton("btn_gerar_tabela", "Gerar / atualizar tabela de entrada",
                          icon = icon("table"), class = "btn-primary"),
             helpText("Reconstrói a tabela da aba '1. Dados de Entrada' com as durações e TRs ",
                      "definidos acima. Valores já preenchidos que ainda existirem ",
                      "(mesma duração e mesmo TR) são preservados.")
    ),
    
    # ---- ABA 1: DADOS DE ENTRADA -------------------------------------------
    tabPanel("1. Dados de Entrada",
             br(),
             fluidRow(
               column(6,
                      h4("Quantis de precipitação"),
                      helpText("Cole os dados diretamente do Excel (Ctrl+V dentro da tabela). ",
                               "Coluna 'Duracao' aceita formatos como '5 min', '1 h', '2 d'"),
                      tags$div(
                        style = "background-color:#fff3cd; padding:8px 12px; border-radius:4px; margin-bottom:10px;",
                        strong("Nota: "), "idealmente as colunas ", strong("Bloco"), " e ", strong("Sufix"),
                        " não devem ser alteradas."
                      ),
                      rHandsontableOutput("tabela_quantis"),
                      br(),
                      actionButton("add_linha_quantis", "Adicionar linha", icon = icon("plus")),
                      actionButton("rm_linha_quantis", "Remover última linha", icon = icon("minus")),
                      br(), br(),
                      fileInput("upload_quantis", "Ou importar de planilha (.xlsx)", accept = ".xlsx")
               ),
               column(6,
                      h4("Sub-bacias"),
                      helpText("Uma linha por sub-bacia do modelo HEC-HMS. Os nomes devem ser iguais aos da sub-bacias"),
                      rHandsontableOutput("tabela_subbacias"),
                      br(),
                      actionButton("add_linha_sub", "Adicionar linha", icon = icon("plus")),
                      actionButton("rm_linha_sub", "Remover última linha", icon = icon("minus"))
               )
             )
    ),
    
    # ---- ABA 2: PARÂMETROS DO PROJETO --------------------------------------
    tabPanel("2. Parâmetros do Projeto",
             br(),
             fluidRow(
               column(6,
                      shinyDirButton("dir_projeto", "Selecionar pasta do projeto", "Escolha a pasta do projeto HEC-HMS"),
                      helpText("O nome do projeto HEC-HMS será o mesmo nome da pasta selecionada. ",
                               "Selecione a pasta que já contém os arquivos do projeto HMS criados ",
                               "(ex.: .dss, .control, .basin)."),
                      br(),
                      verbatimTextOutput("pasta_selecionada"),
                      br(),
                      textInput("basin_name", "Nome da Basin Model", value = "Basin 1")
               ),
               column(6,
                      h4("Tempos de retorno a processar"),
                      helpText("Baseado nos TRs definidos na aba '0. Configuração'."),
                      uiOutput("selecao_trs")
               )
             )
    ),
    
    # ---- ABA 3: CONFERÊNCIA / PREVIEW --------------------------------------
    tabPanel("3. Conferência",
             br(),
             fluidRow(
               column(6,
                      h4("Curvas de Huff ajustadas"),
                      plotlyOutput("plot_curvas_huff", height = "400px")
               ),
               column(6,
                      h4("Preview de um hietograma"),
                      fluidRow(
                        column(6, uiOutput("preview_tr_select")),
                        column(6, uiOutput("preview_duracao_select"))
                      ),
                      plotlyOutput("plot_preview_hietograma", height = "350px")
               )
             )
    ),
    
    # ---- ABA 4: PROCESSAMENTO -----------------------------------------------
    tabPanel("4. Gerar Arquivos",
             br(),
             actionButton("btn_processar", "Gerar hietogramas e arquivos HEC-HMS",
                          icon = icon("play"), class = "btn-primary btn-lg"),
             br(), br(),
             verbatimTextOutput("log_processamento"),
             br(),
             DTOutput("tabela_resumo")
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # ---- Configuração: durações/blocos -----------------------------------
  duracoes_config_rv <- reactiveVal(
    data.frame(
      Duracao = c("10 min", "15 min", "20 min", "30 min", "1 h", "2 h", "3 h",
                  "4 h", "6 h", "8 h", "10 h", "12 h", "18 h", "24 h", "2 d",
                  "3 d", "5 d", "7 d", "10 d", "15 d", "20 d", "30 d"),
      Bloco = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5, 5, 5, 5, 5, 5, 5),
      Sufix = c("- 10 min", "- 15 min", "- 20 min", "- 30 min", "- 1 h", "- 2 h",
                "- 3 h", "- 4 h", "- 6 h", "- 8 h", "- 10 h", "- 12 h", "- 18 h",
                "- 24 h", "- 2 d", "- 3 d", "- 5 d", "- 7 d", "- 10 d", "- 15 d",
                "- 20 d", "- 30 d"),
      stringsAsFactors = FALSE
    )
  )
  
  output$tabela_duracoes_config <- renderRHandsontable({
    rhandsontable(duracoes_config_rv(), rowHeaders = NULL, stretchH = "all") %>%
      hot_col("Duracao", type = "text") %>%
      hot_col("Bloco", type = "numeric") %>%
      hot_col("Sufix", type = "text")
  })
  
  observeEvent(input$tabela_duracoes_config, {
    duracoes_config_rv(hot_to_r(input$tabela_duracoes_config))
  })
  
  observeEvent(input$add_duracao, {
    df <- duracoes_config_rv()
    nova <- df[1, ]
    nova[1, ] <- NA
    duracoes_config_rv(rbind(df, nova))
  })
  
  observeEvent(input$rm_duracao, {
    df <- duracoes_config_rv()
    if (nrow(df) > 1) duracoes_config_rv(df[-nrow(df), , drop = FALSE])
  })
  
  # ---- Estado reativo: tabela de quantis (dados reais Maravilhas II) --------
  quantis_rv <- reactiveVal(
    data.frame(
      Duracao = c("10 min", "15 min", "20 min", "30 min", "1 h", "2 h", "3 h",
                  "4 h", "6 h", "8 h", "10 h", "12 h", "18 h", "24 h", "2 d",
                  "3 d", "5 d", "7 d", "10 d", "15 d", "20 d", "30 d"),
      `2`     = c(17.8, 23.1, 27.0, 32.4, 41.6, 53.9, 61.0, 66.1, 73.3, 78.4, 82.4, 85.6, 92.8, 97.9, 125.1, 152.9, 196.8, 232.4, 277.4, 338.8, 390.8, 478.2),
      `5`     = c(22.4, 29.2, 34.0, 40.7, 52.3, 68.0, 77.2, 83.8, 93.0, 99.5, 104.6, 108.7, 117.9, 124.5, 160.3, 195.4, 251.4, 296.5, 352.4, 431.1, 495.8, 602.3),
      `10`    = c(25.5, 33.1, 38.5, 46.1, 59.1, 77.2, 87.8, 95.3, 105.9, 113.4, 119.2, 124.0, 134.6, 142.1, 183.6, 223.6, 287.5, 338.9, 402.0, 492.2, 565.3, 684.4),
      `20`    = c(28.4, 36.8, 42.7, 51.1, 65.5, 85.9, 97.8, 106.3, 118.2, 126.6, 133.2, 138.6, 150.5, 159.0, 206.0, 250.7, 322.2, 379.5, 449.6, 550.9, 632.0, 763.2),
      `25`    = c(29.3, 38.0, 44.1, 52.7, 67.5, 88.6, 101.0, 109.8, 122.1, 130.9, 137.7, 143.2, 155.6, 164.3, 213.1, 259.2, 333.2, 392.4, 464.7, 569.5, 653.1, 788.2),
      `50`    = c(32.1, 41.5, 48.1, 57.5, 73.6, 97.0, 110.7, 120.4, 134.0, 143.8, 151.3, 157.4, 171.1, 180.8, 235.0, 285.6, 367.1, 432.2, 511.2, 626.7, 718.3, 865.2),
      `100`   = c(33.0, 43.5, 51.0, 61.5, 79.5, 105.2, 120.2, 130.8, 145.9, 156.5, 164.8, 171.5, 186.6, 197.2, 256.7, 311.9, 400.7, 471.6, 557.4, 683.6, 783.0, 941.7),
      `200`   = c(35.5, 46.8, 54.8, 66.0, 85.3, 113.2, 129.6, 141.2, 157.6, 169.2, 178.2, 185.6, 201.9, 213.5, 278.3, 338.0, 434.3, 510.9, 603.4, 740.3, 847.4, 1017.8),
      `500`   = c(38.9, 51.0, 59.7, 71.9, 92.7, 123.7, 141.9, 154.8, 173.0, 185.8, 195.8, 204.0, 222.2, 235.1, 306.8, 372.5, 478.5, 562.8, 664.1, 815.0, 932.4, 1118.3),
      `1000`  = c(41.3, 54.1, 63.2, 76.1, 98.0, 131.5, 151.0, 164.9, 184.5, 198.3, 209.1, 217.9, 237.5, 251.3, 328.4, 398.5, 511.9, 602.0, 709.9, 871.5, 996.7, 1194.3),
      `10000` = c(49.4, 64.3, 74.9, 89.9, 115.4, 156.9, 181.1, 198.3, 222.5, 239.7, 253.1, 264.0, 288.2, 305.4, 399.9, 485.0, 622.9, 732.1, 862.2, 1059.1, 1210.0, 1446.4),
      `PMP`         = c(61.4, 80.0, 93.2, 112.0, 144.0, 195.0, 225.0, 247.0, 277.0, 298.0, 315.0, 328.0, 359.0, 380.0, 403.0, 433.0, 620.0, 839.0, 932.0, 1060.0, 1299.0, 1495.0),
      `PMP majorada` = c(67.54, 88.00, 102.52, 123.20, 158.40, 214.50, 247.50, 271.70, 304.70, 327.80, 346.50, 360.80, 394.90, 418.00, 443.30, 476.30, 682.00, 922.90, 1025.20, 1166.00, 1428.90, 1644.50),
      Bloco = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5, 5, 5, 5, 5, 5, 5),
      Sufix = c("- 10 min", "- 15 min", "- 20 min", "- 30 min", "- 1 h", "- 2 h",
                "- 3 h", "- 4 h", "- 6 h", "- 8 h", "- 10 h", "- 12 h", "- 18 h",
                "- 24 h", "- 2 d", "- 3 d", "- 5 d", "- 7 d", "- 10 d", "- 15 d",
                "- 20 d", "- 30 d"),
      check.names = FALSE
    )
  )
  
  subbacias_rv <- reactiveVal(
    data.frame(Nome = c("Sub1", "Sub2"), stringsAsFactors = FALSE)
  )
  
  log_rv <- reactiveVal("")
  
  # ---- (Re)construção da tabela de quantis a partir da configuração -----------
  observeEvent(input$btn_gerar_tabela, {
    
    trs <- input$trs_config
    duracoes_cfg <- duracoes_config_rv()
    
    if (is.null(trs) || length(trs) == 0) {
      showNotification("Defina ao menos um TR na Configuração.", type = "warning")
      return(NULL)
    }
    if (is.null(duracoes_cfg) || nrow(duracoes_cfg) == 0 || any(is.na(duracoes_cfg$Duracao))) {
      showNotification("Preencha todas as durações na Configuração.", type = "warning")
      return(NULL)
    }
    
    # Nova estrutura base (Duracao, Bloco, Sufix + uma coluna por TR, todas NA)
    nova_tabela <- duracoes_cfg
    for (tr in trs) {
      nova_tabela[[tr]] <- NA_real_
    }
    nova_tabela <- nova_tabela[, c("Duracao", trs, "Bloco", "Sufix")]
    
    # Preservar valores já existentes (match por Duracao + nome de coluna)
    antiga <- quantis_rv()
    if (!is.null(antiga) && nrow(antiga) > 0) {
      cols_comuns <- intersect(trs, names(antiga))
      for (i in seq_len(nrow(nova_tabela))) {
        linha_antiga <- antiga[antiga$Duracao == nova_tabela$Duracao[i], ]
        if (nrow(linha_antiga) == 1) {
          for (col in cols_comuns) {
            if (!is.na(linha_antiga[[col]])) {
              nova_tabela[i, col] <- linha_antiga[[col]]
            }
          }
        }
      }
    }
    
    quantis_rv(nova_tabela)
    showNotification("Tabela de entrada atualizada. Valores existentes foram preservados quando possível.",
                     type = "message")
  })
  
  # ---- Tabela de quantis (rhandsontable) -----------------------------------
  output$tabela_quantis <- renderRHandsontable({
    rhandsontable(quantis_rv(), rowHeaders = NULL, stretchH = "all") %>%
      hot_col("Duracao", type = "text") %>%
      hot_col("Bloco", type = "numeric") %>%
      hot_col("Sufix", type = "text")
  })
  
  observeEvent(input$tabela_quantis, {
    quantis_rv(hot_to_r(input$tabela_quantis))
  })
  
  observeEvent(input$add_linha_quantis, {
    df <- quantis_rv()
    nova <- df[1, ]
    nova[1, ] <- NA
    quantis_rv(rbind(df, nova))
  })
  
  observeEvent(input$rm_linha_quantis, {
    df <- quantis_rv()
    if (nrow(df) > 1) quantis_rv(df[-nrow(df), , drop = FALSE])
  })
  
  # Upload de planilha de quantis
  observeEvent(input$upload_quantis, {
    req(input$upload_quantis)
    tryCatch({
      df <- readxl::read_excel(input$upload_quantis$datapath)
      quantis_rv(as.data.frame(df))
      showFeedbackSuccess("upload_quantis", "Planilha importada com sucesso.")
    }, error = function(e) {
      showFeedbackDanger("upload_quantis", paste("Erro ao importar:", e$message))
    })
  })
  
  # ---- Tabela de sub-bacias -------------------------------------------------
  output$tabela_subbacias <- renderRHandsontable({
    rhandsontable(subbacias_rv(), rowHeaders = NULL, stretchH = "all")
  })
  
  observeEvent(input$tabela_subbacias, {
    subbacias_rv(hot_to_r(input$tabela_subbacias))
  })
  
  observeEvent(input$add_linha_sub, {
    df <- subbacias_rv()
    subbacias_rv(rbind(df, data.frame(Nome = NA_character_)))
  })
  
  observeEvent(input$rm_linha_sub, {
    df <- subbacias_rv()
    if (nrow(df) > 1) subbacias_rv(df[-nrow(df), , drop = FALSE])
  })
  
  # ---- Seleção de pasta -------------------------------------------------
  volumes <- c(Home = fs::path_home(), "Disco C" = "C:/")
  shinyDirChoose(input, "dir_projeto", roots = volumes, session = session)
  
  pasta_projeto <- reactive({
    if (is.null(input$dir_projeto)) return(NULL)
    parseDirPath(volumes, input$dir_projeto)
  })
  project_name <- reactive({
    p <- pasta_projeto()
    req(length(p) > 0)
    basename(p)
  })
  output$pasta_selecionada <- renderText({
    p <- pasta_projeto()
    if (is.null(p) || length(p) == 0) {
      "Nenhuma pasta selecionada."
    } else {
      paste0("Pasta: ", p, "\nNome do projeto: ", basename(p))
    }
  })
  
  # ---- TRs vindos da Configuração (aba 0) -----------------------------------
  colunas_tr <- reactive({
    req(input$trs_config)
    input$trs_config
  })
  
  output$selecao_trs <- renderUI({
    trs <- colunas_tr()
    if (length(trs) == 0) {
      return(helpText("Nenhum TR definido. Configure na aba '0. Configuração'."))
    }
    checkboxGroupInput("trs_selecionados", NULL, choices = trs, selected = trs)
  })
  
  # ---- Quantis processados (com Duracao convertida) --------------------------
  quantis_processados <- reactive({
    df <- quantis_rv()
    req(nrow(df) > 0)
    tryCatch({
      df$Duracao <- converter_duracao_minutos(df$Duracao)
      df
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 8)
      NULL
    })
  })
  
  # ---- Ajuste das curvas de Huff ---------------------------------------------
  modelos_huff <- reactive({
    tryCatch(
      ajustar_curvas_huff(dados_huff_default),
      error = function(e) {
        showNotification(paste("Erro ao ajustar curvas de Huff:", e$message), type = "error")
        NULL
      }
    )
  })
  
  # ---- Gráfico das curvas ajustadas -----------------------------------------
  output$plot_curvas_huff <- renderPlotly({
    modelos <- modelos_huff()
    req(modelos)
    
    tempo_seq <- seq(0, 100, by = 1)
    preds <- data.frame(
      Tempo = tempo_seq,
      Q1 = predict(modelos$Q1, newdata = data.frame(Tempo = tempo_seq)),
      Q2 = predict(modelos$Q2, newdata = data.frame(Tempo = tempo_seq)),
      Q3 = predict(modelos$Q3, newdata = data.frame(Tempo = tempo_seq)),
      Q4 = predict(modelos$Q4, newdata = data.frame(Tempo = tempo_seq))
    )
    
    plot_ly() %>%
      add_lines(data = preds, x = ~Tempo, y = ~Q1, name = "Q1 (ajuste)") %>%
      add_lines(data = preds, x = ~Tempo, y = ~Q2, name = "Q2 (ajuste)") %>%
      add_lines(data = preds, x = ~Tempo, y = ~Q3, name = "Q3 (ajuste)") %>%
      add_lines(data = preds, x = ~Tempo, y = ~Q4, name = "Q4 (ajuste)") %>%
      add_markers(data = dados_huff_default, x = ~Tempo, y = ~Q1, name = "Q1 (dados)", marker = list(size = 5)) %>%
      add_markers(data = dados_huff_default, x = ~Tempo, y = ~Q2, name = "Q2 (dados)", marker = list(size = 5)) %>%
      add_markers(data = dados_huff_default, x = ~Tempo, y = ~Q3, name = "Q3 (dados)", marker = list(size = 5)) %>%
      add_markers(data = dados_huff_default, x = ~Tempo, y = ~Q4, name = "Q4 (dados)", marker = list(size = 5)) %>%
      layout(xaxis = list(title = "Tempo (%)"), yaxis = list(title = "Precipitação acumulada (%)"))
  })
  
  # ---- Preview de hietograma individual --------------------------------------
  output$preview_tr_select <- renderUI({
    trs <- colunas_tr()
    req(length(trs) > 0)
    selectInput("preview_tr", "Tempo de retorno", choices = trs)
  })
  
  output$preview_duracao_select <- renderUI({
    df <- quantis_rv()
    req(nrow(df) > 0)
    selectInput("preview_duracao", "Duração", choices = df$Duracao)
  })
  
  output$plot_preview_hietograma <- renderPlotly({
    req(input$preview_tr, input$preview_duracao)
    df <- quantis_processados()
    req(df)
    modelos <- modelos_huff()
    req(modelos)
    
    linha <- df[quantis_rv()$Duracao == input$preview_duracao, ][1, ]
    req(!is.na(linha$Bloco))
    
    NB <- as.integer(linha$Duracao / linha$Bloco)
    intensidade <- linha[[input$preview_tr]]
    quartil <- determinar_quartil(linha$Duracao)
    
    precip <- gerar_hietograma(modelos[[quartil]], NB, intensidade)
    incrementos <- diff(c(0, precip))
    tempos <- seq(0, linha$Duracao, by = linha$Bloco)
    
    plot_ly(x = tempos, y = incrementos, type = "bar") %>%
      layout(title = paste("Quartil utilizado:", quartil),
             xaxis = list(title = "Tempo (min)"),
             yaxis = list(title = "Precipitação incremental (mm)"))
  })
  
  # ---- Processamento em lote --------------------------------------------------
  resumo_rv <- reactiveVal(NULL)
  
  observeEvent(input$btn_processar, {
    
    df <- quantis_processados()
    trs_sel <- input$trs_selecionados
    subs <- subbacias_rv()$Nome
    pasta <- pasta_projeto()
    
    if (is.null(df)) {
      showNotification("Corrija os dados de quantis antes de continuar.", type = "error")
      return(NULL)
    }
    if (is.null(trs_sel) || length(trs_sel) == 0) {
      showNotification("Selecione ao menos um tempo de retorno.", type = "warning")
      return(NULL)
    }
    if (is.null(pasta) || length(pasta) == 0) {
      showNotification("Selecione a pasta do projeto na aba 'Parâmetros do Projeto'.", type = "warning")
      return(NULL)
    }
    if (any(is.na(subs)) || length(subs) == 0) {
      showNotification("Preencha ao menos uma sub-bacia válida.", type = "warning")
      return(NULL)
    }
    
    modelos <- modelos_huff()
    
    log_rv("Iniciando processamento...\n")
    
    withProgress(message = "Gerando hietogramas", value = 0, {
      resultado <- gerar_arquivos_hms(
        quantis = df,
        trs = trs_sel,
        basin = input$basin_name,
        subbasins = subs,
        projectName = project_name(),
        projectFolder = pasta,
        modelos_huff = modelos,
        progress_callback = function(frac, nome_evento) {
          incProgress(1 / (length(trs_sel) * nrow(df)), detail = nome_evento)
          log_rv(paste0(log_rv(), "Processado: ", nome_evento, "\n"))
        }
      )
    })
    
    log_rv(paste0(log_rv(), "\nConcluído! Total de eventos: ", resultado$total_eventos, "\n"))
    
    resumo_rv(data.frame(
      Projeto = project_name(),
      Pasta = pasta,
      `Total de eventos` = resultado$total_eventos,
      `TRs processados` = paste(trs_sel, collapse = ", "),
      `Arquivos gerados` = paste(basename(resultado$arquivos), collapse = ", "),
      check.names = FALSE
    ))
    
    showNotification("Processamento concluído!", type = "message")
  })
  
  output$log_processamento <- renderText({ log_rv() })
  
  output$tabela_resumo <- renderDT({
    req(resumo_rv())
    datatable(resumo_rv(), options = list(dom = "t"))
  })
}

runApp(shinyApp(ui, server))

# Ao fechar o navegador, a sessão do Shiny termina e o app para (stopApp()).
# Em seguida, reinicia a sessão do R — funciona apenas dentro do RStudio.
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  rstudioapi::restartSession()
}

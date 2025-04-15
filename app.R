library(shiny)  # Biblioteca principal para criação de aplicativos interativos
library(lubridate)  # Para manipulação de datas
library(bslib)  # Para aplicar um tema baseado na identidade MUVA

# Definir a data de início e data final (90 dias após 3 de fevereiro)
data_inicio <- as.POSIXct("2025-02-01", format = "%Y-%m-%d", tz = "UTC")
data_final <- data_inicio + days(90)

# Lista de prioridades com datas reais ajustadas
prioridades <- list(
  list(nome = "Rever as responsabilidades dos PM e fazer kick-off de todos os projetos",
       deadline = as.POSIXct("2025-02-26", format = "%Y-%m-%d", tz = "UTC"),
       responsavel = "Direção e PMs",
       Status= "A ser realizado"),
  list(nome = "Plano de comunicação para 2025",
       deadline = as.POSIXct("2025-02-14", format = "%Y-%m-%d", tz = "UTC"),
       responsavel = "Rebeca",
       Status= "A espera da aprovação da Direcção"),
  list(nome = "Revisar pagamento MEIO-MELS",
       deadline = as.POSIXct("2025-02-21", format = "%Y-%m-%d", tz = "UTC"),
       responsavel = "Ana",
       Status= "Concluído"),
  list(nome = "Mesa redonda sobre empregabilidade",
       deadline = as.POSIXct("2025-03-11", format = "%Y-%m-%d", tz = "UTC"),
       responsavel = "-",
       Status= "Concluído"),
  list(nome = "Política de RH",
       deadline = as.POSIXct("2025-03-28", format = "%Y-%m-%d", tz = "UTC"),
       responsavel = "Inocencio",
       Status= "Em Andamento"),
  list(nome = "Finalizar transição para Fundação",
       deadline = as.POSIXct("2025-03-28", format = "%Y-%m-%d", tz = "UTC"),
       responsavel = "Luize & Lucia",
       Status= "Em Andamento"),
  list(nome = "Rever alocação da equipe de operação",
       deadline = as.POSIXct("2025-03-28", format = "%Y-%m-%d", tz = "UTC"),
       responsavel = "Inocencio",
       Status= "Em Andamento"),
  list(nome = "Trabalhar a ligação territorial e OCBs",
       deadline = as.POSIXct("2025-03-28", format = "%Y-%m-%d", tz = "UTC"),
       responsavel = "Lucia, Tania & Suraia",
       Status= "Em Andamento"),
  list(nome = "Calendário chinês digital",
       deadline = as.POSIXct("2025-04-30", format = "%Y-%m-%d", tz = "UTC"),
       responsavel = "Dercio",
       Status= "Concluído"),
  list(nome = "Novos encontros regulares para garantir melhor comunicação interna",
       deadline = as.POSIXct("2025-02-26", format = "%Y-%m-%d", tz = "UTC"),
       responsavel = "Direção e PMs",
       Status= "Concluído"),
  list(nome = "Planos de carreiras: pétalas, grelha e níveis de competência",
       deadline = as.POSIXct("2025-03-28", format = "%Y-%m-%d", tz = "UTC"),
       responsavel = "Direção e PMs",
       Status= "A ser realizado"),
  list(nome = "Estar presente nos grupos de trabalho Governo-doador",
       deadline = as.POSIXct("2025-02-01", format = "%Y-%m-%d", tz = "UTC"),
       responsavel = "Paula e Geraldo",
       Status= "Concluído"),
  list(nome = "Definir os apoios dos transversais para cada intervenção",
       deadline = as.POSIXct("2025-03-28", format = "%Y-%m-%d", tz = "UTC"),
       responsavel = "Luize",
       Status= "Concluído"),
  list(nome = "Activar o nosso sistema de parceria",
       deadline = as.POSIXct("2025-03-28", format = "%Y-%m-%d", tz = "UTC"),
       responsavel = "Rebeca, Iana e Lourreine",
       Status= "A ser realizado")
)



# Interface do usuário (UI)
ui <- fluidPage(
  theme = bs_theme(bg = "#6B4FA4", fg = "#30E3CA", primary = "#FF8F4E"),  # Aplicação do tema baseado na identidade MUVA

  tags$head(
    tags$style(HTML("#relogio { font-size: 50px; text-align: center; font-weight: bold; color: #FF8F4E; } .card { padding: 20px; border-radius: 10px; box-shadow: 0px 0px 10px rgba(255, 255, 255, 0.1); margin-bottom: 15px; background-color: #512D6D; color: #FFFFFF; } .progress-bar { font-size: 14px; text-align: center; color: black; background-color: #30E3CA; } h3 { font-weight: bold; text-align: center; color: #FF8F4E; } .progress-bar-danger { background-color: #FF4C4C; }"))
  ),

  div(class = "card",
      h3("⏳ Controle de Prazos: Corra Que Ainda Dá Tempo!"),
      div(id = "relogio", textOutput("contador")),  # Exibição do tempo restante
      h4("Previsão de término:", align = "center"),
      tags$div(style = "text-align: center; font-size: 18px;", textOutput("previsao_termino"))
  ),

  div(class = "card",
      h3("📊 Planeamento em Ação: Porquê Deixar para Depois?"),
      uiOutput("lista_prioridades")  # Exibição dinâmica das prioridades
  )
)

# Servidor
server <- function(input, output, session) {
  autoInvalidate <- reactiveTimer(1000)  # Atualização automática a cada segundo

  output$contador <- renderText({
    autoInvalidate()
    tempo_restante <- as.numeric(difftime(data_final, Sys.time(), units = "secs"))

    if (tempo_restante > 0) {
      dias <- as.integer(tempo_restante %/% (24 * 3600))
      horas <- as.integer((tempo_restante %% (24 * 3600)) %/% 3600)
      minutos <- as.integer((tempo_restante %% 3600) %/% 60)
      segundos <- as.integer(tempo_restante %% 60)
      sprintf("%02d dias | %02d:%02d:%02d", dias, horas, minutos, segundos)  # Formato grande e destacado
    } else {
      "00:00:00:00"
    }
  })

  output$previsao_termino <- renderText({
    format(data_final, "%d/%m/%Y %H:%M:%S")  # Exibição da data final formatada
  })

  output$lista_prioridades <- renderUI({
    prioridades_ordenadas <- prioridades[order(sapply(prioridades, function(p) p$deadline), decreasing = TRUE)]

    lapply(rev(prioridades_ordenadas), function(p) {
      progresso <- max(0, min(100, as.numeric(difftime(Sys.time(), data_inicio, units = "secs")) / as.numeric(difftime(p$deadline, data_inicio, units = "secs")) * 100))
      # Calcular o número de dias passados
      dias_passados <- as.numeric(difftime(Sys.time(), data_inicio, units = "days"))

      # Calcular o total de dias do prazo
      dias_totais <- as.numeric(difftime(p$deadline, data_inicio, units = "days"))

      cor_barra <- if (p$Status == "Concluído") {
        "progress-bar-success"  # Azul Escuro para indicar que a tarefa foi finalizada
      } else if (progresso >= 100 && p$Status != "Concluído") {
        "progress-bar-danger"   # Vermelho Forte se o prazo expirou e não foi concluído
      } else if (progresso >= 50 && progresso < 100 && p$Status != "Concluído") {
        "progress-bar-warning"  # Laranja Intenso para alertar sobre a proximidade do vencimento
      } else {
        "progress-bar-dark"  # Verde Vibrante para progresso normal
      }

      tags$head(
        tags$style(HTML("
      .progress-bar-success { background-color: #004085 !important; } /* Azul Escuro - Concluído */
      .progress-bar-danger { background-color: #8B0000 !important; }  /* Vermelho Forte - Prazo expirado */
      .progress-bar-warning { background-color: #FF8C00 !important; } /* Laranja Intenso - Próximo do vencimento */
      .progress-bar-primary { background-color: #228B22 !important; } /* Verde Vibrante - Dentro do prazo */
      .progress-bar-dark { background-color: #000000 !important; color: white !important; } /* Preto - Sem progresso ou bloqueado */
  "))
      )



      tagList(
        h5(paste(p$nome, "- Data limite:", format(p$deadline, "%d/%m/%Y"), "---------Status:", p$Status)),
        div(class = "progress",
            div(class = paste("progress-bar", cor_barra),
                role = "progressbar",
                style = paste0("width: ", progresso, "%"),
                {
                  # Mensagem de status
                  if (progresso >= 100 && p$Status == "Concluído") {
                    "Parabéns, foi concluído dentro do prazo!"
                  } else if (progresso >= 100) {
                    "Fora do prazo!"
                  } else {
                    paste0("Já se passou ", round(dias_totais, 1), " dias do prazo total.")
                  }
                }
            )
        ),
        p(paste("Responsável:", p$responsavel), style = "font-size: 14px; color: #30E3CA;")
      )


    })
  })
}

# Executar o app
shinyApp(ui = ui, server = server)  # Inicialização do aplicativo Shiny


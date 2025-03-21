library(shiny)
library(bslib)
library(DT)
library(Epi)
library(psych)

ui <- page_fluid(
  titlePanel("Statistics with R for begginers"),
  
  # Instructions
  uiOutput("instructions"),
  textOutput("explanations"),
  
  layout_columns(
    col_width=2,
    card(
      # Zone de saisie pour entrer du code
      textAreaInput("code_input", "Type your code here :", rows = 1),
      
      # Affichage de la "console"
      verbatimTextOutput("console_output"),
      
      # Boutons d'exécution et de passage à l'étape suivante
      actionButton("run_code", "Execute"),
      actionButton("next_step", "Next", disabled = TRUE),
      
      # Feedback sur la validation
      textOutput("feedback")
    ),
    card(
      # Affichage des objets en mémoire comme dans RStudio
      h4("📂 Environment"),
      tabsetPanel(
        tabPanel("📊 Data", DTOutput("data_memory")),
        tabPanel("🔢 Values", DTOutput("values_memory"))
      )
    )
  ),
  
  # Résultat du code exécuté
  verbatimTextOutput("execution_output"),
  
  plotOutput("user_plot")
  
)

server <- function(input, output, session) {
  
  # suppression de toutes les variables globales
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  
  # Liste des étapes du tutoriel
  steps <- list(
    list(
      instruction = "🔹 Upload data from the file 'smcovid.csv'.",
      validation = function() {
        objs <- ls(envir = .GlobalEnv)
        valid <- any(sapply(objs, function(x) inherits(get(x, envir = .GlobalEnv), "data.frame")))
        return(valid)
      },
      solution = "smc <- read.csv2('smcovid.csv')"
    ),
    list(
      instruction = "🔹 Observe the data you uploaded.",
      explanations = "Try functions such as head, str, summary, describe and dim.",
      validation = function() {
        T
      },
      # pas de validation. A terme, demander qch sur ces fonctions, genre le sd, la première valeur, etc dans un QCM
      conclusion = "Have you seen the difference between these functions ?"
    ),
    list(
      instruction = "🔹 How many features do you have for age ? Stock the value in tot.",
      explanations = "You can have age with the formula data$age",
      validation = function() {
        if (!"tot" %in% ls(envir = .GlobalEnv)) return(FALSE)  # Vérifie si 'tot' existe
        value <- get("tot", envir = .GlobalEnv)  # Récupère la valeur de 'tot'
        return(is.numeric(value) && value == 748)  # Vérifie si c'est un nombre et vaut 748
      },
      solution = "tot <- sum(table(data$age))",
      conclusion = "There are as many values for age than there are for the dataset. Which information does it give you ?"
    ),
    list(
      instruction = "🔹 Draw a histogram of age.",
      validation = function() {
        grepl(paste0("hist\\(", df_name(), "\\$age\\)"), input$code_input)
      },
      solution = reactive({
        if (!is.null(df_name())) {
          paste0("hist(", df_name(), "$age)")
        } else {
          "hist(data$age)"  # Valeur par défaut si df_name() est NULL
        }
      }),
      conclusion = "Looking at the histogram can give clue about wether the variable follows or not a normal distribution. What do you think ? \n"
    ),
    list(
      instruction = "🔹 What is the mean of age ?",
      validation = function() {
        T
      },
      solution = reactive({
        if (!is.null(df_name())) {
          paste0("mean(", df_name(), "$age)")
        } else {
          "mean(data$age)"  # Valeur par défaut si df_name() est NULL
        }
      }),
      conclusion = "You can also check standard deviation with sd(). Now we know the mean of the age of the 748 people that conduct the study. But what about the mean age of our global population ?"
    ),
    list(
      instruction = "🔹 **Step 6** : What is the mean of age of our global population ?"
    )
  )
  
  
  # Étape actuelle du tutoriel
  current_step <- reactiveVal(1)
  df_name <- reactiveVal(NULL)
  
  # Instructions mises à jour
  output$instructions <- renderUI({
    step <- current_step()
    if (step <= length(steps)) {
      HTML(paste("<h4>", steps[[step]]$instruction, "</h4>"))
    } else {
      HTML("<h3>🎉 Congrats ! You just finished the tutorial !</h3>")
    }
  })
  output$explanations <- renderText({
    step <- current_step()
    if (step <= length(steps)) {
      HTML(paste(steps[[step]]$explanations))
    }
  })
  
  # Affichage type "console"
  output$console_output <- renderText({
    paste0("> ", input$code_input)
  })
  
  # Exécution du code utilisateur
  observeEvent(input$run_code, {
    step <- current_step()
    if (step > length(steps)) return(NULL)  # Fin du tutoriel
    
    output$feedback <- renderText("")
    
    user_code <- input$code_input
    validation_fn <- steps[[step]]$validation
    
    # Test du code utilisateur
    tryCatch({
      
      output_capture <- capture.output({
        eval(parse(text = user_code), envir = .GlobalEnv)
        })
      
      # Vérifier si le code contient un appel à une fonction de plot
      if (grepl("\\bplot\\b|\\bhist\\b|\\bggplot\\b|\\bboxplot\\b", user_code)) {
        output$user_plot <- renderPlot({
          eval(parse(text = user_code), envir = .GlobalEnv)
        })
      } else {
        output$user_plot <- renderPlot(NULL)  # Efface le graphique si aucun plot
      }
      
      # Affichage du résultat
      output$execution_output <- renderText({
        paste(output_capture, collapse = "\n")
      })
      
      # Vérification de la réponse
      if (validation_fn()) {
        if (step == 1){
          objs <- ls(envir = .GlobalEnv)
          dataframes <- objs[sapply(objs, function(x) inherits(get(x, envir = .GlobalEnv), "data.frame"))]
          df_name(dataframes[1]) # On vérifie pas que dataframes n'est pas nul car c'est déjà la condition de validité de l'étape 1
        }
        output$feedback <- renderText(paste("✅ Correct !", steps[[step]]$conclusion, " Click on 'Next' to continue"))
        updateActionButton(session, "next_step", disabled = FALSE)
      } else {
        output$feedback <- renderText(paste("❌ Incorrect, try again! If your're blocked, try that:",steps[[step]]$solution))
        # updateTextAreaInput(session, "code_input", value = steps[[step]]$solution)
      }
      
    }, error = function(e) {
      output$execution_output <- renderText("")  # Effacer la sortie
      output$feedback <- renderText(paste("🚨 Erreur dans le code :", e$message))
    })
  })
  
  # Passer à l'étape suivante
  observeEvent(input$next_step, {
    step <- current_step()
    if (step < length(steps)) {
      current_step(step + 1)
      updateTextAreaInput(session, "code_input", value = "")  # Réinitialisation du champ
      output$feedback <- renderText("")  # Effacer les messages
      output$execution_output <- renderText("")
      updateActionButton(session, "next_step", disabled = TRUE)  # Désactiver "Suivant"
    } else {
      output$instructions <- renderUI({
        HTML("<h3>🎉 Félicitations ! Vous avez terminé le tutoriel !</h3>")
      })
    }
  })
  
  # Fonction pour récupérer les objets en mémoire
  get_memory_objects <- reactivePoll(
    intervalMillis = 1000,  # Vérifie toutes les 1 seconde
    session = session,
    checkFunc = function() {
      ls(envir = .GlobalEnv)  # Vérifie les objets présents
    },
    valueFunc = function() {
      objs <- ls(envir = .GlobalEnv)
      if (length(objs) == 0) return(data.frame(Nom = "Aucun objet en mémoire", Type = "", Taille = ""))
      
      data <- data.frame(
        Nom = objs,
        Type = sapply(objs, function(x) class(get(x, envir = .GlobalEnv))[1]),
        Taille = sapply(objs, function(x) format(object.size(get(x, envir = .GlobalEnv)), units = "auto")),
        stringsAsFactors = FALSE
      )
      
      return(data)
    }
  )
  
  # Mise à jour du tableau "Data"
  output$data_memory <- renderDT({
    data_mem <- get_memory_objects()
    data_mem <- data_mem[data_mem$Type %in% c("data.frame", "tibble"), ]
    if (nrow(data_mem) == 0) data_mem <- data.frame(Nom = "Aucune table de données")
    datatable(data_mem, options = list(pageLength = 5))
  })
  
  # Mise à jour du tableau "Values"
  output$values_memory <- renderDT({
    val_mem <- get_memory_objects()
    val_mem <- val_mem[!val_mem$Type %in% c("data.frame", "tibble"), ]
    if (nrow(val_mem) == 0) val_mem <- data.frame(Nom = "Aucune variable en mémoire")
    datatable(val_mem, options = list(pageLength = 5))
  })
}

shinyApp(ui = ui, server = server)
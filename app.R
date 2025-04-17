library(shiny)
library(bslib)
library(DT)
library(Epi)
library(psych)
library(jsonlite)

ui <- page_fluid(
  layout_sidebar(
    sidebar = sidebar(
      open = "closed",
      title = "Go straight to what interest you :",
      actionButton("intro", "Go back to the beggining"),
      actionButton("binar_test", "Statistical tests with 2 binary var"),
      actionButton("continuous_test", "Statistical tests with continuous var"),
      actionButton("lm", "Linear Regression")
      ),
  
    titlePanel("Statistics with R for begginers"),
    uiOutput("progress_bar"),
    
    # Instructions
    uiOutput("instructions"),
    textOutput("explanations"),
  
  
  layout_columns(
    card(
      # Zone de saisie pour entrer du code
      uiOutput("code_input_ui"),
      
      # Affichage de la "console"
      verbatimTextOutput("console_output"),
      
      # Boutons d'exécution et de passage à l'étape suivante
      actionButton("run_code", "Execute"),
      actionButton("next_step", "Next", disabled = TRUE),
      
      # Feedback sur la validation
      textOutput("feedback")
    ),
    card(
      uiOutput("questionnaire")
    ),
    card(
      class = "p-3",
      style = "background-color: #f8f9fa; border: 1px solid #dee2e6; font-family: 'Courier New', monospace;",
      
      h4("📂 Environment", style = "margin-bottom: 15px;"),
      
      tabsetPanel(
        tabPanel(
               tags$div("📊 Data", style = "font-weight: bold; margin-bottom: 5px;"),
               DTOutput("data_memory")
        ),
        tabPanel(
               tags$div("🔢 Values", style = "font-weight: bold; margin-bottom: 5px;"),
               DTOutput("values_memory")
        )
      )
    ))
  ),
  
  layout_columns(
    # Résultat du code exécuté
    verbatimTextOutput("execution_output"),
    
    plotOutput("user_plot") # AFFICHER LA ZONE SLMT S'IL Y A UN PLOT ET PAS OUTPUT
  )
)

server <- function(input, output, session) {
  
  # suppression de toutes les variables globales
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  assign("a", "This is a sentence", envir = .GlobalEnv)
  assign("b", -27L, envir = .GlobalEnv)
  assign("name", data("mtcars"), envir = .GlobalEnv)
  
  delete_feedback <- function(){
    updateTextAreaInput(session, "code_input", value = "")  # Réinitialisation du champ
    output$feedback <- renderText("")  # Effacer les messages
    output$execution_output <- renderText("")
    updateActionButton(session, "next_step", disabled = TRUE)  # Désactiver "Suivant"
  }
  
  steps <- fromJSON("steps.json", simplifyVector = FALSE)
  
  is_validate <- function(step_idx) {
    # Récupère le code
    code <- isolate(input$code_input)
    if (is.null(code)) return(FALSE)  # protection si code est NULL
    
    # Vérifie la validation par mots-clés (si elle existe)
    keywords <- steps[[step_idx]]$validation
    if (!is.null(keywords) && length(keywords) > 0) {
      if (!all(sapply(keywords, function(k) grepl(k, code)))) {
        return(FALSE)
      }
    }
    
    # Vérification spéciale selon l'id
    id <- steps[[step_idx]]$id
    if (!is.null(id) && grepl("check", id)) {
      return(tryCatch({
        df <- get("data", envir = .GlobalEnv) 
        
        if (grepl("data", id)) {
          objs <- ls(envir = .GlobalEnv)
          dataframes <- objs[sapply(objs, function(x) inherits(get(x, envir = .GlobalEnv), "data.frame"))]
          if (length(dataframes) == 0) return(FALSE)
        }
        
        if (grepl("b", id)) {
          if (!"Mai_depression.b" %in% colnames(df)) return(FALSE)
        }
        
        TRUE  # Si tout est bon
      }, error = function(e) {
        FALSE
      }))
    }
    
    return(TRUE)  # Si aucune condition spéciale n’est requise
  }
  
  output$progress_bar <- renderUI({
    step <- current_step()
    total <- length(steps)
    progress_percent <- round((step - 1) / total * 100)
    
    tagList(
      tags$div(style = "width: 100%; background-color: #eee; height: 20px; border-radius: 10px; overflow: hidden;",
               tags$div(
                 style = paste0(
                   "width:", progress_percent, "%;",
                   "height: 100%;",
                   "background-color: #4CAF50;",
                   "text-align: center;",
                   "line-height: 20px;",
                   "color: white;",
                   "font-weight: bold;"
                 ),
                 paste0(progress_percent, "%")
               )
      )
    )
  })
  
  # Étape actuelle du tutoriel
  current_step <- reactiveVal(1)
  df_name <- reactiveVal(NULL)
  validation_state_q <- reactiveValues(valid = FALSE)
  validation_state_d <- reactiveValues(valid = F)
  
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
  
  output$code_input_ui <- renderUI({
    step <- current_step()
    rows <- ifelse(step > 5, 2, 1)  # Change X par le numéro de l'étape où il faut plus de place
    textAreaInput("code_input", "Type your code here :", rows = rows, width = 1000)
  })
  
  # Affichage type "console"
  output$console_output <- renderText({
    code_lines <- unlist(strsplit(input$code_input, "\n"))  # Sépare par lignes
    paste0("> ", code_lines, collapse = "\n")  # Ajoute ">" à chaque ligne
  })
  
  # Vérification des réponses
  observeEvent({
    step <- current_step()
    if (step > length(steps)) return()  # Fin du tutoriel

    questions <- steps[[step]]$questions
    if (is.null(questions)) {
      validation_state_q$valid <- T
      return()  # Si pas de questions, ne rien faire
    }

    valid <- TRUE
    
    for (i in seq_along(questions)) {
      question <- questions[[i]]
      answer_id <- paste0("question_", step, ".",i)
      user_answer <- input[[answer_id]]
      correct <- question$options[question$correct]

      if (is.null(user_answer) || user_answer != correct) {
        valid <- FALSE
        break  # Dès qu'une réponse est fausse, on arrête
      }
    }

    # Mise à jour de l'état de validation
    isolate({ validation_state_q$valid <- valid })
    
    
    if(validation_state_d$valid) {
      if (valid) output$feedback <- renderText(paste("✅ Correct !", steps[[step]]$conclusion, " Click on 'Next' to continue"))
      else output$feedback <- renderText(paste("✅ Correct ! Answer to the questions to continue."))
      updateActionButton(session, "next_step", disabled = !valid)
    }

  }, eventExpr = {
    tagList(
      lapply(seq_along(steps[[current_step()]]$questions), function(i) {
        qid <- paste0("question_", current_step(), ".", i)
        input[[paste0(qid)]]
      })
    )
  })
  
  output$questionnaire <- renderUI({
    step <- current_step()
    if (step <= length(steps)) {
      questions <- steps[[step]]$questions
      if (!is.null(questions)) {
        tagList(
          lapply(seq_along(questions), function(i) {
            question <- questions[[i]]
            qid <- paste0("question_", step, ".", i)
            
            # On garde la sélection de l'utilisateur s'il y en a une
            selected_val <- if (!is.null(input[[qid]])) input[[qid]] else character(0)
            
            tagList(
              radioButtons(
                inputId = qid,
                label = question$question,
                choices = question$options,
                selected = selected_val,
                inline = TRUE
              ),
              if (!is.null(input[[qid]]) && input[[qid]] == question$options[question$correct] && !is.null(question$cl)) {
                div(style = "color: green; font-weight: bold;", question$cl)
              }
            )
          })
        )
      }
    }
  })
  
  # Exécution du code utilisateur
  observeEvent(input$run_code, { # REGLER str(data) QUI FONCTIONNE PAS
    step <- current_step()
    if (step > length(steps)) return(NULL)  # Fin du tutoriel
    
    output$feedback <- renderText("")
    
    user_code <- input$code_input
    
    # Test du code utilisateur
    tryCatch({
      
      # Séparer le code en plusieurs lignes
      lines <- unlist(strsplit(user_code, "\n"))
      all_output <- c()  # Stocke toutes les sorties
      
      for (line in lines) {
        if (nchar(trimws(line)) > 0) {  # Ignore les lignes vides
          result <- tryCatch({
            eval(parse(text = line), envir = .GlobalEnv)
          }, error = function(e) {
            paste("🚨 Erreur :", e$message) # GERER LES ERREURS POUR LA VALIDATION
          })
          
          line_output <- capture.output(print(result))  # Affiche la sortie explicite
          
          all_output <- c(all_output, paste("> ", line), line_output, "")  # Ajoute une ligne vide entre chaque commande
        }
      }
      
      # Affichage du résultat
      output$execution_output <- renderText({
        paste(all_output, collapse = "\n")
      })
      
      # Vérifier si le code contient un appel à une fonction de plot
      if (grepl("\\bplot\\b|\\bhist\\b|\\bggplot\\b|\\bboxplot\\b", user_code)) {
        output$user_plot <- renderPlot({
          eval(parse(text = user_code), envir = .GlobalEnv)
        })
      } 
      #else {
        #output$user_plot <- renderPlot(NULL)  # Efface le graphique si aucun plot
      #}
      
      # Vérification de la réponse
      if (is_validate(step)) { # AJOUTER UNE VERIFICATION DE LA REPONSE : si erreur ou NA, pas valider
        
        if (!is.null(steps[[step]]$id) && steps[[step]]$id == "check_b"){
          # Créer la nouvelle variable Mai_anxiete.b si l'étape de création de Mai_depression.b est réussie
          df <- get("data", envir = .GlobalEnv)  # Récupérer la dataframe
          df$Mai_anxiete.b <- ifelse(df$Mai_anxiete >= 2, 1, 0)
          df$Oct_anixete.b <- ifelse(df$Oct_anxiete >=2, 1, 0)
          assign("data", df, envir = .GlobalEnv) 
        }
        
        validation_state_d$valid = T
        if(validation_state_q$valid){
          output$feedback <- renderText(paste("✅ Correct !", steps[[step]]$conclusion, " Click on 'Next' to continue"))
          updateActionButton(session, "next_step", disabled = FALSE)
        }
        else output$feedback <- renderText(paste("✅ Correct ! Answer to the questions to continue."))
      } else {
        output$feedback <- renderText(paste("❌ Incorrect, try again! If your're blocked, try that:",steps[[step]]$solution))
        # updateTextAreaInput(session, "code_input", value = steps[[step]]$solution)
      }
      
    }, error = function(e) {
      #output$execution_output <- renderText("")  # Effacer la sortie
      output$feedback <- renderText(paste("🚨 Erreur dans le code :", e$message))
    })
  })
  
  # Passer à l'étape suivante
  observeEvent(input$next_step, {
    step <- current_step()
    validation_state_d$valid <- F
    if (step < length(steps)) {
      current_step(step + 1)
      delete_feedback()
    } else {
      output$instructions <- renderUI({
        HTML("<h3>🎉 Félicitations ! Vous avez terminé le tutoriel !</h3>")
      })
    }
  })
  
  observeEvent(input$intro, {
    current_step(2)
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
    delete_feedback()
  })
  
  find_idx <- function(name) {which(sapply(steps, function(step) !is.null(step$id) && step$id == name))}
  
  binar_idx <- find_idx("binar_test")
  continuous_idx <- find_idx("continuous_test")
  lm_idx <- find_idx("lm")
  
  initialize_data_if_needed <- function() {
    
    if (!("data" %in% ls(envir = .GlobalEnv)) || !"Mai_depression" %in% names(data)) {
      rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
      assign("data", read.csv2("smcovid.csv"), envir = .GlobalEnv)
    }
    assign("tot", 748, envir = .GlobalEnv)
    
    if (!"Mai_depression.b" %in% names(get("data", envir = .GlobalEnv))) { # As they are created in the same step if one doesn't exist neither the other
      data <- get("data", envir = .GlobalEnv)
      tryCatch({
        data$Mai_depression.b <- ifelse(data$Mai_depression >= 2, 1, 0)
        data$Mai_anxiete.b <- ifelse(data$Mai_anxiete >= 2, 1, 0)
        data$Oct_anxiete.b <- ifelse(data$Oct_anxiete >= 2, 1, 0)
      }, error = function(e) {
          print(e)
        }
      )
      assign("data", data, envir = .GlobalEnv)
    }
  }
  
  
  observeEvent(input$binar_test, {
    current_step(binar_idx)
    delete_feedback()
    initialize_data_if_needed()
  })
  observeEvent(input$continuous_test, {
    current_step(continuous_idx)
    delete_feedback()
    initialize_data_if_needed()
  })
  observeEvent(input$lm, {
    current_step(lm_idx)
    delete_feedback()
    initialize_data_if_needed()
  })
  
  # Fonction pour récupérer les objets en mémoire
  get_memory_objects <- function() {
    objs <- ls(envir = .GlobalEnv)
    objs <- objs[!sapply(objs, function(x) is.function(get(x, envir = .GlobalEnv)))]
    
    info <- lapply(objs, function(obj_name) {
      obj <- get(obj_name, envir = .GlobalEnv)
      obj_class <- class(obj)[1]
      
      # Taille ou dimensions
      if (is.data.frame(obj)) {
        taille <- paste(nrow(obj), "obs. of", ncol(obj), "variables")
        valeur <- ""
      } else if (length(obj) == 1) {
        taille <- "1"
        valeur <- as.character(obj)
      } else {
        taille <- length(obj)
        valeur <- ""
      }
      
      list(
        Nom = obj_name,
        Type = obj_class,
        Taille = taille,
        Valeur = valeur
      )
    })
    
    do.call(rbind, lapply(info, as.data.frame, stringsAsFactors = FALSE))
  }
  memoryData <- reactivePoll(1000, session,
    checkFunc = function() {
      ls(envir = .GlobalEnv)  # Change si un objet change
    },
    valueFunc = function() {
      get_memory_objects()
    }
  )
  
  # Mise à jour du tableau "Data"
  output$data_memory <- renderDT({
    data_mem <- memoryData()
    data_mem <- data_mem[data_mem$Type %in% c("data.frame", "tibble"), ]
    if (is.null(nrow(data_mem)) || nrow(data_mem) == 0) data_mem <- data.frame(Nom = "No dataset in memory", Type = "", Taille = "", Valeur = "")
    datatable(data_mem[, c("Nom", "Type", "Taille")], options = list(dom = 't', pageLength = 5))
  })
  
  
  # Mise à jour du tableau "Values"
  output$values_memory <- renderDT({
    val_mem <- memoryData()
    val_mem <- val_mem[!val_mem$Type %in% c("data.frame", "tibble"), ]
    if (is.null(nrow(val_mem)) || nrow(val_mem) == 0) val_mem <- data.frame(Nom = "No variable in memory", Type = "", Taille = "", Valeur = "")
    datatable(val_mem[, c("Nom", "Type", "Valeur")], options = list(dom = 't', pageLength = 5))
  })
  
}

shinyApp(ui = ui, server = server)
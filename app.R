library(shiny)
library(bslib)
library(DT)
library(Epi)
library(psych)

generate_reactive_string <- function(valid_expr, default_expr) {
  reactive({
    if (!is.null(df_name())) {
      valid_expr
    } else {
      default_expr # Valeur par défaut si df_name() est NULL
    }
  })
}


ui <- page_fluid(
  titlePanel("Statistics with R for begginers"),
  
  # Instructions
  uiOutput("instructions"),
  textOutput("explanations"),
  
  layout_columns(
    col_width=2,
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
      conclusion = "Have you seen the difference between these functions ?",
      solution = "head(data)"
    ),
    list(
      instruction = "🔹 How many features do you have for age ? Stock the value in tot.",
      explanations = "Little reminder: you can catch age with the formula data$age.",
      validation = function() {
        if (!"tot" %in% ls(envir = .GlobalEnv)) return(FALSE)  # Vérifie si 'tot' existe
        value <- get("tot", envir = .GlobalEnv)  # Récupère la valeur de 'tot'
        return(is.numeric(value) && value == 748)  # Vérifie si c'est un nombre et vaut 748
      },
      solution = generate_reactive_string(
        valid_expr = paste0("tot <- sum(table(", df_name(), "$age))"), 
        default_expr = "tot <- sum(table(data$age))"
        ),
      conclusion = "There are as many values for age than there are for the dataset. Which information does it give you ?"
    ),
    list(
      instruction = "🔹 What is the mean of age ?",
      validation = function() {
        # AJOUTER UNE VERIFICATION DE TYPE
        if (is.null(df_name())) return(FALSE)
        expected_value <- mean(get(df_name(), envir = .GlobalEnv)$age, na.rm = TRUE) # calcul de la moyenne
        
        # Capture et évalue la sortie utilisateur
        tryCatch({
          user_value <- eval(parse(text = input$code_input), envir = .GlobalEnv)
          
          if (is.numeric(user_value) && !is.na(user_value)) {  # Vérifie si c'est bien un nombre
            return(abs(user_value - expected_value) < 1e-6)  # Vérifie si la valeur est proche de la vraie valeur
          } else {
            return(FALSE)
          }
        }, error = function(e) {
          return(FALSE)  # Retourne FALSE si une erreur survient
        })
      },
      solution = generate_reactive_string(
        valid_expr = paste0("mean(", df_name(), "$age)"),
        default_expr = "mean(data$age)"
      ),
      conclusion = {
        "If you didn't do so, you can add the option na.rm = T which allows to calculate the mean if some data was missing.
        You can also check standard deviation with sd(). 
        Now we know the mean of the age of the 748 people that conduct the study. 
        But what about the mean age of our global population ? \n"
        }
    ),
    list(
      instruction = "🔹 What is the mean of age of our global population ?",
      explanations = "Find the 95% confidence interval.",
      validation = function() {
        if (is.null(df_name())) return(FALSE)
        T
        # A COMPLETER
      },
      solution = generate_reactive_string(
        valid_expr = paste0("mean(", df_name(), "$age)-1.96*sd(", df_name(), "$age)/sqrt(tot), mean(", df_name(), "$age)+1.96*sd(", df_name(), "$age)/sqrt(tot)"),
        default_expr = "mean(data$age)-1.96*sd(data$age)/sqrt(tot), mean(data$age)+1.96*sd(data$age)/sqrt(tot)"
        ),
      conclusion = "What does it mean ? It means that the global mean of the population is 95% likely to be in between these 2 values."
    ),
    list(
      instruction = "🔹 Draw a histogram of age.",
      validation = function() {
        if (is.null(df_name())) return(F)
        grepl(paste0("hist\\(", df_name(), "\\$age\\)"), input$code_input)
      },
      solution = generate_reactive_string(
        valid_expr = paste0("hist(", df_name(), "$age)"),
        default_expr = "hist(data$age)"
      ),
      conclusion = "Looking at the histogram can give you a clue wether the variable follows a normal distribution or not. What do you think ? \n"
    ),
    list(
      instruction = "🔹 Display the percentage of people that had depression in May.",
      explanations = "That is with Mai_depression==3",
      validation = function() {
        if (is.null(df_name())) return(FALSE)
        grepl(paste0("prop.table(table(", df_name(), "$Mai_depression==3)"), input$code_input)
      },
      solution = generate_reactive_string(
        valid_expr = paste0("prop.table(table(", df_name(), "$Mai_depression==3))*100"),
        default_expr = "prop.table(table(data$Mai_depression==3))*100"
      )
    ),
    list(
      instruction = "🔹 Let's create a binary variable, Mai_depression.b.",
      explanations = "To conduct tests on percentages, we need binary variables. Put the threshold at 1.5.",
      validation = function() {
        if (is.null(df_name())) return(FALSE)
        var_name <- paste0(df_name(), "$Mai_depression.b")
        if (!exists(var_name, envir = .GlobalEnv)) return(FALSE)  # Vérifie si Mai_depression.b existe
        grepl("Mai_depression\\s*[>=]\\s*1\\.?5?", input$code_input) # Vérifie si le threshold est bien mis
      },
      solution = generate_reactive_string(
        valid_expr = paste0(df_name(), "$Mai_depression.b <- ifelse(", df_name(), "$Mai_depression >= 2, 1, 0)"),
        default_expr = "data$Mai_depression.b <- ifelse(data$Mai_depression >= 2, 1, 0)"
      )
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
  
  output$code_input_ui <- renderUI({
    step <- current_step()
    rows <- ifelse(step == 5, 2, 1)  # Change X par le numéro de l'étape où il faut plus de place
    textAreaInput("code_input", "Type your code here :", rows = rows)
  })
  
  # Affichage type "console"
  output$console_output <- renderText({
    code_lines <- unlist(strsplit(input$code_input, "\n"))  # Sépare par lignes
    paste0("> ", code_lines, collapse = "\n")  # Ajoute ">" à chaque ligne
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
      
      # Séparer le code en plusieurs lignes
      lines <- unlist(strsplit(user_code, "\n"))
      all_output <- c()  # Stocke toutes les sorties
      
      for (line in lines) {
        if (nchar(trimws(line)) > 0) {  # Ignore les lignes vides
      result <- tryCatch({
        eval(parse(text = line), envir = .GlobalEnv)
      }, error = function(e) {
        paste("🚨 Erreur :", e$message)
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
      } else {
        output$user_plot <- renderPlot(NULL)  # Efface le graphique si aucun plot
      }
      
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
      #output$execution_output <- renderText("")  # Effacer la sortie
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
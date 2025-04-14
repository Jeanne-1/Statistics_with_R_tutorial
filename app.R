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
  card(
    titlePanel("Statistics with R for begginers"),
    
    # Instructions
    uiOutput("instructions"),
    textOutput("explanations")
  ),
  
  layout_columns(
    col_width=3,
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
  
  plotOutput("user_plot") # AFFICHER LA ZONE SLMT S'IL Y A UN PLOT ET PAS OUTPUT
)

server <- function(input, output, session) {
  
  # suppression de toutes les variables globales
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  
  # Liste des étapes du tutoriel
  steps <- list(
    list(
      instruction = "🔹 Upload data from the file 'smcovid.csv'.",
      explanations = "Use read.csv2 as the separator of the excel file is ; and not ,.",
      validation = function() {
        objs <- ls(envir = .GlobalEnv)
        dataframes <- objs[sapply(objs, function(x) inherits(get(x, envir = .GlobalEnv), "data.frame"))]
        if (length(dataframes) == 0) { # GERER L'ERREUR Erreur dans le code : type 'list' d'indice incorrect
          F
        }
        df_name(dataframes[1]) # Stock the value in df_name()
        # if(dim(dataframes[1])!= c(748,33)) F
        T
      },
      solution = "smc <- read.csv2('smcovid.csv')",
      questions = list(
        list(
          question = "You will have some questions to answer here before being able to go to the next page. Got it ?",
          options = c("Yes", "No", "The answer D"),
          correct = "Yes"
        )
      )
    ),
    list(
      instruction = "🔹 Observe the data you uploaded.",
      explanations = "Try functions such as head, str, summary, describe and dim.",
      validation = function() {
        T
      }, # pas de validation, que sur le QCM
      conclusion = "Have you seen the difference between these functions ?",
      solution = "head(data)",
      questions = list(
        list(
          question = "How many records are there?",
          options = c("629", "748", "811", "956"),  # Options du QCM
          correct = "748"  # Réponse correcte
        ),
        list(
          question = "How many variables ?",
          options = c("6", "13", "26", "33"),
          correct = "33"
        ),
        list(
          question = "How old is the first record ?",
          options = c("23", "25", "27", "29"),
          correct = "23"
        ),
        list(
          question = "Which variable seems to be incomplete ?",
          options = c("prof", "subst.cons", "Prise_Poids", "alc.cons"),
          correct = "Prise_Poids"
        ),
        list(
          question = "How many childs in the family are there max ?",
          options = c("10", "11", "12", "13"),
          correct = "11"
        )
      )
    ),
    list(
      instruction = "🔹 How many features do you have for age ? Stock the value in tot.",
      explanations = "Little reminder: you can catch age with the formula data$age.",
      validation = function() { # REGLER L'ERREUR cannot coerce type 'closure' to vector of type 'character'
        if (!"tot" %in% ls(envir = .GlobalEnv)) return(FALSE)  # Vérifie si 'tot' existe
        value <- get("tot", envir = .GlobalEnv)  # Récupère la valeur de 'tot'
        return(is.numeric(value) && value == 748)  # Vérifie si c'est un nombre et vaut 748
      },
      solution = generate_reactive_string(
        valid_expr = paste0("tot <- sum(table(", df_name(), "$age))"),
        default_expr = "tot <- sum(table(data$age))"
        ),
      questions = list(
        list(
          question = "Which information does it give you about age ?",
          options = c("Nothing, we miss other piece of info", "There are some missing values", "There are no missing values"),
          correct = "There are no missing values"
        )
      )
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
        "Now we know the mean of the age of the 748 people that conducted the study.
        But what about the mean age of our global population ? \n"
        },
      questions = list(
        list(
          question = "How would you do if you had some missing values for age ?",
          options = c("I would panic", "I delete the Na from the dataset (na.omit(data))", "I add the option na.rm = T"),
          correct = "I add the option na.rm = T"
        ),
        list(
          question = "How do you get the standard deviation of data ?",
          options = c("std(data)", "sd(data)", "data.std", "data.sd"),
          correct = "sd(data)"
        ),
        list(
          question = "Is the mean age of the global population can be estimated with this one ?",
          options = c("Yes", "No"),
          correct = "Yes",
          cl = "Observations in the sample can give information about the global population, but there are less precise."
        )
      )
    ),
    list(
      instruction = "🔹 What is the mean of age of our global population ?",
      explanations = "Find the 95% confidence interval (95CI).",
      validation = function() {
        # AJOUTER UNE VERIFICATION DE TYPE
        if (is.null(df_name())) return(FALSE)
        expected_value <- 36.90453
        # VERIFIER QUE A UN MOMENT DANS LA SORTIE CE CHIFFRE APPARAIT
      },
      solution = generate_reactive_string(
        valid_expr = paste0("mean(", df_name(), "$age)-1.96*sd(", df_name(), "$age)/sqrt(tot), mean(", df_name(), "$age)+1.96*sd(", df_name(), "$age)/sqrt(tot)"),
        default_expr = "mean(data$age)-1.96*sd(data$age)/sqrt(tot), mean(data$age)+1.96*sd(data$age)/sqrt(tot)"
        ),
      questions = list(
        list(
          question = "What is the expected result ?",
          options = c("An interval", "An int", "A percentage"),
          correct = "An interval"
        ),
        list(
          question = "What does 95CI mean ?",
          options = c("We are 95% confident that the result is not in between these values", "The global mean of the population is 95% likely to be in between these 2 values", "The global mean of the population is in between these 2 values, and there are 95% chance it is the mean of the sample"),
          correct = "The global mean of the population is 95% likely to be in between these 2 values"
        )
      )
    ),
    list(
      instruction = "🔹 Draw a histogram of age.",
      validation = function() {
        if (is.null(df_name())) return(F)
        grepl(paste0("hist\\(", df_name(), "\\$age."), input$code_input)
      },
      solution = generate_reactive_string(
        valid_expr = paste0("hist(", df_name(), "$age)"),
        default_expr = "hist(data$age)"
      ),
      conclusion = "Looking at the histogram can give you a clue whether the variable follows a normal distribution or not. What do you think ? \n",
      questions = list(
        list(
          question = "What is the range that appears the most in the sample ?",
          options = c("20-25", "25-30", "35-40", "45-50"),
          correct = "25-30"
        ),
        list(
          question = "Do you think the variable can follow a normal distribution ?",
          options = c("Yes without a doubt", "It doesn't follow a normal distribution but enough to consider as so", "Not at all"),
          correct = "It doesn't follow a normal distribution but enough to consider as so"
        ),
        list(
          question = "What is the advantage of considering it as gaussian ?",
          options = c("We can only perform some tests on gaussian variable", "It is more interpretable", "You cannot perform any statistical test on the variable if not"),
          correct = "We can only perform some tests on gaussian variable"
        ),
        list(
          question = "With which option can you change the title of the plot ?",
          options = c("title = ", "main = ", "set_title = ", "lab = "),
          correct = "main = "
        ),
        list(
          question = "With which option can you change the label of the x variable ?",
          options = c("xtitle = ", "xlab = ", "xstick = "),
          correct = "xlab = "
        )
      )
    ),
    list(
      instruction = "🔹 Display the percentage of people that had depression in May.",
      explanations = "That is with Mai_depression==3",
      validation = function() {
        if (is.null(df_name())) return(FALSE)
        grepl(paste0("prop\\.table\\(table\\(", df_name(), "\\$Mai_depression\\s*==\\s*3\\)\\)\\s*\\*\\s*100"), input$code_input)
      },
      solution = "prop.table(table(data$Mai_depression==3))*100", # CORRIGER CA
      #   generate_reactive_string(
      #   valid_expr = paste0("prop.table(table(", df_name(), "$Mai_depression==3))*100"),
      #   default_expr = "prop.table(table(data$Mai_depression==3))*100"
      # ),
      questions = list(
        list (
          question = "From this piece of information, can you say that there are less people with depression in May than people without ?",
          options = c("Yes", "No", "It depends on where you put the threshold concerning considering someone in depression or not"),
          correct = "It depends on where you put the threshold considering someone in depression or not"
        ),
        list(
          question = "What does table(data$subst.cons == 1) display about the binary variable substance consumption ?",
          options = c("The percentage of people with vs without", "The number of person with vs without", "A table of 748 False/True"),
          correct = "The number of person with vs without"
        )
      )
    ),
    list(
      instruction = "🔹 Let's create a binary variable, Mai_depression.b.",
      explanations = "Put the threshold at 1.5.",
      validation = function() {
        if (is.null(df_name())) return(FALSE)
        df <- get(df_name(), envir = .GlobalEnv)
        if (!"Mai_depression.b" %in% colnames(df)) return(FALSE)  # Vérifie si Mai_depression.b existe
        grepl(".Mai_depression\\s*(>=\\s*2|>\\s*1\\.?\\d*)", input$code_input) # Vérifie si le threshold est bien mis
      },
      solution = "data$Mai_depression.b <- ifelse(data$Mai_depression >= 2, 1, 0)", # CORRIGER CA
      # generate_reactive_string(
      #   valid_expr = paste0(df_name(), "$Mai_depression.b <- ifelse(", df_name(), "$Mai_depression >= 2, 1, 0)"),
      #   default_expr = "data$Mai_depression.b <- ifelse(data$Mai_depression >= 2, 1, 0)"
      # ),
      conclusion= "Another binary variable has been created, Mai_anxiete.b. Let's see more about that.",
      questions = list(
        list(
          question = "Why do you want to create a binary variable ?",
          options = c("To conduct tests on percentages", "For explainability", "To have simpler tests"),
          correct = "To conduct tests on percentages"
        )
      )
    ),
    # A PARTIR D'ICI, PLUS DE SOLUTIONS ADAPTATIVES
    list(
      instruction = "🔹 Display a confusion matrix of Mai_depression.b and Mai_anxiete.b.",
      explanations = "You can use the option deparse.level = 2 to see which column corresponds to which variable.",
      validation = function() {
        if (grepl("table", input$code_input)
            & grepl("Mai_depression\\.b", input$code_input)
            & grepl("Mai_anxiete\\.b", input$code_input)) T
        else F
      },
      solution = "table(data$Mai_depression.b, data$Mai_anxiete.b, deparse.level=2)",
      conclusion = "What do you see ? How do you interprete that ?",
      questions = list(
        list(
          question = "How many people have both depression and anxiety ?",
          options = c("173", "318", "28", "229"),
          correct = "229"
        ),
        list(
          question = "How many people have depression but not anxiety ?",
          options = c("173", "318", "28", "229"),
          correct = "28"
        ),
        list(
          question = "What does the option useNA = 'always'",
          options = c("It adds a column for the null values", "It ignores null values", "It is not possible as useNA is a boolean option"),
          correct = "It adds a column for the null values"
        )
      )
    ), # ADD A QUESTION ABOUT PEARSON COR
    list(
      instruction = "🔹 Find the Relative Risk and Odds Ratio of depression according to anxiousness (in May).",
      explanations = "Use the twoby2 function. Watch out, the order count, and it considers that 0 is sick and 1 is not.",
      validation = function() {
            if (grepl("twoby2", input$code_input)
                & grepl("1\\s*-", input$code_input)
                & grepl("Mai_anxiete\\.b.*Mai_depression\\.b", input$code_input) #anxiete AVANT depression
                ) T
            else F
        },
      solution = "twoby2(1- data$Mai_anxiete.b, 1 - data$Mai_depression.b)",
      questions = list(
        list(
          question = "What is the rounded relative risk (RR) observed in the sample ?",
          options = c("[1.4,1.6]", "1.5", "[2.1,4.3]", "3.0"),
          correct = "3.0"
        ),
        list(
          question = "And the odds ratio (OR) ? ",
          options = c("4.4", "[2.9, 6.9]", "[2.8, 7.1]", "0.3"),
          correct = "4.4"
        ),
        list(
          question = "What information does it gives you ? ",
          options = c("The relation strength between 2 continuous var", "The relation strength between 2 binary var"),
          correct = "The relation strength between 2 binary var"
        ),
        list(
          question = "Why are they different ?",
          options = c("It gives 2 different pieces of information", "The prevalence of the disease is not low", "It depends on the sample size"),
          correct = "The prevalence of the disease is not low",
          cl = "OR is almost the same as RR iif the disease is not frequent (<5%)."
        ),
        list( # AJOUTER UNE PIC DE what is OR et RR
          question = "Which one should we watch in our situation ?",
          options = c("OR", "RR", "None of them"),
          correct = ("RR")
        ),
        list(
          question = "Why ?",
          options = c("Because it is more understandable",
                      "Because it's a case control study (same amount of sick / non-sick in the sample)"),
          correct = "Because it is more understandable",
          cl = "When someone has anxiousness he is 3 times more likely to have depression."
        )
      )
    ),
    list(
      instruction = "🔹 What is the percentage of depressed people according to their anxiousness ?",
      explanations = "Try to add an option, 1, or 2, and see what changes.",
      validation = function() {
        if (grepl("prop\\.table\\(", input$code_input)
            & grepl("Mai_anxiete\\.b", input$code_input)
            & grepl("Mai_depression\\.b", input$code_input)) T
        else F
      },
      solution = "prop.table(table(data$Mai_anxiete.b, data$Mai_depression.b, deparse.level=2), 1)*100",
      conclusion = "But are these informations statistically relevant for the whole population ?",
      questions = list(
        list(
          question = "If the person is anxious ? (Let's call the result a)",
          options = c("30.6%", "41.9%", "42.5%", "58.1%", "89.1%"),
          correct = "41.9%"
        ),
        list(
          question = "What does it means ?",
          options = c("If a person is anxious, there are a% chance it is depressed", "If a person is depressed, there are a% chance it is anxious", "If a person is anxious, there are a% chance it is not depressed", "If a person is depressed, there are a% chance it is not anxious"),
          correct = "If a person is anxious, there are a% chance it is depressed"
        ),
        list(
          question = "If the person is not anxious ?",
          options = c("3.7%", "10.9%", "13.9%", "23.1%"),
          correct = "13.9%"
        ),
        list(
          question = "And the chance a person is anxious if it is depressed ?",
          options = c("30.6%", "41.9%", "42.5%", "58.1%", "89.1%"),
          correct = "89.1%"
        ),
        list(
          question = "With which test can you be sure there is really a difference of prevalence of depression between anxious and non anxious people ?",
          options = c("Chi-2 test", "Student t test", "McNemar test", "Wilcoxon t test"),
          correct = "Chi-2 test"
        )
      )
    ),
    list(
      instruction = "🔹 Conduct a chi-2 test.",
      explanations = "Add the option correct = F, otherwise R conducts a continuous test.",
      validation = function() {
        if (grepl("chisq\\.test\\(", input$code_input)
            & grepl("Mai_anxiete\\.b", input$code_input)
            & grepl("Mai_depression\\.b", input$code_input)) T
        else F
      },
      solution = "chisq.test(data$Mai_anxiete.b, data$Mai_depression.b, correct=F)",
      questions = list(
        list(
          question = "Why do we conduct statistical tests ?",
          options = c("Additionnally to other discoveries to cross the obtained value",
                      "To make sure the discoveries are not by chance"),
          correct = "To make sure the discoveries are not by chance"
        ),
        list(
          question = "What is the p-value used for ?",
          options = c("To confirm with a high level of certitude that the results cannot be explained just with luck",
                      "To obtain the probability the results are not based on luck"),
          correct = "To confirm with a high level of certitude that the results cannot be explained just with luck",
          cl = "p is the probability that chance alone can explain a difference at least as large as the one observe. It depends a lot on the size of the sample."
        ),
        list(
          question = "What are the validation criteria to use the Chi-2 test ?",
          options = c("Big sample & the percentages to compare are not too close to 100 or 0",
                      "Big sample or normal distribution",
                      "Big difference between the amount of sick and non sick people",
                      "None"),
          correct = "Big sample & the percentages to compare are not too close to 100 or 0",
          cl = "Btw, a binary variable cannot follow a normal distribution :)"
        ),
        list(
          question = "If they are not followed, which test should you use ?",
          options = c("Student t test (t.test)", "Fisher's exact test (fisher.test)", "Welch's appr t test (t.test)", "Pearson's correlation coef (cor.test)"),
          correct = "Fisher's exact test (fisher.test)"
        ),
        list(
          question = "What is the difference between the Fisher and the Neyman & Pearson approach ?",
          options = c(
            "Neyman & Person shows the strength of the conclusion",
            "Neyman & Pearson has a continuous result, Fisher a binary one",
            "Neyman & Pearson has a binary result, Fisher a continuous one"
          ),
          correct = "Neyman & Pearson has a binary result, Fisher a continuous one",
          cl = {
            "According to Neyman & Pearson, when validating or non validating an hypothesis, there are 2 possible risks:
            alpha (the probability to accept H1 whereas H0, the status quo, is true) and beta (the probability to reject H1 whereas it was true).
            We try then to minimize alpha and beta (and specially alpha, which is the 'worst'). We fix the value of alpha to a threshold,
            and we observe the p-value: if it respects this threshold, H1 is accepted, otherwise it's not.
            On the contrary, Fisher's approach is considering the more p is low the more the test is relevant."
          }
        ),
        list(
          question = "Habitually, with Neyman & Pearson approach, which value need to be p for the test to be relevant ?",
          options = c("More than 50%","More than 5%", "Less than 5%", "Less than 1%"),
          correct = "Less than 5%"
        ),
        list(
          question = "Let's consider 3 p-values : p1 = 0.1%, p2 = 4.9%, p3 = 5.1%. What does it means for a Fisher approach ?",
          options = c("p1 is more relevant than p2 or p3 (which are equivalent)",
                      "p2 or p3 (which are equivalent) are more relevant than p1",
                      "p1 and p2 (which are equivalent) are relevant, p3 is not",
                      "p3 is relevant, p1 and p2 (which are equivalent) are not"),
          correct = "p1 is more relevant than p2 or p3 (which are equivalent)"
        ),
        list(
          question = "For Neyman and Pearson ?",
          options = c("p1 is more relevant than p2 or p3 (which are equivalent)",
                      "p2 or p3 (which are equivalent) are more relevant than p1",
                      "p1 and p2 (which are equivalent) are relevant, p3 is not",
                      "p3 is relevant, p1 and p2 (which are equivalent) are not"),
          correct = "p1 and p2 (which are equivalent) are relevant, p3 is not",
          cl = "Did you get the difference ?"
        ),
        list(
          question = "What is the relevance of the result according to Fisher ?",
          options = c("Very relevant", "Quite relevant", "Not that much relevant", "Not relevant"),
          correct = "Very relevant"
        )
      )
    ),
    list(
      instruction = "🔹 Plot a boxplot of age according to anxiety (in May).",
      explanations = {"Try the structure varA~varB which means varA comparing to varB."},
      validation = function() {
        grepl("boxplot\\(.*\\$age~.*\\$Mai_anxiete", input$code_input) # Either binary or non binary is ok
      },
      solution = "boxplot(data$age~data$Mai_anxiete.b)",
      questions = list(
        list(
          question = "What do you see about the sample ?",
          options = c("On average, youngest people are more anxious", "On average, oldest people are more anxious", "On average, anxious people are older", "On average, anxious people are younger", "It seems to be more or less the same between anxious and non anxious people"),
          correct = "On average, anxious people are younger"
        ),
        list(
          question = "What test should we conduct to see if this discovery is reliable or not for the whole population ?",
          options = c("Chi-2 test (chisq.test)", "Student t test (t.test)", "McNemar test (mcnemar.test)", "Fisher's exact test (fisher.test)"),
          correct = "Student t test (t.test)"
        ),
        list(
          question = "What is NOT one of the validation criteria ?",
          options = c("Size of the sample big enough", "The continuous variable follows a normal distribution", "The variances between the 2 sub-samples are equal", "There are all validation criteria"),
          correct = "There are all validation criteria"
        ),
        list(
          question = "Welch's approximation t test (t.test) allows to compare 2 means too. What is NOT one of the validation criteria for this test ?",
          options = c("Size of the sample big enough", "The continuous variable follows a normal distribution", "The variances between the 2 sub-samples are equal", "There are all validation criteria"),
          correct = "The variances between the 2 sub-samples are equal"
        ),
        list(
          question = "There is still another test that compares 2 means, called Wilcoxon (wilcox.test). This one has no validation criteria. Then, why should we chose another test ?",
          options = c("It does not compare average but ranks of individuals", "You cannot use any other method that needs a normal distribution", "Both"),
          correct = "Both"
        )
      )
    ),
    list(
      instruction = "🔹 Are the variance of the 2 subsamples equal ? Use the function by",
      explanations = {"We learnt how to compare 2 percentages (binary variables). We now want to compare 2 means (continuous variable)
        (here, the age of those who suffer from anxiete in May and those who didn't)."},
      validation = function() {
        grepl("by", input$code_input) &
        grepl("\\$Mai_anxiete\\.b", input$code_input) &
        grepl("\\$age", input$code_input) &
        grepl("\\b(sd|var)\\b", input$code_input)
            },
      solution = "by(data$age, data$Mai_anxiete.b, sd, na.rm=T)",
      questions = list(
        list(
          question = "Is the size of the sample big enough ?",
          options = c("Yes", "No"),
          correct = "Yes"
        ),
        list(
          question = "Remember about the histogram you plotted at the beggining (you can plot it again) ? We said we can consider age follows a normal distribution. Why ?",
          options = c("We recognized the bell shape of a normal distribution", "Frequency strictly increased and then decreased", "Frequency is not constant"),
          correct = "Frequency strictly increased and then decreased",
          cl = "There is another test that can say if it follows a normal distribution : qqnorm(data$age); qqline(data$age). If all the dots are above the line, it's ok, otherwize it's not."
        ),
        list(
          question = "Are the variance of the 2 subsamples equal ? ",
          options = c("Yes", "Not totally, but we can consider as so", "No"),
          correct = "No"
        ),
        list(
          question = "Then in our case, which test should we conduct ?",
          options = c("Student t test (t.test)", "Welch's appr t test (t.test)", "Wilcoxon test (wilcox.test)"),
          correct = ("Welch's appr t test (t.test)")
        )
      )
    ),
    list(
      instruction = "🔹 On average, is there a significant age difference between someone that suffers from anxiete and someone who don't ?",
      explanations = {"Perform a Welch's appr t test. In these test, you need to use the varA~varB structure."},
      validation = function() {
        grepl("t\\.test\\(.*\\$age~.*\\$Mai_anxiete\\.b", input$code_input) &
        !grepl("var\\.equal", input$code_input)
      },
      solution = "t.test(data$age~data$Mai_anxiete.b)",
      questions = list(
        list(
          question = "Remembered a student t test uses the same function ? But what changes ?",
          options = c("R knows if the variances are equal or not automatically, you don't have to change anything", "You add the option method='student'", "You add the option var.equal = T"),
          correct = "You add the option var.equal = T"
        ),
        list(
          question = "What piece of information does this function NOT gives you ?",
          options = c("The p-value", "The mean of the 2 groups", "The variance of the 2 groups"),
          correct = "The variance of the 2 groups"
        ),
        list(
          question = "What can you say about the result ?",
          options = c("The 2 means are statistically different", "The 2 means are different but it is not a statistical difference", "The 2 means are statistically different according to Neyman & Pearson but not Fisher"),
          correct = "The 2 means are statistically different"
        ),
        list(
          question = "Does this confirm the observation you did based on the boxplots ?",
          options = c("Yes", "No"),
          correct = "Yes"
        )
      ),
      conclusion = "Another thing you can see with the t.test is a difference with an integer."
    ),
    list(
      instruction = "🔹 Does my sample represents well my population in age ?",
      explanations = "You can perform a One sample t-test using the same function with the option mu. Let's consider our population is the United States inhabitants. The average age there is 38 years old.",
      validation = function() {
        grepl("t\\.test\\(.*\\$age,\\s*mu.*38\\)", input$code_input)
      },
      solution = "t.test(data$age, mu=38)",
      questions = list(
        list(
          question = "Can we say the sample represents well my population in age ?",
          options = c("Yes, the sample has the same mean", "Yes, it's inside the 95CI", "No, it's not too far but is still different", "No because the result is not statistically relevant"),
          correct = "No because the result is not statistically relevant"
        ),
        list(
          question = "Can we say the sample does not represent well my population in age ? (Tip: use =!).",
          options = c("Yes, the means are statistically different", "No, the sample's mean is inside the 95CI", "No because the result is not statistically relevant"),
          correct = "No, the sample's mean is inside the 95CI"
        )
      )
    ),
    list(
      instruction = "🔹 What is the correlation between age and the number of childs (n.enfant) ?",
      explanations = "The correlation gives you a clue about the strength of association between 2 variables. You can help you with the questions.",
      validation = function() {
        grepl("cor", input$code_input) &
        grepl("\\$n\\.enfant", input$code_input) &
        grepl("\\$age", input$code_input)
      },
      solution = "cor(data$n.enfant, data$age, use = 'complete.obs')",
      questions = list(
        list(
          question = "What option do you need to use to avoid empty values ?",
          options = c("use = 'remove.na'", "useNA = F", "use = 'complete.obs'", "na.rm = T"),
          correct = "use = 'complete.obs'"
        ),
        list(
          question = "What is r if it is a complete dependence between the 2 var ?",
          options = c("+inf", "+/-inf", "0", "1", "+/-1"),
          correct = "+/-1",
          cl = "The correlation coefficient r is in [-1,1]. A correlation of 1 or -1 means the 2 variables are mutually determined : knowing one gives the other."
        ),
        list(
          question = "What does r=0 means about the 2 variables ?",
          options = c("They are mutually determined", "They are totally independant", "It's not enough to say anything"),
          correct = "It's not enough to say anything",
          cl = "They are independant iif they both follow a normal distribution. Otherwise, as the Pearson correlation looks at a linear correlation, they can still be dependant (quadratic relationship or concordance)."
        ),
        list(
          question = "Is the obtained correlation statistically relevant ?",
          options = c("Yes", "No", "We don't know"),
          correct = "We don't know",
          cl = "We don't have any information about the p-value or an 95CI."
        )
      ),
      conclusion = "Let's see if the correlation is statistically relevant."
    ),
    list(
      instruction = "🔹 Perform a test of nullity",
      explanations = "The objective is to see wether the correlation between age and the number of childs exists or not. Use the function cor.test()",
      validation = function() {
          grepl("cor\\.test", input$code_input) &
          grepl("\\$n\\.enfant", input$code_input) &
          grepl("\\$age", input$code_input)
      },
      solution = "cor.test(data$n.enfant, data$age)",
      questions = list(
        list(
          question = "What is the validation criteria to perform this test ?",
          options = c("The variables follow a normal distribution", "At least one of the variables follows a normal distribution", "The variance of the 2 variables are equal", "The sample is big enough"),
          correct = "At least one of the variables follows a normal distribution"
        ),
        list(
          question = "In our case, was it ok to perform this test ?",
          options = c("Yes", "No"),
          correct = "Yes"
        ),
        list(
          question = "If not, which test should we use ?",
          options = c("Linear correlation", "Pearson's product-moment correlation", "Spearman rank correlation", "Neyman correlation"),
          correct = "Spearman rank correlation",
          cl = "The name of the test you performed is the Pearson's product-moment correlation. If it doesn't follow the validation criteria, you can use the Spearman rank correlation. You will use the same function cor.test but with the option method = 'spearman'."
        ),
        list(
          question = "So, why can we say the correlation obtained is statistically relevant ?",
          options = c("Because the 95CI exclude 0", "Because the correlation is in between the 95CI", "Because the p-value is very close to 0", "Because the p-value is enough far from 0"),
          correct = "Because the p-value is very close to 0",
          cl = "By the way, the correlation is always in between the 95CI as it is centered in r."
        )
      )
    ),
    list(
      instruction = "🔹 Compares the anxiete before and after the confinement (in May vs in October. Oct_anxiete.b has been created)",
      explanations = "Another type of test you can perform is the paired test. It compares the same thing before and after sth happens. It takes into account that each patient is its own control data. Use the questions to help you.",
      validation = function() {
        grepl("mcnemar\\.test", input$code_input) &
          grepl("\\$Mai_anxiete", input$code_input) &
          grepl("\\$Oct_anxiete", input$code_input)
      },
      solution = "mcnemar.test(data$Mai_anxiete.b, data$Oct_anxiete.b)",
      questions = list(
        list(
          question = "To compare 2 binary values, which chi-2 test should we use ?",
          options = c("Chi-2 test (chisq.test)", "Student t test (t.test)", "McNemar test (mcnemar.test)", "Fisher's exact test (fisher.test)"),
          correct = "McNemar test (mcnemar.test)"
        ),
        list(
          question = "To compare 2 quantitative values, which test should we use ?",
          options = c("Chi-2 test (chisq.test)", "Student t test (t.test)", "McNemar test (mcnemar.test)", "Fisher's exact test (fisher.test)"),
          correct = "Student t test (t.test)",
          cl = "Yes, again ! You need to put the option paired to True"
        )
      )
    )
  )
  
  
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
    rows <- ifelse(step == 5, 2, 1)  # Change X par le numéro de l'étape où il faut plus de place
    textAreaInput("code_input", "Type your code here :", rows = rows)
  })
  
  # Affichage type "console"
  output$console_output <- renderText({
    code_lines <- unlist(strsplit(input$code_input, "\n"))  # Sépare par lignes
    paste0("> ", code_lines, collapse = "\n")  # Ajoute ">" à chaque ligne
  })
  
  # Vérification des réponses
  observeEvent({
    step <- current_step()
    validation_fn <- steps[[step]]$validation
    if (step > length(steps)) return()  # Fin du tutoriel

    questions <- steps[[step]]$questions
    if (is.null(questions)) {
      validation_state_q$valid <- T
      return()  # Si pas de questions, ne rien faire
      }

    valid <- TRUE
    for (i in seq_along(questions)) {
      question <- questions[[i]]
      answer_id <- paste0("question_", step, question$correct)
      user_answer <- input[[answer_id]]

      if (is.null(user_answer) || user_answer != question$correct) {
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
    lapply(steps[[current_step()]]$questions, function(q) input[[paste0("question_", current_step(), q$correct)]])
  })
  
  output$questionnaire <- renderUI({
    step <- current_step()
    if (step <= length(steps)) {
      questions <- steps[[step]]$questions
      if (!is.null(questions)) {
        # Crée les éléments de QCM
        lapply(questions, function(q) {
          radioButtons(
            inputId = paste0("question_", step, q$correct),
            label = q$question,
            choices = q$options,
            selected = 1,
            inline = T
          )
          # if (validation_state_q$valid) textOutput(q$cl)
        })
      }
    }
  })
  
  # Exécution du code utilisateur
  observeEvent(input$run_code, { # REGLER str(data) QUI FONCTIONNE PAS
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
      } else {
        output$user_plot <- renderPlot(NULL)  # Efface le graphique si aucun plot
      }
      
      # Vérification de la réponse
      if (validation_fn()) {
        if (steps[[step]]$instruction == "🔹 Let's create a binary variable, Mai_depression.b."){
          # Créer la nouvelle variable Mai_anxiete.b si l'étape de création de Mai_depression.b est réussie
          df <- get(df_name(), envir = .GlobalEnv)  # Récupérer la dataframe
          df$Mai_anxiete.b <- ifelse(df$Mai_anxiete >= 2, 1, 0)
          df$Oct_anixete.b <- ifelse(df$Oct_anxiete >=2, 1, 0)
          assign(df_name(), df, envir = .GlobalEnv) 
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
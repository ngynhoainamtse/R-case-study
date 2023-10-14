library(shiny)
library(ggplot2)

processus <- function(n, cas, taille_popu = 3 * n) {
  # Initialize pivot variables
  pivot_M1 <- 0
  pivot_M2 <- 0
  
  # Simulation of the choice of voters
  vec_etat <- switch(cas, IC = rbinom(3, n, 0.5), IAC_star = rbinom(3, n, runif(3)))
  
  # Vainqueur de l'élection M1
  vote_popu <- sum(vec_etat)
  val_seuil_pop <- (taille_popu + 1) / 2
  winn_D_M1 <- (vote_popu >= val_seuil_pop)
  
  # Vainqueur de l'élection M2
  nb_etat_gagnant <- sum(vec_etat >= (n + 1) / 2)
  winn_D_M2 <- (nb_etat_gagnant > 1)
  
  # Probability of being a pivot in the case of M1
  if ((winn_D_M1 & (vote_popu == val_seuil_pop)) | (!winn_D_M1 & (vote_popu == (taille_popu - 1) / 2))) {
    pivot_M1 <- val_seuil_pop
  }
  
  # Probability of being a pivot in the case of M2
  if (winn_D_M2 & nb_etat_gagnant == 2) {
    pivot_M2 <- sum((n + 1) / 2 * (vec_etat == (n + 1) / 2))
  } else if (!winn_D_M2 & nb_etat_gagnant == 1) {
    pivot_M2 <- sum((n + 1) / 2 * (vec_etat == (n - 1) / 2))
  }
  
  return(c(pivot_M1 = pivot_M1, pivot_M2 = pivot_M2))
}

# The function that returns for each mechanism, the average percentage on the B simulations of people satisfied by the election
simul_elec <- function(n, cas, B = 1000) {
  
  # vérification
  stopifnot(n %% 2 == 1, cas %in% c("IC", "IAC_star"))
  # initialisation: nombre total d'électeurs
  taille_popu <- 3 * n
  
  # réplication of the function processus()
  res_simul <- replicate(B, processus(n = n, cas = cas, taille_popu = taille_popu))
  # on retourne les résultats
  return(rowMeans(res_simul) / taille_popu)
}


# Define UI for the application
ui <- fluidPage(
  titlePanel("Case Study 4"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("Size", "Size of population:", value = 5),
      selectInput("Option", "Probabilistic Model:", choices = c("IC", "IAC_star")),
      numericInput("Rep", "Number of replications:", value = 100000),
      selectInput("PlotType", "Type of Output:", choices = c("Histogram", "Pie Chart", "Table"))
    ),
    
    mainPanel(
      uiOutput("dynamic_output")
    )
  )
)

server <- function(input, output) {
  
  output$dynamic_output <- renderUI({
    switch(input$PlotType,
           "Histogram" = plotOutput("histogram", width = "500px", height = "300px"),
           "Pie Chart" = plotOutput("piechart", width = "500px", height = "300px"),
           "Table" = tableOutput("table")
    )
  })
  
  output$histogram <- renderPlot({
    res <- simul_elec(n = input$Size, cas = input$Option, B = input$Rep)
    
    data <- data.frame(Model = c("M1", "M2"), Probability = res)
    p <- ggplot(data, aes(x = Model, y = Probability, fill = Model)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.3) +
      xlab("Mechanism") +
      ylab("Probability") +
      labs(fill = "Mechanism") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.2))
    
    p + ggtitle("Mechanism comparison")
  })
  
  output$piechart <- renderPlot({
    res <- simul_elec(n = input$Size, cas = input$Option, B = input$Rep)
    
    data <- data.frame(Model = c("M1", "M2"), Probability = res)
    p <- ggplot(data, aes(x="", y=Probability, fill=Model)) +
      geom_bar(width=1, stat="identity") +
      coord_polar(theta="y") +
      xlab("") +
      ylab("") +
      labs(fill="Mechanism") +
      theme_void()
    
    p + ggtitle("Mechanism comparison")
  })
  
  output$table <- renderTable({
    res <- simul_elec(n = input$Size, cas = input$Option, B = input$Rep)
    
    data.frame(Model=c("M1", "M2"), Probability=res)
  })
}
# Run the application

#HOLA
shinyApp(ui = ui, server = server) #I add a comment here

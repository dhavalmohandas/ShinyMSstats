

home = fluidPage(
  headerPanel("Welcome to MSstats-shiny"),
  tags$br(),
  mainPanel(
    div(tagList(
      h4("About MSstats-Shiny"),
      p("This is a web tool for the statistical analysis of quantitative proteomic data.  It is built based on the R package ", a("MSstats (v 3.19.2).", href="http://msstats.org/")),
      p("PLease note that some calculations may take some time to compute."),
      p("The full code can be accessed online at this ", a("github repository", href="https://github.com/dhavalmohandas/ShinyMSstats"),"."),
      br(),
#      p("There are "),
#      verbatimTextOutput("count"),
#      p("people currently using Shiny-MSstats")
      ),
     actionButton(inputId = "proceed", label = "Let's Start")
      )
    )
  )
  


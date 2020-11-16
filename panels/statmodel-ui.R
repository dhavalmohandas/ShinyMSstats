
statmodel = fluidPage(
  tags$head(
    tags$style(HTML('#submit1{background-color:orange}')),
    tags$style(HTML('#clear1{background-color:orange}')),
    tags$style(HTML('#submit2{background-color:orange}')),
    tags$style(HTML('#clear2{background-color:orange}')),
    tags$style(HTML('#calculate{background-color:orange}')),
    tags$style(HTML('#plotresults{background-color:orange}')),
    tags$style(HTML('#viewresults{background-color:orange}'))
  ),
  headerPanel("Statistical Model"),
  p("In this tab a statistical model is built in three steps: (i)Create a contrast matrix with the correct Group comparisons, then (ii) generate the model and (iii) view result plots."),
  p("More info ", a("here", href="https://www.rdocumentation.org/packages/MSstats/versions/3.4.0/topics/groupComparisonPlots")),
    
# statistical model
    
    sidebarPanel(
                        h3("STEP (i) - Define comparisons", tipify(icon("question-circle"), title="Choose pairwise comparisons to find significantly expressed proteins")),
                        fluidRow(
                                 radioButtons("def_comp", "Define contrast matrix", c("All possible pairwise comparisons" = "all_pair", "Compare all against one" = "all_one", "Create custom comparisons" = "custom"), selected = character(0)),
                                 conditionalPanel(condition = "input.def_comp == 'custom'",
                                                  uiOutput('choice1'),
                                                  h6("vs"),
                                                  uiOutput("choice2"),
                                                  actionButton("submit", "Add"),
                                                  actionButton("clear", "Clear matrix")
                                                  ),
                                 conditionalPanel(condition = "input.def_comp == 'all_one'",
                                                  h5("Compare all groups against:"),
                                                  uiOutput("choice3"),
                                                  actionButton("submit1", "Submit"),
                                                  actionButton("clear1", "Clear matrix")
                                                  ),
                                 conditionalPanel(condition = "input.def_comp == 'all_pair'",
                                                  actionButton("submit2", "Submit"),
                                                  actionButton("clear2", "Clear matrix")
                                                  ),
                                 sliderInput("signif", 
                                             label = h4("Significance level", tipify(icon("question-circle"), title="Probability of rejecting the null hypothesis given that it is true (probability of type I error)")) , 0, 1, 0.05)
                                 ),

# table of significant proteins 
               fluidRow(
                 column(12,
                        h3("STEP (ii) - Group Comparison"),
                        actionButton("calculate", "Start")
                 )
               ),
             tags$br(),

               fluidRow(
                 column(12,
                        h3("STEP (iii) - Visualization"),
                                         fluidRow(
    
                                                  selectInput("typeplot", 
                                                              label = h4("Select plot type"), c("Volcano Plot" = "VolcanoPlot", "Heatmap"="Heatmap", "Comparison Plot"="ComparisonPlot")),
                                                  conditionalPanel(condition = "input.typeplot == 'VolcanoPlot'",
                                                                   uiOutput("WhichComp")),
                                                  tags$br(),
                                                  conditionalPanel(condition = "input.typeplot == 'VolcanoPlot'",
                                                                   checkboxInput("pname", 
                                                                                 label = h5("display protein name"))),
                                                  tags$br(),
                                                  sliderInput("sig", 
                                                              label = h4("Significance level", tipify(icon("question-circle"), title="Probability of rejecting the null hypothesis given that it is true (probability of type I error)")) , 0, 1, 0.05),
                                           
                                                  tags$br(),
                                                   conditionalPanel(condition = "input.typeplot == 'ComparisonPlot'",
                                                                    uiOutput("WhichProt")),
                                                  tags$br(),
                                                  conditionalPanel(condition = "input.typeplot == 'VolcanoPlot' || input.typeplot == 'Heatmap'",
                                                                   checkboxInput("FC1", 
                                                                                 label = h5("Apply specific fold change cutoff for significance")),
                                                                   conditionalPanel(condition = "input.FC1 == true",
                                                                                    numericInput("FC", "cutoff", 1, 0, 100, 0.1)),
                                                                   tags$br(),
                                                                   selectInput("logp", 
                                                                               label = h4("Log transformation of adjusted p-value"),
                                                                               c("base two" = "2", "base ten" = "10"), selected = "10")),
                                                  tags$br(),
                                                  conditionalPanel(condition = "input.typeplot == 'Heatmap'",
                                                                   numericInput("nump", "Number of proteins in heatmap", 100, 1, 180, 1),
                                                                   selectInput("cluster",
                                                                               label = h5("Cluster analysis", tipify(icon("question-circle"), 
                                                                                                                     title= "How to order proteins and comparisons: compute protein dendrogram and reorder based on protein means; compute comparison dendrogram and reorder based on comparison means; or both", 
                                                                                                                     placement = "top")), 
                                                                               c("protein dendogram" = "protein", "comparison dendogram" = "comparison", "protein and comparison dendograms" = "both"))),
                                                  conditionalPanel(condition = "input.typeplot == 'ComparisonPlot'",
                                                                   uiOutput("WhichComp1"))

                                         ),
                        actionButton("plotresults", "Save Plot Results as pdf"),
                        actionButton("viewresults", "View Plot in browser (only for one comparison/protein)")
                 )
               )
    ),

fluidRow(
  column(7,
         uiOutput("matrix"),
         conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                          tags$br(),
                          tags$h4("Calculation in progress (it may take a while)...")),
         uiOutput("table_results"),
         tags$br(),
         tags$br(),
         conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                          tags$br(),
                          tags$br(),
                          tags$h4("Calculation in progress...")),
         tags$br(),
         uiOutput("comparison_plots")
  ))
    

)


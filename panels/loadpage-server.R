# toggle ui (DDA DIA SRM)


observe({
  if (input$DDA_DIA == "DDA") {
    shinyjs::runjs("$('[type=radio][name=filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
    shinyjs::enable("filetype")
    shinyjs::disable(selector = "[type=radio][value=open]")
    shinyjs::disable(selector = "[type=radio][value=spec]")
    shinyjs::runjs("$.each($('[type=radio][name=filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
  }
  else if (input$DDA_DIA == "DIA") {
    shinyjs::runjs("$('[type=radio][name=filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
    shinyjs::enable("filetype")
    shinyjs::disable(selector = "[type=radio][value=maxq]")
    shinyjs::disable(selector = "[type=radio][value=prog]")
    shinyjs::disable(selector = "[type=radio][value=PD]")
    shinyjs::runjs("$.each($('[type=radio][name=filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
  }
  else if (input$DDA_DIA == "SRM_PRM") {
    shinyjs::runjs("$('[type=radio][name=filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
    shinyjs::enable("filetype")
    shinyjs::disable(selector = "[type=radio][value=open]")
    shinyjs::disable(selector = "[type=radio][value=spec]")
    shinyjs::disable(selector = "[type=radio][value=maxq]")
    shinyjs::disable(selector = "[type=radio][value=prog]")
    shinyjs::disable(selector = "[type=radio][value=PD]")
    shinyjs::runjs("$.each($('[type=radio][name=filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
    
  }
  
})

### functions ###

get_annot = reactive({
  annot <- input$annot
  if(is.null(annot)) {
    return(NULL)
  }
  read.csv(annot$datapath)
})

get_annot1 = reactive({
  annot1 <- input$annot1
  if(is.null(annot1)) {
    return(NULL)
  }
  read.csv(annot1$datapath, header = T)
})

get_evidence = reactive({
  evidence <- input$evidence
  if(is.null(evidence)) {
    return(NULL)
    }
  read.table(evidence$datapath, sep="\t", header=TRUE)
  cat(file=stderr(), "Reached in evidence")
})

get_proteinGroups = reactive({
  pGroup <- input$pGroup
  if(is.null(pGroup)) {
    return(NULL)
  }
  read.table(pGroup$datapath, sep="\t", header=TRUE)
})

get_data = reactive({
  if(is.null(input$filetype)) {
    return(NULL)
    }
  if(input$filetype == 'sample') {
     if(input$DDA_DIA == "SRM_PRM") {
       mydata <- SRM_yeast
     }
#     else if(input$DDA_DIA == "DDA") {
 #      mydata <- DDARawData
#     }
     else if(input$DDA_DIA == "DIA")
    mydata <- read.csv("dataset.csv", header = T, sep = ";")
    }
  else {
    infile <- input$data
    if(is.null(infile)) {
      return(NULL)
    }
    if(input$filetype == '10col') {
      mydata <- read.csv(infile$datapath, header = T, sep = input$sep)
    }
    else if(input$filetype == 'sky') {
      data <- read.csv(infile$datapath, header = T, sep = input$sep, stringsAsFactors=F)
      data <- data[which(data$Fragment.Ion %in% c( "precursor", "precursor [M+1]","precursor [M+2]")), ]
      cat(file=stderr(), "Reached here in skyline\n")
      mydata <- SkylinetoMSstatsFormat(data,
                                       annotation = get_annot(),
                                       fewMeasurements="remove",
                                       removeProtein_with1Feature = input$remove)
    }
    else if(input$filetype == 'maxq') {
      ev <- get_evidence()
      an <- get_annot1()
      pg <- get_proteinGroups()
      cat(file=stderr(), "Reached in maxq\n")
      mydata <- MaxQtoMSstatsFormat(evidence= ev, annotation= an, proteinGroups= pg,
                                   useUniquePeptide = TRUE,
                                   summaryforMultipleRows = max,
                                   removeProtein_with1Peptide=input$remove)
    }
    else if(input$filetype == 'prog') {
      data <- read.csv(infile$datapath, header = T, sep = input$sep, stringsAsFactors=F)
      mydata <- ProgenesistoMSstatsFormat(data, annotation = get_annot(), removeProtein_with1Peptide = input$remove)
    }
    else if(input$filetype == 'PD') {
      data <- read.csv(infile$datapath, header = T, sep = input$sep)
      mydata <- PDtoMSstatsFormat(data, annotation = get_annot(), removeProtein_with1Peptide = input$remove)
    }
    else if(input$filetype == 'spec') {
      mydata <- SpectronauttoMSstatsFormat(data)
    }
    else if(input$filetype == 'open') {
      data <- read.csv(infile$datapath, header = T, sep = input$sep)
      OpenSWATHtoMSstatsFormat(raw,
                               annotation = get_annot(),
                               filter_with_mscore = TRUE, ## same as default
                               mscore_cutoff = 0.01, ## same as default
                               fewMeasurements="remove",
                               removeProtein_with1Feature = input$remove)
      # raw <- sample_annotation(data=data,
      #                          sample.annotation=get_annot(),
      #                          data.type='OpenSWATH')
      # data.filtered <- filter_mscore(raw, 0.01)
      # data.transition <- disaggregate(data.filtered)
      # mydata <- convert4MSstats(data.transition)
    }}
  mydata <- unique(data.frame(mydata))
  return(mydata)
})

### outputs ###

get_summary <- reactive({
  if(is.null(get_data())) {
    return(NULL)
  }
  data1 <- get_data()
  data_summary <- Hmisc::describe(data1)
})

output$template <- downloadHandler(
  filename <- function() {
    paste("templateannotation", "csv", sep=".")
  },
  
  content <- function(file) {
    file.copy("templateannotation.csv", file)
  },
  contentType = "csv"
)

output$template1 <- downloadHandler(
  filename <- function() {
    paste("templateevidence", "txt", sep = ".")
  },
  
  content <- function(file) {
    file.copy("templateevidence.txt", file)
  },
  contentType = "txt"
)

output$summary <- renderTable(
  {
    req(get_data())
    head(get_data())
  }, bordered = T
)

output$summary1 <-  renderTable(
  {
    req(get_data())
    df <- get_data()
    
    if ("Fraction" %in% colnames(df)){
      df1 <- df %>% summarise("Number of Conditions" = n_distinct(Condition),
                              "Number of Biological Replicates" = n_distinct(BioReplicate),
                              "Number of Technical Replicates" = n(),
                              "Number_of_Fraction" = n_distinct(Fraction),
                              "Number of MS runs" = n_distinct(Run)
                              
                              
                              
      )
      
      df2 <- df %>% group_by(Condition, Run) %>% summarise("Condition_Run" = n()) %>% ungroup() %>% 
        select("Condition_Run")
      df3 <- df %>% group_by(Run, BioReplicate) %>% summarise("BioReplicate_Run" = n()) %>% ungroup() %>% 
        select("BioReplicate_Run")
      df <- cbind(df1,df2,df3) %>% 
        mutate("Number of Technical Replicates" = Condition_Run/(BioReplicate_Run*Number_of_Fraction) ) %>%
        select(-Condition_Run,-BioReplicate_Run)
      
      
    }
    
    else{
      df <- df %>% summarise("Number of Conditions" = n_distinct(Condition),
                             "Number of Biological Replicates" = n_distinct(BioReplicate),
                             "Number of MS runs" = n_distinct(Run)
                             
      )
      
    }
    
    df <- head(df,1)
    t_df <- transpose(df)
    rownames(t_df) <- colnames(df)
    t_df <- cbind(rownames(t_df), t_df)
    
    colnames(t_df) <- c("", "")
    t_df
    
  }, bordered = T
)

output$summary2 <-  renderTable(
  {
    req(get_data())
    df <- get_data()
    df <- df %>% mutate("FEATURES" = ifelse(input$filetype == 'prog'||
                                            input$filetype == 'PD',
                                          paste(PeptideModifiedSequence, ProteinName, PrecursorCharge, FragmentIon, sep = '_'),
                                          paste(PeptideSequence, ProteinName, PrecursorCharge, FragmentIon, sep = '_'))
                        )
    
    if(input$filetype == 'prog' || input$filetype == 'PD' ){
      Peptides_Proteins <- df %>% group_by(PeptideModifiedSequence, ProteinName)  %>%
        summarise("Number of peptides/proteins" = n()) %>% ungroup() %>% select("Number of peptides/proteins")

      Features_Peptides <- df %>% group_by(FEATURES, PeptideModifiedSequence)  %>%
        summarise("Number of features/peptides" = n()) %>% ungroup() %>% select("Number of features/peptides")

    }
    else {
      Peptides_Proteins <- df %>% group_by(PeptideSequence, ProteinName)  %>%
        summarise("Number of peptides/protein" = n()) %>% ungroup() %>% select("Number of peptides/protein")

      Features_Peptides <- df %>% group_by(FEATURES, PeptideSequence)  %>%
        summarise("Number of features/peptides" = n()) %>% ungroup() %>% select("Number of features/peptides")
    }
    
    df <- df %>% summarise("Number of Protiens" = n_distinct(ProteinName), 
                                   "Number of Peptides" = ifelse(input$filetype == 'prog'||
                                                                   input$filetype == 'PD',
                                                                 n_distinct(PeptideModifiedSequence),
                                                                 n_distinct(PeptideSequence)),
                                   "Number of features" = n_distinct(FEATURES),
                                   "Max Intensity" = ifelse(!is.finite(max(Intensity, na.rm=T)),0,
                                                            max(Intensity, na.rm=T)),
                                   "Min Intensity" = ifelse(!is.finite(min(Intensity, na.rm=T)),0,
                                                            min(Intensity, na.rm=T))
    )


    df <- head(cbind(df,Peptides_Proteins,Features_Peptides),1)
    df <- df[,c(1,2,3,6,7,4,5)]
    t_df <- transpose(df)
    rownames(t_df) <- colnames(df)
    t_df <- cbind(rownames(t_df), t_df)

    colnames(t_df) <- c("", "value")
    t_df$value <- sub("\\.\\d+$", "", t_df$value)

    colnames(t_df) <- c("", "")
    t_df
  }, bordered = T
)


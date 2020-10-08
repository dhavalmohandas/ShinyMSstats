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

get_evidence = reactive({
  evidence <- input$evidence
  if(is.null(evidence)) {
    return(NULL)
    }
  read.table(evidence$datapath, sep="\t", header=TRUE)
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
      mydata <- SkylinetoMSstatsFormat(data,
                                       annotation = get_annot(),
                                       fewMeasurements="remove",
                                       removeProtein_with1Feature = input$remove)
    }
    else if(input$filetype == 'maxq') {
      #data <- read.table(infile$datapath, header = T, sep = input$sep)
      #mydata <- MaxQtoMSstatsFormat(proteinGroups = data, annotation = get_annot(), evidence = get_evidence(), removeProtein_with1Peptide = input$remove)
      mydata <- MaxQtoMSstatsFormat(evidence=get_evidence(), annotation=get_annot(), proteinGroups=get_proteinGroups(),
                                   useUniquePeptide = TRUE,
                                   summaryforMultipleRows = max,
                                   removeProtein_with1Peptide=TRUE)
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
      raw <- sample_annotation(data=data,
                               sample.annotation=get_annot(),
                               data.type='OpenSWATH')
      data.filtered <- filter_mscore(raw, 0.01)
      data.transition <- disaggregate(data.filtered)
      mydata <- convert4MSstats(data.transition)
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
    if(input$filetype == 'sky'){
      df <- get_data() %>% summarise("Number of Conditions" = n_distinct(Condition),
                                     "Number of Biological Replicates" = n_distinct(BioReplicate),
                                     "Number of Technical Replicates" = n(),
                                     "Number of Fraction" = n(),
                                     "Number of MS runs" = n_distinct(Run)
     
      )
      df <- head(df,1)
      t_df <- transpose(df)
      rownames(t_df) <- colnames(df)
      t_df <- cbind(rownames(t_df), t_df)
  
    }
    else if(input$filetype == 'maxq'){
      
      df<- get_data()
      
    }
    colnames(t_df) <- c("", "")
    t_df
    
  }, bordered = T
)

output$summary2 <-  renderTable(
  {
    req(get_data())
    
    df <- get_data() %>% summarise("Number of Protiens" = n_distinct(ProteinName), 
                                   "Number of Peptides" = n_distinct(PeptideSequence),
                                   "Number of peptides/protein" = n_distinct(ProteinName), 
                                   "Number of features/peptides" = n_distinct(PeptideSequence),
                                   "Max Intensity" = max(Intensity, na.rm=T),
                                   "Min Intensity" = min(Intensity, na.rm=T)
    )
    Num_features <- get_data() %>% group_by(PeptideSequence, ProteinName, PrecursorCharge, FragmentIon)  %>% 
      summarise("Number of features" = n()) %>% ungroup() %>% select("Number of features")
      
    df <- head(cbind(Num_features,df),1)
    df <- head(df,1)
    t_df <- transpose(df)
    rownames(t_df) <- colnames(df)
    t_df <- cbind(rownames(t_df), t_df)
    colnames(t_df) <- c("", "")
    t_df
  }, bordered = T
)


##load packages
library(readxl)
library(shiny)
library(shinyWidgets)
library(htmlwidgets)
library(bslib)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(tidyr)
library(naniar)
library(janitor)
library(stringr)
library(ggfittext)
library(openxlsx)
library(plotly)
library(htmltools)
library(data.table)

df_swarm1 <- read.csv("https://raw.githubusercontent.com//MatthewPaulSu/MINMATT_data/main/data/Literature_database_Swarm.csv")
df_WBF1 <- read.csv("https://raw.githubusercontent.com//MatthewPaulSu/MINMATT_data/main/data/Literature_database_Wing_Beat_Frequency.csv")
df_MT1 <- read.csv("https://raw.githubusercontent.com//MatthewPaulSu/MINMATT_data/main/data/Literature_database_Mechanical_Tuning.csv")
df_ET1 <- read.csv("https://raw.githubusercontent.com//MatthewPaulSu/MINMATT_data/main/data/Literature_database_Electrical_Tuning.csv")
df_phono_all1 <- read.csv("https://raw.githubusercontent.com//MatthewPaulSu/MINMATT_data/main/data/Literature_database_Phonotaxis_All.csv")
df_phono_freq1 <- read.csv("https://raw.githubusercontent.com//MatthewPaulSu/MINMATT_data/main/data/Literature_database_Phonotaxis_Frequency_Responses.csv")

df_swarm <- df_swarm1
df_WBF <- df_WBF1
df_MT <- df_MT1
df_ET <- df_ET1
df_phono_all <- df_phono_all1
df_phono_freq <- df_phono_freq1

df_swarm$Tethered_Free <- rep("NA", length(df_swarm$Group))
df_MT$Tethered_Free <- rep("NA", length(df_MT$Group))
df_ET$Tethered_Free <- rep("NA", length(df_ET$Group))
df_phono_all$Tethered_Free <- rep("NA", length(df_phono_all$Group))
df_phono_freq$Tethered_Free <- rep("NA", length(df_phono_freq$Group))
df_phono_freq$Age <- as.character(df_phono_freq$Age)

df_swarm$Stimulus <- rep("NA", length(df_swarm$Group))
df_WBF$Stimulus <- rep("NA", length(df_WBF$Group))
df_phono_all$Stimulus <- rep("NA", length(df_phono_all$Group))
df_phono_freq$Stimulus <- rep("NA", length(df_phono_freq$Group))

df_swarm$Mechanical_State <- rep("NA", length(df_swarm$Group))
df_WBF$Mechanical_State <- rep("NA", length(df_WBF$Group))
df_phono_all$Mechanical_State <- rep("NA", length(df_phono_all$Group))
df_phono_freq$Mechanical_State <- rep("NA", length(df_phono_freq$Group))

df_swarm$'Swarming reports' <- rep("One",length(df_swarm$Group))
df_WBF$'Wing Beat Frequency reports' <- rep("One",length(df_WBF$Group))
df_MT$'Mechanical Tuning reports' <- rep("One",length(df_MT$Group))
df_ET$'Electrical Tuning reports' <- rep("One",length(df_ET$Group))
df_phono_all$'Phonotaxis reports' <- rep("One",length(df_phono_all$Group))

df <- full_join(x=df_WBF, y=df_swarm)[]
df <- full_join(x=df, y=df_MT)[]
df <- full_join(x=df, y=df_ET)[]
df <- full_join(x=df, y=df_phono_all)[]
df <- full_join(x=df, y=df_phono_freq)[]

colnames(df)[11] <- "Wing Beat Frequency (Hz)"
colnames(df)[19] <- "Wing Beat Frequency reports"
colnames(df)[22] <- 'Swarming duration (min)'
colnames(df)[23] <- 'Start (relative to sunrise)'
colnames(df)[24] <- 'Start (relative to sunset)'
colnames(df)[26] <- 'Males collected (field)'
colnames(df)[27] <- 'Swarm size by eye (field)'
colnames(df)[28] <- 'Height (m above ground)'
colnames(df)[29] <- "Swarming reports"
colnames(df)[34] <- "Mechanical Tuning Frequency (Hz)"
colnames(df)[40] <- "Mechanical Tuning reports"
colnames(df)[46] <- "Electrical Tuning Frequency (Hz)"
colnames(df)[48] <- "Electrical Tuning reports"
colnames(df)[63] <- "Phonotaxis reports"
colnames(df)[69] <- "Normalised frequency response"

suppressWarnings(df$'Swarming duration (min)' <- as.numeric(df$'Swarming duration (min)')) 
suppressWarnings(df$'Start (relative to sunrise)' <- as.numeric(df$'Start (relative to sunrise)'))
suppressWarnings(df$'Start (relative to sunset)' <- as.numeric(df$'Start (relative to sunset)'))
suppressWarnings(df$'Males collected (field)' <- as.numeric(df$'Males collected (field)')) 
suppressWarnings(df$'Swarm size by eye (field)' <- as.numeric(df$'Swarm size by eye (field)'))
suppressWarnings(df$'Height (m above ground)' <- as.numeric(df$'Height (m above ground)'))

df <- df %>% group_by(Group, Genus, Species, Paper,Experiment_location)

ui <- fluidPage(tags$head(
  tags$style(HTML("
      .Text pre {
        color: black;
        background-color: white;
        font-family: Helvetica;
        font-size: 14px
      }"))),
     fluidRow(
    column(width = 3,
           wellPanel(
             pickerInput("features",
                         label = NULL,
                         choices = list(Swarming = c("Swarming reports",
                                     "Swarming duration (min)", 
                                     "Start (relative to sunrise)", 
                                     "Start (relative to sunset)",
                                     "Swarm size by eye (field)", 
                                     "Height (m above ground)"),
                                     WBF = c("Wing Beat Frequency reports",
                                             "Wing Beat Frequency (Hz)"),
                                     Function = c("Mechanical Tuning reports",
                                       "Mechanical Tuning Frequency (Hz)",
                                       "Electrical Tuning reports",
                                     "Electrical Tuning Frequency (Hz)"),
                                     Phonotaxis = c("Phonotaxis reports",
                                                    "Normalised frequency response")),
                         selected = "Wing Beat Frequency (Hz)",
                         options = list(
                           title = "Select data type:",
                           `selected-text-format` = "static"),
                         multiple = FALSE),
             div(uiOutput("input_genus", inline = TRUE), width = "100%",style = "font-style: bold;"),
             div(uiOutput("input_species", inline = TRUE), width = "100%",style = "font-style: bold;"),
           )),
    column(width = 3,
           wellPanel( 
             conditionalPanel(
               condition = "input.features == 'Wing Beat Frequency reports'||input.features == 'Wing Beat Frequency (Hz)'||input.features == 'Mechanical Tuning reports' ||input.features == 'Mechanical Tuning Frequency (Hz)'||input.features == 'Electrical Tuning reports' ||input.features == 'Electrical Tuning Frequency (Hz)'||input.features == 'Phonotaxis reports'", 
               uiOutput("input_sex", inline = TRUE), width = "100%"),
             conditionalPanel(
               condition = "input.features != 'Mechanical Tuning reports' & input.features != 'Mechanical Tuning Frequency (Hz)'& input.features != 'Electrical Tuning reports'& input.features != 'Electrical Tuning Frequency (Hz)'", 
               uiOutput("input_location", inline = TRUE), width = "100%"),
             conditionalPanel(
               condition = "input.features == 'Wing Beat Frequency reports'||input.features == 'Wing Beat Frequency (Hz)'", 
               uiOutput("input_tether", inline = TRUE), width = "100%"),
             conditionalPanel(
               condition = "input.features == 'Mechanical Tuning reports' || input.features == 'Mechanical Tuning Frequency (Hz)'|| input.features == 'Electrical Tuning reports'|| input.features == 'Electrical Tuning Frequency (Hz)'", 
               uiOutput("input_state", inline = TRUE), width = "100%",
               uiOutput("input_stimulus", inline = TRUE), width = "100%"), 
             conditionalPanel(
               condition = "input.features == 'Wing Beat Frequency (Hz)' || input.features == 'Mechanical Tuning Frequency (Hz)'||input.features == 'Electrical Tuning Frequency (Hz)'", 
               uiOutput("input_facet_opt", inline = TRUE), width = "100%"),)
    ),
    column(width = 3,
           wellPanel(style = "center",
                     div(style = "display: inline-block;",downloadButton("downloadPlot", "Plot")), 
                     div(style = "display: inline-block;",downloadButton("downloadPlotData", "Plot Data")),
                     div(style = "display: inline-block;",downloadButton("downloadAllData", "All Data")),
                     br(),br(),
                     splitLayout(cellWidths = c("55%", "45%"), 
                                 div(class = "Text",style = "display: inline-block;",verbatimTextOutput(outputId = "date")),
                     div(style = "margin-top: 5px;display: inline-block;",a(actionButton(inputId = "email", label = "Contact authors", 
                                                                                     icon = icon("envelope")),
                                                                        href="https://forms.gle/Rxft47zq5hwLCCbR9"),)),
                     splitLayout(cellWidths = c("25%", "75%"), 
                                 h2(strong("Reference"), style = "font-size:16px;margin-top: 10px;"),
                                 div(class = "Text",verbatimTextOutput(outputId = "text"))), )
    )
    ),
 
 mainPanel(fillRow(
   plotlyOutput("crossbar", height = 600, width = 1350), flex = 1, width = "100%", height = "100%", align = "center"),
   )
 )

server <- function(input, output, session) {
 
  df.reactive <- 
    reactive({
      df[!is.na(df[[input$features]]),]
      })
  
  #processing input Genus
  output$input_genus <- renderUI({
    
    selected_genus <- sort(unique(df.reactive()$Genus))
    
    pickerInput("Genus",
                choices = sort(unique(selected_genus)),
                selected = sort(unique(selected_genus)),
                options = list(
                  title = "Select genus:",
                  `selected-text-format` = "static",
                  `actions-box` = TRUE,
                  size = 10,
                  `live-search` = TRUE),
                choicesOpt = list(
                  content = sprintf("<i>%s</i>", sort(unique(selected_genus)))),
                multiple = TRUE,
                width = '100%')
    
  })

  #processing input location
  output$input_location <- renderUI({
    
    selected_location <- df %>% dplyr::filter(Genus %in% input$Genus) %>%
      pull(unique(.data$Experiment_location))
    
    pickerInput(
      inputId = "Experiment_location",
      choices = sort(unique(na.omit(selected_location))),
      selected = unique(na.omit(selected_location)),
      options = list(
        title = "Select location:",
        `selected-text-format` = "static"), 
      multiple = TRUE,
      width = '100%'
    )

  })
  
  #processing input genus to give an output displaying input species
  output$input_species <- renderUI({
    
    selected_species <- df.reactive() %>% dplyr::filter(Genus %in% input$Genus) %>%
      pull(unique(.data$Group))
 
    pickerInput(
      inputId = "Group",
      choices = list(
        Aedes = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Aedes"]))),
        Anopheles = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Anopheles"]))),
        Armigeres = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Armigeres"]))),
        Culiseta = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Culiseta"]))),
        Culex = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Culex"]))),
        Deinocerites = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Deinocerites"]))),
        Hulecoetomyia = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Hulecoetomyia"]))),
        Mansonia = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Mansonia"]))),
        Mimomyia = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Mimomyia"]))),
        Orthopodomyia = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Orthopodomyia"]))),
        Psorophora = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Psorophora"]))),
        Toxorhynchites = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Toxorhynchites"]))),
        Uranotaenia = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Uranotaenia"]))),
        Verrallina = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Verrallina"]))),
        Wyeomyia = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Wyeomyia"])))),
      selected = sort(unique(selected_species)),
      options = list(
        title = "Select species:",
        `selected-text-format` = "static",
        `actions-box` = TRUE,size = 10,
        `live-search` = TRUE),
      choicesOpt = list(
        content = sprintf("<i>%s</i>", sort(unique(df.reactive()$Group)))),
      multiple = TRUE,
      width = '100%'
    )
  })

  #processing input genus to give an output displaying input sex
  output$input_sex <- renderUI({
    
    selected_sex <- df.reactive() %>% dplyr::filter(Genus %in% input$Genus) %>%
      pull(unique(.data$Sex))
    
    pickerInput(
      inputId = "Sex",
      choices = sort(unique(na.omit(selected_sex))),
      selected = unique(na.omit(selected_sex)),
      options = list(
        title = "Select sex:",
        `selected-text-format` = "static"), 
      multiple = TRUE,
      width = '100%'
    )

  })
  
  #processing input genus to give an output displaying input state
  output$input_state <- renderUI({
    
    selected_state <- df.reactive() %>% dplyr::filter(Genus %in% input$Genus) %>%
      pull(unique(.data$Mechanical_State))
    
    pickerInput(
      inputId = "Mechanical_State",
      choices = list(Active = c("Active quiescent","Active SSO"),
                    Passive = c("Passive pymetrozine","Passive sedation")),
      selected = unique(selected_state),
      options = list(
        title = "Select state:",
        `selected-text-format` = "static",
        `actions-box` = TRUE), 
      multiple = TRUE,
      width = '100%'
    )
    
  })
  
  #processing input genus to give an output displaying input stimulus
  output$input_stimulus <- renderUI({
    
    selected_stimulus <- df.reactive() %>% dplyr::filter(Genus %in% input$Genus) %>%
      pull(unique(.data$Stimulus))
    
    pickerInput(
      inputId = "Stimulus", 
      choices = list(Stimulated = c("Sweep","White noise"),
                     Unstimulated = c("Unstimulated")),
      selected = unique(selected_stimulus),
      options = list(
        title = "Select stimulus type:",
        `selected-text-format` = "static",
        `actions-box` = TRUE), 
      multiple = TRUE,
      width = '100%'
    )

  })
  
  #processing input genus to give an output displaying input sex
  output$input_tether <- renderUI({
    
    selected_tether <- df.reactive() %>% dplyr::filter(Genus %in% input$Genus) %>%
      pull(unique(.data$Tethered_Free))
    
    pickerInput(
      inputId = "Tethered_Free", 
      choices = c("Free","Tethered"),
      selected = unique(selected_tether),
      options = list(
        title = "Select flight type:",
        `selected-text-format` = "static"), 
      multiple = TRUE,
      width = '100%'
    )

  })
  
  #processing input facet option to give an output displaying input facet option
  output$input_facet_opt <- renderUI({
    
    selected_facet_opt <- df.reactive() %>% dplyr::filter(Genus %in% input$Genus) %>%
      pull(unique(.data$Group))
    
    pickerInput(
      inputId = "Facet_Option", 
      choices = c("Within species","Within genus"),
      selected = "Within genus",
      options = list(
        title = "Select how to group data:",
        `selected-text-format` = "static"), 
      multiple = FALSE,
      width = '100%'
    )
    
  })
  
  
  dataInput <- reactive({
    
    selected_feature <- switch(input$features,
                               "Swarming reports" = "Swarming reports",
                               "Swarming duration (min)" = "Swarming duration (min)",
                               "Start (relative to sunrise)" = "Start (relative to sunrise)", 
                               "Start (relative to sunset)" = "Start (relative to sunset)",
                               "Swarm size by eye (field)" = "Swarm size by eye (field)",
                               "Height (m above ground)" = "Height (m above ground)",
                               "Wing Beat Frequency reports" = "Wing Beat Frequency reports",
                               "Wing Beat Frequency (Hz)" = "Wing Beat Frequency (Hz)",
                               "Mechanical Tuning reports" = "Mechanical Tuning reports",
                               "Mechanical Tuning Frequency (Hz)"= "Mechanical Tuning Frequency (Hz)",
                               "Electrical Tuning reports" = "Electrical Tuning reports",
                               "Electrical Tuning Frequency (Hz)"= "Electrical Tuning Frequency (Hz)",
                               "Phonotaxis reports" = "Phonotaxis reports",
                               "Normalised frequency response" = "Normalised frequency response")
    
    selected_data <- df[df$Group %in% input$Group & df$Experiment_location %in% input$Experiment_location & df$Sex %in% input$Sex & df$Tethered_Free %in% c(input$Tethered_Free, "NA" ) & df$Mechanical_State %in% c(input$Mechanical_State,"NA") & df$Stimulus %in% c(input$Stimulus,"NA") &!is.na(df[[selected_feature]]), ]
    
  })
  
  plotInput <- reactive({
    selected_feature <- switch(input$features,
                               "Swarming reports" = "Swarming reports",
                               "Swarming duration (min)" = "Swarming duration (min)",
                               "Start (relative to sunrise)" = "Start (relative to sunrise)", 
                               "Start (relative to sunset)" = "Start (relative to sunset)",
                               "Swarm size by eye (field)" = "Swarm size by eye (field)",
                               "Height (m above ground)" = "Height (m above ground)",
                               "Wing Beat Frequency reports" = "Wing Beat Frequency reports",
                               "Wing Beat Frequency (Hz)" = "Wing Beat Frequency (Hz)",
                               "Mechanical Tuning reports" = "Mechanical Tuning reports",
                               "Mechanical Tuning Frequency (Hz)"= "Mechanical Tuning Frequency (Hz)",
                               "Electrical Tuning reports" = "Electrical Tuning reports",
                               "Electrical Tuning Frequency (Hz)"= "Electrical Tuning Frequency (Hz)",
                               "Phonotaxis reports" = "Phonotaxis reports",
                               "Normalised frequency response" = "Normalised frequency response")
    
    selected_data <- df[df$Group %in% input$Group & df$Experiment_location %in% input$Experiment_location & df$Sex %in% input$Sex & df$Tethered_Free %in% c(input$Tethered_Free, "NA" ) & df$Mechanical_State %in% c(input$Mechanical_State,"NA") & df$Stimulus %in% c(input$Stimulus,"NA") & !is.na(df[[selected_feature]]), ]
    selected_data$Genus <- sprintf("<i><b>%s</i></b>", selected_data$Genus)
    selected_data$Group <- sprintf("<i>%s</i>", selected_data$Group)
    
    for (i in 1:length(selected_data$Sex)){
    if (selected_data$Sex[i] == "Female"){
      selected_data$Sex[i] <- intToUtf8(9792)}
      else{
      selected_data$Sex[i] <- intToUtf8(9794)
    }
    }

    suppressWarnings(if(selected_feature == "Normalised frequency response"){
      p <- ggplot(selected_data, aes(x = round_any(Phono_stim_frequency,25), y = .data[[selected_feature]], group = Group)) +
        stat_summary(fun="median",geom="crossbar",mapping = aes(color = Group,ymin=after_stat(y),ymax=after_stat(y)),width=35,lwd=2.8)+
        geom_jitter(aes(text = Paper, shape = Experiment_location),size=1.5, alpha=0.25)+
        labs(
           x = "",
            y = input$features) +
        theme_classic()+
        theme(legend.position = "none",
              axis.text.x = element_text(size = 22, face = "italic", angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.5, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Group," ",Sex), nrow = 1)
    }    else if (selected_feature == "Swarming reports") {
       df1 <- selected_data %>%  group_by(Group, Sex, Genus,Paper,'Swarming reports') %>% tally()
      df1 <- na.omit(df1)%>%  group_by(Group,Genus, Sex) %>% tally()
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group)) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6, color="black", size=6.5)+
       labs(
            x = "",
            y = input$features
            ) +
        theme_classic()+
         theme(legend.position = "none",
              axis.text.x = element_text(size = 22, face = "italic", angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.5, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Genus," ",Sex), nrow = 1, scales = "free_x")
    }   else if (selected_feature == "Wing Beat Frequency reports") {
      df1 <- selected_data %>%  group_by(Group, Sex, Genus,Paper,'Wing Beat Frequency reports') %>% tally()
      df1 <- na.omit(df1)%>%  group_by(Group, Genus, Sex) %>% tally()
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group)) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6,  color="black", size=6.5)+
        labs(
          x = "",
          y = input$features) +
        theme_classic()+
        theme(legend.position = "none",
              axis.text.x = element_text(size = 22, face = "italic", angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.5, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Genus," ",Sex), nrow = 1, scales = "free_x")
    }    else if (selected_feature == "Mechanical Tuning reports") {
      df1 <- selected_data %>%  group_by(Group, Sex, Genus, Paper,'Mechanical Tuning reports') %>% tally()
      df1 <- na.omit(df1)%>%  group_by(Group, Genus, Sex) %>% tally()
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group)) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6,  color="black", size=6.5)+
        labs(
          x = "",
          y = input$features) +
        theme_classic()+
        theme(legend.position = "none",
              axis.text.x = element_text(size = 22, face = "italic", angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.5, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Genus," ",Sex), nrow = 1, scales = "free_x")
    }  else if (selected_feature == "Electrical Tuning reports") {
      df1 <- selected_data %>%  group_by(Group, Sex, Genus, Paper, 'Electrical Tuning reports') %>% tally()
      df1 <- na.omit(df1)%>%  group_by(Group, Genus, Sex) %>% tally()
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group)) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6,    color="black", size=6.5)+
        labs(
          x = "",
          y = input$features) +
        theme_classic()+
        theme(legend.position = "none",
             axis.text.x = element_text(size = 22, face = "italic", angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.5, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Genus," ",Sex), nrow = 1, scales = "free_x")
    }  else if (selected_feature == "Phonotaxis reports") {
      df1 <- selected_data %>%  group_by(Group, Sex, Genus, Paper, 'Phonotaxis reports') %>% tally()
      df1 <- na.omit(df1)%>%  group_by(Group, Genus, Sex) %>% tally()
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group)) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6, color="black", size=6.5)+
        labs(
          x = "",
          y = input$features) +
        theme_classic()+
        theme(legend.position = "none",
              axis.text.x = element_text(size = 22, face = "italic", angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.5, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Genus," ",Sex), nrow = 1, scales = "free_x")
    } else if (input$Facet_Option == "Within species"){
      ggplot(selected_data, aes(x = Sex, y = .data[[selected_feature]],color = Group,
                                customdata=Link)) +
        stat_summary(inherit.aes = FALSE,aes(x = Sex, y = .data[[selected_feature]],color = Group),fun="median",geom="crossbar",width=0.7,lwd=3.6)+
        geom_jitter(aes(text = Paper,shape = Experiment_location),size=4.5, alpha=0.25)+
        stat_summary(inherit.aes = FALSE,aes(x = Sex, y = .data[[selected_feature]],label = round(after_stat(y), 0)),fun = median, geom = "text", color="black", fontface = "bold", size=5.5) +
        labs(
          x = "",
          y = input$features) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 21, face = "bold"),
              axis.text.x = element_text(size = 28, face = "bold.italic", angle = 0, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
               strip.background = element_blank())+
        facet_wrap(~Group, nrow = 1, scales = "free_x")
    }else{
      ggplot(selected_data, aes(x = Group, y = .data[[selected_feature]],color = Group,
                                customdata=Link)) +
      stat_summary(inherit.aes = FALSE,aes(x = Group, y = .data[[selected_feature]],color = Group),fun="median",geom="crossbar",width=0.7,lwd=3.6)+
      geom_jitter(aes(text = Paper,shape = Experiment_location),size=4.5, alpha=0.25)+
      stat_summary(inherit.aes = FALSE,aes(x = Group, y = .data[[selected_feature]],label = round(after_stat(y), 0)),fun = median, geom = "text", color="black", fontface = "bold", size=5.5) +
      labs(
            x = "",
           y = input$features) +
      theme_classic()+
       theme(legend.position = "none",
            title = element_text(size = 21, face = "bold"),
            axis.text.x = element_text(size = 22, face = "italic", angle = 45, vjust = 1, hjust = 1),
            axis.title.x = element_text(size = 1, face = "bold"),
            axis.text.y = element_text(size = 22),
            axis.title.y = element_text(size = 22, face = "bold"),
            strip.text.x = element_text(size = 22, face = "bold.italic"), 
            panel.spacing = unit(0.3, "lines"),
            strip.background = element_blank())+
      facet_wrap(~paste0(Genus," ",Sex), nrow = 1, scales = "free_x")
    }
    )
    
  })
  
  plotInput1 <- reactive({
    selected_feature <- switch(input$features,
                               "Swarming reports" = "Swarming reports",
                               "Swarming duration (min)" = "Swarming duration (min)",
                               "Start (relative to sunrise)" = "Start (relative to sunrise)", 
                               "Start (relative to sunset)" = "Start (relative to sunset)",
                               "Swarm size by eye (field)" = "Swarm size by eye (field)",
                               "Height (m above ground)" = "Height (m above ground)",
                               "Wing Beat Frequency reports" = "Wing Beat Frequency reports",
                               "Wing Beat Frequency (Hz)" = "Wing Beat Frequency (Hz)",
                               "Mechanical Tuning reports" = "Mechanical Tuning reports",
                               "Mechanical Tuning Frequency (Hz)"= "Mechanical Tuning Frequency (Hz)",
                               "Electrical Tuning reports" = "Electrical Tuning reports",
                               "Electrical Tuning Frequency (Hz)"= "Electrical Tuning Frequency (Hz)",
                               "Phonotaxis reports" = "Phonotaxis reports",
                               "Normalised frequency response" = "Normalised frequency response")
    
    selected_data <- df[df$Group %in% input$Group & df$Experiment_location %in% input$Experiment_location & df$Sex %in% input$Sex & df$Tethered_Free %in% c(input$Tethered_Free, "NA" ) & df$Mechanical_State %in% c(input$Mechanical_State,"NA") & df$Stimulus %in% c(input$Stimulus,"NA") & !is.na(df[[selected_feature]]), ]
    
    for (i in 1:length(selected_data$Sex)){
      if (selected_data$Sex[i] == "Female"){
        selected_data$Sex[i] <- intToUtf8(9792)}
      else{
        selected_data$Sex[i] <- intToUtf8(9794)
      }
    }
    
    if(selected_feature == "Normalised frequency response"){
      p <- ggplot(selected_data, aes(x = round_any(Phono_stim_frequency,25), y = .data[[selected_feature]], group = Group)) +
        stat_summary(fun="median",geom="crossbar",mapping = aes(color = Group,ymin=after_stat(y),ymax=after_stat(y)),width=35,lwd=2.8)+
        geom_jitter(aes(text = Paper, shape = Experiment_location),size=1.5, alpha=0.25)+
        labs(
          x = "",
          y = input$features) +
        theme_classic()+
        theme(legend.position = "none",
               axis.text.x = element_text(size = 22, face = "italic", angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.5, "lines"),
               strip.background = element_blank())+
        facet_wrap(~paste0(Group," ",Sex), nrow = 1)
    }    else if (selected_feature == "Swarming reports") {
      df1 <- selected_data %>%  group_by(Group, Sex, Genus,Paper,'Swarming reports') %>% tally()
      df1 <- na.omit(df1)%>%  group_by(Group,Genus, Sex) %>% tally()
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group)) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6, color="black", size=8.5)+
        labs(
           x = "",
          y = input$features
        ) +
        theme_classic()+
        theme(legend.position = "none",
               axis.text.x = element_text(size = 25, face = "italic", angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 25),
              axis.title.y = element_text(size = 25, face = "bold"),
              strip.text.x = element_text(size = 25, face = "bold.italic"), 
              panel.spacing = unit(0.5, "lines"),
               strip.background = element_blank())+
        facet_wrap(~paste0(Genus," ",Sex), nrow = 1, scales = "free_x")
    }   else if (selected_feature == "Wing Beat Frequency reports") {
      df1 <- selected_data %>%  group_by(Group, Sex, Genus,Paper,'Wing Beat Frequency reports') %>% tally()
      df1 <- na.omit(df1)%>%  group_by(Group, Genus, Sex) %>% tally()
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group)) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6,  color="black", size=8.5)+
        labs(
           x = "",
          y = input$features) +
        theme_classic()+
        theme(legend.position = "none",
              axis.text.x = element_text(size = 25, face = "italic", angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 25),
              axis.title.y = element_text(size = 25, face = "bold"),
              strip.text.x = element_text(size = 25, face = "bold.italic"), 
              panel.spacing = unit(0.5, "lines"),
               strip.background = element_blank())+
        facet_wrap(~paste0(Genus," ",Sex), nrow = 1, scales = "free_x")
    }    else if (selected_feature == "Mechanical Tuning reports") {
      df1 <- selected_data %>%  group_by(Group, Sex, Genus, Paper,'Mechanical Tuning reports') %>% tally()
      df1 <- na.omit(df1)%>%  group_by(Group, Genus, Sex) %>% tally()
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group)) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6,  color="black", size=8.5)+
        labs(
          x = "",
          y = input$features) +
        theme_classic()+
        theme(legend.position = "none",
              axis.text.x = element_text(size = 25, face = "italic", angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 25),
              axis.title.y = element_text(size = 25, face = "bold"),
              strip.text.x = element_text(size = 25, face = "bold.italic"), 
              panel.spacing = unit(0.5, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Genus," ",Sex), nrow = 1, scales = "free_x")
    }  else if (selected_feature == "Electrical Tuning reports") {
      df1 <- selected_data %>%  group_by(Group, Sex, Genus, Paper, 'Electrical Tuning reports') %>% tally()
      df1 <- na.omit(df1)%>%  group_by(Group, Genus, Sex) %>% tally()
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group)) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6,    color="black", size=8.5)+
        labs(
          x = "",
          y = input$features) +
        theme_classic()+
        theme(legend.position = "none",
              axis.text.x = element_text(size = 25, face = "italic", angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 25),
              axis.title.y = element_text(size = 25, face = "bold"),
              strip.text.x = element_text(size = 25, face = "bold.italic"), 
              panel.spacing = unit(0.5, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Genus," ",Sex), nrow = 1, scales = "free_x")
    }  else if (selected_feature == "Phonotaxis reports") {
      df1 <- selected_data %>%  group_by(Group, Sex, Genus, Paper, 'Phonotaxis reports') %>% tally()
      df1 <- na.omit(df1)%>%  group_by(Group, Genus, Sex) %>% tally()
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group)) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6, color="black", size=8.5)+
        labs(
          x = "",
          y = input$features) +
        theme_classic()+
        theme(legend.position = "none",
              axis.text.x = element_text(size = 25, face = "italic", angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 25),
              axis.title.y = element_text(size = 25, face = "bold"),
              strip.text.x = element_text(size = 25, face = "bold.italic"), 
              panel.spacing = unit(0.5, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Genus," ",Sex), nrow = 1, scales = "free_x")
    } else if (input$Facet_Option == "Within species"){
      ggplot(selected_data, aes(x = Sex, y = .data[[selected_feature]],color = Group,
                                customdata=Link)) +
        stat_summary(inherit.aes = FALSE,aes(x = Sex, y = .data[[selected_feature]],color = Group),fun="median",geom="crossbar",width=0.7,lwd=3.6)+
        geom_jitter(aes(text = Paper,shape = Experiment_location),size=4.5, alpha=0.25)+
        stat_summary(inherit.aes = FALSE,aes(x = Sex, y = .data[[selected_feature]],label = round(after_stat(y), 0)),fun = median, geom = "text", color="black", fontface = "bold", size=9.5) +
        labs(
          x = "",
          y = input$features) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 21, face = "bold"),
              axis.text.x = element_text(size = 28, face = "bold.italic", angle = 0, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 25),
              axis.title.y = element_text(size = 25, face = "bold"),
              strip.text.x = element_text(size = 25, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~Group, nrow = 1, scales = "free_x")
    }else{
      ggplot(selected_data, aes(x = Group, y = .data[[selected_feature]],color = Group,
                                customdata=Link)) +
        stat_summary(inherit.aes = FALSE,aes(x = Group, y = .data[[selected_feature]],color = Group),fun="median",geom="crossbar",width=0.7,lwd=3.6)+
        geom_jitter(aes(text = Paper,shape = Experiment_location),size=4.5, alpha=0.25)+
        stat_summary(inherit.aes = FALSE,aes(x = Group, y = .data[[selected_feature]],label = round(after_stat(y), 0)),fun = median, geom = "text", color="black", fontface = "bold", size=9.5) +
        labs(
          x = "",
          y = input$features) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 25, face = "bold"),
              axis.text.x = element_text(size = 25, face = "italic", angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 25),
              axis.title.y = element_text(size = 25, face = "bold"),
              strip.text.x = element_text(size = 25, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Genus," ",Sex), nrow = 1, scales = "free_x")
    }
    
    
  })
  
  output$crossbar <- renderPlotly({
    ggplotly(plotInput()
             + theme(axis.text.x = element_text(size = 14),
                     axis.text.y = element_text(size = 16),
                     axis.title.y = element_text(size = 18),
                     strip.text.x = element_text(size = (10 + 9/length(unique(input$Group))))),
             tooltip = c(input$features,"Paper")
    )
    })
  
  output$text <- suppressWarnings(renderPrint({
   click <- event_data("plotly_click")
    req(click)
    cat(click$customdata)
     }))
  
  output$date <- renderText({"Version: 5th Apr 2024"})
  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste(input$features, ".png", sep ="")},
    content = function(file) {
      ggsave(file, plot = plotInput1() , device = "png",
             width = 900, height = 600, dpi = 300, units = "mm")
    }
  )
  
  output$downloadPlotData <- downloadHandler(
    filename = function(){paste("Plot_data.csv", sep ="")},
    content = function(file) {
      write.csv(Filter(function(x)!all(is.na(x)), dataInput())[1:(length(Filter(function(x)!all(is.na(x)), dataInput()))-1)], file, row.names = FALSE)
    }
  )

  output$downloadAllData <- downloadHandler(
    filename = function(){paste("All_data.xlsx", sep ="")},
    content = function(file) {
      dataset_list <- list("Swarming" = df_swarm1, "WBF" = df_WBF1,"Mechanical Tuning" = df_MT1, "Electrical Tuning" = df_ET1,"Phonotaxis all" = df_phono_all1, "Phonotaxis frequency" = df_phono_freq1)
      write.xlsx(dataset_list, file)   
      }
  )
  
}

shinyApp(ui = ui, server = server)

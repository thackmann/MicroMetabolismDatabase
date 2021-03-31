###############
#Load packages
###############
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(shinyjs)
library(grid)
library(gridExtra)
library(tidyr)
library(shinycssloaders)
library(stringr)
library(icons)

###################
#Load internal data
###################
#Database
data_fp = "data/database.csv"
raw_data = read.csv(data_fp, stringsAsFactors=FALSE)
raw_data[is.na(raw_data)] = "NA"

data_fp = "data/query.csv"
example_query = read.csv(data_fp, stringsAsFactors=FALSE)

#Icons
app_icons <- icon_set("icons")

#################
#Define functions
#################
#Define navigation buttons found on home page
  navigation_button <- function(title_box, image_name, button_name) {
    div(class="landing-page-box",
        div(class = "landing-page-icon", tags$img(src=paste0(image_name, ".svg"), style="display: block; max-width: 100%; max-height: 175px; height:  auto; margin: 0 auto;"),
        div(title_box, class = "landing-page-box-title", style="margin:  auto")
    ),
        actionButton(button_name, NULL, class="landing-page-button")
    )
  }

#Define file input box with added download link
  fileInput_custom=function(inputId, label, multiple = FALSE, accept = NULL, width = NULL, buttonLabel = "Browse...", placeholder = "No file selected", downloadId, downloadlabel) 
  {
    restoredValue <- restoreInput(id = inputId, default = NULL)
    if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
      warning("Restored value for ", inputId, " has incorrect format.")
      restoredValue <- NULL
    }
    if (!is.null(restoredValue)) {
      restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
    }
    inputTag <- tags$input(id = inputId, name = inputId, type = "file", 
                           style = "display: none;", `data-restore` = restoredValue)
    if (multiple) 
      inputTag$attribs$multiple <- "multiple"
    if (length(accept) > 0) 
      inputTag$attribs$accept <- paste(accept, collapse = ",")
    div(
      class = "form-group shiny-input-container", style = if (!is.null(width)) 
        paste0("width: ", validateCssUnit(width), ";"),
      shinyInputLabel(inputId, label), 
      div(
        class = "input-group", 
        tags$label(class = "input-group-btn input-group-prepend", 
                   span(class = "btn btn-default btn-file", 
                        buttonLabel, inputTag)), 
        tags$input(type = "text", 
                   class = "form-control", placeholder = placeholder, 
                   readonly = "readonly")),
      downloadLink(downloadId, downloadlabel),
      tags$div(
        id = paste(inputId, "_progress", sep = ""), class = "progress active shiny-file-input-progress", 
        tags$div(class = "progress-bar")),
    )
  }

#Define helper function for upload file box function
  shinyInputLabel <- function(inputId, label = NULL) {
    tags$label(
      label,
      class = "control-label",
      class = if (is.null(label)) "shiny-label-null",
      `for` = inputId
    )
  }

#Plot piecharts
  plot_piechart=function(data, var, max_cat=5, max_char=15)
  {
    #Get data
    if(is.null(data)|nrow(data)==0)
    {
      data = data.frame(V1=NA, n=1)
      names(data)[names(data)=="V1"]=var
    }else{
      data = data %>% dplyr::count(!!sym(var))
    }
    
    #Replace NA
    data[[1]][which(is.na(data[[1]]))]="NA"
    data[[2]][which(is.na(data[[2]]))]=0
    
    #Order data by count
    data = data %>% dplyr::arrange(desc(data[[2]]))
    
    #If number of categorical variables exceeds max, create "other" variable
    if(nrow(data)>max_cat)
    {
      data[[2]][max_cat]=sum(as.numeric(data[[2]][max_cat:nrow(data)]))
      data[[1]][max_cat]="Other"
      data=data[1:max_cat,]
    }
    
    #If a name of a categorical variable exceed max, truncate it
    to_truncate=which(nchar(data[[1]])>max_char)
    data[[1]][to_truncate]=substr(x=data[[1]][to_truncate], start=0, stop=(max_char-3))
    data[[1]][to_truncate]=paste0(data[[1]][to_truncate],"...")
    
    #Create factor levels to order data
    data[[1]]=as.factor(data[[1]])
    data[[1]] <- factor(data[[1]], levels = data[[1]][order(data[[2]])])
    
    #Plot data
    plot=
      ggplot(data) +
      geom_bar(aes(x="", y=n, fill=!!sym(var), color=!!sym(var)), stat="identity", width=1, alpha=0.5, size=1) +
      coord_polar("y", start=0) +
      ggtitle(sym(var))+
      guides(fill=guide_legend(nrow=3,byrow=TRUE, reverse = TRUE), color=guide_legend(nrow=3,byrow=TRUE, reverse = TRUE))+
      theme_void() + # remove background, grid, numeric labels
      theme(legend.position="bottom",
            plot.title=element_text(vjust=-5),
            legend.title = element_blank(),
            legend.text = element_text(color="black", size="10"),# family="ArialMT"),
            plot.margin=margin(t = -0.75, r = 0, b = 0, l = 0, unit = "cm"),
            legend.margin=margin(t = -0.5, unit='cm')
      )
    return(plot)
  }


#Arrange plots in grid
  arrange_plots = function(plots)
  {
    # remove null objects
    to_delete <- !sapply(plots,is.null)
    plots <- plots[to_delete] 
    if (length(plots)==0) return(NULL)
    
    plots_arranged = grid.arrange(grobs=plots,ncol=length(plots))
    
    return(plots_arranged)
  }

#Get choices for drop down boxes for Metabolic_Traits
  get_choices = function(Metabolic_Traits=data$Metabolic_Traits, trait_index=1)
  {
    choices=unique(unlist(strsplit(Metabolic_Traits, ";")))
    choices=strsplit(choices, ":")
    choices=lapply(choices, function(l) l[[trait_index]])
    choices=unique(unlist(choices))
    choices=gsub(pattern="NA", replacement="Any", x=choices)
    
    return(choices)
  }

#Make html link to external website
  createLink <- function(ID,url) {
      if(!is.na(ID)&ID!="NA")
      {
        #Seperate multiple IDs  by ","
        ID_split=unlist(strsplit(x=ID, split=", "))
        
        #Create one link per ID, with commas seperating multiple links
        for(i in 1:length(ID_split))
        {
          temp=sprintf(paste0('<a href="', URLdecode(paste0(url, ID_split[i])),'" target="_blank">', ID_split[i],'</a>'))
          if(i==1)
          {
            link=temp
          }else{
            link=paste(link,temp, sep=",")
          }
          
        }
      }else
      {
        link="NA"
      }
      return(link)
  }

#Make html links with button
  createLinkButton <- function(url) {
    if(!is.na(url))
    {
      sprintf(paste0('<a href="', URLdecode(paste0(url)),'" target="_blank" class="btn btn-primary">Link</a>'))
    }else
    {
      "NA"
    }
  }
  
#Filter data according to preferred taxonomy
  filter_by_taxonomy = function(data, input_taxonomy)
  {
    if(is.null(input_taxonomy[1]))
    {
      data = data[0,]
    }else if(length(input_taxonomy)==2&input_taxonomy[1]=="Bergey"&input_taxonomy[2]=="NCBI")
    {
      data = data
      data$Phylum = ifelse(data$Phylum=="NA",data$NCBI_Phylum,data$Phylum)
      data$Class = ifelse(data$Class=="NA",data$NCBI_Class,data$Class)
      data$Order = ifelse(data$Order=="NA",data$NCBI_Order,data$Order)
      data$Family = ifelse(data$Family=="NA",data$NCBI_Family,data$Family)
      data$Genus = ifelse(data$Genus=="NA",data$NCBI_Genus,data$Genus)
      data$Species = ifelse(data$Species=="NA",data$NCBI_Phylum,data$Species)
    }else if(input_taxonomy[1]=="Bergey")
    {
      data = data %>% filter(Phylum!="NA")
      data$Phylum = data$Phylum
      data$Class = data$Class
      data$Order = data$Order
      data$Family = data$Family
      data$Genus = data$Genus
      data$Species = data$Species
    }else if(input_taxonomy[1]=="NCBI") 
    {
      data = data %>% filter(NCBI_Phylum!="NA")
      data$Phylum = data$NCBI_Phylum
      data$Class = data$NCBI_Class
      data$Order = data$NCBI_Order
      data$Family = data$NCBI_Family
      data$Genus = data$NCBI_Genus
      data$Species = data$NCBI_Species
    }
    
    return(data)
  }
  
#Filter data according to preference for AI predictions
  filter_by_AI = function(data, input_AI)
  {
    if(input_AI=="Show")
    {
      data = data
    }else if(input_AI=="Hide") 
    {
      data = data %>% filter(AI_Predicted!="Y")
    }   
    
    return(data)
  }
 
##############
#Set variables
##############
#Choices for checkboxes
  choices_taxonomy = vector()
  names_taxonomy = c("Phylum", "Class", "Order", "Family", "Genus", "Species", "NCBI_Phylum", "NCBI_Class", "NCBI_Order", "NCBI_Family", "NCBI_Genus", "NCBI_Species")
  choices_taxonomy = 1:length(names_taxonomy)
  names(choices_taxonomy) = names_taxonomy 
  
  choices_traits = vector()
  names_traits = c("Metabolism type", "Carbon source", "Energy source", "Electron source", "Electron acceptor", "Metabolite")
  choices_traits = 1:length(names_traits)
  names(choices_traits) = names_traits 
  
  choices_links = vector()
  names_links = c("Article url", "NCBI taxonomy ID", "GOLD organism ID", "GOLD project ID", "IMG genome ID")
  choices_links = 1:length(names_links)
  names(choices_links) = names_links 

#Choices for drop down boxes
  choices_metabolism_type=get_choices(Metabolic_Traits=raw_data$Metabolic_Traits, trait_index=1)
  choices_carbon_source=get_choices(Metabolic_Traits=raw_data$Metabolic_Traits, trait_index=2)
  choices_energy_source=get_choices(Metabolic_Traits=raw_data$Metabolic_Traits, trait_index=3)
  choices_electron_source=get_choices(Metabolic_Traits=raw_data$Metabolic_Traits, trait_index=4)
  choices_electron_acceptor=get_choices(Metabolic_Traits=raw_data$Metabolic_Traits, trait_index=5)
  choices_metabolite=get_choices(Metabolic_Traits=raw_data$Metabolic_Traits, trait_index=6)

#Dimensions of piecharts
  width_piechart = 225
  height_piechart = 250

###########################
#Define user interface (UI)
###########################
ui <- 
  tagList(
    fluidPage(
      
      #*********
      #Set style
      #*********
      #All tabs/elements
      useShinydashboard(), 
      #Navigation bar
        #Shadow
        tags$style(type="text/css", "nav {box-shadow: 0 1px 3px 0 rgba(0,0,0,.2);}"),
      #Brand logo
      tags$head(
        tags$style(HTML('.navbar-brand {
          padding-top:10px !important; 
          padding-bottom:0 !important;
          height: 70px;
          }
         .navbar {min-height:25px !important;}')),
        #Drop down tabs
        tags$style(HTML('.navbar-nav > li > a {
          padding-top:25px !important; 
          padding-bottom:0 !important;
          height: 70px;
          }
         .navbar {min-height:25px !important;}'))
      ),
      #Right justified drop down tabs
        tags$head(tags$style(HTML("
          .navbar-nav {
          float: none !important;
          }
          .navbar-nav > li:nth-child(6) {
          float: right;
          }
          "))),
      #Caret icons in drop down tabs
      tags$head(tags$style(HTML("
        .caret {
          display: none !important;
        }
       "))),
      #Home
        #Action button
        tags$head(
          tags$style(HTML('
            .Start{font:white}
          ')
         ),
          
        #Navigation icons
        tags$style('
          .landing-page-box {width:100%; height:100%; background-color:white; transition: 0.5s ease; position: relative; object-fit: scale-down;}
        '),
        
        tags$style('
          .landing-page-box:hover {-webkit-transform: scale(1.05); -ms-transform: scale(1.05); transform: scale(1.05); } 
        '),
        
        tags$style('
          .landing-page-button { position: absolute; top:0; width: 100%; height: 100%; opacity: 0;}'
        ),
        
        tags$style('        
          .landing-page-icon {width:100%; height:75%; min-height:10vh; border: 0 ; padding-top: 0vh; object-fit: scale-down; }
        '),
        
        tags$style('
          .landing-page-box-title {font-size: clamp(1px, 1.5vw, 15px); text-align:center; #color: #000000;
            font-weight: bold; background-color: none; width:100%; 
            max-height: 15px; margin-top: 0vh;}
         '),
        
        #Footer
        tags$style('
          .test-class {font-size: 12px; display:inline-block; vertical-align:top; text-align:right;
          position:fixed; width:100%; height:40px; color: white; bottom:0; 
          padding: 10px; background-color: black;z-index: 1000;}
        '),
      
      ),
      
      #*********************
      #Define layout of tabs
      #*********************
      fluidRow(
        
        #NAVIGATION BAR--------------------------------------------------------------------------------------
        navbarPage(
          id="tabs", windowTitle="MicroMetabolism", 
          title=div(tags$img(src='MicroMetabolism_logo.svg', width=200)),
          
          #HOME--------------------------------------------------------------------------------------  
          tabPanel(
            value="home",
            title=div(app_icons$home, "Home"),
            splitLayout(
              p(""),
              p(h3("Welcome to MicroMetabolism")),
              p(""),
              cellWidths = c("8.3333%","91.667%", "8.3333%"),
              cellArgs = list(style = "padding-left: 1vw; padding-right: 1vw; white-space: normal")
            ),
            splitLayout(
              p(""),
              p(h5("With metabolic information on thousands of microbes, MicroMetabolism can help you unravel what microbes are doing in the system you study.  Get started by choosing an option below.")),
              p(""),
              cellWidths = c("8.3333%","91.667%", "8.3333%"),
              cellArgs = list(style = "padding-left: 1vw; padding-right: 1vw; white-space: normal")
            ),
            splitLayout(
              p(""),
              p("", navigation_button(button_name = 'jump_database_by_trait', image_name="database_by_trait", title_box = "Database by trait")),
              p("", navigation_button(button_name = 'jump_database_by_taxa', image_name="database_by_taxa", title_box = "Database by taxa")),
              p("", navigation_button(button_name = 'jump_predict_from_taxa', image_name="predict_from_taxa", title_box = "Predict from taxa")),
              p("", navigation_button(button_name = 'jump_predict_from_text', image_name="predict_from_text", title_box = "Predict from text")),
              p("", navigation_button(button_name = 'jump_download_database', image_name="download_database", title_box = "Download database")),
              p(""),
              cellWidths = c("8.3333%","16.6666%","16.6666%","16.6666%","16.6666%", "16.6666%"),
              cellArgs = list(style = "padding-left: 1vw; padding-right: 1vw; padding-top: 1vh; padding-bottom: 1vw; margin-top: -1vh;")
            ),
            splitLayout(
              p(""),
              p("", navigation_button(button_name = 'jump_neural_nets_tutorial', image_name="neural_nets_tutorial", title_box = "Neural nets tutorial")),
              p("", navigation_button(button_name = 'jump_help', image_name="help", title_box = "Help")),
              p(""),
              cellWidths = c("33.3333%","16.6666%","16.6666%","33.3333%"),
              cellArgs = list(style = "padding-left: 1vw; padding-right: 1vw; padding-top: 1vh; padding-bottom: 1vw; margin-top: -2vh;")
            )
          ),
          
          #DATABASE--------------------------------------------------------------------------------------------
          navbarMenu(title=div(app_icons$database, "Database"),
                     
                     #DATABASE BY TRAIT--------------------------------------------------------------------------------------
                     tabPanel(
                       value="database_by_trait",
                       title="By trait",
                       fluidRow(
                         column(width=12, h3("Database by trait"))
                       ),
                       sidebarPanel(
                         width=3, #style = "box-shadow: 0 2px 4px 0 rgba(0,0,0,.2);", #style = "background-color:#cccccc;",
                         selectInput("metabolism_type", "Metabolism type", choices=choices_metabolism_type),
                         selectInput("carbon_source", "Carbon source", choices=choices_carbon_source),
                         selectInput("energy_source", "Energy source", choices=choices_energy_source),
                         selectInput("electron_source", "Electron source", choices=choices_electron_source),
                         selectInput("electron_acceptor", "Electron acceptor", choices=choices_electron_acceptor),
                         selectInput("metabolite", "Metabolite", choices=choices_metabolite),
                         checkboxGroupInput(inputId="database_trait_taxonomy", label="Taxonomy", choices=list("Bergey","NCBI"), selected=list("Bergey", "NCBI"), inline=TRUE),
                         radioButtons(inputId="database_trait_AI", label="Neural net predictions", choiceNames = list("Show","Hide"), choiceValues = list("Show","Hide"), inline=TRUE),
                         actionButton("database_trait_reset", "Reset selection")
                        ),
                       mainPanel(
                         width=9,
                         useShinyjs(),
                         div(
                           id = "loading_page_database_trait",
                           h4("Loading . . . ")
                         ),
                         hidden(
                           div(
                             id = "results_page_database_trait",
                             fluidRow(
                               box(
                                 title = textOutput("n_match_database_trait"), downloadButton('download_data_database_trait', 'Download results'), status = "primary", solidHeader = TRUE
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Overview", column(width=6, plotOutput("plot_database_trait", height=height_piechart) %>% withSpinner(color="#3C8DBC")), width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE, style = 'overflow-x: scroll;'
                               )
                             ),
                             fluidRow(
                                 box(
                                   title = "Detailed results",  
                                   fluidRow(
                                     column(width=12, offset = 0, dataTableOutput("table_database_trait") %>% withSpinner(color="#3C8DBC"))
                                   ),
                                   fluidRow(
                                     column(width=4, checkboxGroupInput("columns_database_traits_taxonomy", "Taxonomy", choices=choices_taxonomy, selected=c(which(names(choices_taxonomy)=="Genus"), which(names(choices_taxonomy)=="Species")))), 
                                     column(width=4, checkboxGroupInput("columns_database_traits_links", "Links", choices=choices_links, selected=c(which(names(choices_links)=="Article_Url"), which(names(choices_links)=="NCBI_Taxonomy_ID")))), 
                                   ),
                                   width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE
                                 )                              
                             )
                           )
                         )
                       )
                     ),
                     
                     #Database by taxa--------------------------------------------------------------------------------------
                     tabPanel(
                       value="database_by_taxa",
                       title="By taxa",
                       fluidRow(
                         column(width=12, h3("Database by taxa"))
                       ),
                       sidebarPanel(
                         width=3, #style = "box-shadow: 0 2px 4px 0 rgba(0,0,0,.2);", #style = "background-color:#cccccc;",
                         selectizeInput("taxa_phylum", div("Phylum"), choices=c("Any", raw_data$Phylum)),
                         selectizeInput("taxa_class", div("Class"), choices=c("Any", raw_data$Class)),
                         selectizeInput("taxa_order", div("Order"), choices=c("Any", raw_data$Order)),
                         selectizeInput("taxa_family", div("Family"), choices=c("Any", raw_data$Family)),
                         selectizeInput("taxa_genus", div("Genus"), choices=c("Any", raw_data$Genus)),
                         selectizeInput("taxa_species", div("Species"), choices=c("Any", raw_data$Species)),
                         checkboxGroupInput(inputId="database_taxa_taxonomy", label="Taxonomy", choices=list("Bergey","NCBI"), selected=list("Bergey", "NCBI"), inline=TRUE),
                         radioButtons(inputId="database_taxa_AI", label="Neural net predictions", choiceNames = list("Show","Hide"), choiceValues = list("Show","Hide"), inline=TRUE),
                         actionButton("database_taxa_reset", "Reset selection")
                       ),
                       mainPanel(
                         width=9,
                         useShinyjs(),
                         div(
                           id = "loading_page_database_taxa",
                           h4("Loading . . . ")
                         ),
                         hidden(
                           div(
                             id = "results_page_database_taxa",
                             fluidRow(
                               box(
                                 title = textOutput("n_match_database_taxa"), downloadButton('download_data_database_taxa', 'Download results'), status = "primary", solidHeader = TRUE
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Overview", column(width=6, plotOutput("plot_database_taxa", height=height_piechart)  %>% withSpinner(color="#3C8DBC")), width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE, style = 'overflow-x: scroll;'
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Detailed results",  
                                 fluidRow(
                                  column(width=12, offset = 0, dataTableOutput("table_database_taxa") %>% withSpinner(color="#3C8DBC"))
                                 ),
                                 fluidRow(
                                   column(width=4, checkboxGroupInput("columns_database_taxa_taxonomy", "Taxonomy", choices=choices_taxonomy, selected=c(which(names(choices_taxonomy)=="Genus"), which(names(choices_taxonomy)=="Species")))), 
                                   column(width=4, checkboxGroupInput("columns_database_taxa_traits", "Traits", choices=choices_traits, selected=c(which(names(choices_traits)=="Metabolism type"), which(names(choices_traits)=="Carbon source")))), 
                                   column(width=4, checkboxGroupInput("columns_database_taxa_links", "Links", choices=choices_links, selected=c(which(names(choices_links)=="Article_Url"), which(names(choices_links)=="NCBI_Taxonomy_ID")))), 
                                 ),
                                 width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE
                               )
                              )
                           )
                         )
                       )
                     )
          ),
          
          #PREDICT--------------------------------------------------------------------------------------  
          navbarMenu(title=div(app_icons$desktop, "Predict"),
                     
                     #PREDICT FROM TAXA--------------------------------------------------------------------------------------
                     tabPanel(
                       value="predict_from_taxa",
                       title="From taxa",
                       fluidRow(
                         column(width=12, 
                                h3("Predict traits from names of taxa")),
                       ),
                       sidebarPanel(width = 3, 
                                    fileInput_custom("file1", "Upload names of taxa",multiple = TRUE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv"), downloadId="downloadExample", downloadlabel="Download example"),
                                    sliderInput("threshold", "Prediction threshold:",min = 0, max = 1,value = 1),
                                    checkboxGroupInput(inputId="predict_taxa_taxonomy", label="Taxonomy", choices=list("Bergey","NCBI"), selected=list("Bergey", "NCBI"), inline=TRUE),
                                    radioButtons(inputId="predict_taxa_AI", label="Neural net predictions", choiceNames = list("Show","Hide"), choiceValues = list("Show","Hide"), inline=TRUE),
                       ),
                       mainPanel(width = 9,
                                 useShinyjs(),
                                 div(
                                   id = "loading_page_predict_taxa",
                                   h4("Please upload a file at the left")
                                 ),
                                 hidden(
                                   div(
                                     id = "results_page_predict_taxa",
                                     fluidRow(
                                       box(
                                         title = textOutput("n_match_predict_taxa"), downloadButton('download_data_predict_taxa', 'Download results') %>% withSpinner(color="#3C8DBC"), status = "primary", solidHeader = TRUE
                                       )
                                     ),
                                     fluidRow(
                                       box(
                                         title = "Overview", column(width=6, plotOutput("plot_predict_taxa", height=height_piechart) %>% withSpinner(color="#3C8DBC")), width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE, style = 'overflow-x: scroll;'
                                       )
                                     ),
                                     fluidRow(
                                       box(
                                         title = "Detailed results",  dataTableOutput("table_predict_taxa") %>% withSpinner(color="#3C8DBC"),  width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE
                                       )
                                     ),
                                     fluidRow(
                                       box(
                                         #checkboxGroupInput("columns","Select Columns",choices=vchoices, selected=vchoices_checked, inline = T), status = "primary", solidHeader = TRUE, width =12,
                                       )
                                     )
                                   )
                                 )
                       )
                       
                     ),
                     
                     #PREDICT FROM TEXT--------------------------------------------------------------------------------------
                      tabPanel(
                       value="predict_from_text", 
                       title="From text",
                           fluidRow(
                             column(width=10, offset=1, 
                                    h3("Predict traits from text"),
                                    "Please be patient-content can take up to 60 s to load"
                                    )
                           ),
                           fluidRow(
                             column(width=10, offset=1, htmlOutput("predict_text_frame"))
                           )
                          )
          ),
          
          #DOWNLOAD--------------------------------------------------------------------------------------       
          tabPanel(
            value="download_database", 
            title=div(app_icons$download, "Download"),
            splitLayout(
              p(""),
              p(h3("Download database")),
              p(""),
              cellWidths = c("8.3333%","91.667%", "8.3333%"),
              cellArgs = list(style = "padding-left: 10px; padding-right: 10px; white-space: normal")
            ),
            splitLayout(
              p(""),
              p(h5("Click below to download the full database in csv format.")),
              p(""),
              cellWidths = c("8.3333%","91.667%", "8.3333%"),
              cellArgs = list(style = "padding-left: 10px; padding-right: 10px; white-space: normal")
            ),
            splitLayout(
              p(""),
              p(downloadButton('download_database_full', 'Download')),
              p(""),
              cellWidths = c("8.3333%","91.667%", "8.3333%"),
              cellArgs = list(style = "padding-left: 10px; padding-right: 10px; white-space: normal")
            )
          ),
          
          #NEURAL NETS TUTORIAL--------------------------------------------------------------------------------------  
          tabPanel(
            value="neural_nets_tutorial", 
            title=div(app_icons$`graduation-cap`, "Tutorial"),
              fluidRow(
                column(width=10, offset=1,
                       h3("Neural nets tutorial"),
                       tags$div("Many of the metabolic traits in this database were predicted using", strong("neural networks"), ".  To learn more about the power of neural networks, let's see how they can rate movies."),
                       tags$br(),
                       tags$div("Use the example movie review below, or write your own.  Please be patient-content can take 60 s to load."))
              ),
              fluidRow(
                column(width=10, offset=1,
                       column(width=10, offset=1, 
                              fluidRow(
                                htmlOutput("tutorial_frame")
                              )
                       )
                )
              ),
              fluidRow(
                column(width=10, offset=1,
                       "From text alone, a neural network could judge whether this movie was good or bad.  How can it do this?",
                       h3("A look inside neural networks"),
                       tags$br(),
                       tags$div("The tutorial will continue here in the full version of the database.")
                       
                  )
                )
          ),
          
          #HELP--------------------------------------------------------------------------------------       
          tabPanel(
            value="help", 
            title=div(app_icons$`question-circle`, "Help"),
            splitLayout(
              p(""),
              p(h3("Help")),
              p(""),
              cellWidths = c("8.3333%","91.667%", "8.3333%"),
              cellArgs = list(style = "padding-left: 10px; padding-right: 10px; white-space: normal")
            ),
            splitLayout(
              p(""),
              p(h5("How to predict metabolic traits from names of taxa:")),
              #p(uiOutput("video_predict_taxa")),
              cellWidths = c("8.3333%","91.667%", "8.3333%"),
              cellArgs = list(style = "padding-left: 10px; padding-right: 10px; white-space: normal")
            ),
            splitLayout(
              p(""),
              p(uiOutput("video_help_predict_taxa")),
              #HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/N-hoYdGE4xc" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
              p(""),
              cellWidths = c("8.3333%","91.667%", "8.3333%"),
              cellArgs = list(style = "padding-left: 10px; padding-right: 10px; white-space: normal")
            ),
            splitLayout(
              p(""),
              p(h5("This database is a prototype.  More videos will follow here in the full version.")),
              p(""),
              cellWidths = c("8.3333%","91.667%", "8.3333%"),
              cellArgs = list(style = "padding-left: 10px; padding-right: 10px; white-space: normal")
            ),

          ),
          
          #NAVIGATION BAR OPTIONS--------------------------------------------------------------------
          selected="home"
          
        )
      )
    ),
    
    #FOOTER--------------------------------------------------------------------
    tags$footer(
      div(class="test-class", "This work is free to use under the terms of a license from", a(img(src="CC_logo.svg", width = 100), href="https://creativecommons.org/licenses/by-nc/4.0/"))
    )
  )


##############
#Define server
##############
server <- function(input, output, session) {
  
  #***********************************
  #Get user input and generate outputs
  #***********************************
  observeEvent(input$tabs, {
    #HOME---------------------------------------------------------------------------------------------------
    if(input$tabs == "home"){
      #Navigate to tab selected by navigation button
      observeEvent(input$jump_database_by_trait, ignoreInit=TRUE, {
        updateNavbarPage(session, inputId="tabs", selected = "database_by_trait")
      })
      observeEvent(input$jump_database_by_taxa, ignoreInit=TRUE, {
        updateNavbarPage(session, inputId="tabs", selected = "database_by_taxa")
      })
      observeEvent(input$jump_predict_from_taxa, ignoreInit=TRUE, {
        updateNavbarPage(session, inputId="tabs", selected = "predict_from_taxa")
      })
      observeEvent(input$jump_predict_from_text, ignoreInit=TRUE, {
        updateNavbarPage(session, inputId="tabs", selected = "predict_from_text")
      })
      observeEvent(input$jump_download_database, ignoreInit=TRUE, {
        updateNavbarPage(session, inputId="tabs", selected = "download_database")
      })
      observeEvent(input$jump_neural_nets_tutorial, ignoreInit=TRUE, {
        updateNavbarPage(session, inputId="tabs", selected = "neural_nets_tutorial")
      })
      observeEvent(input$jump_help, ignoreInit=TRUE, {
        updateNavbarPage(session, inputId="tabs", selected = "help")
      })
    }    
    
    #DATABASE BY TRAIT--------------------------------------------------------------------------------------
    if(input$tabs == "database_by_trait"){
      
      #Reset choices in selectize input objects (drop down menus)
      observeEvent(input$database_trait_reset,{
        updateSelectInput(session, "metabolism_type",  selected="Any")
        updateSelectInput(session, "carbon_source", selected="Any")
        updateSelectInput(session, "energy_source", selected="Any")
        updateSelectInput(session, "electron_source", selected="Any")
        updateSelectInput(session, "electron_acceptor", selected="Any")
        updateSelectInput(session, "metabolite", selected="Any")
      },
      ignoreNULL = TRUE,  ignoreInit=TRUE)
      
      #Get raw data and filter
      get_data = eventReactive({input$database_trait_taxonomy
        input$database_trait_AI},
        {
          #Get data
          data = raw_data
          
          #Filter according to preferred taxonomy
          data=filter_by_taxonomy(data=data, input_taxonomy=input$database_trait_taxonomy)
        
          #Filter data according to preference for AI predictions
          data=filter_by_AI(data=data, input_AI=input$database_trait_AI)
       
          return(data)
        },
        ignoreNULL = FALSE,  ignoreInit=FALSE)
      
      #Get matching organisms
      get_organisms <- reactive({
        
        #Get data
        data = get_data()
        
        #Get queries
        metabolism_type=input$metabolism_type
        carbon_source=input$carbon_source
        energy_source=input$energy_source
        electron_source=input$electron_source
        electron_acceptor=input$electron_acceptor
        metabolite=input$metabolite
        
        #Set "Any" query to match any value
        metabolism_type = recode(.x=metabolism_type, Any='.*')
        carbon_source=recode(.x=carbon_source, Any='.*')
        energy_source=recode(.x=energy_source, Any='.*')
        electron_source=recode(.x=electron_source, Any='.*')
        electron_acceptor=recode(.x=electron_acceptor, Any='.*')
        metabolite = recode(.x=metabolite, Any='.*')
        
        #Format query as single string
        query=paste(c(metabolism_type, carbon_source, energy_source, electron_source, electron_acceptor, metabolite), collapse=":")
        
        #Keep only matching organisms
        data = data %>% filter(grepl(pattern=query, x=data$Metabolic_Traits)) 
        
        return(data)
        
      })
      
      #Get links to external websites
      get_links <- reactive({
        #Get table with matching organisms
        data = get_organisms()
        
        #Add links
        data$NCBI_Taxonomy_ID <- sapply(X=data$NCBI_Taxonomy_ID, FUN=createLink, url="https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=")
        data$GOLD_Organism_ID <- sapply(X=data$GOLD_Organism_ID, FUN=createLink, url="https://gold.jgi.doe.gov/organism?id=")
        data$GOLD_Project_ID <- sapply(X=data$GOLD_Project_ID, FUN=createLink, url="https://gold.jgi.doe.gov/project?id=")
        data$IMG_Genome_ID <- sapply(X=data$IMG_Genome_ID, FUN=createLink, url="https://img.jgi.doe.gov/cgi-bin/m/main.cgi?section=TaxonDetail&page=taxonDetail&taxon_oid=")
        data$Article_Url <- sapply(X=data$Article_Url, FUN=createLinkButton)
        
        return(data)
      })
      
      #Get selected columns
      get_columns <- reactive({
        #Get table with matching organisms and links
        data = get_links()
        
        #Get names of columns selected via checkbox
        columns_taxonomy = names(choices_taxonomy[as.numeric(input$columns_database_traits_taxonomy)])
        columns_links = names(choices_links[as.numeric(input$columns_database_traits_links)])
        
        #Format names of columns to match data
        columns_links = str_to_title(columns_links)
        columns_links = gsub(pattern=" ", replacement="_", x=columns_links)
        columns_links = gsub(pattern="Id", replacement="ID", x=columns_links)
        columns_links = gsub(pattern="Ncbi", replacement="NCBI", x=columns_links)
        columns_links = gsub(pattern="Gold", replacement="GOLD", x=columns_links)
        columns_links = gsub(pattern="Img", replacement="IMG", x=columns_links)
        
        #Keep only selected columns
        columns=c(columns_taxonomy, columns_links)
        data = data %>% select(all_of(columns))
        
        #Format names of columns for display
        colnames(data) = gsub(pattern="_", replacement=" ", x=colnames(data))
        colnames(data) = str_to_sentence(colnames(data))
        colnames(data) = gsub(pattern="id", replacement="ID", x=colnames(data))
        colnames(data) = gsub(pattern="Ncbi", replacement="NCBI", x=colnames(data))
        colnames(data) = gsub(pattern="Gold", replacement="GOLD", x=colnames(data))
        colnames(data) = gsub(pattern="Img", replacement="IMG", x=colnames(data))
        
        return(data)
        
      })
      
      #Output number of matching organisms
      output$n_match_database_trait=renderText(paste0(nrow(get_organisms()), " organisms have matching traits"))
      
      #Output table with matching organisms and columns
      output$table_database_trait <- renderDataTable({
        get_columns()
      }, escape = FALSE, options = list(scrollX = TRUE))
      
      #Output downloadable csv with matching results
      output$download_data_database_trait <- downloadHandler(
        filename = function() {
          paste("results", "csv", sep = ".")
        },
        content = function(file) {
          sep <- switch("csv", "csv" = ",", "tsv" = "\t")
          
          # Write to a file specified by the 'file' argument
          write.table(get_organisms(), file, sep = sep,
                      row.names = FALSE)
        }
      )   

      output$plot_database_trait = renderPlot(exp={
        grobs=list(plot_piechart(data=get_organisms(), var="Phylum", max_cat=5, max_char=15), plot_piechart(data=get_organisms(), var="Class", max_cat=5, max_char=15), plot_piechart(data=get_organisms(), var="Order", max_cat=5, max_char=15), plot_piechart(data=get_organisms(), var="Family", max_cat=5, max_char=15), plot_piechart(data=get_organisms(), var="Genus", max_cat=5, max_char=15))
        grid.arrange(grobs=grobs,ncol=length(grobs))
      }, width=width_piechart*5, height=height_piechart)
      
      
      #Hide loading page and show results page
      observe({
        if(nrow(get_organisms())>0)
        {
          hide("loading_page_database_trait")
          show("results_page_database_trait")
        }
      })
      
    }
    
    #Database by taxa-------------------------------------------------------------------------------------
    if(input$tabs == "database_by_taxa"){
      
      #***********************
      #Get user input (events)
      #***********************
      
      #Get raw data and filter
      get_data = eventReactive({input$database_taxa_taxonomy
        input$database_taxa_AI},
        {
          #Get data
          data = raw_data
          
          #Filter according to preferred taxonomy
          data=filter_by_taxonomy(data=data, input_taxonomy=input$database_taxa_taxonomy)
          
          #Filter data according to preference for AI predictions
          data=filter_by_AI(data=data, input_AI=input$database_taxa_AI)

          return(data)
        },
        ignoreNULL = FALSE,  ignoreInit=FALSE)
      
      #Get matching organisms
      get_organisms <- reactive({
        
        #Get data
        data = get_data()
        
        #Get queries
        taxa_phylum=input$taxa_phylum
        taxa_class=input$taxa_class
        taxa_order=input$taxa_order
        taxa_family=input$taxa_family
        taxa_genus=input$taxa_genus
        taxa_species=input$taxa_species
        
        #Set "Any" query to match any value
        taxa_phylum = recode(.x=taxa_phylum, Any='.*')
        taxa_class = recode(.x=taxa_class, Any='.*')
        taxa_order = recode(.x=taxa_order, Any='.*')
        taxa_family = recode(.x=taxa_family, Any='.*')
        taxa_genus = recode(.x=taxa_genus, Any='.*')
        taxa_species = recode(.x=taxa_species, Any='.*')
        
        #Keep only matching organisms
        data = data %>% filter(grepl(pattern=taxa_phylum, x=data$Phylum))
        data = data %>% filter(grepl(pattern=taxa_class, x=data$Class))
        data = data %>% filter(grepl(pattern=taxa_order, x=data$Order))
        data = data %>% filter(grepl(pattern=taxa_family, x=data$Family)) 
        data = data %>% filter(grepl(pattern=taxa_genus, x=data$Genus))
        data = data %>% filter(grepl(pattern=taxa_species, x=data$Species))
        
        return(data)
      })
      
      #Reset choices in selectize input objects (drop down menus)
      observeEvent(input$database_taxa_reset,{
        updateSelectInput(session, "taxa_phylum",  selected="Any")
        updateSelectInput(session, "taxa_class", selected="Any")
        updateSelectInput(session, "taxa_order", selected="Any")
        updateSelectInput(session, "taxa_family", selected="Any")
        updateSelectInput(session, "taxa_genus", selected="Any")
        updateSelectInput(session, "taxa_species", selected="Any")
      },
      ignoreNULL = TRUE,  ignoreInit=TRUE)
      
      #Update choices in selectize input objects (drop down menus)
      observeEvent(
        {
          input$taxa_phylum
          input$taxa_class
          input$taxa_order
          input$taxa_family
          input$taxa_genus
          input$taxa_species
          input$database_taxa_reset
          input$database_taxa_taxonomy
          input$database_taxa_AI
        },
        {
          #Get data
          data = get_organisms()
          
          #If user makes a choice for high taxonomic rank (e.g,, phylum), choices for all ranks below (e.g., genus) will be updated
          #All ranks below must have no choice made (must have value of "Any"), or they will not update
          taxa_input=c(input$taxa_phylum, input$taxa_class, input$taxa_order, input$taxa_family, input$taxa_genus, input$taxa_species)
          
          #Find ranks that have no choice made
          taxa_any=(taxa_input!="Any")
          
          #Find ranks below which no choice is made
          vec_sum_to_end=function(vec){
            vec_sum=vector()
            for(i in 1:length(vec))
            {
              vec_sum[i]=sum(vec[i:length(vec)])
            }
            
            return(vec_sum)
          }
          
          taxa_update=vec_sum_to_end(taxa_any)
          taxa_update=(taxa_update==0)
          
          if(taxa_update[2]==1)
          {
            updateSelectInput(session, "taxa_class", choices=c("Any", data$Class))
          }
          if(taxa_update[3]==1)
          {
            updateSelectInput(session, "taxa_order", choices=c("Any", data$Order))
          }
          if(taxa_update[4]==1)
          {
            updateSelectInput(session, "taxa_family", choices=c("Any", data$Family))
          }
          if(taxa_update[5]==1)
          {
            updateSelectInput(session, "taxa_genus", choices=c("Any", data$Genus))
          }
          if(taxa_update[6]==1)
          {
            updateSelectInput(session, "taxa_species", choices=c("Any", data$Species))
          }
        }, 
        ignoreNULL = TRUE,  ignoreInit=TRUE)
      
      #Get traits for matching organisms
      get_traits <- reactive({
        #Get data for matching organisms
        data = get_organisms()
        
        #Place each metabolic phenotype in a seperate row
        data=separate_rows(data=data, Metabolic_Traits, sep = ";", convert = FALSE)
        
        #Place each Metabolic_Traits into a seperate column
        data=separate(data = data, col = Metabolic_Traits, into = c("Metabolism type", "Carbon source", "Energy source", "Electron source", "Electron acceptor", "Metabolite"), sep = ':')
        
        return(data)
      })
      
      #Get links to external websites
      get_links <- reactive({
        #Get table with matching organisms
        data = get_traits()
        
        #Add links
        data$NCBI_Taxonomy_ID <- sapply(X=data$NCBI_Taxonomy_ID, FUN=createLink, url="https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=")
        data$GOLD_Organism_ID <- sapply(X=data$GOLD_Organism_ID, FUN=createLink, url="https://gold.jgi.doe.gov/organism?id=")
        data$GOLD_Project_ID <- sapply(X=data$GOLD_Project_ID, FUN=createLink, url="https://gold.jgi.doe.gov/project?id=")
        data$IMG_Genome_ID <- sapply(X=data$IMG_Genome_ID, FUN=createLink, url="https://img.jgi.doe.gov/cgi-bin/m/main.cgi?section=TaxonDetail&page=taxonDetail&taxon_oid=")
        data$Article_Url <- sapply(X=data$Article_Url, FUN=createLinkButton)
        
        return(data)
      })
      
      #Get selected columns
      get_columns <- reactive({
        
        #Get table with matching organisms and links
        data = get_links()
        
        #Get names of columns selected via checkbox
        columns_taxonomy = names(choices_taxonomy[as.numeric(input$columns_database_taxa_taxonomy)])
        columns_traits = names(choices_traits[as.numeric(input$columns_database_taxa_traits)])
        columns_links = names(choices_links[as.numeric(input$columns_database_taxa_links)])
        
        #Format names of columns to match data
        columns_links = str_to_title(columns_links)
        columns_links = gsub(pattern=" ", replacement="_", x=columns_links)
        columns_links = gsub(pattern="Id", replacement="ID", x=columns_links)
        columns_links = gsub(pattern="Ncbi", replacement="NCBI", x=columns_links)
        columns_links = gsub(pattern="Gold", replacement="GOLD", x=columns_links)
        columns_links = gsub(pattern="Img", replacement="IMG", x=columns_links)
        
        #Keep only selected columns
        columns=c(columns_taxonomy, columns_traits, columns_links)
        data = data %>% select(all_of(columns))
        
        #Format names of columns for display
        colnames(data) = gsub(pattern="_", replacement=" ", x=colnames(data))
        colnames(data) = str_to_sentence(colnames(data))
        colnames(data) = gsub(pattern="id", replacement="ID", x=colnames(data))
        colnames(data) = gsub(pattern="Ncbi", replacement="NCBI", x=colnames(data))
        colnames(data) = gsub(pattern="Gold", replacement="GOLD", x=colnames(data))
        colnames(data) = gsub(pattern="Img", replacement="IMG", x=colnames(data))
        
        return(data)
      })
      
      #****************
      #Generate outputs
      #****************
      
      #Output number of matching organisms
      output$n_match_database_taxa=renderText(paste0(nrow(get_organisms()), " matching organisms have ", nrow(get_traits())," metabolic phenotypes"))
      
      #Output table with matching organisms and columns
      output$table_database_taxa <- renderDataTable({
        get_columns()
        }, escape = FALSE, options = list(scrollX = TRUE))
      
      #Output downloadable csv with matching results
      output$download_data_database_taxa <- downloadHandler(
        filename = function() {
          paste("results", "csv", sep = ".")
        },
        content = function(file) {
          sep <- switch("csv", "csv" = ",", "tsv" = "\t")
          
          # Write to a file specified by the 'file' argument
          write.table(get_organisms(), file, sep = sep,
                      row.names = FALSE)
        }
      )   
      
      #Output pie charts
      output$plot_database_taxa = renderPlot(exp={
        grobs=list(plot_piechart(data=get_traits(), var="Metabolism type", max_cat=5, max_char=15), plot_piechart(data=get_traits(), var="Carbon source", max_cat=5, max_char=15), plot_piechart(data=get_traits(), var="Energy source", max_cat=5, max_char=15), plot_piechart(data=get_traits(), var="Electron source", max_cat=5, max_char=15), plot_piechart(data=get_traits(), var="Electron acceptor", max_cat=5, max_char=15), plot_piechart(data=get_traits(), var="Metabolite", max_cat=5, max_char=15))
        grid.arrange(grobs=grobs,ncol=length(grobs))
      }, width=width_piechart*6, height=height_piechart)
      
      hide("loading_page_database_taxa")
      show("results_page_database_taxa")
      
    }
    
    #PREDICT FROM TAXA--------------------------------------------------------------------------------------
    if(input$tabs == "predict_from_taxa"){
      
      #Read in taxonomy of query organisms
      get_query_taxonomy <- reactive({
        req(input$file1)
        query_taxonomy = read.csv(input$file1$datapath, stringsAsFactors=FALSE)
        return(query_taxonomy)
      })
      
      #Output downloadable csv with example taxonomy
      output$downloadExample <- downloadHandler(
        filename = function() {
          paste("query", "csv", sep = ".")
        },
        content = function(file) {
          table=example_query
          write.csv(table, file, row.names = FALSE)
        }
      )   
      
      #Get raw data and filter
      get_data = eventReactive({input$predict_taxa_taxonomy
        input$predict_taxa_AI},
        {
          #Get data
          data = raw_data
          
          #Filter according to preferred taxonomy
          data=filter_by_taxonomy(data=data, input_taxonomy=input$predict_taxa_taxonomy)
          
          #Filter data according to preference for AI predictions
          data=filter_by_AI(data=data, input_AI=input$predict_taxa_AI)
          
          return(data)
        },
        ignoreNULL = FALSE,  ignoreInit=FALSE)
      
      #Get predictions
      predict_traits <- reactive({
        #Get data
        data = get_data()
        
        #Load taxonomy of query organisms
        query_taxonomy = get_query_taxonomy()
        
        #Set threshhold for making predictions (fraction of species in database that must share a given trait)  
        prediction_threshold=input$threshold
        
        #Predict Metabolic_Traits for organisms in query
        matching_traits_all_queries=data.frame("Phylum"=character(), "Class"=character(), "Order"=character(), "Family"=character(), "Genus"=character(), "Species"=character(), "Metabolism type"=character(), "Carbon_source"=character(), "Energy source"=character(), "Electron source"=character(), "Electron acceptor"=character(), "Metabolite"=character())
        for(i in 1:nrow(query_taxonomy))
        {
          #Get organisms in database with matching taxonomy (NA ignored)
          matching_organisms = data
          for(j in 1:6)
          {
            var=colnames(matching_traits_all_queries)[j]
            if(!is.na(query_taxonomy[i,][[sym(var)]]))
            {
              matching_organisms = matching_organisms %>% filter(!!sym(var)==query_taxonomy[i,][[sym(var)]])
            }
          }
          
          #Get Metabolic_Traits of matching organisms
          matching_traits_all=unique(unlist(strsplit(matching_organisms$Metabolic_Traits, ";")))
          if(is.null(matching_traits_all))
          {
            matching_traits_all="NA:NA:NA:NA:NA:NA"
          }
          
          #Set "NA" to match any value
          matching_traits_all = gsub(pattern="NA", replacement='[a-zA-Z]*', x=matching_traits_all)
          
          #Get only Metabolic_Traits that are shared by a threshold of organisms in the database
          matching_traits_above_threshold=matching_traits_all
          if(nrow(matching_organisms)>0)
          {
            for(j in 1:length(matching_traits_all))
            {
              matches=sum(matching_organisms %>% grepl(pattern=matching_traits_all[j], x=matching_organisms$Metabolic_Traits))
              total=nrow(matching_organisms)
              if((matches/total)>=prediction_threshold)
              {
                matching_traits_above_threshold[j]=matching_traits_above_threshold[j]
              }else
              {
                matching_traits_above_threshold[j]="NA:NA:NA:NA:NA:NA"
              }
            }
            matching_traits_above_threshold=matching_traits_above_threshold[!is.na(matching_traits_above_threshold)]
          }
          
          #Set "NA" back to its original value
          matching_traits_above_threshold = gsub(pattern="[a-zA-Z]*", replacement="NA", x=matching_traits_above_threshold, fixed=TRUE)
          
          #Place Metabolic_Traits in a dataframe
          #Put traits into dataframe
          matching_traits_above_threshold = data.frame(matching_traits_above_threshold)
          matching_traits_above_threshold = separate(data = matching_traits_above_threshold, col = matching_traits_above_threshold, into = c("Metabolism type", "Carbon source", "Energy source", "Electron source", "Electron acceptor", "Metabolite"), sep = ':')
          
          #Add taxonomy of query organism
          for(j in 1:6)
          {
            var=colnames(matching_traits_all_queries)[j]
            matching_traits_above_threshold[[sym(var)]]=as.character(query_taxonomy[i,][j])
          }
          matching_traits_above_threshold = matching_traits_above_threshold %>% relocate("Phylum", "Class", "Order", "Family", "Genus", "Species")
          
          #Remove rows with NA for all Metabolic_Traits
          matching_traits_above_threshold = matching_traits_above_threshold %>% filter(`Metabolism type`!="NA"|`Carbon source`!="NA"|`Energy source`!="NA"|`Electron source`!="NA"|`Electron acceptor`!="NA"|`Metabolite`!="NA")
          
          #Add to dataframe with predictions for other organisms
          matching_traits_all_queries=rbind(matching_traits_all_queries, matching_traits_above_threshold)
        }  
        return(matching_traits_all_queries)
        
      })

      #Output number of matching organisms and traits
      output$n_match_predict_taxa=renderText(paste0(nrow(predict_traits()), " metabolic phenotypes predicted for ", nrow(get_query_taxonomy()), " query taxa"))
      
      #Output downloadable csv with matching results
      output$download_data_predict_taxa <- downloadHandler(
        filename = function() {
          paste("results", "csv", sep = ".")
        },
        content = function(file) {
          sep <- switch("csv", "csv" = ",", "tsv" = "\t")
          
          # Write to a file specified by the 'file' argument
          write.table(predict_traits(), file, sep = sep,
                      row.names = FALSE)
        }
      ) 
      
      #Output table with predicted Metabolic_Traits
      output$table_predict_taxa <- renderDataTable({
        predict_traits()
        }, 
        escape = FALSE, options = list(scrollX = TRUE)) 
      
      #Output pie charts
      output$plot_predict_taxa = renderPlot(exp={
        grobs=list(plot_piechart(data=predict_traits(), var="Metabolism type", max_cat=5), plot_piechart(data=predict_traits(), var="Carbon source", max_cat=5, max_char=15), plot_piechart(data=predict_traits(), var="Energy source", max_cat=5, max_char=15), plot_piechart(data=predict_traits(), var="Electron source", max_cat=5, max_char=15), plot_piechart(data=predict_traits(), var="Electron acceptor", max_cat=5, max_char=15), plot_piechart(data=predict_traits(), var="Metabolite", max_cat=5, max_char=15))
        grid.arrange(grobs=grobs,ncol=length(grobs))
      }, width=width_piechart*6, height=height_piechart)
     
      #Hide loading page and show results page
      observe({
        if(!is.null(input$file1$datapath))
        {
          is.null(input$file1$datapath)
          hide("loading_page_predict_taxa")
          show("results_page_predict_taxa")
        }
      })
      
    }  
    
    #PREDICT FROM TEXT--------------------------------------------------------------------------------------
    if(input$tabs == "predict_from_text"){
      observe({ 
        link <<- paste0("https://timothy-hackmann.shinyapps.io/MicroMetabolismPredictFromText/")
      }) 
      
      output$predict_text_frame <- renderUI({
        frame <- tags$iframe(src=link, width="100%", height=600, style="border:none;")
        return(frame)
      })
    }  
    
    #DOWNLOAD DATABASE--------------------------------------------------------------------------------------
    if(input$tabs == "download_database"){
      
      #Get data
      data = raw_data
      
      #Output downloadable csv of full database
      output$download_database_full <- downloadHandler(
        filename = function() {
          paste("results", "csv", sep = ".")
        },
        content = function(file) {
          sep <- switch("csv", "csv" = ",", "tsv" = "\t")
          
          # Write to a file specified by the 'file' argument
          write.table(data, file, sep = sep,
                      row.names = FALSE)
        }
      ) 
      
    } 
    
    #NEURAL NETS TUTORIAL--------------------------------------------------------------------------------------
    if(input$tabs == "neural_nets_tutorial"){
      observe({ 
        link <<- paste0("https://timothy-hackmann.shinyapps.io/MicroMetabolismTutorial/")
      }) 
      
      output$tutorial_frame <- renderUI({
        frame <- tags$iframe(src=link, width="100%", height=250, style="border:none;")
        return(frame)
      })
    }
    
    #HELP----------------------------------------------------------------------------------------------------
    if(input$tabs == "help"){
      
    output$video_help_predict_taxa <- renderUI({
        HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/N-hoYdGE4xc" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    })
      
    }
 
  })
}

########
#Run app
########
shinyApp(ui = ui, server = server)
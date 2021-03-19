#Deploy app using blue icon in upper right hand corner of this window, then run L2 to L3 in console
#setwd("C:\\My Directory\\MicroMetabolismTutorial")
#rsconnect::configureApp("MicroMetabolismTutorial", account="my-account-name", size="xlarge")

##########################
#Load and install packages
##########################
#Install Python packages (done only on non-Windows platforms)
if(.Platform$OS.type != "windows") {
  reticulate::virtualenv_create("myenv", python="/usr/bin/python3")
  
  if (!keras::is_keras_available()) {
    
    reticulate::py_install(packages = c("tensorflow==2.4.0", "keras", "tensorflow-hub", "h5py", "pyyaml==3.12", "requests", "Pillow", "scipy"), 
                           envname = "myenv", method = "virtualenv", conda = "auto", python_version = "3.6", 
                           pip = FALSE)
    
    reticulate::use_virtualenv("myenv", required=TRUE)
  }
}

#Load R packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(shinycssloaders)

#################
#Define functions
#################

#Plot gauge chart
plot_gauge  <- function(pos, breaks = c(0, 33, 66, 100), determinent="") 
{
  ggplot() + 
    geom_segment(data = get.poly(breaks[1],breaks[4]), aes(x = x, y = y, xend = xend, yend = yend, color = xend)) +
    scale_color_gradientn(colors = c("red", "gold", "green")) +
    geom_segment(data = get.poly(pos - 0.25, pos + 0.25, 0.2), aes(x = x, y  =y-1, xend = xend, yend = yend+1)) +
    geom_text(data=as.data.frame(breaks), size = 6, fontface = "bold", vjust = 0, aes(x = 1 * breaks,  y = -4), label = c('-', '', '', "+")) +
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm"),
          legend.position = "none")
}

#Define helper function for gauge chart  
get.poly <- function(a=0, b=100, r1 = 0.5, r2 = 1.0) 
{
  th       <- seq(a, b, length = 1000)
  x        <- th
  xend     <- th
  y        <- 0
  yend     <- 5
  data.frame(x, y, xend, yend)
}

#Get sentences containing keywords
get_sentences_with_keywords=function(text, keywords)
{
  text = unlist(strsplit(text, "\\."))
  text = text[grep(pattern = keywords, x=text)]
  text = unique(text)
  text = paste(text, collapse=".") 
  
  return(text)
}

#Tokenize text and predict traits
predict_traits = function(tokenizer, model, text, maxlen = 3000)
{
  text_seqs=keras::texts_to_sequences(tokenizer, text)
  text_seqs_padded = text_seqs %>% keras::pad_sequences(maxlen = maxlen)
  x_eval = rbind(text_seqs_padded, text_seqs_padded)
  prediction = model %>% predict(x_eval[1:nrow(x_eval), ])
  prediction=prediction[1]	  
  
  return(prediction)
}	  

###########################
#Define user interface (UI)
###########################
ui <- 
  tagList(
    fluidPage(
      #*********
      #Set style
      #*********
      useShinydashboard(),
      
      #*************
      #Define layout
      #*************
      fluidRow(
          box(width=12, title="Let's rate movies!", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              column(width=6, align = 'center', 
                     textAreaInput(inputId ="input_tutorial", label = "", value = "This movie had everything!  I loved it!", height='125px')
              ),
              column(width=6, align = 'center', style='border-left: 1px solid gray', 
                     uiOutput("prediction_icon_tutorial") %>% withSpinner(color="#3C8DBC", proxy.height='125px'),
                     uiOutput("prediction_text_tutorial")
                     
              )
          )
        )
    )
)



##############
#Define server
##############
server <- function(input, output, session) {
  
  #******************
  #Initialize session
  #******************
  cat("Doing session startup\n")
  #Unload keras and reticulate (can crash app if already loaded)
  if("keras" %in% tolower((.packages())))
  {
    detach("package:keras", unload=TRUE)
  }
  if("reticulate" %in% tolower((.packages())))
  {
    detach("package:reticulate", unload=TRUE)
  }
  
  #***********************************
  #Get user input and generate outputs
  #***********************************
  #Instead of loading keras, call functions via keras::
  
    #Load the tokenizer
    tokenizer=reticulate::py_load_object(filename="tokenizer_tutorial.pkl", pickle = "pickle")
    
    # Load the model
    model <- keras::load_model_tf("model_tutorial")
    
    #Get prediction for neural network
    get_prediction = reactive({
      text = input$input_tutorial
      prediction=predict_traits(tokenizer=tokenizer, model=model, text=text, maxlen = 400)                            
      
      return(prediction)
      
    })
    
    #Output icon (thumbs up or down)
    output$prediction_icon_tutorial = renderUI({
      
      if(get_prediction()>=0.5) {
        styleTag <- "color:green"
        thumbs = "thumbs-up"
      }else if(get_prediction()>=0){
        styleTag <- "color:red"
        thumbs = "thumbs-down"
      }
      
      tags$span(
        style = styleTag, 
        icon(thumbs, "fa-10x")
      )
      
    })
    
    #Output number
    output$prediction_number_tutorial = renderUI({
      
      number=round(get_prediction()*100, digits=0)
      
      return(number)
    })
    
    #Output text (good, OK, or bad)
    output$prediction_text_tutorial = renderUI({
      
      number=round(get_prediction()*100, digits=0)
      
      if(get_prediction()>=0.5) {
        styleTag <- "color:green"
        text = "good"
      }else if(get_prediction()>=0){
        styleTag <- "color:red"
        text = "bad"
      }
      
      div("This movie is", tags$span(style= styleTag, strong(text)), "(positivity score: ", number, ")")
      
    })
  
  #************
  #Stop session
  #************
  session$onSessionEnded(function() 
  {
    #Unload keras and reticulate (can crash app for next session if loaded)
    if("keras" %in% tolower((.packages())))
    {
      detach("package:keras", unload=TRUE)
    }
    if("reticulate" %in% tolower((.packages())))
    {
      detach("package:reticulate", unload=TRUE)
    }
  })
}

########
#Run app
########
shinyApp(ui = ui, server = server)
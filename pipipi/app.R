library(shiny)
library(shinyjs)
library(ggplot2)
library(phonTools)

# Module to generate table
gen_table <- function(input, output, session) {
  dt = data.frame(x = c(1:7, 1:7, 1:7, 2:6, 1, 7),
                  y = c(rep(4, 7), rep(3, 7), rep(2, 7), rep(1, 7)),
                  val = c(toupper(letters), '', ''),
                  col = c(rep('blue', 26), 'white', 'white'),
                  stringsAsFactors = FALSE)
  
  return(dt)
}

# Module to generate word
gen_word <- function(input, output, session) {
  # Load words from CSV
  dt = read.csv('most-common-nouns-english.csv')
  words = toupper(dt[nchar(as.character(dt[, 1])) > 2, 1])
  word_ = sample(words, 1)
  
  dt_word = data.frame(letter = unlist(strsplit(word_, "")),
                       guessed = rep('_', nchar(word_)),
                       stringsAsFactors = FALSE)
  
  return(dt_word)
}

# Define UI for application that draws on a spectrogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    body {
                      background-color: black; /* Black background for the whole page */
                      color: white; /* White text */
                    }
                    .spectrum {
                      position: absolute;
                      left: 0;
                      top: 0;
                      width: 700px;
                      height: 400px;
                    }
                    .wrapper {
                      position: relative;
                      width: 700px;
                      height: 400px;
                      visibility: visible;
                      -moz-user-select: none;
                      -webkit-user-select: none;
                      -ms-user-select: none;
                      user-select: none;
                    }
                    #allSpectrum {
                      position: relative;
                    }
                    h1, h2 {
                      color: white; /* White headers */
                    }
                    #image {
                      max-height: 400px; /* Ensure the image fits properly */
                    }
                    .button-style {
                      background-color: #e74c3c; /* Red button */
                      color: white;
                      width: 100%;
                      font-size: 18px;
                      padding: 15px;
                      border: none;
                      border-radius: 5px;
                      cursor: pointer;
                      transition: all 0.3s ease;
                    }
                    .button-style:hover {
                      background-color: #c0392b; /* Darker red on hover */
                    }
                    .modal-header, .modal-footer {
                      background-color: #2c3e50; /* Dark Grey */
                      border: none;
                    }
                    .modal-body {
                      background-color: #34495e; /* Darker Grey */
                    }
                    "))
  ),
  
  titlePanel("Guess the word or be hanged!!!"),
  
  br(),
  
  fluidRow(column(3, br(), br(), br(), br(), br(), br(), h1(textOutput("word_hidden"))),
           column(3, imageOutput("image", height = "400px"))),
  
  br(),
  
  useShinyjs(),
  
  bootstrapPage(
    tags$script(HTML(
      "
      spectrum1_click = function(event) {
      var canvas = document.getElementById('spectrum1');
      if (canvas.getContext)
      {
      var context = canvas.getContext('2d');
      Shiny.setInputValue('x1', event.offsetX);
      Shiny.setInputValue('y1', event.offsetY)
      }  
      };
      "
    )),
    
    mainPanel(
      div(class = "wrapper",
          plotOutput("allSpectrum", click = "plot1_click"),
          HTML("<canvas id='spectrum1' class='spectrum' width=700 height=400 onclick='spectrum1_click(event)'></canvas>")
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Call modules
  responses <<- callModule(module = gen_table, id = "gen_table")
  dt_word   <<- callModule(module = gen_word, id = "gen_word")
  counter   <<- 0
  
  # Render Spectrum
  output$allSpectrum <- renderPlot({
    color_palette = c("#85C1E9", "#FFFFFF", "FFFFFF")
    
    ggplot(responses, aes(x, y, fill = col, label = val)) +
      geom_tile(colour = 'white', show.legend = FALSE, width = 0.99, height = 0.99, size = 0.5) +
      geom_text(hjust = 0.5, vjust = 0, size = 10) +
      scale_fill_manual(values = color_palette, drop = FALSE) +
      theme_void() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_blank())
  })
  
  # Observe Click Event
  observeEvent(c(input$x1), {
    output$allSpectrum <- renderPlot({
      color_palette = c("#85C1E9", "#FFFFFF", "#FFFFFF")
      
      ggplot(responses, aes(x, y, fill = col, label = val)) +
        geom_tile(colour = 'white', show.legend = FALSE, width = 0.99, height = 0.99, size = 0.5) +
        geom_text(hjust = 0.5, vjust = 0, size = 10) +
        scale_fill_manual(values = color_palette, drop = FALSE) +
        theme_void() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_blank())
    })
  })
  
  # Render Hidden Word
  output$word_hidden <- renderText({
    paste(dt_word$guessed, sep = ' ')
  })
  
  # Handle Game Progress
  observeEvent(input$x1, {
    output$word_hidden <- renderText({
      if (counter == 10) {
        paste(dt_word$letter, sep = ' ') 
      } else {
        paste(dt_word$guessed, sep = ' ')  
      }
    })
  })
  
  # Handle User Click and Game Updates
  observeEvent(input$x1, {
    x_pos = ceiling(as.numeric(input$x1) / 100)
    y_pos = abs(ceiling(as.numeric(input$y1) / 100) - 5)
    
    letter = responses$val[responses$x == x_pos & responses$y == y_pos]
    used   = responses$col[responses$val == letter] != 'blue'
    
    responses$col[responses$val == letter] <<- 'red'
    
    dt_word$guessed[dt_word$letter == letter] <<- letter
    
    if (sum(dt_word$letter == letter) == 0 & !used) {
      counter <<- counter + 1
    }
    
  }, priority = 1)
  
  # Game End Logic
  observeEvent(input$x1, {
    if (sum(grepl('_', dt_word$guessed)) == 0) {
      showModal(modalDialog(
        tags$div(
          style = "text-align: center;",
          tags$h2(
            tags$span(icon("laugh-wink"), style = "color: #FF7034;"),
            "Well done! You've won!",
            tags$span(icon("laugh-wink"), style = "color: #FF7034;")
          ),
          tags$br(), tags$br(),
          actionButton(inputId = "reload", label = "Play again!", style = "width: 100%;")
        ),
        footer = NULL,
        easyClose = FALSE
      ))
      
    } else if (counter == 10) {
      msg = paste("the word was", paste0(dt_word$letter))
      
      showModal(modalDialog(
        tags$div(
          style = "text-align: center;",
          tags$h2(
            tags$span(icon("sad-cry"), style = "color: #6e91cc;"),
            "Oh, no! You lost...",
            tags$span(icon("sad-cry"), style = "color: #6e91cc;")
          ),
          tags$br(), tags$br(),
          actionButton(inputId = "reload", label = "Don't despair! Try again!", style = "width: 100%;")
        ),
        footer = NULL,
        easyClose = FALSE
      ))
    }   
  }, priority = -1)
  
  # Reload the game on button click
  observeEvent(input$reload, {
    session$reload()
  }, ignoreInit = TRUE)
  
  # Render Image Based on Counter
  observeEvent(c(input$x1), {
    output$image <- renderImage({
      return(list(
        src = paste0("images/0", counter, ".png"),
        contentType = "image/png",
        alt = "Face1"
      ))
    }, deleteFile = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

library(shiny)

# UI
ui <- fluidPage(
  titlePanel("Tic Tac Toe"),
  fluidRow(
    column(12, align = "center", 
           h3("Player 1: X | Player 2: O"),
           uiOutput("gameBoard"),
           actionButton("reset", "Reset Game"),
           h4(textOutput("status"))
    )
  )
)

# Server
server <- function(input, output, session) {
  # Initial game state
  game <- reactiveValues(
    board = matrix("", nrow = 3, ncol = 3),
    currentPlayer = "X",
    winner = NULL
  )
  
  # Render game board dynamically
  output$gameBoard <- renderUI({
    grid <- lapply(1:3, function(i) {
      fluidRow(
        lapply(1:3, function(j) {
          cell_id <- paste0("cell_", i, "_", j)
          actionButton(
            inputId = cell_id,
            label = game$board[i, j],
            style = "width: 60px; height: 60px; font-size: 20px; margin: 5px;"
          )
        })
      )
    })
    do.call(tagList, grid)
  })
  
  # Observe button clicks
  observe({
    for (i in 1:3) {
      for (j in 1:3) {
        local({
          row <- i
          col <- j
          cell_id <- paste0("cell_", row, "_", col)
          observeEvent(input[[cell_id]], {
            if (game$board[row, col] == "" && is.null(game$winner)) {
              game$board[row, col] <- game$currentPlayer
              game$currentPlayer <- ifelse(game$currentPlayer == "X", "O", "X")
              game$winner <- check_winner(game$board)
            }
          })
        })
      }
    }
  })
  
  # Reset game
  observeEvent(input$reset, {
    game$board <- matrix("", nrow = 3, ncol = 3)
    game$currentPlayer <- "X"
    game$winner <- NULL
  })
  
  # Display game status
  output$status <- renderText({
    if (!is.null(game$winner)) {
      if (game$winner == "Draw") {
        "It's a draw!"
      } else {
        paste("Winner:", game$winner)
      }
    } else {
      paste("Current Player:", game$currentPlayer)
    }
  })
  
  # Check winner function
  check_winner <- function(board) {
    winning_positions <- list(
      c(1, 1, 1, 2, 1, 3), c(2, 1, 2, 2, 2, 3), c(3, 1, 3, 2, 3, 3), # Rows
      c(1, 1, 2, 1, 3, 1), c(1, 2, 2, 2, 3, 2), c(1, 3, 2, 3, 3, 3), # Columns
      c(1, 1, 2, 2, 3, 3), c(1, 3, 2, 2, 3, 1)  # Diagonals
    )
    
    for (pos in winning_positions) {
      if (board[pos[1], pos[2]] != "" && 
          board[pos[1], pos[2]] == board[pos[3], pos[4]] && 
          board[pos[1], pos[2]] == board[pos[5], pos[6]]) {
        return(board[pos[1], pos[2]])
      }
    }
    
    if (all(board != "")) {
      return("Draw")
    }
    
    NULL
  }
}

# Run the app
shinyApp(ui = ui, server = server)

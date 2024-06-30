#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
################################################################################

library(shiny)
library(httr)
library(stringr)

# https://blog.devgenius.io/a-guide-to-creating-a-basic-r-shiny-app-from-a-github-readme-markdown-c10a3677113d

fetchReadme <- function() {
  url <- "https://raw.githubusercontent.com/slicesofdata/fods24/main/README.md"
  res <- GET(url)
  content <- content(res, "text")
  return(content)
}

#
#fetchReadme: A custom function defined to fetch the README file.
#It constructs a URL pointing to the raw README.md file on GitHub.
#GET(url) fetches the file, and content(res, "text") extracts its text content.

#3. Parsing Markdown Content
#parseMarkdown splits the markdown content into sections for easy display. Can we discuss the regex another day?

parseMarkdown <- function(mdContent) {
    sections <- unlist(str_split(mdContent, "\n## "))
    parsedSections <- lapply(sections[-1], function(section) {
      header <- str_extract(section, "^[^\n]+")
      bullets <- str_match_all(section, "- (.*)")[[1]][,2]
      bullets <- sapply(bullets, markdownToHTML, USE.NAMES = FALSE)
      list(header = header, bullets = bullets)
    })
    return(parsedSections)
  }

# Splits the markdown into sections based on headers (denoted by ##).
# For each section, it extracts the header and bullet points.
# Converts markdown links in bullet points to HTML using markdownToHTML.

# 4. Converting Markdown Links to HTML
# markdownToHTML converts markdown links to HTML format, making them clickable in the web app.

markdownToHTML <- function(text) {
  return(gsub("\\[([^]]+)\\]\\(([^)]+)\\)", "<a href='\\2' target='_blank'>\\1</a>", text))
}
# A helper function that transforms markdown link syntax to HTML hyperlinks.


# 5. User Interface Design
#The ui object uses fluidPage to create a flexible layout. It includes styling for tabs and a title panel.

# Defines the layout and style of the app’s user interface.
# Uses fluidPage for a flexible layout.
# The style within tags$style(HTML("...")) customizes the appearance of tabs.

# Define UI for application that ...

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
            .nav-tabs .nav-link {
                color: blue !important; /* Tab text color */
                font-weight: bold !important; /* Make tab titles bold */
            }
            .nav-tabs .nav-link.active {
                background-color: #5b5ba6 !important; /* Active tab color */
                border-color: #5b5ba6 !important; /* Border color for active tab */
            }
            .nav-tabs .nav-link:hover {
                background-color: #6d6dbf !important; /* Hover state color */
            }
        "))
  )
)


# 6. Server Function
# The server function handles the back-end operations of the app:

#  Fetching and Parsing Data: It uses the fetchReadme and parseMarkdown functions to get and process the GitHub markdown.
#Dynamic UI: Uses renderUI to create tabs dynamically for each section of the markdown file.


# The core logic of the Shiny app.
# markdownContent is a reactive value, updated with the latest README content.
# observe block fetches and parses the markdown file every 5 minutes.
# output$tabsUI dynamically creates tabs in the UI based on the parsed markdown.

# Define server logic required to ...

server <- function(input, output, session) {
  markdownContent <- reactiveVal(list())

  observe({
    content <- fetchReadme()
    parsedContent <- parseMarkdown(content)
    markdownContent(parsedContent)
    invalidateLater(300000, session)  # Refresh every 5 minutes
  })

  output$tabsUI <- renderUI({
    content <- markdownContent()
    if (is.null(content)) return(NULL)

    tabs <- lapply(content, function(section) {
      tabPanel(title = section$header,
               HTML(paste("<ul><li>", paste(section$bullets, collapse = "</li><li>"), "</li></ul>")))
    })

    do.call(navbarPage, c("Foundations of Data Science Resources", id = "nav", tabs))
  })
}

mutate(across(.cols = c(x1, x2),
              .fns = ~as.numeric(scale(.x)),
              .names = "{.col}_Z"
              )
       ) |>

# 7. Running the App
# The shinyApp(ui, server) call at the end of the script is what makes the app run.

# Run the application
shinyApp(ui = ui, server = server)

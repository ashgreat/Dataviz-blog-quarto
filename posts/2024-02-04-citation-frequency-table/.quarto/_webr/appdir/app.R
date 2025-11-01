library(shiny)
library(officer)
library(stringr)
library(bslib)
library(shinycssloaders)
library(DT)



















ui = page_sidebar(
  theme = bs_theme(bootswatch = "cosmo"),
  title = "",
  sidebar = sidebar(
    fileInput("mydoc", "Upload a Word document (.docx extension)"),
    actionButton("run", "Get Frequency")
  ),
  shinycssloaders::withSpinner(dataTableOutput("tab1"), type = 5)
)

server = function(input, output, session) {
  
  output$tab1 = renderDataTable(NULL)
  
  observeEvent(input$run, {
    output$tab1 = renderDataTable({
      
      pattern_updated = "\\(((?![^\\)]*accessed)[\\w\\s,.&;-]+? \\d{4}(?:;[\\w\\s,.&;-]+? \\d{4})*?)\\)"
      
      texts = read_docx(isolate(input$mydoc$datapath)) |> docx_summary()
      full_text = paste(texts$text, collapse = " ")
      citations_updated = str_extract_all(full_text, pattern_updated)[[1]]
      flat_citations = unlist(str_split(citations_updated, ";"))
      flat_citations = str_replace_all(flat_citations, "e.g.,", "") |> 
        trimws()
      Papers = str_replace_all(flat_citations, "\\(|\\)", "")
      citation_counts_updated = table(Papers)
      sorted_citations_updated = sort(citation_counts_updated, decreasing = TRUE) 
      
      freq_tab = data.frame(sorted_citations_updated)
      
      return(freq_tab)
      
    },
    caption = htmltools::tags$caption(style = 'caption-side: top; color:black;', 
                                      paste("Filename:", isolate(input$mydoc$name))),
    server = FALSE, # outputs entire table. Otherwise only the visible table.
    extensions = 'Buttons', 
    options = list(dom = 'Bfrtip',
                   buttons = list('copy',
                                  list(extend='csv', filename = gsub("\\.docx?\\b","", isolate(input$mydoc$name))),
                                  list(extend='excel', filename = gsub("\\.docx?\\b","", isolate(input$mydoc$name)))
                                  ))
    )# belongs to renderDataTable
    
  }) # belongs to observeEvent
  
}



shiny::shinyApp(ui, server)

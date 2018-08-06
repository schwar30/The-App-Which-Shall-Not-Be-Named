# Test Demo
# Random Code

library(shiny)

# Probably useless below

library(shinyjs)

library(shinydashboard)
library(dplyr)
library(stringr)
library(DT)
library(data.table)

# I don't think I use any relevant things from this either

library(ReporteRs)

# Both are used for API calls

library(RAdwords)
library(RGA)

# Below is likely unnecessary

library(googlesheets)

library(shinyWidgets)

# This is particularly for call files which tend to be rather large

options(shiny.maxRequestSize = 30 * 1024^2)

ui <- dashboardPage(
  
  dashboardHeader(title = "\nThe\nApp\nWhich\nShall\nNot\nBe\nNamed", titleWidth = 400),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      # There seems to be a huge dip in referral traffic for almost every site, so I 
      # wanted a way to quantify the losses
      
      menuItem("Referral Analytics", tabName = "referral", icon = icon("rebel")), 
      
      # Many campaigns see dips in performance despite the fact that clicks go up and 
      # spend goes up, but many times there's a push to disconinue brand keywords. This is 
      # meant to show paused keywords and how many conversions that were lost as a result of this.
      
      menuItem("Paused Keywords", tabName = "paused", icon = icon("empire"),
               menuSubItem("File Upload", tabName = "paused_upload"),
               menuSubItem("Filtered Paused", tabName = "list_keyword"),
               menuSubItem("Count of Paused Keywords", tabName = "count_keyword"),
               menuSubItem("Paused & Removed Keywords", tabName = "totals_keyword"),
               menuSubItem("Summaries", tabName = "summary_keyword")),
      
      # On occassion we will be asked about inquiry zipcode breakouts, which isn't very common, but it 
      # would take like half an hour or so to finalize, and this is just easier since its now generalized
      
      menuItem("Zipcode Breakout", tabName = "zipcode", icon = icon("magic")),
      
      # Allows for numbers from Dialog Tech to be found and exported very quickly and nicely
      
      menuItem("Dialog Numbers", tabName = "dialog", icon = icon("gamepad")),
      
      # For local reporting, we need to generate slides for VOC, and its a total pain to find,
      # so I just wanted it put in a place I know I won't lose it.
      
      menuItem("VOC Slides", tabName = "voc", icon = icon("bomb")),
      
      # Currently a misnomer, since generally speaking all of the bot traffic is actually through 
      # direct traffic, not organic, but it doesn't really matter since everything is done the same
      # way.
      
      menuItem("Organic Bounce Rates", tabName = "bounce", icon = icon("bolt")),
      
      # More of a testing thing than anything else. Playing around with some of the API stuff is probably
      # a good idea for here and beyond just incase something breaks or a new call is needed. The stuff I
      # struggle with right now is unfortunately the stuff I'll never have to deal with in the near future, 
      # but I think it's still worth knowing.
      
      menuItem("API Call", tabName = "api", icon = icon("ban")), 
      
      # More of a basic reporting kind of thing that Stullzy wants to lay out. It's not difficult, but the problem
      # is I don't explicitly know everything to filter, but I think it's pretty decent.
      
      menuItem("Paid Search", tabName = "paid_search", icon = icon("cog")),
      
      # A hot mess of garbage. It's not that I couldn't get it to work, but that it's going to be ineffective because of 
      # the limitations of Google's API. Every cell is considered a request, and only 500 requests are available every 100
      # seconds. Ultimately, unless it's a small data set, its just more worth to do it yourself unless you're of the mindset
      # that this or multiple sheets need to be created overnight or something. Either way, not too worth allocating a ton of
      # time into. Probably would work well for my strongman app though.
      
      menuItem("Google Sheets", tabName = "gsheet", icon = icon("exclamation"),
               menuSubItem("New Google Sheet", tabName = "new_gsheet"),
               menuSubItem("Edit Google Sheet", tabName = "edit_gsheet"),
               menuSubItem("Remove Google Sheet", tabName = "remove_gsheet"))
      
    ) 
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "referral",
              
              titlePanel("Referral Analytics"),
              
              # While an API call would be more effective for this, I don't know how to set it up yet. All that's 
              # really important is that a few lines are skipped and that we can identify the rows based off a certain
              # year, which is currently 2018 but can be changed. Otherwise, this will just show the differences in 
              # referral sessions year over year totals, and by site.
              
              sidebarPanel(
                fileInput(inputId = "analytics_file", label = "Upload Analytics Data:"),
                numericInput(inputId = "analytics_lines", label = "Number of Lines to Skip:", value = ""),
                textInput(inputId = "analytics_year", label = "Current Year:", value = "2018"),
                actionButton(inputId = "analytics_update", label = "Get Referral Differences", icon = icon("empire")),
                actionButton(inputId = "analytics_count", label = "Get Referral Counts", icon = icon("rebel"))
              ),
              
              tableOutput(outputId = "analytics_sum"),
              dataTableOutput(outputId = "analytic_table")
              
      ),
      
      tabItem(tabName = "dialog",
              
              titlePanel("Call App"),
              
              fluidRow(
                
                # Needs a file from dialogTech but will then need a phone number and will allow for 
                # counts of that number and an export of all calls with that number.
                
                column(4,
                       fileInput(inputId = "dialog_file", label = "Input Call File"),
                       actionButton(inputId = "dialog_total", label = "Click for Count"),
                       downloadButton(outputId = "dialog_export", label = "Download Cleaned numbers", icon = icon("empire")),
                       tableOutput("dialog_count")
                ),
                
                column(8,
                       textInput(inputId = "dialog_number", label = "Desired Phone Number")
                       
                ))),
      
      tabItem(tabName = "zipcode",
              
              titlePanel("Zip Code Breakout"),
              
              sidebarLayout(
                
                # Just needs a lead export and allows an initial view and a named download
                
                sidebarPanel(
                  textInput(inputId = "zipcode_coop", label = "Name of Co-ops / Groups / Dealers:"),
                  fileInput(inputId = "zipcode_lead", label = "Please Upload Desired .csv lead Export(s):", multiple = T),
                  actionButton(inputId = "zipcode_list", label = "List Zipcode Breakout"),
                  downloadButton(outputId = "zipcode_download", label = "Download Zipcode Breakout")
                ),
                
                column(4, tableOutput("zipcode_table"))
              )),
      
      tabItem(tabName = "paused_upload",
              
              # When I originally built the paused keyword stuff, I was playing around with other page setups with 
              # shinyjs, so I kept this as a multitab, and since there are so many buttons and outputs, its probably
              # a decent idea. This part is simply a file upload.
              
              sidebarPanel(
                fileInput(inputId = "paused_file", label = "Please Input Desired .csv Keyword File")
              )),
      
      tabItem(tabName = "list_keyword",
              
              # This component just lists all of the keywords that are paused and can be all of them, just the brand, 
              # or just the nonbrand. Also allows for downloads of these keywords, though the use of this is probably
              # very limited
              
              sidebarPanel(
                actionButton(inputId = "paused_filter", label = "View Paused Keywords"),
                actionButton(inputId = "paused_filter_brand", label = "View Paused Brand Keywords"),
                actionButton(inputId = "paused_filter_nonbrand", label = "View Paused\nNonBrand Keywords"),
                downloadButton(outputId = "paused_download_all", label = "All Paused Keywords"),
                downloadButton(outputId = "paused_download_brand", label = "Paused Brand Keywords"),
                downloadButton(outputId = "paused_download_nonbrand", label = "Paused Nonbrand Keywords")
              ),
              
              fluidRow(
                tableOutput("paused_filter_table")
              )),
      
      tabItem(tabName = "count_keyword",
              
              # This provides a count of how many brand keywords are paused, nonbrand keywords, or total paused keywords
              # there are.
              
              sidebarPanel(
                
                actionButton(inputId = "paused_count_brand", label = "Brand Keyword Count"),
                actionButton(inputId = "paused_count_nonbrand", label = "Nonbrand Keyword Count"),
                actionButton(inputId = "paused_count_all", label = "All Keyword Count")
              ),
              
              
              fluidRow(
                tableOutput(outputId = "paused_count_table")
              )),
      
      tabItem(tabName = "totals_keyword",
              
              # Shows how many lost conversions there are by keyword. Allows for the downloads too.
              
              sidebarPanel(
                
                actionButton(inputId = "paused_all", label = "View Inactive"),
                actionButton(inputId = "paused_brand", label = "View Brand Inactive"),
                actionButton(inputId = "paused_nonbrand", label = "View Nonbrand Inactive"),
                downloadButton(outputId = "paused_all_download", label = "Download Inactive"),
                downloadButton(outputId = "paused_brand_download", label = "Download Brand Inactive"),
                downloadButton(outputId = "paused_nonbrand_download", label = "Download Nonbrand Inactive")
              ),
              
              tableOutput("paused_all_table")
              
      ),
      
      # Shows total loss of conversions for brand, nonbrand, and total groups.
      
      tabItem(tabName = "summary_keyword",
              
              sidebarPanel(
                actionButton(inputId = "paused_all_total", label = "Inactive Summary"),
                actionButton(inputId = "paused_brand_total", label = "Brand Inactive Summary"),
                actionButton(inputId = "paused_nonbrand_total", label = "Nonbrand Inactive Summary")
                
              ),
              tableOutput("paused_all_total_table")
      ),
      
      # Allows a merged file to be input and then exports a lot of VOC slides in the reporting folder
      
      tabItem(tabName = "voc",
              
              titlePanel("VOC Slides"),
              
              sidebarPanel(
                
                fileInput(inputId = "voc_file", label = "Upload Merged VOC file:"),
                "*You must merge the file prior to uploading the file\n\n",
                textInput(inputId = "voc_text", label = "Reporting Period:"), 
                actionButton(inputId = "voc_generate", label = "Generate VOC Slides")
                
              ),
              
              verbatimTextOutput("voc_instruct")
              
              # I thought putting it right in the shiny app would be sufficient, 
              # and it may have been, but the text output really is what makes it look 
              # ok. This stuff is just thrown into the server later on.
              
              # "The column names need to include the following:
              # \nTotal.views
              # \nPhone.call.actions
              # \nDirections.actions
              # \nWebsite.actions
              # \nAverage.Star.Rating
              # \nNumber.of.Reviews
              # \n\nDo NOT include citySt"
              
              ),
      
      tabItem(tabName = "bounce",
              
              titlePanel("Direct / Organic Bounce Rates"),
              
              # Certain lines need to be skipped so the file can be read, and they're not all the same.
              # Also needs the current year to separate values.
              
              sidebarPanel(
                
                fileInput(inputId = "bounce_file", label = "Upload Organic Traffic with Service Provider:"),
                numericInput(inputId = "bounce_lines", label = "Number of Lines Desired to Skip:", value = ""),
                numericInput(inputId = "bounce_year", label = "Current Year:", value = "2018"),
                actionButton(inputId = "bounce_update", label = "View Bounce Rates")
                
              ), 
              
              dataTableOutput(outputId = "bounce_table")
              
              ),
      
      # I really want to try with this bit so I can see if I can make my own API calls, but right now, im failing to
      # understand what on earth is happening. I understand the basic structure of the function to make the call, but 
      # the authorize_adwords bit doesn't make any sense at all. I tried pulling from the original source, but it doesn't
      # seem to be working the way I have it scripted.
      
      tabItem(tabName = "api",
              
              titlePanel("Test API Call"),
              
              sidebarPanel(
                
                dateInput(inputId = "first_date", label = "First Date"),
                dateInput(inputId = "last_date", label = "Last Date"),
                actionButton(inputId = "api_call", label = "API Call")
                
              ),
              
              textOutput("api_text")
              
              ),
      
      tabItem(tabName = "paid_search",
              
              # Just a leads export and a download button
              
              titlePanel("Paid Search"),
              
              sidebarPanel(
                
                fileInput(inputId = "paid_search_file", label = "Input export file"),
                downloadButton(outputId = "paid_search_download")
                
              )),
      
      # All of this stuff is still in "development", which generally means its not going to get touched
      # unless I really want to, which I'm not sure if I do since its only helpful in limited scenarios.
      
      tabItem(tabName = "new_gsheet",
              
              titlePanel("New Google Sheet"),
              
              sidebarPanel(
                
                fileInput(inputId = "gsheet_file", label = "Data Wanted to be Put in A Google Sheet:"),
                textInput(inputId = "gsheet_name", label = "Name of New Google Sheet:"),
                textInput(inputId = "worksheet_first_name", label = "Worksheet Name:"),
                actionBttn(inputId = "gsheet_create", label = "Create Sheet")
                
              )),
      
      tabItem(tabName = "edit_gsheet",
              
              titlePanel("Edit Google Sheet"),
              
              sidebarPanel(
                
                actionBttn(inputId = "update_gsheet", label = "Edit Sheet")
                
              )),
      
      tabItem(tabName = "remove_gsheet",
              
              titlePanel("Remove Google Sheet"),
              
              sidebarPanel(
                
                actionBttn(inputId = "delete_gsheet", label = "Deleet Sheet")
                
              ))
      
    )))

server <- function(input, output, session) {
  
  observeEvent(input$analytics_update, {
    
    output$analytic_table <- renderDataTable({
      
      # This is alwways helpful so it doesn't error out when nothing is included, and allows everything
      # to be isolated so it doesn't fire only one time.
      
      file_to_read <- isolate(input$analytics_file)
      
      if(is.null(file_to_read)){
        return()
      }
      
      # Allows the lines to be skipped. Both analytics and adwords have weird exports, so I usually
      # throw the UTF-8 tag on the read.csv
      
      data_table <- read.csv(file_to_read$datapath, skip = input$analytics_lines, encoding = "UTF-8")
      
      # This needs to be all separated so they can be joined and then mutated
      
      data_table <- data_table %>% 
        select(Source, Date.Range, Sessions)
      
      analytics_2018 <- data_table %>% 
        filter(str_detect(Date.Range, input$analytics_year)) %>% 
        filter(Sessions != "")
      
      analytics_2017 <- data_table %>% 
        filter(!str_detect(Date.Range, input$analytics_year)) %>% 
        filter(Sessions != "")
      
      # Joins the date tables and selects only the relevant metrics and then creates a difference and percentage change column. 
      # I also thought it was worth while to only show referral traffic that saw a change.
      
      combined_table <- left_join(analytics_2017, analytics_2018, by = "Source")
      combined_table <- combined_table %>% 
        select(Source, Sessions.x, Sessions.y) %>% 
        rename("Website" = Source, "prior_year" = Sessions.x, "current_year" = Sessions.y) %>% 
        filter(Website != "") %>% 
        mutate(Difference = current_year - prior_year, percent_change = paste0(round(Difference / prior_year * 100, 2), "%")) %>% 
        arrange(Difference) %>% 
        select(Website, prior_year, current_year, Difference, percent_change) %>% 
        rename("Change (%)" = percent_change, "Prior Year" = prior_year, "Current Year" = current_year) %>% 
        filter(Difference < 0)
      
      # datatable is used here, but honestly, its not that necessary. I thought it might be nice for identifying culligan.com specifically,
      # but its almost always in the top 10 (if there's a severe referral drop), so its not helpful. Nice to know for later though.
      
      datatable(combined_table)
      
    })
  })
  
  observeEvent(input$analytics_count, {
    
    output$analytics_sum <- renderTable({
      
      file_to_read <- isolate(input$analytics_file)
      
      if(is.null(file_to_read)){
        return()
      }
      
      data_table <- read.csv(file_to_read$datapath, skip = input$analytics_lines, encoding = "UTF-8")
      data_table <- data_table %>% 
        select(Source, Date.Range, Sessions)
      
      analytics_2018 <- data_table %>% 
        filter(str_detect(Date.Range, input$analytics_year)) %>% 
        filter(Sessions != "")
      
      analytics_2017 <- data_table %>% 
        filter(!str_detect(Date.Range, input$analytics_year)) %>% 
        filter(Sessions != "")
      
      # Everything is pretty much the same as before, but the focus is to get the sums instead of everything else
      
      combined_table <- left_join(analytics_2017, analytics_2018, by = "Source")
      combined_table <- combined_table %>% 
        select(Source, Sessions.x, Sessions.y) %>% 
        rename("Website" = Source, "prior_year" = Sessions.x, "current_year" = Sessions.y) %>% 
        filter(Website != "") %>% 
        mutate(Difference = current_year - prior_year, percent_change = paste0(round(Difference / prior_year * 100, 2), "%")) %>% 
        arrange(Difference) %>% 
        # select(Website, Difference) %>% 
        summarise("prior" = sum(prior_year), "current" = sum(current_year)) %>% 
        mutate("Difference" = current - prior) %>% 
        rename("Prior Year" = prior, "Current Year" = current)
      
      # This part is only important for letting you know the scope of the drop.
      
      combined_table
      
    })
    
  })
  
  observeEvent(input$zipcode_list, {
    
    output$zipcode_table <- renderTable({
      
      input$zipcode_list
      
      file_to_read <- isolate(input$zipcode_lead)
      if(is.null(file_to_read)) {
        return()
      }
      
      # The reason this is used instead of read.csv is the file input allows for
      # multiple files. It's kind of a nice thing when its a weird combination of dealers
      # that want aggregated data

      lead_export <- rbindlist(lapply(file_to_read$datapath, fread), 
                               use.names = T, fill = T)
      
      # Customers are able to put in their own zipcodes, and this is what we really use for zipcode
      # placement, so a new column is made to overwrite postal code if its available. Then all inquiries
      # are grouped by zipcode and counted.
      
      lead_export <- lead_export %>% 
        select(SentTo, PostalCode, IVRZip) %>% 
        mutate(Zipcode = ifelse(is.na(IVRZip), PostalCode, IVRZip)) %>% 
        group_by(Zipcode) %>% 
        count(Zipcode) %>% 
        filter(Zipcode != "") %>% 
        arrange(-n) %>% 
        rename("Inquiries" = n)
      
      lead_export
    })
    
  })
  
  download_setup <- reactive({
    
    file_to_read <- isolate(input$zipcode_lead)
    if(is.null(file_to_read)) {
      return()
    }
    
    # All of this code is the exact code for what is shown in the browser, but
    # it needs to go here so everything can be setup to be downloaded.

    lead_export <- rbindlist(lapply(file_to_read$datapath, fread), 
                             use.names = T, fill = T)
    
    lead_export <- lead_export %>% 
      select(SentTo, PostalCode, IVRZip) %>% 
      mutate(Zipcode = ifelse(is.na(IVRZip), PostalCode, IVRZip)) %>% 
      group_by(Zipcode) %>% 
      count(Zipcode) %>% 
      filter(Zipcode != "") %>% 
      arrange(-n) %>% 
      rename("Inquiries" = n)
    
    lead_export
    
  })
  
  output$zipcode_download <- downloadHandler(
    
    # Not too difficult, allows the breakout file to be named if there is an entry, and skips it if it does 
    # not, and then downloads everything.
    
    filename = function() {
      paste0(ifelse(input$zipcode_coop == "", "", paste0(input$zipcode_coop, " ")), "Zip Code Breakout.csv")
    },
    
    content = function(file) {
      write.csv(download_setup(), file, row.names = F)
    }
  )
  
  observeEvent(input$dialog_total, {
    
    output$dialog_count <- renderTable({
      
      # Again, this file to read stuff is nice for preventing errors.
      
      isolate(input$dialog_total)
      
      file_to_read <- isolate(input$dialog_file)
      
      if(is.null(file_to_read)) {
        return()
      }
      
      # Changes number from numeric to character. I don't know why I didn't have this
      # as a textinput to be completely honest. Just counts calls.
      
      phone_export <- read.csv(isolate(file_to_read$datapath))
      
      phone_export$dnis <- as.character(phone_export$dnis)

      phone_export <- phone_export %>% 
        filter(str_detect(dnis, isolate(input$dialog_number))) %>% 
        group_by(dnis) %>% 
        count(dnis) %>% 
        rename("Number" = dnis) %>% 
        rename("Count" = n)
      
      phone_export
      
    })
    
  })
  
  download_setup <- reactive({
    
    file_to_read <- isolate(input$dialog_file)
    
    if(is.null(file_to_read)) {
      return()
    }
    
    # More setup. What this does is select the rows that include the desired number
    
    phone_export <- read.csv(file_to_read$datapath)
    
    phone_export$dnis <- as.character(phone_export$dnis)
    
    phone_export <- phone_export %>% 
      filter(str_detect(dnis, input$dialog_number)) 
    
    phone_export
  })
  
  output$dialog_export <- downloadHandler(
    
    # Exports the call setup
    
    filename = "Cleaned Numbers.csv",
    
    content = function(file) {
      write.csv(download_setup(), file, row.names = F)
    }
    
  )
  
  observeEvent(input$paused_all, {
    
    output$paused_all_table <- renderTable({
      
      file_to_read <- isolate(input$paused_file)
      
      if(is.null(file_to_read)){
        return()
      }
      
      data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
      
      # Shows the loss of conversions of all keywords 
      
      data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
      data_table$Keyword <- str_to_lower(data_table$Keyword)
      data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
      data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
      data_table <- data_table %>% 
        filter(Keyword.status != "Enabled") %>% 
        select(Keyword, Conversions..Change.) %>% 
        filter(Keyword != "") %>% 
        group_by(Keyword) %>% 
        summarise("conversions_change" = sum(Conversions..Change.)) %>% 
        arrange(conversions_change) %>% 
        filter(conversions_change != 0) %>% 
        rename("Conversions Change" = conversions_change)
      data_table
      
    })
  })
  
  all_setup <- reactive({
    
    file_to_read <- isolate(input$paused_file)
    
    if(is.null(file_to_read)){
      return()
    }
    
    data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
    
    data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
    data_table$Keyword <- str_to_lower(data_table$Keyword)
    data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
    data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
    data_table <- data_table %>% 
      filter(Keyword.status != "Enabled") %>% 
      select(Keyword, Conversions..Change.) %>% 
      filter(Keyword != "") %>% 
      group_by(Keyword) %>% 
      summarise("conversions_change" = sum(Conversions..Change.)) %>% 
      arrange(conversions_change) %>% 
      filter(conversions_change != 0) %>%
      rename("Conversions Change" = conversions_change)
    data_table
    
  })
  
  output$paused_all_download <- downloadHandler(
    
    filename = "All Removed And Paused Keywords.csv",
    
    content = function(file) {
      write.csv(all_setup(), file, row.names = F)
    }
  )
  
  observeEvent(input$paused_brand, {
    
    output$paused_all_table <- renderTable({
      
      file_to_read <- isolate(input$paused_file)
      
      if(is.null(file_to_read)){
        return()
      }
      
      data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
      
      data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
      data_table$Keyword <- str_to_lower(data_table$Keyword)
      data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
      data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
      data_table <- data_table %>% 
        filter(Keyword.status != "Enabled") %>% 
        select(Keyword, Conversions..Change.) %>% 
        filter(Keyword != "") %>% 
        group_by(Keyword) %>% 
        summarise("conversions_change" = sum(Conversions..Change.)) %>% 
        arrange(conversions_change) %>% 
        filter(conversions_change != 0) %>% 
        filter(str_detect(Keyword, "culligan")) %>% 
        rename("Conversions Change" = conversions_change)
      data_table
      
    })
    
  })
  
  brand_setup <- reactive({
    
    file_to_read <- isolate(input$paused_file)
    
    if(is.null(file_to_read)){
      return()
    }
    
    data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
    
    data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
    data_table$Keyword <- str_to_lower(data_table$Keyword)
    data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
    data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
    data_table <- data_table %>% 
      filter(Keyword.status != "Enabled") %>% 
      select(Keyword, Conversions..Change.) %>% 
      filter(Keyword != "") %>% 
      group_by(Keyword) %>% 
      summarise("conversions_change" = sum(Conversions..Change.)) %>% 
      arrange(conversions_change) %>% 
      filter(conversions_change != 0) %>%
      filter(str_detect(Keyword, "culligan")) %>% 
      rename("Conversions Change" = conversions_change)
    data_table
    
  })
  
  output$paused_brand_download <- downloadHandler(
    
    filename = "All Removed And Paused Brand Keywords.csv",
    
    content = function(file) {
      write.csv(brand_setup(), file, row.names = F)
    }
    
  )
  
  observeEvent(input$paused_nonbrand, {
    
    output$paused_all_table <- renderTable({
      
      file_to_read <- isolate(input$paused_file)
      
      if(is.null(file_to_read)){
        return()
      }
      
      data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
      
      data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
      data_table$Keyword <- str_to_lower(data_table$Keyword)
      data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
      data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
      data_table <- data_table %>% 
        filter(Keyword.status != "Enabled") %>% 
        select(Keyword, Conversions..Change.) %>% 
        filter(Keyword != "") %>% 
        group_by(Keyword) %>% 
        summarise("conversions_change" = sum(Conversions..Change.)) %>% 
        arrange(conversions_change) %>% 
        filter(conversions_change != 0) %>% 
        filter(!str_detect(Keyword, "culligan")) %>% 
        rename("Conversions Change" = conversions_change)
      data_table
    })
    
  })
  
  nonbrand_setup <- reactive({
    
    file_to_read <- isolate(input$paused_file)
    
    if(is.null(file_to_read)){
      return()
    }
    
    data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
    
    data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
    data_table$Keyword <- str_to_lower(data_table$Keyword)
    data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
    data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
    data_table <- data_table %>% 
      filter(Keyword.status != "Enabled") %>% 
      select(Keyword, Conversions..Change.) %>% 
      filter(Keyword != "") %>% 
      group_by(Keyword) %>% 
      summarise("conversions_change" = sum(Conversions..Change.)) %>% 
      arrange(conversions_change) %>% 
      filter(conversions_change != 0) %>% 
      filter(!str_detect(Keyword, "culligan")) %>% 
      rename("Conversions Change" = conversions_change)
    data_table
    
  })
  
  output$paused_nonbrand_download <- downloadHandler(
    
    filename = "All Removed And Paused Nonbrand Keywords.csv",
    
    content = function(file) {
      write.csv(nonbrand_setup(), file, row.names = F)
    }
    
  )
  
  # This is the only other distinct stuff and is the main reason I even built this out. 
  # pretty much, it lets you know if conversions are up or down YOY, but with a little more interest,
  # it allows you to see if brand or nonbrand keywords saw the greater hit. It's nice for when keywords
  # are paused or removed (specifically from brand which this tends to happen) and it is easy to say, 
  # "yup, conversions are down, but that's because we paused brand keywords and brand keywords are down
  # [x] amount.
  
  observeEvent(input$paused_all_total, {
    
    output$paused_all_total_table <- renderTable({
      
      file_to_read <- isolate(input$paused_file)
      
      if(is.null(file_to_read)){
        return()
      }
      
      data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
      
      data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
      data_table$Keyword <- str_to_lower(data_table$Keyword)
      data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
      data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
      
      data_table <- data_table %>% 
        filter(Keyword.status != "Enabled") %>% 
        select(Keyword, Conversions..Change.) %>% 
        filter(Keyword != "") %>% 
        group_by(Keyword) %>% 
        summarise("conversions_change" = sum(Conversions..Change.)) %>% 
        arrange(conversions_change) %>% 
        filter(conversions_change != 0) 
      
      data_table <- data_table %>% 
        summarise("All Paused & Removed Total" = sum(conversions_change))
      
    })
  })
  
  observeEvent(input$paused_brand_total, {
    
    output$paused_all_total_table <- renderTable({
      
      file_to_read <- isolate(input$paused_file)
      
      if(is.null(file_to_read)){
        return()
      }
      
      data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
      
      data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
      data_table$Keyword <- str_to_lower(data_table$Keyword)
      data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
      data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
      data_table <- data_table %>% 
        filter(Keyword.status != "Enabled") %>% 
        select(Keyword, Conversions..Change.) %>% 
        filter(Keyword != "") %>% 
        group_by(Keyword) %>% 
        summarise("conversions_change" = sum(Conversions..Change.)) %>% 
        arrange(conversions_change) %>% 
        filter(conversions_change != 0) %>% 
        filter(str_detect(Keyword, "culligan")) 
      
      data_table <- data_table %>% 
        summarise("All Paused & Removed Brand Total" = sum(conversions_change))
      
    })
    
  })
  
  observeEvent(input$paused_nonbrand_total, {
    
    output$paused_all_total_table <- renderTable({
      
      file_to_read <- isolate(input$paused_file)
      
      if(is.null(file_to_read)){
        return()
      }
      
      data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
      
      data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
      data_table$Keyword <- str_to_lower(data_table$Keyword)
      data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
      data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
      data_table <- data_table %>% 
        filter(Keyword.status != "Enabled") %>% 
        select(Keyword, Conversions..Change.) %>% 
        filter(Keyword != "") %>% 
        group_by(Keyword) %>% 
        summarise("conversions_change" = sum(Conversions..Change.)) %>% 
        arrange(conversions_change) %>% 
        filter(conversions_change != 0) %>% 
        filter(!str_detect(Keyword, "culligan")) 
      
      data_table <- data_table %>% 
        summarise("All Paused & Removed Nonbrand Total" = sum(conversions_change))
      
    })
    
  })
  
  observeEvent(input$paused_filter, {
    
    output$paused_filter_table <- renderTable({
      
      file_to_read <- isolate(input$paused_file)
      
      if(is.null(file_to_read)){
        return()
      }
      
      data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
      
      # browser()
      
      data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
      data_table$Keyword <- str_to_lower(data_table$Keyword)
      data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
      data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
      data_table <- data_table %>% 
        filter(Keyword.status != "Enabled") %>% 
        select(Keyword) %>% 
        filter(Keyword != "") %>% 
        distinct() %>% 
        arrange(Keyword)
      
    })
  })
  
  observeEvent(input$paused_filter_brand, {
    
    output$paused_filter_table <- renderTable({
      
      file_to_read <- isolate(input$paused_file)
      
      if(is.null(file_to_read)){
        return()
      }
      
      data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
      
      data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
      data_table$Keyword <- str_to_lower(data_table$Keyword)
      data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
      data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
      data_table <- data_table %>% 
        filter(Keyword.status != "Enabled") %>% 
        select(Keyword) %>% 
        filter(Keyword != "") %>% 
        filter(str_detect(Keyword, "culligan")) %>% 
        distinct() %>% 
        arrange(Keyword)
      
    })
  })
  
  observeEvent(input$paused_filter_nonbrand, {
    
    output$paused_filter_table <- renderTable({
      
      file_to_read <- isolate(input$paused_file)
      
      if(is.null(file_to_read)){
        return()
      }
      
      data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
      
      data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
      data_table$Keyword <- str_to_lower(data_table$Keyword)
      data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
      data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
      data_table <- data_table %>% 
        filter(Keyword.status != "Enabled") %>% 
        select(Keyword) %>% 
        filter(Keyword != "") %>% 
        filter(!str_detect(Keyword, "culligan")) %>% 
        distinct() %>% 
        arrange(Keyword)
      
    })
  })
  
  paused_download_setup <- reactive({
    
    file_to_read <- isolate(input$paused_file)
    
    if(is.null(file_to_read)){
      return()
    }
    
    data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
    
    # browser()
    
    data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
    data_table$Keyword <- str_to_lower(data_table$Keyword)
    data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
    data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
    data_table <- data_table %>% 
      filter(Keyword.status != "Enabled") %>% 
      select(Keyword) %>% 
      filter(Keyword != "") %>% 
      distinct() %>% 
      arrange(Keyword)
    
    data_table
    
  })
  
  
  output$paused_download_all <- downloadHandler(
    
    filename = "All Paused Keywords.csv",
    
    content = function(file) {
      write.csv(paused_download_setup(), file, row.names = F)
    }
    
  )
  
  paused_brand_setup <- reactive({
    
    file_to_read <- isolate(input$paused_file)
    
    if(is.null(file_to_read)){
      return()
    }
    
    data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
    
    data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
    data_table$Keyword <- str_to_lower(data_table$Keyword)
    data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
    data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
    data_table <- data_table %>% 
      filter(Keyword.status != "Enabled") %>% 
      select(Keyword) %>% 
      filter(Keyword != "") %>% 
      filter(str_detect(Keyword, "culligan")) %>% 
      distinct() %>% 
      arrange(Keyword)
    data_table
    
  })
  
  output$paused_download_brand <- downloadHandler(
    
    filename = "All Paused Brand Keywords.csv",
    
    content = function(file) {
      write.csv(paused_brand_setup(), file, row.names = F)
    }
    
  )
  
  paused_nonbrand_setup <- reactive({
    
    file_to_read <- isolate(input$paused_file)
    
    if(is.null(file_to_read)){
      return()
    }
    
    data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
    
    data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
    data_table$Keyword <- str_to_lower(data_table$Keyword)
    data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
    data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
    data_table <- data_table %>% 
      filter(Keyword.status != "Enabled") %>% 
      select(Keyword) %>% 
      filter(Keyword != "") %>% 
      filter(!str_detect(Keyword, "culligan")) %>% 
      distinct() %>% 
      arrange(Keyword)
    data_table
    
  })
  
  output$paused_download_nonbrand <- downloadHandler(
    
    filename = "All Paused Nonbrand Keywords.csv",
    
    content = function(file) {
      write.csv(paused_nonbrand_setup(), file, row.names = F)
    }
    
  )
  
  observeEvent(input$paused_count_brand, {
    
    output$paused_count_table <- renderTable({
      
      file_to_read <- isolate(input$paused_file)
      
      if(is.null(file_to_read)){
        return()
      }
      
      data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
      data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
      data_table$Keyword <- str_to_lower(data_table$Keyword)
      data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
      data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
      data_table <- data_table %>% 
        filter(Keyword.status != "Enabled") %>% 
        select(Keyword) %>% 
        filter(Keyword != "") %>% 
        filter(str_detect(Keyword, "culligan")) %>% 
        distinct() %>% 
        arrange(Keyword) %>% 
        count() %>% 
        rename("Number of Brand Keywords" = n)
      data_table
    })
  })
  
  observeEvent(input$paused_count_nonbrand, {
    
    output$paused_count_table <- renderTable({
      
      file_to_read <- isolate(input$paused_file)
      
      if(is.null(file_to_read)){
        return()
      }
      
      data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
      data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
      data_table$Keyword <- str_to_lower(data_table$Keyword)
      data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
      data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
      data_table <- data_table %>% 
        filter(Keyword.status != "Enabled") %>% 
        select(Keyword) %>% 
        filter(Keyword != "") %>% 
        filter(!str_detect(Keyword, "culligan")) %>% 
        distinct() %>% 
        arrange(Keyword) %>% 
        count() %>% 
        rename("Number of Nonbrand Keywords" = n)
      data_table
    })
  })
  
  observeEvent(input$paused_count_all, {
    
    output$paused_count_table <- renderTable({
      
      file_to_read <- isolate(input$paused_file)
      
      if(is.null(file_to_read)){
        return()
      }
      
      data_table <- read.csv(file_to_read$datapath, skip = 2, encoding = "UTF-8")
      data_table$Keyword <- gsub("[[:punct:]]", "", data_table$Keyword)
      data_table$Keyword <- str_to_lower(data_table$Keyword)
      data_table$Conversions..Change. <- gsub("\\,", "", data_table$Conversions..Change.)
      data_table$Conversions..Change. <- as.integer(as.character(data_table$Conversions..Change.))
      data_table <- data_table %>% 
        filter(Keyword.status != "Enabled") %>% 
        select(Keyword) %>% 
        filter(Keyword != "") %>% 
        # filter(str_detect(Keyword, "culligan")) %>% 
        distinct() %>% 
        arrange(Keyword) %>% 
        count() %>% 
        rename("Number of Paused Keywords" = n)
      data_table
    })
  })
  
  observeEvent(input$voc_generate, {
    
    # browser()
    
    file_to_read <- isolate(input$voc_file)

    if(is.null(file_to_read)){
      return()
    }

    dat <- read.csv(file_to_read$datapath)
    
    dat <- dat %>% 
      mutate(citySt = paste0(Location.City, " ", Location.State))
    
    # dat = read.csv("~/Desktop/GMB and Review Tracker3.csv")
    
    # This little bit needs to go in because otherwise the system crashes and nothing is made.
    # I would put an exists function in here, but there shouldn't be a time where the button is  
    # clicked and the file already exists 
    
    dir.create(paste0("/Volumes/Front/Culligan/Local Website Reporting/VOC ", input$voc_text))
    
    # I have to figure out how to make this work so that a button can be clicked and that's the end of it.
    # What sucks about this is you have to know what to explicity name each variable. I think if everything stays
    # the same, its only the citySt column that needs to change, so there is a quick mutate above.
    
    dat=as.data.frame(lapply(dat, as.character), stringsAsFactors=F)
    for(i in 1:nrow(dat)){
      mydoc = pptx(template ="/Volumes/Front/Adam/Reporting/Co-op_template.pptx")
      mydoc=addSlide(mydoc,slide.layout = "VOC Title")
      mydoc=addSlide(mydoc,slide.layout = "VOC Info")
      mydoc = addParagraph(mydoc,dat$Total.views[i])
      mydoc = addParagraph(mydoc,dat$Phone.call.actions[i])
      mydoc = addParagraph(mydoc,dat$Directions.actions[i])
      mydoc = addParagraph(mydoc,dat$Website.actions[i])
      mydoc = addParagraph(mydoc,dat$Average.Star.Rating[i])
      mydoc = addParagraph(mydoc,dat$Number.of.Reviews[i])
      mydoc = addParagraph(mydoc,dat$citySt[i])
      file = paste0("/Volumes/Front/Culligan/Local Website Reporting/VOC ",input$voc_text,"/VOC - Culligan of ", dat$citySt[i], ".pptx")
      writeDoc(mydoc, file)
    }
    
  })
  
  # I decided this is really the only way to really have a way for this to work in shiny. These are the only columns that really matter 
  # (in terms of our VOC slides), but should be descriptive enough. In any event, not much of the original code is changed (obviously), 
  # so at least it wont' get lost.
  
  output$voc_instruct <- renderText({
    
    paste("The column names need to include the following:", "",
    "Total.views","Phone.call.actions", "Directions.actions",
    "Website.actions", "Average.Star.Rating","Number.of.Reviews",
    "Location.City", "Location.State", "",
    "", "Do NOT include citySt", sep = "\n")
    
    # paste("hello", "world", sep="\n")
    
  })
  
  observeEvent(input$bounce_update, {
    
    output$bounce_table <- renderDataTable({
      
      file_to_read <- isolate(input$bounce_file)
      
      if(is.null(file_to_read)){
        return()
      }
      
      data_table <- read.csv(file_to_read$datapath, skip = input$bounce_lines)
      data_table <- data_table %>% 
        select(Service.Provider, Date.Range, Sessions, Bounce.Rate) %>% 
        filter(Service.Provider != "(not set)")
      
      data_table$Bounce.Rate <- gsub("\\%", "", data_table$Bounce.Rate)
      data_table$Bounce.Rate <- as.numeric(data_table$Bounce.Rate)
      data_table$Sessions <- as.integer(data_table$Sessions)
      
      # browser()
      
      data_table_2017 <- data_table %>% 
        filter(!str_detect(Date.Range, as.character(input$bounce_year)))
      
      data_table_2018 <- data_table %>% 
        filter(str_detect(Date.Range, as.character(input$bounce_year)))
      
      combined_data_table <- left_join(data_table_2017, data_table_2018, by = "Service.Provider")
      combined_data_table <- combined_data_table %>% 
        select(Service.Provider, Sessions.x, Bounce.Rate.x, Sessions.y, Bounce.Rate.y) %>% 
        rename("prior_sessions" = Sessions.x, "prior_bounce_rate" = Bounce.Rate.x, "current_sessions" = Sessions.y,
               "current_bounce_rate" = Bounce.Rate.y) %>% 
        filter(prior_sessions > 3 | current_sessions > 3) %>% 
        arrange(-current_sessions) %>% 
        filter(prior_bounce_rate > 90 & current_bounce_rate > 90)
      
      combined_data_table$prior_bounce_rate <- combined_data_table$prior_bounce_rate / 100
      combined_data_table$current_bounce_rate <- combined_data_table$current_bounce_rate / 100
      
      combined_data_table <- combined_data_table %>% 
        rename("Service Provder" = Service.Provider, "Prior Sessions" = prior_sessions, "Prior Bounce Rate" = prior_bounce_rate,
               "Current Sessions" = current_sessions, "Current Bounce Rate" = current_bounce_rate)
      
      datatable(combined_data_table, options = list(lengthMenu = list(c(25, 50, -1), list("25", "50", "All"))))
    })
    
  })
  
  observeEvent(input$api_call, {

    output$api_text <- renderText({

      # browser()

      body <- statement(select = "Clicks",
                        report = "CAMPAIGN_PERFORMANCE_REPORT",
                        start = isolate(input$first_date),
                        end = isolate(input$last_date))

      source("/Volumes/Front/Adam/Shiny8/scripts/googleAuth.R")
      source("~/Desktop/Rob Scripts/Reference Files/source.R")

    browser()

      metric <- getData(clientCustomerId = client_id,
                        google_auth = authorize_Adwords(),
                        statement = body)
    })

  })
  
  
  paid_search_setup <- reactive({
    
    file_to_read <- isolate(input$paid_search_file)
    
    if(is.null(file_to_read)) {
      return()
    }
    
    # browser()
    
    data_table <- read.csv(file_to_read$datapath)
    data_table <- data_table %>% 
      select(LeadUID, ForceID, Local_ID, SentTo, Type, Campaign, MarketingSource, Website, PageLink, Phone, IVR.Number,
             TransferredTo, Date, Year, Month, Day, Time, CallDuration, PostalCode, IVRZip, City, 
             CallResult, PhoneRecording, DateAdded, SendCount, Category, FormType, FirstName, LastName, 
             Email, Address, Province, AppointmentRequestDate, ProductInterest, Comments, State, Template,
             Country, CallDate, LeadSource, DateVerified, VerifiedBy, VerifyResponse, CallVerified, 
             LastReturnActionDate, VisitID, RecordID, GClickID, PaidSearchKeyword) %>% 
      mutate(Disposition = "") %>% 
      mutate(Revenue = "") %>% 
      mutate("New/Returning Customer" = "") %>% 
      filter(MarketingSource == "Paid Search") %>% 
      filter(IVR.Number != 8779361556 & IVR.Number != 8779513644 & IVR.Number != 8779660399)
    
    # data_table <- data_table %>% 
    #  
    #   filter(IVR.Number != 8779361556 & IVR.Number != 8779513644 & IVR.Number != 8779660399)
    # View(data_table)
  })
  
  output$paid_search_download <- downloadHandler(
    
    filename = "Paid Search Leads.csv",
    
    content = function(file) {
      write.csv(paid_search_setup(), file, row.names = F)
    }
    
  )
  
  # observeEvent(input$gsheet_create, {
  #   
  #   file_to_read <- isolate(input$gsheet_file)
  #   
  #   if(is.null(file_to_read)) {
  #     return()
  #   }
  #   
  #   data_table <- read.csv(file_to_read$datapath)
  #   
  # browser()
  # 
  # google_sheet <- gs_new(title = input$gsheet_name, ws = input$worksheet_first_name, input = data_table)
  # 
  # This is unfortunately, going to by and large be entirely useless. Works well for very small data sets 
  # (less than 500 entries TOTAL), but gets increasingly worse the more entries included. Google's brief
  # API page on this says that there is a limit of 500 requests per 100 seconds per project, so pretty much
  # if there's anything sizeable, say 10,000 entries, its calculated to take over 30 mins for something that wouldn't 
  # take exceptionally long to just copy. Pretty nice for SMALL data sets, but anything we would use it for makes this
  # near useless here.
  # })
  
}

shinyApp(ui = ui, server = server)
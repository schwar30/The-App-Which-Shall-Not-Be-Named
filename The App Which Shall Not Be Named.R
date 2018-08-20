# Adwords API Call, Google Sheets do not work. Not honestly sure if I should
# try to make sheets work since its API restrictions renders it ineffective.
# I can't figure out the Adwords API call yet, but if I can, that would be a 
# decent addition.

# This is ultimately the life-blood of this whole script

library(shiny)

# Probably useless at the time being

library(shinyjs)

library(shinydashboard)
library(dplyr)
library(stringr)
library(DT)
library(data.table)

# I can't get the auto authorization to work without this option.

options(httr_oob_default = T)
library(googleAnalyticsR)

# browser()

# I don't think I use any relevant things from this either.
# Even if I did, I should be using the packages officer and
# FlexTable

library(ReporteRs)

# Both are used for API calls

library(RAdwords)

# There isn't anything particularly wrong with RGA, I just wanted to 
# try something different so I wouldn't just copy Adam's / Austin's code
# for analytics API calls. Might do the same for Adowrds API.

# library(RGA)

# Below is likely unnecessary because the google sheets API is ridiculously slow.
# Took over a half hour to input a datatable with ~10,000 entries.

library(googlesheets)

library(shinyWidgets)

# This is particularly for the call files and lead exports which tend to be rather large

options(shiny.maxRequestSize = 30 * 1024^2)

# I need this because I want the selectize option in the UI for API calls.

client_csv <- read.csv("~/Desktop/Rob Scripts/Reference Files/client_website_ga_id.csv")
client_csv <- client_csv %>% 
  filter(str_detect(UA, "UA"))

# This file isn't particularly updated with correct corresponding http:// or https:// starters,
# so i just remove them initially so they actually match up.

client_csv$name <- gsub("^.*\\/\\/", "", client_csv$name)

# For the Magic Spreadsheet component, I make update selectize based off other inputs, but that 
# still requires an initial value. I could directly set the choices equal to NULL, but having them
# exist first is a decent thing to do anyway. I should also rename these so they aren't general.

data1 <- NULL
data2 <- NULL
data_set <- NULL
date_ranges <- NULL

ui <- dashboardPage(
  
  dashboardHeader(title = "\nThe\nApp\nWhich\nShall\nNot\nBe\nNamed", titleWidth = 400),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      # This tab is my most expansive and most useful project, which definitely puts it at the top
      # of the app.
      
      menuItem("Magic Spreadsheet", tabName = "magic", icon = icon("magic"),
               menuSubItem("Upload Properties", tabName = "magic_upload"),
               menuSubItem("Join Options", tabName = "magic_join"),
               menuSubItem("Spreadsheet Cleanup", tabName = "magic_cleanup"),
               menuSubItem("View", tabName = "magic_view"), 
               menuSubItem("Download", tabName = "magic_download")),
      
      # There seems to be a huge dip in referral traffic for almost every site, so I 
      # wanted a way to quantify the losses
      
      menuItem("Referral Analytics", tabName = "referral", icon = icon("rebel")), 
      
      # Many campaigns see dips in performance despite the fact that clicks go up and 
      # spend goes up, but many times there's a push to disconinue brand keywords. This is 
      # meant to show paused keywords and how many conversions that were lost as a result of this.
      # This ended up not haveing any benefit to reporting, as it loses scope of so much, and according
      # to the PPC team, looking at the changes in raw keywords is not particulary a beneficial thing.
      #it is with a heavy heart then, that I decide this needs to be sunset.
      
      # menuItem("Paused Keywords", tabName = "paused", icon = icon("empire"),
      #          menuSubItem("File Upload", tabName = "paused_upload"),
      #          menuSubItem("Filtered Paused", tabName = "list_keyword"),
      #          menuSubItem("Count of Paused Keywords", tabName = "count_keyword"),
      #          menuSubItem("Paused & Removed Keywords", tabName = "totals_keyword"),
      #          menuSubItem("Summaries", tabName = "summary_keyword")),
      
      # On occassion we will be asked about inquiry zipcode breakouts, which isn't very common, but it 
      # would take like half an hour or so to finalize, and this is just easier since its now generalized
      
      menuItem("Zipcode Breakout", tabName = "zipcode", icon = icon("empire")),
      
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
      
      # menuItem("Analytics API Call", tabName = "api", icon = icon("tree")), 
      
      # The API call for analytics wasn't easy, but I was still able to do it, so I guess it's also probably worth
      # getting a test page for this as well.
      
      # menuItem("Adwords API Call", tabName = "adwords", icon = icon("adn")),
      
      # More of a basic reporting kind of thing that Stullzy wants to lay out. It's not difficult, but the problem
      # is I don't explicitly know everything to filter, but I think it's pretty decent.
      
      menuItem("Paid Search", tabName = "paid_search", icon = icon("cog")),
      
      # A hot mess of garbage. It's not that I couldn't get it to work, but that it's going to be ineffective because of 
      # the limitations of Google's API. Every cell is considered a request, and only 500 requests are available every 100
      # seconds. Ultimately, unless it's a small data set, its just more worth to do it yourself unless you're of the mindset
      # that this or multiple sheets need to be created overnight or something. Either way, not too worth allocating a ton of
      # time into. Probably would work well for my strongman app though.
      
      # Decided to sunset this as well, since I can't think of a single time that using this would be more effective than just
      # downloading the export or copy pasting a dataset. It's really unfortunate the googleSheets API is so restrictive, because
      # Otherwise, this could really have some nice potential.
      
      # menuItem("Google Sheets", tabName = "gsheet", icon = icon("exclamation"),
      #          menuSubItem("New Google Sheet", tabName = "new_gsheet"),
      #          menuSubItem("Edit Google Sheet", tabName = "edit_gsheet"),
      #          menuSubItem("Remove Google Sheet", tabName = "remove_gsheet")),
      
      # This is a tab that shows and allows downloads for the breakout of corporate zipcodes
      
      menuItem("Corporate Zipcode Breakdown", tabName = "corporate", icon = icon("steam")),
      
      # This allows the leads to be filtered either by phone number or website
      
      menuItem("Export Wrangling", tabName = "wrangle", icon = icon("book"))
      
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
                numericInput(inputId = "analytics_lines", label = "Number of Lines to Skip:", value = "6"),
                
                # I decided that it was probably more worthwhile to not have this since I found a way to compare
                # without a given year, allowing for period over period calculations and no yearly updates. Depends a 
                # little more heavily on the analytics download, but (since I'm the only person I forsee using this)
                # I should just know what I'm downloading in the first place. Maybe wouldn't hurt to actually show the date 
                # range somewhere.
                
                # textInput(inputId = "analytics_year", label = "Current Year:", value = "2018"),
                
                actionButton(inputId = "analytics_update", label = "Get Referral Differences", icon = icon("empire")),
                actionButton(inputId = "analytics_count", label = "Get Referral Counts", icon = icon("rebel"))
              ),
              
              tableOutput(outputId = "analytics_date_range"),
              tableOutput(outputId = "analytics_sum"),
              dataTableOutput(outputId = "analytic_table")
              
      ),
      
      tabItem(tabName = "dialog",
              
              titlePanel("Dialog Tech Call App"),
              
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
      
      # I have recently learned that none of this is particularly helpful, because while there are many keywords that get 
      # paused, generally (not always) keywords are paused to replace with either a more specific or general keyword. Unfortunately,
      # this does not help actually show which clients pause their brand keywords, although that was the intial purpose. This was,
      # however, the first tool I made in shiny, so while ineffective for anything, its still a nice relic.
      
      
      # tabItem(tabName = "paused_upload",
      #         
      #         # When I originally built the paused keyword stuff, I was playing around with other page setups with 
      #         # shinyjs, so I kept this as a multitab, and since there are so many buttons and outputs, its probably
      #         # a decent idea. This part is simply a file upload.
      #         
      #         sidebarPanel(
      #           fileInput(inputId = "paused_file", label = "Please Input Desired .csv Keyword File")
      #         )),
      # 
      # tabItem(tabName = "list_keyword",
      #         
      #         # This component just lists all of the keywords that are paused and can be all of them, just the brand, 
      #         # or just the nonbrand. Also allows for downloads of these keywords, though the use of this is probably
      #         # very limited
      #         
      #         sidebarPanel(
      #           actionButton(inputId = "paused_filter", label = "View Paused Keywords"),
      #           actionButton(inputId = "paused_filter_brand", label = "View Paused Brand Keywords"),
      #           actionButton(inputId = "paused_filter_nonbrand", label = "View Paused\nNonBrand Keywords"),
      #           downloadButton(outputId = "paused_download_all", label = "All Paused Keywords"),
      #           downloadButton(outputId = "paused_download_brand", label = "Paused Brand Keywords"),
      #           downloadButton(outputId = "paused_download_nonbrand", label = "Paused Nonbrand Keywords")
      #         ),
      #         
      #         fluidRow(
      #           tableOutput("paused_filter_table")
      #         )),
      # 
      # tabItem(tabName = "count_keyword",
      #         
      #         # This provides a count of how many brand keywords are paused, nonbrand keywords, or total paused keywords
      #         # there are.
      #         
      #         sidebarPanel(
      #           
      #           actionButton(inputId = "paused_count_brand", label = "Brand Keyword Count"),
      #           actionButton(inputId = "paused_count_nonbrand", label = "Nonbrand Keyword Count"),
      #           actionButton(inputId = "paused_count_all", label = "All Keyword Count")
      #         ),
      #         
      #         
      #         fluidRow(
      #           tableOutput(outputId = "paused_count_table")
      #         )),
      # 
      # tabItem(tabName = "totals_keyword",
      #         
      #         # Shows how many lost conversions there are by keyword. Allows for the downloads too.
      #         
      #         sidebarPanel(
      #           
      #           actionButton(inputId = "paused_all", label = "View Inactive"),
      #           actionButton(inputId = "paused_brand", label = "View Brand Inactive"),
      #           actionButton(inputId = "paused_nonbrand", label = "View Nonbrand Inactive"),
      #           downloadButton(outputId = "paused_all_download", label = "Download Inactive"),
      #           downloadButton(outputId = "paused_brand_download", label = "Download Brand Inactive"),
      #           downloadButton(outputId = "paused_nonbrand_download", label = "Download Nonbrand Inactive")
      #         ),
      #         
      #         tableOutput("paused_all_table")
      #         
      # ),
      # 
      # # Shows total loss of conversions for brand, nonbrand, and total groups.
      # 
      # tabItem(tabName = "summary_keyword",
      #         
      #         sidebarPanel(
      #           actionButton(inputId = "paused_all_total", label = "Inactive Summary"),
      #           actionButton(inputId = "paused_brand_total", label = "Brand Inactive Summary"),
      #           actionButton(inputId = "paused_nonbrand_total", label = "Nonbrand Inactive Summary")
      #           
      #         ),
      #         tableOutput("paused_all_total_table")
      # ),
      
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
      
      # tabItem(tabName = "api",
      #         
      #         titlePanel("Test Analytics API Call"),
      #         
      #         sidebarPanel(
      #           
      #           selectizeInput(inputId = "api_website", label = "Select Website", choices = client_csv$name,
      #                          multiple = T),
      #           dateInput(inputId = "first_date", label = "First Date"),
      #           dateInput(inputId = "last_date", label = "Last Date"),
      #           actionButton(inputId = "api_call", label = "API Call")
      #           
      #         ),
      #         
      #         textOutput("api_text")
      #         
      #         ),
      # 
      # tabItem(tabName = "adwords",
      #         
      #         titlePanel("Test Adwords API Call"),
      #         
      #         sidebarPanel(
      #           
      #           dateInput(inputId = "adwords_first_date", label = "First Date"),
      #           dateInput(inputId = "adwords_last_date", label = "Last Date"),
      #           actionButton(inputId = "adwords_api_call", label = "API Call")
      #           
      #         ),
      #         
      #         textOutput("adwords_api_text")
      #         
      #         ),
      
      
      tabItem(tabName = "paid_search",
              
              # Just a leads export and a download button
              
              titlePanel("Paid Search"),
              
              sidebarPanel(
                
                fileInput(inputId = "paid_search_file", label = "Input export file"),
                downloadButton(outputId = "paid_search_download")
                
              )),
      
      # All of this stuff is still in "development", which generally means its not going to get touched
      # unless I really want to, which I'm not sure if I do since its only helpful in limited scenarios.
      
      # tabItem(tabName = "new_gsheet",
      #         
      #         titlePanel("New Google Sheet"),
      #         
      #         sidebarPanel(
      #           
      #           fileInput(inputId = "gsheet_file", label = "Data Wanted to be Put in A Google Sheet:"),
      #           textInput(inputId = "gsheet_name", label = "Name of New Google Sheet:"),
      #           textInput(inputId = "worksheet_first_name", label = "Worksheet Name:"),
      #           actionBttn(inputId = "gsheet_create", label = "Create Sheet")
      #           
      #         )),
      # 
      # tabItem(tabName = "edit_gsheet",
      #         
      #         titlePanel("Edit Google Sheet"),
      #         
      #         sidebarPanel(
      #           
      #           actionBttn(inputId = "update_gsheet", label = "Edit Sheet")
      #           
      #         )),
      # 
      # tabItem(tabName = "remove_gsheet",
      #         
      #         titlePanel("Remove Google Sheet"),
      #         
      #         sidebarPanel(
      #           
      #           actionBttn(inputId = "delete_gsheet", label = "Deleet Sheet")
      #           
      #         )),
      
      tabItem(tabName = "corporate",
              
              titlePanel("Corporate Zipcode Breakout"),
              
              sidebarPanel(
                
                textInput(inputId = "corpID", label = "Corporate ID"),
                "or", 
                textInput(inputId = "corp_dealer", label = "Dealer Name"),
                actionBttn(inputId = "corp_view", label = "View Associations"),
                downloadButton(outputId = "corp_download", label = "Download Associations")
                
              ),
              
              tableOutput("corp_table")
              
              ),
      
      tabItem(tabName = "wrangle",
              
              titlePanel("Export Wrangling"),
              
              sidebarPanel(
                
                fileInput(inputId = "export_file", label = "Please Upload Export:"),
                textInput(inputId = "export_website", label = "Filter Website"),
                textInput(inputId = "export_phone", label = "Filter Phone", value = ""),
                actionButton(inputId = "export_clean", label = "Show Table"), 
                actionButton(inputId = "export_count", label = "Lead Count"),
                downloadButton(outputId = "export_download", label = "Download Cleaned Export")
                
              ),
              
              tableOutput("export_count_table"),
              tableOutput("export_clean_table")
              
              ),
      
      tabItem(tabName = "magic_upload",
              
              titlePanel("Upload Properties"),
              
              sidebarPanel(
                
                fileInput(inputId = "file", label = "Please Input Desired File(s)", multiple = T), 
                radioButtons(inputId = "deliminator", label = "Deliminator", choices = c(Comma = ",", Tab = "\t", Space = " ", Semicolon = ";"), selected = ","),
                numericInput(inputId = "skip", label = "Number of lines you want to skip (optional)", value = "", min = 0),
                numericInput(inputId = "magic_keep", label = "What is the last row you wish to keep? (optional)", value = "", min = 0),
                radioButtons(inputId = "encode", label = "Encoding", choices = c("unknown", "UTF-8", "UTF-16", "UTF-32"), selected = "unknown"),
                actionButton(inputId = "problems", label = "Save")
                
              )),
      
      tabItem(tabName = "magic_join",
              
              titlePanel("Join Options"),
              
              sidebarPanel(
                
                radioButtons(inputId = "combine_choice", label = "Join Options", choices = c(rBind = "rbindlist",
                                                                                             cBind = "cbindlist",
                                                                                             "Left Join" = "left_join",
                                                                                             "Right Join" = "right_join",
                                                                                             "Inner Join" = "inner_join",
                                                                                             "Full Join" = "full_join")),
                selectizeInput(inputId = "join_prop", label = "Join by which column?", choices = colnames(intersect(data1, data2)), multiple = T),
                actionButton(inputId = "combine_activate", label = "Select")
                
              )),
      
      tabItem(tabName = "magic_cleanup",
              
              titlePanel("SpreadSheet Cleanup"),
              
              sidebarPanel(
                
                textInput(inputId = "filter_text", label = "What would you like to filter?"),
                selectizeInput(inputId = "filter_selectize", label = "Choose column to filter through:", choices = colnames(data_set), multiple = T),
                prettyToggle(inputId = "filter_toggle",
                             label_on = "Include",
                             icon_on = icon("check"),
                             status_on = "success",
                             status_off = "danger",
                             label_off = "Exclude",
                             icon_off = icon("remove"))
              ),
              
              sidebarPanel(
                
                selectizeInput(inputId = "arrange_selectize", label = "Arrange by which column?", choices = colnames(intersect(data1, data2)), multiple = T),
                checkboxInput(inputId = "arrange_checkbox", label = "Select for high to low or reverse alphabetical order:")
                
              ),
              
              sidebarPanel(
                
                selectizeInput(inputId = "select_selectize", label = "Which columns do you want? (Order matters)", choices = colnames(data_set), multiple = T)
                
              ),
              
              column(12, 
              
              actionButton(inputId = "cleanup_update", label = "Update")
              
              )),
      
      tabItem(tabName = "magic_view",
              
              titlePanel("View Magic"),
              
              actionButton(inputId = "real_table", label = "Click to View Changes"),
              tableOutput("selected_table")
              
              ),
      
      tabItem(tabName = "magic_download",
              
              titlePanel("Magic Download"),
              
              sidebarPanel(
                textInput(inputId = "magic_download_name", label = "Input Download Name:", value = ""),
                downloadButton(outputId = "download", label = "Download")
              ))
      
    )))

server <- function(input, output, session) {
  
  observeEvent(input$analytics_update, {
    
    
    # browser()
    
    # This is alwways helpful so it doesn't error out when nothing is included, and allows everything
    # to be isolated so it doesn't fire only one time.
    
    file_to_read <- isolate(input$analytics_file)
    
    # if(is.null(file_to_read)){
    #   return()
    # }
    
    # Allows the lines to be skipped. Both analytics and adwords have weird exports, so I usually
    # throw the UTF-8 tag on the read.csv
    
    # browser()
    
    data_table <- try(read.csv(file_to_read$datapath, skip = input$analytics_lines, encoding = "UTF-8"))
    
    # browser()
    
    # This needs to be all separated so they can be joined and then mutated
    
    if(class(data_table) != "try-error") {
      
      # browser()
      
      if("Date.Range" %in% colnames(data_table)){
        
        # browser()
      
      data_table <- data_table %>% 
        select(Source, Date.Range, Sessions)
      
      first_range <- as.character(data_table$Date.Range[1])
      second_range <- as.character(data_table$Date.Range[2])
      
      date_ranges <- t(c(second_range, first_range))
      colnames(date_ranges) <- c("First Date Range", "Second Date Range")
      date_ranges <- as.data.frame(date_ranges)
      
      analytics_2018 <- data_table %>% 
        filter(str_detect(Date.Range, first_range)) %>% 
        filter(Sessions != "")
      
      analytics_2017 <- data_table %>% 
        filter(!str_detect(Date.Range, first_range)) %>% 
        filter(Sessions != "")
      
      }else{
        
        analytics_2017 <- 1
        analytics_2018 <- 1
        
      }
      
    }else{
      
      # browser()
      
      analytics_2017 <- 0
      analytics_2018 <- 0
      
    }
    
    # browser()
    
    if(is.null(nrow(analytics_2018)) | is.null(nrow(analytics_2017)) | is.null(input$analytics_file)) {
      
      # browser()
      
      if(analytics_2018 == 1 | analytics_2017 == 1) {
        
        confirmSweetAlert(session = session, 
                          inputId = "wrong_analytics_year",
                          title = "You either do not have year over year data or a valid referral analytics export!",
                          text = "If you do think it's valid, check the number of lines you skipped!",
                          type = "warning",
                          btn_labels = "OK!", 
                          danger_mode = T)
        
      }
      
      # if(is.null(analytics_2017)) {
      #   
      #   confirmSweetAlert(session = session, 
      #                     inputId = "no_prior_analytics",
      #                     title = "You have no prior period or prior year data!",
      #                     type = "warning",
      #                     btn_labels = "OK!", 
      #                     danger_mode = T)
      #   
      # }
      
      if(is.null(input$analytics_file)) {
        
        confirmSweetAlert(session = session, 
                          inputId = "no_analytics_file",
                          title = "Please input a referral file!",
                          type = "warning",
                          btn_labels = "OK!", 
                          danger_mode = T)
        
      }
      
      combined_table <- NULL
      
    }else{
      
      # Joins the date tables and selects only the relevant metrics and then creates a difference and percentage change column. 
      # I also thought it was worth while to only show referral traffic that saw a change.
      
      combined_table <- left_join(analytics_2017, analytics_2018, by = "Source")
      
      # browser()
      
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
      
      confirmSweetAlert(session = session, 
                        inputId = "referral_table_success",
                        title = "Successfully generated referral table!",
                        type = "success",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }
    
    
    
    output$analytic_table <- renderDataTable({
 
      if(is.null(combined_table)){
        
      }else{
      
      datatable(combined_table)
        
      }
      
    })
    
    output$analytics_date_range <- renderTable({
      
      # browser()

      if(is.data.frame(date_ranges)){

        date_ranges

      }else{


      }

    })
    
  })
  
  observeEvent(input$analytics_count, {
    
    # browser()
    
    file_to_read <- isolate(input$analytics_file)
    
    data_table <- try(read.csv(file_to_read$datapath, skip = input$analytics_lines, encoding = "UTF-8"))
    
    # browser()
    
    # This needs to be all separated so they can be joined and then mutated
    
    if(class(data_table) != "try-error") {
      
      # browser()
      
      if("Date.Range" %in% colnames(data_table)){
        
        # browser()
        
        data_table <- data_table %>% 
          select(Source, Date.Range, Sessions)
        
        first_range <- as.character(data_table$Date.Range[1])
        
        analytics_2018 <- data_table %>% 
          filter(str_detect(Date.Range, first_range)) %>% 
          filter(Sessions != "")
        
        analytics_2017 <- data_table %>% 
          filter(!str_detect(Date.Range, first_range)) %>% 
          filter(Sessions != "")
        
      }else{
        
        analytics_2017 <- 1
        analytics_2018 <- 1
        
      }
      
    }else{
      
      # browser()
      
      analytics_2017 <- 0
      analytics_2018 <- 0
      
    }
    
    if(is.null(nrow(analytics_2018)) | is.null(nrow(analytics_2017)) | is.null(input$analytics_file)) {
      
      if(analytics_2018 == 1 | analytics_2017 == 1) {
        
        confirmSweetAlert(session = session, 
                          inputId = "wrong_analytics_year",
                          title = "You either do not have year over year data or a valid referral analytics export!",
                          type = "warning",
                          btn_labels = "OK!", 
                          danger_mode = T)
        
      }
      
      if(is.null(input$analytics_file)) {
        
        confirmSweetAlert(session = session, 
                          inputId = "no_analytics_file",
                          title = "Please input a referral file!",
                          type = "warning",
                          btn_labels = "OK!", 
                          danger_mode = T)
      }
      
      combined_table <- NULL
      
    }else{
      
    
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
    
    # combined_table
    
    confirmSweetAlert(session = session, 
                      inputId = "referral_table_successc",
                      title = "Successfully generated referral counts!",
                      type = "success",
                      btn_labels = "OK!",
                      danger_mode = T)
    
    }
    
    output$analytics_sum <- renderTable({
      
      # file_to_read <- isolate(input$analytics_file)
      # 
      # if(is.null(file_to_read)){
      #   return()
      # }
      # 
      # data_table <- read.csv(file_to_read$datapath, skip = input$analytics_lines, encoding = "UTF-8")
      # data_table <- data_table %>% 
      #   select(Source, Date.Range, Sessions)
      # 
      # first_range <- as.character(data_table$Date.Range[1])
      # 
      # analytics_2018 <- data_table %>% 
      #   filter(str_detect(Date.Range, input$analytics_year)) %>% 
      #   filter(Sessions != "")
      # 
      # analytics_2017 <- data_table %>% 
      #   filter(!str_detect(Date.Range, input$analytics_year)) %>% 
      #   filter(Sessions != "")
      
      # Everything is pretty much the same as before, but the focus is to get the sums instead of everything else
      
     combined_table
      
    })
    
  })
  
  observeEvent(input$zipcode_list, {
    
    # browser()
    
    input$zipcode_list
    
    file_to_read <- isolate(input$zipcode_lead)
    # if(is.null(file_to_read)) {
    #   return()
    # }
    
    # The reason this is used instead of read.csv is the file input allows for
    # multiple files. It's kind of a nice thing when its a weird combination of dealers
    # that want aggregated data
    
    # browser()
    
    if(is.null(nrow(file_to_read))){
      
      lead_export <- NULL
      
      confirmSweetAlert(session = session, 
                        inputId = "zipcode_no_file",
                        title = "Please Input Lead Export(s)!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
    
    lead_export <- rbindlist(lapply(file_to_read$datapath, fread), 
                             use.names = T, fill = T)
    
    # Customers are able to put in their own zipcodes, and this is what we really use for zipcode
    # placement, so a new column is made to overwrite postal code if its available. Then all inquiries
    # are grouped by zipcode and counted.
    
    # if(("SentTo" %in% colnames(lead_export) & "PostalCode" %in% colnames(lead_export) & "IVRZip" %in% colnames(lead_export)) &
    #    (any(is.na(lead_export$SentTo)) )) {
      
      # browser()
      
    # }
    
    if(ncol(lead_export) == 99) {
    
    lead_export <- lead_export %>% 
      select(SentTo, PostalCode, IVRZip) %>% 
      mutate(Zipcode = ifelse(is.na(IVRZip), PostalCode, IVRZip)) %>% 
      group_by(Zipcode) %>% 
      count(Zipcode) %>% 
      filter(Zipcode != "") %>% 
      arrange(-n) %>% 
      rename("Inquiries" = n)
    
    confirmSweetAlert(session = session, 
                      inputId = "zipcode_success",
                      title = "Zipcode Ordering Complete!",
                      type = "success",
                      btn_labels = "OK!",
                      danger_mode = T)
    
    }else{
      
      lead_export <- NULL
      
      if(nrow(input$zipcode_lead) == 1) {
      
      confirmSweetAlert(session = session, 
                        inputId = "zipcode_bad_file_one",
                        title = "This is not a lead export!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
        
      }else{
        
        confirmSweetAlert(session = session, 
                          inputId = "zipcode_bad_file_multiple",
                          title = "At least one file is not a lead export. Check uploaded files!",
                          type = "warning",
                          btn_labels = "OK!",
                          danger_mode = T)
        
      }
      
    }
    }
    # browser()
    output$zipcode_table <- renderTable({
      
      lead_export
      
    })
    
  })
  
  download_setup <- reactive({
    
    file_to_read <- isolate(input$zipcode_lead)
    if(is.null(file_to_read)) {
      return()
    }
    
    # browser()
    
    # if(input$zipcode_coop == ""){
    #   
    #   confirmSweetAlert(session = session,
    #                     inputId = "download_warning",
    #                     title = "There is no dealer name. Input if you want a name!",
    #                     type = "warning",
    #                     btn_labels = "OK!",
    #                     danger_mode = T)
    #   
    # }
    
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
    
    # Again, this file to read stuff is nice for preventing errors.
    
    # isolate(input$dialog_total)
    
    file_to_read <- isolate(input$dialog_file)
    
    # if(is.null(file_to_read)) {
    #   return()
    # }
    
    # Changes number from numeric to character. I don't know why I didn't have this
    # as a textinput to be completely honest. Just counts calls.
    
    # browser()
    
    if(input$dialog_number == "") {
      
      phone_export <- NULL
      
    }else{
    
    phone_export <- try(read.csv(file_to_read$datapath))
    
    }
    
    # browser()
    
    if(class(phone_export) == "try-error") {
      
      confirmSweetAlert(session = session, 
                        inputId = "dialog_no_file",
                        title = "Please upload an export from DialogTech!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
      
      if(input$dialog_number != "") {
        
      
    if("dnis" %in% colnames(phone_export)) {
    
    phone_export$dnis <- as.character(phone_export$dnis)
    
    phone_export <- phone_export %>% 
      filter(str_detect(dnis, isolate(input$dialog_number))) %>% 
      group_by(dnis) %>% 
      count(dnis) %>% 
      rename("Number" = dnis) %>% 
      rename("Count" = n)
    
    
    output$dialog_count <- renderTable({
      
      phone_export
      
    
      
    })
    
    confirmSweetAlert(session = session,
                      inputId = "dialog_success",
                      title = "Successufly counted calls from DialogTech!",
                      type = "success",
                      btn_labels = "OK!",
                      danger_mode = T)
    
    }else{
      
      confirmSweetAlert(session = session,
                        inputId = "wrong_file_dialog",
                        title = "This file does not include a dnis column! Is this file from Dialog Tech?",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }
      }else{
      
        confirmSweetAlert(session = session,
                          inputId = "no_number_dialog",
                          title = "You did not include a phone number!",
                          type = "warning",
                          btn_labels = "OK!",
                          danger_mode = T)
        
    }
    }
  })
  
  download_setup <- reactive({
    
    file_to_read <- isolate(input$dialog_file)
    
    if(is.null(file_to_read)) {
      return()
    }
    
    # More setup. What this does is select the rows that include the desired number
    
    phone_export <- try(read.csv(file_to_read$datapath))
    
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
      
      isolate(input$api_call)
      
      # This little bit of code is just so that the API call can run without needing to sign in to 
      # Google Analytics
      
      ga_auth("~/Desktop/Rob Scripts/Reference Files/ga.oauth")
      
      # This lists all the Kennedy Accounts, UA codes, and analyticsID's.
      
      kennedy_accounts <- ga_account_list()
      
      # browser()
      
      if(nrow(as.data.frame((isolate(input$api_website)))) > 1) {
        
        # browser()
        
        # The selectize option would always pull in the first value if it was not a multiple, but 
        # data over multiple sites means nothing, so I just wanted some alerts to fire if someone 
        # tried to enter more than one or less than 1 site. Probably should have created it such that if 
        # it wasn't equal to 1 it would fire off, but it doesn't matter. It gave me a little bit more insight
        # on the alerts.
        
        confirmSweetAlert(
          
          session = session,
          inputId = "api_2_alert",
          type = "warning",
          title = "Please select only 1 website",
          btn_labels = "OK!",
          danger_mode = T
          
        )
        
      }else{
      
      if(nrow(as.data.frame(isolate(input$api_website))) == 0) {
        
        # Alert if no website is listed
        
        confirmSweetAlert(
          
          session = session,
          inputId = "api_0_alert",
          type = "warning",
          title = "Please select a website",
          btn_labels = "OK!",
          danger_mode = T
          
        )
        
      }else{
  
        isolate(input$api_website)
        
        # Here's all the meat and potatoes. The list doesn't seem to be updated whether the 
        # site is http:// or https://, so I removed it from the original file and from the called
        # Kennedy accounts.
        
        kennedy_accounts$websiteUrl <- gsub("^.*\\/\\/", "", kennedy_accounts$websiteUrl)
        
        # Since we only want a single id, we filter out the row that we need and select the id we want
        
        client_id <- kennedy_accounts %>% 
          filter(websiteUrl == isolate(input$api_website)) %>% 
          select(viewId)
        
        # Since the viewID is still as a dataframe and we want it as a character, so we need to select 
        # the single cell so it is just a character.
        
        client_id <- client_id$viewId[1]
        
        # So many trials at trying to set up the filters. The difficulty came from only being able to have
        # a single operator in the filter_clause_ga4 function. These are all the single shoot offs, and while
        # they are unused, they don't flow together to get the filter I want
        
        # filter_us <- dim_filter("country", "EXACT", "United States")
        # filter_can <- dim_filter("country", "EXACT", "Canada")
        # source1 <- dim_filter("source", "PARTIAL", "doubleclick", not = T)
        # source2 <- dim_filter("source", "PARTIAL", "buttons", not = T)
        # source3 <- dim_filter("source", "PARTIAL", "semalt", not = T)
        # source4 <- dim_filter("hostname", "PARTIAL", "(not set)", not = T)
        # source5 <- dim_filter("source", "PARTIAL", "uptime.com", not = T)
        # source6 <- dim_filter("source", "PARTIAL", "seo", not = T)
        # source7 <- dim_filter("source", "PARTIAL", "monetize", not = T)
        # source8 <- dim_filter("source", "PARTIAL", ".ml", not = T)
        # source9 <- dim_filter("source", "PARTIAL", "amezon", not = T)
        # source10 <- dim_filter("source", "PARTIAL", ".info", not = T)
        # source11 <- dim_filter("source", "PARTIAL", "traffic2money", not = T)
        # source12 <- dim_filter("networkLocation", "PARTIAL", "ovh", not = T)
        # source12 <- dim_filter("networkLocation", "PARTIAL", "ocean", not = T)
        # source13 <- dim_filter("networkLocation", "PARTIAL", "evercompliant", not = T)
        # source14 <- dim_filter("networkLocation", "PARTIAL", "hubspot", not = T)
        # source15 <- dim_filter("networkLocation", "PARTIAL", "microsoft corporation", not = T)
        # source16 <- dim_filter("networkLocation", "PARTIAL", "127.0.0.1:8888 / referral", not = T)
        # source17 <- dim_filter("networkLocation", "PARTIAL", "google llc", not = T)
        # source18 <- dim_filter("networkLocation", "PARTIAL", "amazon technologies inc.", not = T)
        
        # Had to use regular expressions because then we could use deMorgan's laws of equivalence. It was 
        # super annoying to keep track of everything but it now works. Honestly, manybe I shouldn't have struggled
        # as much as I did, because ultimately I didn't need to condense as much as I did, but this is kind of nice,
        # because if I need to change anything directly, I can just change the small word associated with (likely)
        # the service provider.
        
        country <- dim_filter("country", "REGEXP", "United States|Canada")
        source <- dim_filter("source", "REGEXP", "doubleclick|buttons|semalt|uptime\\.com|seo|monetize|\\.ml|amezon|\\.info|traffic2money", not = T)
        hostname <- dim_filter("hostname", "PARTIAL", "(not set)", not = T)
        service_provider <- dim_filter("networkLocation", "REGEXP", "ovh|ocean|evercompliant|hubspot|microsoft corporation|google llc|amazon technologies inc\\.", not = T)
        source_medium <- dim_filter("sourceMedium", "REGEXP", "127\\.0\\.0\\.1\\:8888 \\/ referral", not = T)
        
        # Sets up the filter to MATCH the Analytics filter by combining the aforementioned conditions
        
        new_filter <- filter_clause_ga4(list(country, source, hostname, service_provider, source_medium),
                                        operator = "AND")
        # rv <- list()
        # filter_all <- filter_clause_ga4(list(#filter_us, filter_can,
        #                                         filter_both,
        #                                         source1, source2, source3,
        #                                         source4, source5, source6, source7, source8, source9,
        #                                         source10, source11, source12, source13, source14, source15,
        #                                         source16, source17, source18
        #                                         ), operator = "OR"
        # )
        # filter_test <- filter_clause_ga4(list(filter_us, filter_can),
        #                                     operator = "OR")
        # 
        # filter <- filter_clause_ga4(list(filter_all, filter_test), operator = "AND")
        # 
        # filter_all <- filter_clause_ga4(list(filter_source, filter_country),
        #                                 operator = "OR")
        # browser()
        # call2 <- google_analytics(client_id,
        #                           date_range = c(input$first_date, input$last_date),
        #                           metrics = "sessions",
        #                           dim_filters = filter_test
        # )
        # browser()
        
        # The actual call. Only pulls sessions right now because this is a test and I really have no intention
        # of making this test page / app compete with Shiny, since this would lose anyway.
        
        sessions_call <- google_analytics(client_id,
                                    date_range = c(input$first_date, input$last_date),
                                    metrics = "sessions",
                                    dim_filters = new_filter)
        
        # The call again brings in everything as a dataframe, but I want a character object, so that's what this does.
        # I am honestly super impressed with myself for figuring this junk out. Everything in this was ridiculous because
        # of authorizations, converting to proper types, and having to be concerned with logical equivalence made this 
        # unequivically the most difficult script I've written to date. Luckily, however, if I ever wanted to test anything
        # out here, it would be much, much easier because now it's all about finding the correct ga: tag and setting up the 
        # call. Gold Sticker.
                                   
        total_sessions <- sessions_call$sessions[1]
        total_sessions
    
      }
      }
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
  
  observeEvent(input$gsheet_create, {

    file_to_read <- isolate(input$gsheet_file)

    if(is.null(file_to_read)) {
      return()
    }

    data_table <- read.csv(file_to_read$datapath)

  # browser()

  google_sheet <- gs_new(title = input$gsheet_name, ws = input$worksheet_first_name, input = data_table)

  # This is unfortunately, going to by and large be entirely useless. Works well for very small data sets
  # (less than 500 entries TOTAL), but gets increasingly worse the more entries included. Google's brief
  # API page on this says that there is a limit of 500 requests per 100 seconds per project, so pretty much
  # if there's anything sizeable, say 10,000 entries, its calculated to take over 30 mins for something that wouldn't
  # take exceptionally long to just copy. Pretty nice for SMALL data sets, but anything we would use it for makes this
  # near useless here.
  })
  
  observeEvent(input$corp_view, {
    
    isolate(input$corp_view)
    # isolate(input$corpID)
    # isolate(input$corp_dealer)
    
    output$corp_table <- renderTable({
      
    data_table <- read.csv("~/Desktop/Rob Scripts/Reference Files/zip codes.csv")
    
    data_table <- data_table %>% 
      select(PAR.Name, Dealer, FSA.ZIP.Code, Percentage.of.FSA.ZIP)
    
    # The conditionals need to be isolated otherwise they will auto update
    
    if(isolate(input$corpID) != "") {
      
      corpID <- isolate(input$corpID)
      
      data_table$Dealer <- as.character(data_table$Dealer)
      
      data_table <- data_table %>% 
        filter(str_detect(Dealer, corpID))

    }
    
    # The conditionals need to be isolated otherwise they will auto update

    if(isolate(input$corp_dealer) != "") {

      dealer <- isolate(input$corp_dealer)

      data_table <- data_table %>%
        filter(str_detect(PAR.Name, dealer))

    }
    
    data_table <- data_table %>% 
      arrange(FSA.ZIP.Code) %>% 
      rename("Dealer Name" = PAR.Name) %>% 
      rename("Dealer ID" = Dealer) %>% 
      rename("Zipcode" = FSA.ZIP.Code) %>% 
      rename("Percentage of Zipcode\nAssigned" = Percentage.of.FSA.ZIP)
    
    data_table
      
    })
    
  })
  
  corp_setup <- reactive({
    
    data_table <- read.csv("~/Desktop/Rob Scripts/Reference Files/zip codes.csv")
    
    data_table <- data_table %>% 
      select(PAR.Name, Dealer, FSA.ZIP.Code, Percentage.of.FSA.ZIP)
    
    if(input$corpID != "") {
      
      corpID <- input$corpID
      
      data_table$Dealer <- as.character(data_table$Dealer)
      
      data_table <- data_table %>% 
        filter(str_detect(Dealer, corpID))

    }
    
    if(input$corp_dealer != "") {
      
      dealer <- isolate(input$corp_dealer)
      
      data_table <- data_table %>% 
        filter(str_detect(PAR.Name, dealer))
      
    }
    
    data_table
    
  })
  
  output$corp_download <- downloadHandler(
    
   filename = function(){
     ifelse(input$corp_dealer != "", paste0(input$corp_dealer, " Corporate Zipcodes.csv"), "Corporate Zipcodes.csv")
   },
   
   content = function(file){
     
     write.csv(corp_setup(), file, row.names = F)
     
   })
  
  observeEvent(input$export_clean, {
    
    output$export_clean_table <- renderTable({
      
      isolate(input$export_clean)
      
      file_to_read <- isolate(input$export_file)
      if(is.null(file_to_read)) {
        return()
      }
      
      export_table <- read.csv(file_to_read$datapath)
      
      export_table <- export_table %>% 
        select(SentTo, Type, Campaign, MarketingSource, Website, Phone, IVR.Number, TransferredTo)
      
      export_table$Phone <- gsub("\\..*", "", export_table$Phone)
      export_table$IVR.Number <- gsub("\\..*", "", export_table$IVR.Number)
      export_table$TransferredTo <- gsub("\\..*", "", export_table$TransferredTo)
      
      if(isolate(input$export_website) != "") {
        
        export_table <- export_table %>% 
          filter(Campaign == "Web") %>% 
          filter(Website == isolate(input$export_website))
        
      }
      
      if(isolate(input$export_phone) != "") {
        
        export_table <- export_table %>% 
          filter(Campaign == "Phone") %>% 
          filter(IVR.Number == isolate(input$export_phone))
        
      }else{
        
        export_table <- export_table
        
      }
      
      export_table <- export_table %>% 
        distinct() %>% 
        filter(SentTo != "")
      
      export_table$MarketingSource[export_table$MarketingSource == ""] <- "Web"
      
      export_table
      
    })
    
  })
  
  observeEvent(input$export_count, {
    
    output$export_count_table <- renderTable({
      
      file_to_read <- isolate(input$export_file)
      if(is.null(file_to_read)) {
        return()
      }
      
      export_table <- read.csv(file_to_read$datapath)
      
      export_table <- export_table %>% 
        select(SentTo, Type, Campaign, MarketingSource, Website, Phone, IVR.Number, TransferredTo)
      
      export_table$Phone <- gsub("\\..*", "", export_table$Phone)
      export_table$IVR.Number <- gsub("\\..*", "", export_table$IVR.Number)
      export_table$TransferredTo <- gsub("\\..*", "", export_table$TransferredTo)
      
      if(isolate(input$export_website) != "") {
        
        export_table <- export_table %>% 
          filter(Campaign == "Web") %>% 
          filter(Website == isolate(input$export_website))
        
      }
      
      if(isolate(input$export_phone) != "") {
        
        export_table <- export_table %>% 
          filter(Campaign == "Phone") %>% 
          filter(IVR.Number == isolate(input$export_phone))
        
      }else{
        
        export_table <- export_table
        
      }
      
      export_table <- export_table %>% 
        distinct() %>% 
        filter(SentTo != "") %>% 
        count() %>% 
        rename("Total Number of Leads" = n)
      
      export_table
      
    })
    
  })
  
  export_setup <- reactive({
    
    file_to_read <- isolate(input$export_file)
    if(is.null(file_to_read)) {
      return()
    }
    
    export_table <- read.csv(file_to_read$datapath)
    
    export_table <- export_table %>% 
      select(SentTo, Type, Campaign, MarketingSource, Website, Phone, IVR.Number, TransferredTo)
    
    export_table$Phone <- gsub("\\..*", "", export_table$Phone)
    export_table$IVR.Number <- gsub("\\..*", "", export_table$IVR.Number)
    export_table$TransferredTo <- gsub("\\..*", "", export_table$TransferredTo)
    
    if(input$export_website != "") {
      
      export_table <- export_table %>% 
        filter(Campaign == "Web") %>% 
        filter(Website == input$export_website)
      
    }
    
    if(input$export_phone != "") {
      
      export_table <- export_table %>% 
        filter(Campaign == "Phone") %>% 
        filter(IVR.Number == input$export_phone)
      
    }else{
      
      export_table <- export_table
      
    }
    
    export_table <- export_table %>% 
      distinct() %>% 
      filter(SentTo != "")
    
    export_table$MarketingSource[export_table$MarketingSource == ""] <- "Web"
    
    export_table
    
  })
  
  output$export_download <- downloadHandler(
    
    filename = "Filtered Leads.csv",
    
    content = function(file) {
      write.csv(export_setup(), file, row.names = F)
    }
    
  )
  
  observeEvent(input$adwords_api_call, {
    
    output$adwords_api_text <- renderText({
      
    source("/Volumes/Front/Adam/Shiny8/scripts/googleAuth.R")
      
      # browser()
      
      authorize_Adwords()
      google_auth <- authorize_Adwords()
      
      body <- statement(select = "Clicks",
                        report = "ACCOUNT_PERFORMANCE_REPORT",
                        start = "2018-01-01",
                        end = "2018-01-10")
      
      data1 <- getData(clientCustomerId = "320-743-6876",
                      google_auth = google_auth,
                      statement = body,
                      transformation = F
                      )
      
      data1
      
      # This is quite simply a lot harder. I have no idea how to set up 
      # anything here. Like, seriously, I tried, but as of the day I tried, 
      # there's simply no way that this is actually going to work. Evertything
      # That I think should be standard is not, so I can't even get close to making the 
      # API call. My guess is that I cannot use the same credentials that are being
      # used for the shiny Desktop version, which is dumb. This is the only setup,
      # but not much I can do without being able to authorize anything.
      
      # source("/Volumes/Front/Adam/Shiny8/scripts/googleAuth.R")
      # 
      # trial <- function(){
      #   doAuth()
      # noquote(doAuth()[["credentials"]][["c.id"]])
      # noquote(doAuth()[["credentials"]][["c.secret"]])
      # noquote(doAuth()[["credentials"]][["auth.developerToken"]])
      # }
      # 
      # trial()
      
    })
    
  })
  
  observeEvent(input$problems, {
    
    # Some of this can get harder to read than it probably should. Ultimately, having these notifications are kind of nice on 
    # the ui side of things, so it's important to know how to use them, and if a possible error could occur, where to place them.
    # While I didn't know this at the time of scripting, you are able to have the functions non-embedded, which makes everything
    # so much easier because
    
    file_to_read <- isolate(input$file)
    
    # if(is.null(file_to_read)){
    #   
    #   return()
    #   
    # }
    
    # browser()
    
    if(is.null(file_to_read)) {
      
      confirmSweetAlert(session = session,
                        inputId = "no_magic_file",
                        title = "Please upload 1 or 2 datasets!",
                        btn_labels = "OK!",
                        type = "warning",
                        danger_mode = T)
      
    }else{
      
      if(nrow(input$file) > 2) {
        
        confirmSweetAlert(session = session,
                          inputId = "too_many_magic_file",
                          title = "Program can only handle 1 or 2 datasets!",
                          btn_labels = "OK!",
                          type = "warning",
                          danger_mode = T)
        
      }else{
        
        # browser
        
        if(is.na(input$skip)) {
          
          skip_value <- 0
          
        }else{
          
          skip_value <- input$skip
          
        }
    
    data1 <<- isolate(try(read.csv(isolate(input$file)[[1, "datapath"]], skip = skip_value, sep = input$deliminator, encoding = input$encode), silent = T))
    data2 <<- isolate(try(read.csv(isolate(input$file)[[2, "datapath"]], skip = skip_value, sep = input$deliminator, encoding = input$encode), silent = T))
    
    # browser()
    
    if(!is.na(input$magic_keep) & !((nrow(input$file) == 2 & (class(data1) == "try-error" | class(data2) == "try-error")) | (nrow(input$file) == 1 & class(data1) == "try-error"))){
      
      # browser()
  
        slice_number <- input$magic_keep - skip_value - 1
        
        if(nrow(input$file) == 2){
          
          if((class(data1) != "try-error") | (class(data2) != "try-error")) {
            # browser()
        data1 <- data1 %>% 
          slice(1:slice_number)
        data1 <- as.data.frame(data1)
        data1 <<- isolate(data1)
        
        data2 <- data2 %>% 
          slice(1:slice_number)
        data2 <- as.data.frame(data2)
        data2 <<- isolate(data2)
        
          }
        
        }else{
          
          if((class(data1) != "try-error")) {
            
          # browser()
            
          data1 <- data1 %>% 
            slice(1:slice_number)
          data1 <- as.data.frame(data1)
          data1 <<- isolate(data1)
          
        }}
        
      
      
    }
    
    # browser()
    
    # browser()
    
    # if(nrow(input$file) == 1 & !(str_detect(data1[1], "empty") | !str_detect(data2[1], "empty"))) {
    
    # browser()
    
      if(nrow(input$file) == 1 & class(data1) != "try-error" | class(data2) != "try-error") {

      data_set <<- isolate(data1)
      
      updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                           choices = colnames(data_set))
      
      updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                           choices = colnames(data_set))
      
      updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                           choices = colnames(data_set))

    }
    
    
    if(!exists("data1")|!exists("data2")) {
      
      # browser()
      
      if(str_detect(data1[1], "more columns than column names")| str_detect(data2[1], "more columns than column names")){
        # browser()
        confirmSweetAlert(session = session, 
                          inputId = "bad_file",
                          type = "warning",
                          title = "There are some lines in one or both of these files that need to be skipped. If you are
                          unsure of how many, please check your files.",
                          btn_labels = "OK!",
                          danger_mode = T)
      }else{
        
        # browser()
        
      }
    }else{
      
      # browser()
      if((nrow(input$file) == 2 & (class(data1) == "try-error" | class(data2) == "try-error")) | (nrow(input$file) == 1 & class(data1) == "try-error")) {
      
        # browser()
        
        confirmSweetAlert(session = session,
                          inputId = "bad_error_file",
                          type = "warning",
                          title = "There was some error reading the file. Check to see if it is the desired file with desired settings!",
                          btn_labels = "OK!", 
                          danger_mode = T)
     
      }else{
        
        if(str_detect(data1[1], "empty") | str_detect(data2[1], "empty")){
          
          
          confirmSweetAlert(session = session,
                            inputId = "empty_file",
                            type = "warning",
                            title = "You have uploaded an empty file so the file will not show!",
                            btn_labels = "OK!", 
                            danger_mode = T)
          
        }else{
      
      confirmSweetAlert(session = session,
                        inputId = "good_file",
                        type = "success",
                        title = "This file can be used to do what you please!",
                        btn_labels = "OK!", 
                        danger_mode = T)
      
      updateSelectizeInput(session = session, inputId = "join_prop", label = "Join by which column(s)?", 
                           choices = intersect(colnames(data1), colnames(data2)), selected = NULL)
      
    }}
    }}}
  })
  
  
  
  observeEvent(input$combine_activate, {
    
    # browser()
    
    if(length(input$file) > 0) {
      
      if(nrow(input$file) > 2) {
        
        confirmSweetAlert(session = session,
                          inputId = "combine_more_than_two",
                          type = "warning",
                          title = "Merges can only handle 2 spreadsheets!",
                          btn_labels = "OK!",
                          danger_mode = T)
        
      }else{
      
      if((nrow(input$file) == 2 & (class(data1) == "try-error" | class(data2) == "try-error")) | (nrow(input$file) == 1 & class(data1) == "try-error")) {

        confirmSweetAlert(session = session,
                          inputId = "combine_empty",
                          type = "warning",
                          title = "There was an error merging the file. Make sure that your file and configurations are set as desired!",
                          btn_labels = "OK!",
                          danger_mode = T)

      }else{
      
      if(input$combine_choice == "rbindlist" & nrow(input$file) > 1) {
        
        # browser()
        
        data_set <- try(rbind(data1, data2))
        
        if(class(data_set) == "try-error" | class(data1) == "try-error" | class(data2) == "try-error") {
          
          confirmSweetAlert(session = session, 
                            inputId = "rbind_mess",
                            type = "error",
                            title = "These two sheets do not have the same columns. Try a different operation maybe?",
                            btn_labels = "OK!",
                            danger_mode = T)
          
          # updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
          #                      choices = NULL)
          # 
          # updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
          #                      choices = NULL)
          # 
          # updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
          #                      choices = NULL)
          
          
        }else{
          
          confirmSweetAlert(session = session, 
                            inputId = "rbind_success",
                            type = "success",
                            title = "Successful rBind!",
                            btn_labels = "OK!",
                            danger_mode = T)
          
        }
        
      }
      
      if(input$combine_choice == "left_join" & nrow(input$file) > 1){
        
        data_set <<- try(left_join(data1, data2, by = input$join_prop))
        
        # View(data_set)
        
        if(class(data_set) == "try-error") {
          
          confirmSweetAlert(session = session, 
                            inputId = "ljoin_failure",
                            type = "error",
                            title = "Oh no! Those files do not seem to have any common columns!",
                            btn_labels = "OK!",
                            danger_mode = T)
          
          
        }else{
          
          if(is.null(input$join_prop)) {
            
            confirmSweetAlert(session = session,
                              inputId = "leftj_no_column",
                              type = "warning",
                              title = "There were no columns selected, so spreadsheets are joined by
                              all common columns",
                              btn_labels = "OK!",
                              danger_mode = T)
            
          }else{
            
            confirmSweetAlert(session = session, 
                              inputId = "leftj_success", 
                              type = "success", 
                              title = "Sucessful Left Join!",
                              btn_labels = "OK!",
                              danger_mode = T)
          }
          
        }
        
      }
      
      if(input$combine_choice == "cbindlist" & nrow(input$file) > 1) {
        
        # browser()
        
        data_set <<- try(cbind(data1, data2))
        
        if(class(data_set) == "try-error") {
          
          confirmSweetAlert(session = session, 
                            inputId = "cbind_failure",
                            type = "error",
                            title = "There are not the same number of rows in the data sets!",
                            btn_labels = "OK!",
                            danger_mode = T)
          
        }else{
          
          if(!is.na(input$magic_keep)) {
            
            # browser()
            
            confirmSweetAlert(session = session, 
                              inputId = "cbind_success_keep",
                              type = "success",
                              title = "Successful cBind!",
                              text = "A set number of rows were selected, so cbind works but may not be relevant. Proceed with caution!",
                              btn_labels = "OK!",
                              danger_mode = T)            
          }else{
            
            # browser()
          
          confirmSweetAlert(session = session, 
                            inputId = "cbind_success",
                            type = "success",
                            title = "Successful cBind!",
                            # text = "If you had a predefined number of rows, cBind will always work. Proceed with caution!",
                            btn_labels = "OK!",
                            danger_mode = T)
            
          }
          
        }
        
        
      }
      
      if(input$combine_choice == "right_join" & nrow(input$file) > 1){
        
        data_set <<- try(right_join(data1, data2, by = input$join_prop))
        
        if(class(data_set) == "try-error") {
          
          confirmSweetAlert(session = session, 
                            inputId = "rjoin_failure",
                            type = "error",
                            title = "Oh no! Those files do not seem to have any common columns!",
                            btn_labels = "OK!",
                            danger_mode = T)
        }else{
          
          if(is.null(input$join_prop)) {
            
            confirmSweetAlert(session = session,
                              inputId = "rightj_no_column",
                              type = "warning",
                              title = "There were no columns selected, so spreadsheets are joined by
                              all common columns",
                              btn_labels = "OK!",
                              danger_mode = T)
            
          }else{
            
            confirmSweetAlert(session = session, 
                              inputId = "rightj_success", 
                              type = "success", 
                              title = "Sucessful Right Join!",
                              btn_labels = "OK!",
                              danger_mode = T)
          }
        }
      }
      
      if(input$combine_choice == "inner_join" & nrow(input$file) > 1){
        
        data_set <<- try(inner_join(data1, data2, by = input$join_prop))
        
        if(class(data_set) == "try-error") {
          
          confirmSweetAlert(session = session, 
                            inputId = "innerjoin_failure",
                            type = "error",
                            title = "Oh no! Those files do not seem to have any common columns!",
                            btn_labels = "OK!",
                            danger_mode = T)
        }else{
          
          if(is.null(input$join_prop)) {
            
            confirmSweetAlert(session = session,
                              inputId = "innerj_no_column",
                              type = "warning",
                              title = "There were no columns selected, so spreadsheets are joined by
                              all common columns",
                              btn_labels = "OK!",
                              danger_mode = T)
            
          }else{
            
            confirmSweetAlert(session = session, 
                              inputId = "innerj_success", 
                              type = "success", 
                              title = "Sucessful Inner Join!",
                              btn_labels = "OK!",
                              danger_mode = T)
          }
        }
        
        
      }
      
      if(input$combine_choice == "full_join" & nrow(input$file) > 1) {
        
        data_set <<- try(full_join(data1, data2, by = input$join_prop))
        
        if(class(data_set) == "try-error") {
          
          confirmSweetAlert(session = session, 
                            inputId = "fjoin_failure",
                            type = "error",
                            title = "Oh no! Those files do not seem to have any common columns!",
                            btn_labels = "OK!",
                            danger_mode = T)
        }else{ 
          
          if(is.null(input$join_prop)) {
            
            confirmSweetAlert(session = session,
                              inputId = "fjoin_no_column",
                              type = "warning",
                              title = "There were no columns selected, so spreadsheets are joined by
                              all common columns",
                              btn_labels = "OK!",
                              danger_mode = T)
            
          }else{
            
            confirmSweetAlert(session = session, 
                              inputId = "fjoin_success", 
                              type = "success", 
                              title = "Sucessful Full Join!",
                              btn_labels = "OK!",
                              danger_mode = T)
          }
        }
        
        
        # }else{
        #   
        #   
        #   browser()
        #   # So now this is the only outcome, which is supposed to be an impossible outcome. 
        #   # Who'd a thunk? 
        #   
        #   # It's because the else statement is connected to several pieces
        #   
        #   confirmSweetAlert(session = session, 
        #                     inputId = "what",
        #                     type = "error",
        #                     title = "Wow...you managed to click a radiobutton that didn't exist.
        #                     At least I'm here to congratulate you on your special day.",
        #                     btn_labels = "Congrats!",
        #                     danger_mode = T)
        
      }
      
      if(nrow(input$file) == 1) {
        
        confirmSweetAlert(session = session, 
                          inputId = "one_set_warning", 
                          type = "warning", 
                          title = "You need more than 1 data set to make a join!",
                          btn_labels = "OK!",
                          danger_mode = T)
        
      }}}
    }else{
      
      # if(is.null(input$file)) {
      
      confirmSweetAlert(session = session, 
                        inputId = "no_set_warning", 
                        type = "warning", 
                        title = "You need to upload 2 data sets to make a join!",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }
    
    if(class(data_set) == "try-error" | class(data1) == "try-error" | class(data2) == "try-error") {
    
    updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                         choices = NULL)
    
    updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                         choices = NULL)
    
    updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                         choices = NULL)
    
    }else{
      
      updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                           choices = colnames(data_set))
      
      updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                           choices = colnames(data_set))
      
      updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                           choices = colnames(data_set))
      
    }
    
  })
  
  observeEvent(input$combine_activate, {
    
    if(is.null(input$file)){
      
      confirmSweetAlert(session = session, 
                        inputId = "empty_selectize",
                        title = "Please upload a file first!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    # }
    }
    
  })
  
  observeEvent(input$cleanup_update, {
    
    # browser()
    
    if(class(data_set) == "try-error") {
      
      confirmSweetAlert(session = session, 
                        inputId = "different_kinds_cleanup",
                        title = "Files cannot be merged. Cleanup is impossible!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
    
   
    
    
    if(is.null(data1) | is.null(data2)) {
      
      confirmSweetAlert(session = session, 
                        inputId = "cleanup_no_file",
                        title = "Please upload 1 or 2 files first!", 
                        type = "warning", 
                        btn_labels = "OK!", 
                        danger_mode = T)
      
    }else{
      
      # browser()
      
      if(is.null(input$select_selectize) & is.null(input$filter_selectize) & is.null(input$arrange_selectize) & !is.null(data_set)) {
        
        # browser()

        confirmSweetAlert(session = session,
                          inputId = "cleanup_no_select",
                          title = "Please choose an operation!",
                          type = "warning",
                          btn_labels = "OK!",
                          danger_mode = T)

      }else{
      
      if(!((!is.null(input$select_selectize) & !is.null(input$filter_selectize))|
         (!is.null(input$select_selectize) & !is.null(input$arrange_selectize))|
         (!is.null(input$arrange_selectize) & !is.null(input$filter_selectize)))) {
        
        # browser()
      
      if(is.null(data_set)) {
        
        if(class(data1) == "try-error"){
          
          confirmSweetAlert(session = session, 
                            inputId = "cleanup_no_data",
                            title = "No operations can be selected on this datset!",
                            type = "warning",
                            btn_labels = "OK!", 
                            danger_mode = T)
          
        }else{
        
        confirmSweetAlert(session = session, 
                          inputId = "cleanup_no_merge",
                          title = "Please merge datasets first!",
                          type = "warning",
                          btn_labels = "OK!", 
                          danger_mode = T)
          
        }
        
      }else{
        
        if((nrow(input$file) == 2 & (class(data1) == "try-error" | class(data2) == "try-error")) | (nrow(input$file) == 1 & class(data1) == "try-error")) {
          
          confirmSweetAlert(session = session, 
                            inputId = "empty_cleanup",
                            title = "Upload resulted in an error. Cleanup operations cannot be performed!",
                            type = "warning",
                            btn_labels = "OK!",
                            danger_mode = T)
          
        }else{
        
        if(!is.null(input$arrange_selectize)) {
          
          if(input$arrange_selectize %in% colnames(data_set))
            
            # browser()
            
            selected_column <- grep(paste0("^", noquote(input$arrange_selectize), "$"), colnames(data_set))
          # selected_column_name <- colnames(data_set[selected_column[1]])
          
          data_set <- data_set[order(data_set[selected_column], decreasing = input$arrange_checkbox),]
          # data_set <- data_set[data_set[selected_column] != "",] 
          data_set <<- data_set
          
          updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                               choices = colnames(data_set))
          
          updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                               choices = colnames(data_set))
          
          updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                               choices = colnames(data_set))
          
          confirmSweetAlert(session = session,
                            inputId = "arrange_success",
                            title = "Dataset arranged as described!",
                            btn_labels = "OK!",
                            type = "success",
                            danger_mode = T)
          
        } 
        
        if(!is.null(input$filter_selectize)) {
          
          # browser()
          
          if(input$filter_toggle == F) {
            
            # data_set <- data_set[data_set[selected_column] != input$filter_text]
            
            selected_column <- grep(paste0("^", noquote(input$filter_selectize), "$"), colnames(data_set))
            
            # browser()
            
            data_set <- data_set[data_set[selected_column] != input$filter_text,]
            
            # data_set <- data_set[!str_detect(data_set[selected_column], input$filter_text),]
            
            data_set <<- data_set
            
            updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                                 choices = colnames(data_set))
            
            updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                                 choices = colnames(data_set))
            
            updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                                 choices = colnames(data_set))
            
            updateTextInput(session = session, inputId = "filter_text", label = "What would you like to filter?", value = "")
            
            confirmSweetAlert(session = session,
                              inputId = "filter_remove_success",
                              title = "Data Filtered as Described!",
                              type = "success",
                              btn_labels = "OK!",
                              danger_mode = T)
            
          }else{
            
            selected_column <- grep(paste0("^", noquote(input$filter_selectize), "$"), colnames(data_set))
            data_set <- data_set[data_set[selected_column] == input$filter_text,]
            data_set <<- data_set
            
            updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                                 choices = colnames(data_set))
            
            updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                                 choices = colnames(data_set))
            
            updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                                 choices = colnames(data_set))
            
            updateTextInput(session = session, inputId = "filter_text", label = "What would you like to filter?", value = "")
            
            confirmSweetAlert(session = session,
                              inputId = "filter_keep_success",
                              title = "Data Filtered as Described!",
                              type = "success",
                              btn_labels = "OK!",
                              danger_mode = T)
            
          }
          
        } 
        
        if(!is.null(input$select_selectize)) {
          
          # browser()
          
          selected_columns <- which(colnames(data_set) %in% input$filter_selectize)
          data_set <- data_set[, input$select_selectize]
          data_set <<- data_set
          
          updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                               choices = colnames(data_set))
          
          updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                               choices = colnames(data_set))
          
          updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                               choices = colnames(data_set))
          
          
          confirmSweetAlert(session = session,
                            inputId = "select_success",
                            title = "Selected Columns Successfully!",
                            type = "success",
                            btn_labels = "OK!",
                            danger_mode = T)
        }
        
      
      
      }                 }
    }else{
      
      confirmSweetAlert(session = session, 
                        inputId = "more_than_one_entry",
                        title = "Please only make one change at a time!", 
                        type = "warning", 
                        danger_mode = T, 
                        btn_labels = "OK!")
      
    }}
    }
    updateTextInput(session = session, inputId = "filter_text", label = "What would you like to filter?", value = "")
    
    
    
    }
  })
  
  observeEvent(input$real_table, {
    
    input$real_table
    
    if(class(data_set) != "try-error") {
      
      if(!is.null(input$file)) {
        
        if(nrow(input$file) < 3) {
          
          if(!is.null(data_set)){
            
            # isolate(input$real_table)
            
            # browser()
            
            input$real_table
            
            data_set[data_set == "\x89\xf6_"] <- NA
            data_set <<- data_set
            
            # reactive_data_set <<- isolate(reactive(data_set))
            
            # reactive_data_set()
            
          }
          
        }
        
      }
      
    }
    
    output$selected_table <- isolate(renderTable({
      
    data_set
      
    }))
    
    if(class(data_set) == "try-error") {
      
      confirmSweetAlert(session = session, 
                        inputId = "try_error_view",
                        title = "Files cannot merge, so viewing is not possible!", 
                        type = "warning", 
                        danger_mode = T, 
                        btn_labels = "OK!")
      
    }else{
    
    if(is.null(isolate(input$file))) {
      
      confirmSweetAlert(session = session, 
                        inputId = "no_view_file",
                        title = "Please save uploaded file first!", 
                        type = "warning", 
                        danger_mode = T, 
                        btn_labels = "OK!")
      
    }else{
      
      if(nrow(input$file) > 2) {
        
        confirmSweetAlert(session = session, 
                          inputId = "view_more_than_two",
                          title = "Program cannot merge 3 files, so they cannot be viewed!", 
                          type = "warning", 
                          danger_mode = T, 
                          btn_labels = "OK!")
        
      }else{
      
      if(is.null(data_set)) {
        
        if(class(data1) == "try-error" | class(data2) == "try-error"){
          
          confirmSweetAlert(session = session, 
                            inputId = "empty_file",
                            title = "File upload resulted in an error. Choose different file and/or check configurations!", 
                            type = "warning", 
                            danger_mode = T, 
                            btn_labels = "OK!")
          
        }else{
        
        confirmSweetAlert(session = session, 
                          inputId = "multiple_no_merge",
                          title = "There appears to be several files. Please merge before viewing!", 
                          type = "warning", 
                          danger_mode = T, 
                          btn_labels = "OK!")
          
        }
        
      }else{
  
        confirmSweetAlert(session = session, 
                          inputId = "view_success",
                          title = "File ready to View!", 
                          type = "success", 
                          danger_mode = T, 
                          btn_labels = "OK!")
        
      }
      
    }}
    
    # if(is.null(data_set)) {
    #   
    #   confirmSweetAlert(session = session, 
    #                     inputId = "magic_view_alert",
    #                     text = "There is no data set to view!",
    #                     type = "warning",
    #                     btn_labels = "OK!",
    #                     danger_mode = T)
    #   
    # }else{
    #   
    #   "success"
    #   
    # }
    }
  })
  
  reactive_data <- reactive({data_set})
  
  output$download <- downloadHandler(
    
    # browser(),
    
    filename = ifelse(input$download_name == "", "Magic Spreadsheet.csv", paste0(input$download_name, ".csv")),
    
    content = function(file) {
      
      write.csv(data_set, file, row.names = F)
      
    }
    
  )
  
  # browser()
    
  # download_text <- reactive({input$magic_download_name})
  
  output$download <- downloadHandler(
    
    # Not too difficult, allows the breakout file to be named if there is an entry, and skips it if it does 
    # not, and then downloads everything.
    
    filename = function() {
      ifelse(input$magic_download_name == "", "Magic Spreadsheet.csv", paste0(input$magic_download_name, ".csv"))
    },
    
    content = function(file) {
      write.csv(reactive_data(), file, row.names = F)
    }
  )
  
    # output$download <- downloadHandler(
    #   
    #   # browser(),
    #   
    #   filename = function() {
    #     paste0(ifelse(input$magic_download_name == "", "Magic Spreadsheet.csv", paste0(input$magic_download_name, "")))
    #     },
    #   
    #   content = function(file) {
    #     
    #     write.csv(reactive_data(), file, row.names = F)
    #     
    #   }
    #   
    # )    
  
  
}

shinyApp(ui = ui, server = server)
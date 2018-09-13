# Adwords API Call, Google Sheets do not work. Not honestly sure if I should
# try to make sheets work since its API restrictions renders it ineffective.
# I can't figure out the Adwords API call yet, but if I can, that would be a 
# decent addition.

# This is ultimately the life-blood of this whole script

library(shiny)

# Probably useless at the time being

library(shinyjs)

# Other tools that otherwise would make some neat functions not work.

library(shinydashboard)
library(dplyr)
library(stringr)
library(DT)
library(data.table)
library(shinyjqui)
library(shinyWidgets)

# I can't get the auto authorization to work without this option.

options(httr_oob_default = T)
library(googleAnalyticsR)

# browser()

# I don't think I use any relevant things from this either.
# Even if I did, I should be using the packages officer and
# FlexTable. The important slides (VOC Experimental and Bing Slides)
# only use the current officer package. Migrating away from the ReporteRs
# package and the VOC tab in would be wonderful, needed additions.

# I will keep this in in the off chance that VOC Plus fails and something is needed,
# but I don't intend on that happening. This is an outdated program and nothing will be
# (from this point forward) coded with it, and many other important programs (like VOC Plus)
# rewrote everything so they were up to date. Don't use this package or in general the packages
# associated with it.

library(ReporteRs)

# It's time to start learning these bad boys

# They're not difficult and all relevant programs in here are reworked with them.

library(officer)
library(flextable)

# Both are used for API calls

# I never figured this one out. I think I had to personally set something up, but the fact that I got
# as far as Adam in attempting to set up Bing's API (despite both failing) means that I don't think I 
# need to worry too much about accessing everything

# library(RAdwords)

# There isn't anything particularly wrong with RGA, I just wanted to 
# try something different so I wouldn't just copy Adam's / Austin's code
# for analytics API calls. Might do the same for Adowrds API.

# library(RGA)

# Below is likely unnecessary because the google sheets API is ridiculously slow.
# Took over a half hour to input a datatable with ~10,000 entries. I sunset the tab, so there's no reason
# to actually keep this program in the script

# library(googlesheets)

# This is particularly for the call files and lead exports which tend to be rather large

options(shiny.maxRequestSize = 30 * 1024^2)

# I need this because I want the selectize option in the UI for API calls.

# client_csv <- read.csv("~/Desktop/Rob Scripts/Reference Files/client_website_ga_id.csv")
# client_csv <- client_csv %>% 
#   filter(str_detect(UA, "UA"))

# This file isn't particularly updated with correct corresponding http:// or https:// starters,
# so i just remove them initially so they actually match up.

# client_csv$name <- gsub("^.*\\/\\/", "", client_csv$name)

# For the Magic Spreadsheet component, I make update selectize based off other inputs, but that 
# still requires an initial value. I could directly set the choices equal to NULL, but having them
# exist first is a decent thing to do anyway. I should also rename these so they aren't general.

# These are just to insure that nothing crashes from the "[object] not found" error. Some others 
# Such as the powerpoints and reference docs are referred to in the ui, so they need to be read in
# immediately. None of those files are particularly large though, so it doesn't take (noticably) 
# anymore time.

data1 <- NULL
data2 <- NULL
data_set <- NULL
date_ranges <- NULL
data_set_names <- NULL
gmb <- NULL
review_tracker <- NULL
gmb_filtered <- NULL
review_tracker_filtered <- NULL
shiny_pptx_selected <- NULL
shiny_removed_qui <- NULL
all_qui_entries <- NULL
qui_slide_info <- NULL
shiny_qui_absent <- NULL
shiny_qui_pptx <- officer::read_pptx("~/Desktop/shinyqui test.pptx")

qui_slide_info <- as.data.frame(layout_summary(shiny_qui_pptx)[, 1])
colnames(qui_slide_info) <- "input.qui_order_order"

# browser()
shiny_pptx_selected <- read.csv("~/Desktop/Slide Order.csv")
shiny_pptx_selected$input.qui_order_order <- as.character(shiny_pptx_selected$input.qui_order_order)
shiny_pptx_selected <- semi_join(shiny_pptx_selected, qui_slide_info)
# if(nrow(shiny_pptx_selected) == 0) {
#   
#   shiny_pptx_selected <- NULL
#   
# }
shiny_removed_qui <- read.csv("~/Desktop/Remove Names.csv", stringsAsFactors = F)
colnames(shiny_removed_qui) <- "input.qui_order_order"
shiny_removed_qui <- semi_join(shiny_removed_qui, qui_slide_info)


# browser()
# shiny_removed_qui$input.qui_order_order <- as.character(shiny_removed_qui$input.qui_order_order)

# if(nrow(shiny_removed_qui) == 0) {
#   
#   shiny_removed_qui <- NULL
#   
# }

# browser()

if(nrow(shiny_removed_qui) == 0 | nrow(shiny_pptx_selected) == 0) {
  
  if(nrow(shiny_removed_qui) == 0 & nrow(shiny_pptx_selected) == 0) {
    
    all_qui_entries <- NULL
    
  }else{
    
    if(nrow(shiny_removed_qui) == 0) {
      
      # browser()
      
      all_qui_entries <- shiny_pptx_selected
      shiny_pptx_selected$input.qui_order_order <- as.character(shiny_pptx_selected$input.qui_order_order)
      
      
    }else{
      
      # browser()
      
      all_qui_entries <- shiny_removed_qui
      shiny_removed_qui$input.qui_order_order <- as.character(shiny_removed_qui$input.qui_order_order)
      
    }
    
  }
  
}else{

all_qui_entries <- full_join(shiny_removed_qui, shiny_pptx_selected)

}

# browser()


if(is.null(all_qui_entries)) {
  
  shiny_qui_absent <- qui_slide_info
  
}else{

shiny_qui_absent <- anti_join(qui_slide_info, all_qui_entries)

}

if(nrow(shiny_qui_absent) == 0) {
  
  shiny_qui_absent <- NULL
  
  }else{
    
    # browser()
  
  shiny_qui_absent$input.qui_order_order <- as.character(shiny_qui_absent$input.qui_order_order)
  
}

# browser()

ui <- dashboardPage(
  
  dashboardHeader(title = "\nThe\nApp\nWhich\nShall\nNot\nBe\nNamed", titleWidth = 400),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      # This currently generates all of our Bing slides. No API, but it's much better than manually
      # compiling everything
      
      menuItem("Bing Slides Beta", tabName = "bing", icon = icon("database")),
      
      # This is a tab that shows and allows downloads for the breakout of corporate zipcodes
      
      menuItem("Corporate Zipcode Breakdown", tabName = "corporate", icon = icon("steam")),
      
      # Allows for numbers from Dialog Tech to be found and exported very quickly and nicely
      
      menuItem("Dialog Numbers", tabName = "dialog", icon = icon("gamepad")),
      
      # This allows the leads to be filtered either by phone number or website
      
      menuItem("Export Wrangling", tabName = "wrangle", icon = icon("book")),
      
      # It seemed more important when I made it, but its still a worthwhile app, it's just not as helpful
      # as Bing Slides or VOC Plus for instance.
      
      menuItem("Magic Spreadsheet", tabName = "magic", icon = icon("magic"),
               menuSubItem("Upload Properties", tabName = "magic_upload"),
               menuSubItem("Join Options", tabName = "magic_join"),
               menuSubItem("Spreadsheet Cleanup", tabName = "magic_cleanup"),
               menuItem("Edit & View Table", tabName = "magic_edit_view",
               # menuItem("View", tabName = "magic_view",
                           menuSubItem("View", tabName = "magic_view"),
                           menuSubItem("Edit Column Names", tabName = "magic_colnames"),
                           menuSubItem("Edit Table", tabName = "magic_edit"),
                           menuSubItem("Remove Items", tabName = "magic_remove")), 
               menuSubItem("Download", tabName = "magic_download")),
      
      # Currently a misnomer, since generally speaking all of the bot traffic is actually through 
      # direct traffic, not organic, but it doesn't really matter since everything is done the same
      # way.
      
      menuItem("Organic Bounce Rates", tabName = "bounce", icon = icon("bolt")),
      
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
      
      # This is a demo to show my idea for calling and ordering slides for shiny. It's much more basic, which
      # very well may mean that pushing it to shiny would be unfeasible (more than likely).
      
      menuItem("Shiny Qui Order Input", tabName = "qui", icon = icon("question")),
      
      # For local reporting, we need to generate slides for VOC, and its a total pain to find,
      # so I just wanted it put in a place I know I won't lose it.
      
      # I am happy to say that this is not something that we really need to use anymore. After a few cycles
      # of local reporting, I am probably going to sunset this as well. It's very barebones and VOC Plus is 
      # much much more helpful.
      
      menuItem("VOC Beta", tabName = "voc", icon = icon("bomb")),
      
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
      
      # Nothing has been discussed about this since its inception, so I'm getting the feeling that it is no longer something
      # that is particularly worth much value. Sunset 8/28/2018
      
      # menuItem("Paid Search", tabName = "paid_search", icon = icon("cog")),
      
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
      
      # This is one of the most helpful tabs encountered in this app. Before, the olds script used outdated programs and had horrible
      # naming conventions (although that's not a particular reason to toss a functioning script). All in all, this was built in an 
      # effort to automate the VOC slides which took a fairly long time to actually merge, but now if the association doc is updated,
      # everything should run. Even if it is not, there are tools to interact with the script to dynamically update the associations doc.
      # Anyone using this should give me a big "thank you" because this will save like 2 days of work and now takes like 5 minutes, max.
      
      menuItem("VOC Plus", tabName = "voc_exp_all", icon = icon("cube"),
               menuSubItem("VOC Upload / Generate Slides", tabName = "voc_all"),
               menuSubItem("View Differences", tabName = "voc_all_differences"),
               menuSubItem("VOC Association Edit", tabName = "voc_all_edit"),
               menuSubItem("VOC Association Add", tabName = "voc_all_add"),
               menuSubItem("VOC Association Delete", tabName = "voc_all_delete")),
      
      # On occassion we will be asked about inquiry zipcode breakouts, which isn't very common, but it 
      # would take like half an hour or so to finalize, and this is just easier since its now generalized
      
      menuItem("Zipcode Breakout", tabName = "zipcode", icon = icon("empire"))
      
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
              
              titlePanel("VOC Beta"),
              
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
      
      
      # tabItem(tabName = "paid_search",
      #         
      #         # Just a leads export and a download button
      #         
      #         titlePanel("Paid Search"),
      #         
      #         sidebarPanel(
      #           
      #           fileInput(inputId = "paid_search_file", label = "Input export file"),
      #           downloadButton(outputId = "paid_search_download")
      #           
      #         )),
      
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
                selectizeInput(inputId = "magic_file_order", label = "Select file to be read in first:", choices = NULL, selected = NULL),
                textOutput("magic_order_text"),
                numericInput(inputId = "skip", label = "Number of lines you want to skip in first dataset (optional)", value = "", min = 0),
                numericInput(inputId = "magic_keep", label = "What is the last row you wish to keep in first dataset? (optional)", value = "", min = 0)
                ),
                # radioButtons(inputId = "deliminator", label = "Deliminator", choices = c(Comma = ",", Tab = "\t", Space = " ", Semicolon = ";"), selected = ","),
                
              sidebarPanel(
              
                numericInput(inputId = "skip2", label = "Number of lines you want to skip in second dataset (optional)", value = "", min = 0),
                numericInput(inputId = "magic_keep2", label = "What is the last row you wish to keep in second dataset? (optional)", value = "", min = 0),
                radioButtons(inputId = "encode", label = "Encoding", choices = c("unknown", "UTF-8", "UTF-16", "UTF-32", "latin1"), selected = "unknown")
               
              ),
                
                 column(12, actionButton(inputId = "problems", label = "Save"))
                
              ),
      
      tabItem(tabName = "magic_join",
              
              titlePanel("Join Options"),
              
              sidebarPanel(
                
                radioButtons(inputId = "combine_choice", label = "Join Options", choices = c(rBind = "rbindlist",
                                                                                             cBind = "cbindlist",
                                                                                             "Left Join" = "left_join",
                                                                                             # "Right Join" = "right_join",
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
              )),
      
      tabItem(tabName = "magic_edit",
              
              titlePanel("Edit Table"),
              
              # actionButton(inputId = "update_magic_edit", label = "Update all tables"),
              dataTableOutput("magic_edit_table")
              
              ),
      
      tabItem(tabName = "magic_remove",
              
              titlePanel("Remove Items From Table"),
              
              actionButton(inputId = "magic_remove_activate", label = "Delete Selected Rows"),
              # actionButton(inputId = "update_magic_remove", label = "Update all tables"),
              dataTableOutput("magic_remove_table")
              # selectizeInput(inputId = "magic_remove_selectize", label = "Choose Rows to remove:", choices = NULL, multiple = T),
              # actionButton(inputId = "magic_remove_activate", label = "Delete Selected Rows")
              
              ),
      
      tabItem(tabName = "magic_colnames",
              
              titlePanel("Edit Column Names"),
              
              # actionButton(inputId = "update_magic_colnames", label = "Update all tables"),
              dataTableOutput("magic_colnames_table")),
      
      tabItem(tabName = "bing",
              
              titlePanel("Bing Slide Generation"),
              h4("Make sure you are signed into Kserve first!"),
              
              fileInput(inputId = "bing_file", label = "Please insert collective bing keyword file"),
              prettyToggle(inputId = "bing_performance_toggle",
                           label_on = "Include Performance Slide",
                           icon_on = icon("check"),
                           status_on = "success",
                           status_off = "danger",
                           label_off = "Exclude Performance Slide",
                           icon_off = icon("remove")),
              conditionalPanel(condition = "input.bing_performance_toggle == true",
                               fileInput(inputId = "bing_full_export", label = "Please upload current dealer inquiry file"),
                               fileInput(inputId = "bing_prior_export", label = "Please upload prior year dealer inquiry file")),
              selectizeInput(inputId = "bing_campaign", label = "Please input the campaign you want:", choices = NULL, selected = NULL, multiple = T),
              textInput(inputId = "bing_daterange", label = "Please input the date range of reporting:", value = ""),
              actionButton(inputId = "bing_generate", label = "Generate Slide(s)")),
      
      tabItem(tabName = "voc_all",
              
              titlePanel("VOC Plus"),
              # h4("Please use with caution!"),
              
              fileInput(inputId = "exp_voc_file", label = "Please insert GMB and ReviewTracker Files:", multiple = T),
              textInput(inputId = "exp_voc_date_range", label = "Date Range for VOC Slides:"),
              actionButton(inputId = "exp_voc_gen", label = "Generate VOC Slides")),
      
      tabItem(tabName = "voc_all_edit",
              
              titlePanel("Edit VOC Association Doc"),
              
              dataTableOutput("voc_all_edit_table")),
      
      tabItem(tabName = "voc_all_add",
              
              titlePanel("Add Entry to Association Doc"),
              
              sidebarPanel(
              
              # textInput(inputId = "voc_store_code", label = "Please input Corporate ID:"),
              # textInput(inputId = "voc_business_name", label = "Please input EXPLICIT GMB name:"),
              # textInput(inputId = "voc_location_name", label = "Please input EXPLICIT ReviewTracker name:"),
              # textInput(inputId = "voc_address", label = "Please input EXPLICIT GMB Address:"),
              # textInput(inputId = "voc_location_address", label = "Please input EXPLICIT ReviewTracker name:"),
              
              textInput(inputId = "voc_store_code", label = "Please input Corporate ID:"),
              selectizeInput(inputId = "voc_business_name", label = "Please input GMB name:", choices = NULL, selected = NULL, multiple = T),
              selectizeInput(inputId = "voc_location_name", label = "Please input ReviewTracker name:", choices = NULL, selected = NULL, multiple = T),
              selectizeInput(inputId = "voc_address", label = "Please input GMB Address:", choices = NULL, selected = NULL, multiple = T),
              selectizeInput(inputId = "voc_location_address", label = "Please input ReviewTracker address:", choices = NULL, selected = NULL, multiple = T),
              
              actionButton(inputId = "voc_add_rbind", label = "Add Row To Associations Doc")
              
              ),
              
              tableOutput("voc_all_add_table")
              
              ),
      
      tabItem(tabName = "voc_all_delete",
              
              titlePanel("Remove Entry from Assoication Doc"),
              
              h4("Use with caution. This feature should be rarely or never used."),
              
              actionButton(inputId = "voc_delete_row", label = "Delete Rows"),
              dataTableOutput("voc_all_delete_table")
              
              ),
      
      tabItem(tabName = "voc_all_differences",
              
              titlePanel("Unmatched Businesses"),
              
              actionButton(inputId = "voc_gmb_differences", label = "Show GMB Differences"),
              actionButton(inputId = "voc_review_differences", label = "Show ReviewTracker Differences"),
              textOutput("voc_difference_text"),
              tableOutput("voc_difference_table")
              # tableOutput("voc_review_difference_table")
              ),
      
      tabItem(tabName = "qui",

              titlePanel("This is an ordering test"),
              
              # orderInput(inputId = "qui_slides", label = "Slides", items = layout_summary(shiny_qui_pptx)[, 1],
              #            as_source = F, connect = c("qui_order", "qui_remove")),

              # This stuff is kind of neat. Before anything, I required a few joins which would let R know which columns
              # should remain where. The order Input is just kind of an interesting ui tool though, because while it looks cool,
              # it's actually intuitive for ordering things, which is what the aim of calling specific slides actually is.
             fluidRow(
               column(4,
              orderInput(inputId = "qui_slides", label = "Slides", items = shiny_qui_absent$input.qui_order_order,
                         as_source = F, connect = c("qui_order", "qui_remove"))),
              column(6,
              actionBttn(inputId = "qui_confirm", label = "Confirm Order & Removal", style = "fill")
               )),
              
              fluidRow(
                
              # column(1, 
              # orderInput(inputId = "qui_order", "Items to order:", items = NULL, placeholder = "Drag items here...", connect = c("qui_remove", "qui_slides"), width = "50px")),
              
              column(1,
              orderInput(inputId = "qui_order", "Items to order:", items = shiny_pptx_selected$input.qui_order_order, placeholder = "Drag items here...", connect = c("qui_remove", "qui_slides"), width = "50px")),
              
              column(1,
              orderInput(inputId = "qui_remove", "Items to remove:", items = shiny_removed_qui$input.qui_order_order, placeholder = "Drag items here...", connect = c("qui_order", "qui_slides"), width = "50px"))
      
              # column(3, NULL)
              )#,
              
              # actionButton(inputId = "qui_confirm", label = "Confirm Order & Removal")
              
              )
      
    )))

server <- function(input, output, session) {
  
  # browser()
  
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
    
    if(input$corpID == "" & input$corp_dealer == "") {
      
      confirmSweetAlert(session = session,
                        inputId = "no_corp_input",
                        title = "Please input a corporate ID or Co-op Group!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
    
    isolate(input$corp_view)
    # isolate(input$corpID)
    # isolate(input$corp_dealer)
    
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
    
    if(nrow(data_table) == 0) {
      
      if(input$corpID != "" & input$corp_dealer != ""){
        
        confirmSweetAlert(session = session,
                          inputId = "not_same_group",
                          title = "The Corporate ID and Co-op Name do not match!",
                          text = "All Co-op names must be capitalized",
                          type = "warning",
                          btn_labels = "OK!",
                          danger_mode = T)
        
        
        
      }else{
        
        if(input$corp_dealer != ""){
          
          confirmSweetAlert(session = session, 
                            inputId = "wrong_dealer_name_corp",
                            title = paste0(input$corp_dealer, " is not in the database!"),
                            text = "If you are sure it is, make sure the Co-op name is capitalized",
                            type = "warning",
                            btn_labels = "OK!",
                            danger_mode = T)
          
        }else{
          
          confirmSweetAlert(session = session,
                            inputId = "wrong_corp_id",
                            title = "This corporate ID is not in the database!",
                            type = "warning",
                            btn_labels = "OK!",
                            danger_mode = T)
          
        }
        
      }
      
      data_table <- NULL
      
    }else{
      
      confirmSweetAlert(session = session,
                        inputId = "success_corp_zip",
                        title = paste0(nrow(data_table), " zipcode(s) associated to Co-op!"),
                        type = "success",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }
    
    updateTextInput(session = session, inputId = "corpID", label = "Corporate ID", value = "")
    updateTextInput(session = session, inputId = "corp_dealer", label = "Dealer Name", value = "")
    
    output$corp_table <- renderTable({
    
    data_table
      
    })
    
  }
    
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
  
  observeEvent(input$file, {
    
    # browser()
    
    if(nrow(input$file) > 2){
      
      confirmSweetAlert(session = session,
                        inputId = "file_input_too_much",
                        title = "Please only input 1 or 2 datasets!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
      # updateSelectizeInput(session = session, inputId = "magic_file_order", label = "Select file to be read in first:", 
      #                      choices = NULL)
      # 
      # updateSelectizeInput(session = session, inputId = "skip", label = "Number of lines you want to skip in first dataset (optional)", 
      #                      choices = "")
      # 
      # updateSelectizeInput(session = session, inputId = "skip2", label = "Number of lines you want to skip in second dataset (optional)", 
      #                      choices = "")
      # 
      # updateSelectizeInput(session = session, inputId = "magic_keep", label = "What is the last row you wish to keep in first dataset? (optional)", 
      #                      choices = "")
      # 
      # updateSelectizeInput(session = session, inputId = "magic_keep2", label = "What is the last row you wish to keep in second dataset? (optional)", 
      #                      choices = "")
      
    }else{
    
    updateSelectizeInput(session = session, inputId = "magic_file_order", label = "Select file to be read in first:", 
                         choices = input$file$name)
    }
  })
  
  observeEvent(input$magic_file_order, {
    
    # browser()
    
    if(length(input$file) > 0){
    
    output$magic_order_text <- renderText(paste0(input$magic_file_order, " will be the first data set selected!"))
    
    }
    
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
        
        if(is.na(input$skip2)) {
          
          skip_value2 <- 0
          
        }else{
          
          skip_value2 <- input$skip2
          
        }
        
        # browser()
        
        if(input$magic_file_order == file_to_read$name[1]) {
          
          # browser()
          
          data1 <<- isolate(try(read.csv(isolate(input$file)[[1, "datapath"]], skip = skip_value, encoding = input$encode, stringsAsFactors = F), silent = T))
          data2 <<- isolate(try(read.csv(isolate(input$file)[[2, "datapath"]], skip = skip_value2, encoding = input$encode, stringsAsFactors = F), silent = T))
          
        }else{
          
          data1 <<- isolate(try(read.csv(isolate(input$file)[[2, "datapath"]], skip = skip_value, encoding = input$encode, stringsAsFactors = F), silent = T))
          data2 <<- isolate(try(read.csv(isolate(input$file)[[1, "datapath"]], skip = skip_value2, encoding = input$encode, stringsAsFactors = F), silent = T))
          
        }
        
        # browser()
    
    # data1 <<- isolate(try(read.csv(isolate(input$file)[[1, "datapath"]], skip = skip_value, sep = input$deliminator, encoding = input$encode, stringsAsFactors = F), silent = T))
    # data2 <<- isolate(try(read.csv(isolate(input$file)[[2, "datapath"]], skip = skip_value, sep = input$deliminator, encoding = input$encode, stringsAsFactors = F), silent = T))
    
    if(class(data1) != "try-error") {
      
      # browser()
      
      if(any(str_detect(colnames(data1), "X\\.*\\d*")) == T){

      data1 <<- data1[,colSums(is.na(data1)) < nrow(data1)]

      }
      
    }else{

      data1 <<- data1

    }

    if(class(data2) != "try-error") {

      if(any(str_detect(colnames(data2), "X\\.*\\d*")) == T){
      
      data2 <<- data2[,colSums(is.na(data2)) < nrow(data2)]

      }
      
    }else{

      data2 <<- data2

    }
    
    if(!is.na(input$magic_keep) & !((nrow(input$file) == 2 & (class(data1) == "try-error" | class(data2) == "try-error")) | (nrow(input$file) == 1 & class(data1) == "try-error"))){
      
      # browser()
  
        slice_number <- input$magic_keep - skip_value - 1
        slice_number2 <- input$magic_keep2 - skip_value2 - 1
        
        if(nrow(input$file) == 2){
          
          if((class(data1) != "try-error") | (class(data2) != "try-error")) {
            # browser()
        data1 <- data1 %>% 
          slice(1:slice_number)
        data1 <- as.data.frame(data1)
        data1 <<- isolate(data1)
        
        data2 <- data2 %>% 
          slice(1:slice_number2)
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
                           choices = colnames(data_set), selected = NULL)

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
      
      # if(input$combine_choice == "right_join" & nrow(input$file) > 1){
      #   
      #   data_set <<- try(right_join(data1, data2, by = input$join_prop))
      #   
      #   if(class(data_set) == "try-error") {
      #     
      #     confirmSweetAlert(session = session, 
      #                       inputId = "rjoin_failure",
      #                       type = "error",
      #                       title = "Oh no! Those files do not seem to have any common columns!",
      #                       btn_labels = "OK!",
      #                       danger_mode = T)
      #   }else{
      #     
      #     if(is.null(input$join_prop)) {
      #       
      #       confirmSweetAlert(session = session,
      #                         inputId = "rightj_no_column",
      #                         type = "warning",
      #                         title = "There were no columns selected, so spreadsheets are joined by
      #                         all common columns",
      #                         btn_labels = "OK!",
      #                         danger_mode = T)
      #       
      #     }else{
      #       
      #       confirmSweetAlert(session = session, 
      #                         inputId = "rightj_success", 
      #                         type = "success", 
      #                         title = "Sucessful Right Join!",
      #                         btn_labels = "OK!",
      #                         danger_mode = T)
      #     }
      #   }
      # }
      
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
                         choices = NULL, selected = NULL)
    
    }else{
      
      updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                           choices = colnames(data_set))
      
      updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                           choices = colnames(data_set))
      
      updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                           choices = colnames(data_set), selected = NULL)
      
    }
    
    updateSelectizeInput(session = session, inputId = "join_prop", label = "Join by which column(s)?", 
                         choices = intersect(colnames(data1), colnames(data2)), selected = NULL)
    
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
          
          if(length(input$arrange_selectize) > 1) {
            
            confirmSweetAlert(session = session,
                              inputId = "cleanup_over_one_arrange",
                              title = "Please only select one arrange entry!",
                              text = "If you need to arrange by more than 1 column, alert Rob.",
                              type = "warning",
                              btn_labels = "OK!",
                              danger_mode = T)
            
          }else{
          
          if(input$arrange_selectize %in% colnames(data_set)){
            
            # browser()
            
            
            # column_selection <- as.data.frame(colnames(data_set))
            # selected_column <- grepl(paste0("^", noquote(input$arrange_selectize), "$"), colnames(data_set))
          # selected_column_name <- colnames(data_set[selected_column[1]])
            
            # browser()
            data_set <- data_set[order(data_set[input$arrange_selectize], decreasing = input$arrange_checkbox),]
            # data_set <- data_set[order(data_set[selected_column], decreasing = input$arrange_checkbox),]
            # data_set <- data_set[data_set[selected_column] != "",] 
          data_set <<- data_set
          
          updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                               choices = colnames(data_set))
          
          updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                               choices = colnames(data_set))
          
          updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                               choices = colnames(data_set), selected = NULL)
          
          confirmSweetAlert(session = session,
                            inputId = "arrange_success",
                            title = "Dataset arranged as described!",
                            btn_labels = "OK!",
                            type = "success",
                            danger_mode = T)
          
        }}
          
        } 
        
        if(!is.null(input$filter_selectize)) {
          
          # browser()
          
          if(length(input$filter_selectize) > 1){

            confirmSweetAlert(session = session,
                              inputId = "over_2_filter_selectize",
                              title = "Please only select one column!",
                              btn_labels = "OK!",
                              type = "warning",
                              danger_mode = T)

          }else{
          
          if(input$filter_toggle == F) {
            
            # data_set <- data_set[data_set[selected_column] != input$filter_text]
            
            selected_column <- grep(paste0("^", noquote(input$filter_selectize), "$"), colnames(data_set))
            
            # browser()
            
            if(input$filter_text == ""){
              
              data_set <- data_set[data_set[selected_column] != input$filter_text, ]
              data_set <- data_set[!is.na(data_set[selected_column]), ]
              
              
            }else{
              
            data_set <- data_set[data_set[selected_column] != input$filter_text,]
            
            }
            
            # data_set <- data_set[!str_detect(data_set[selected_column], input$filter_text),]
            
            data_set <<- data_set
            
            updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                                 choices = colnames(data_set))
            
            updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                                 choices = colnames(data_set))
            
            updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                                 choices = colnames(data_set), selected = NULL)
            
            updateTextInput(session = session, inputId = "filter_text", label = "What would you like to filter?", value = "")
            
            confirmSweetAlert(session = session,
                              inputId = "filter_remove_success",
                              title = "Data Filtered as Described!",
                              type = "success",
                              btn_labels = "OK!",
                              danger_mode = T)
            
          }else{
            
            selected_column <- grep(paste0("^", noquote(input$filter_selectize), "$"), colnames(data_set))
            
            if(input$filter_text == ""){
              
              data_set <- data_set[data_set[selected_column] == input$filter_text, ]
              data_set <- data_set[is.na(data_set[selected_column]), ]
              
              
            }else{
              
              data_set <- data_set[data_set[selected_column] == input$filter_text,]
              
            }
            
            data_set <<- data_set
            
            updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                                 choices = colnames(data_set))
            
            updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                                 choices = colnames(data_set))
            
            updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                                 choices = colnames(data_set), selected = NULL)
            
            updateTextInput(session = session, inputId = "filter_text", label = "What would you like to filter?", value = "")
            
            confirmSweetAlert(session = session,
                              inputId = "filter_keep_success",
                              title = "Data Filtered as Described!",
                              type = "success",
                              btn_labels = "OK!",
                              danger_mode = T)
            
            # browser()
            
          }
        }
        } 
        
        if(!is.null(input$select_selectize)) {
          
          # browser()
          
          selected_columns <- which(colnames(data_set) %in% input$select_selectize)
          data_set <- data_set[, input$select_selectize, drop = F]
          data_set <<- data_set
          
          updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                               choices = colnames(data_set))
          
          updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                               choices = colnames(data_set))
          
          updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                               choices = colnames(data_set), selected = NULL)
          
          confirmSweetAlert(session = session,
                            inputId = "select_keep_success",
                            title = "Data selected as Described!",
                            type = "success",
                            btn_labels = "OK!",
                            danger_mode = T)
          
       
        }
        
      
      
        }                 }
        
        if(input$real_table[1] > 0){
          
          data_set_names <- colnames(data_set)
          data_set_names <- as.data.frame(data_set_names)
          # browser()
          colnames(data_set_names) <- "Column Names"
          # browser()
          data_set_names$`Column Names` <- as.character(data_set_names$`Column Names`)
          data_set_names <<- data_set_names
          
          output$magic_colnames_table <- renderDataTable(data_set_names, selection = "none", editable = T, rownames = F)
          
          output$magic_edit_table <- renderDataTable(data_set, selection = "none", editable = T, rownames = F)
          
          output$magic_remove_table <- renderDataTable(data_set, rownames = F)
          
          output$selected_table <- isolate(renderTable({data_set}))
          
          if(!is.null(input$filter_selectize) & length(input$filter_selectize) == 1){
            
            confirmSweetAlert(session = session,
                              inputId = "update_table_cleanup_filter_success",
                              title = "Successfully filtered data!",
                              text = "Tables are updated in view tabs",
                              type = "success",
                              btn_labels = "OK!",
                              danger_mode = T)
            
          }
          
          if(!is.null(input$arrange_selectize)) {
            
            if(length(input$arrange_selectize) > 1) {
            
            confirmSweetAlert(session = session,
                              inputId = "update_table_cleanup_arrange_failure",
                              title = "Please only input one arrange value!",
                              text = "If you need to arrange more than 1 value, alert Rob.",
                              type = "warning",
                              btn_labels = "OK!",
                              danger_mode = T)
              
            }else{
              
              confirmSweetAlert(session = session,
                                inputId = "update_table_cleanup_arrange_success",
                                title = "Successfully arranged data!",
                                text = "Tables are updated in view tabs",
                                type = "success",
                                btn_labels = "OK!",
                                danger_mode = T)
              
            }
            
          }
          
          if(!is.null(input$select_selectize)) {
            
            data_set_names <- colnames(data_set)
            data_set_names <- as.data.frame(data_set_names)
            # browser()
            colnames(data_set_names) <- "Column Names"
            # browser()
            data_set_names$`Column Names` <- as.character(data_set_names$`Column Names`)
            data_set_names <<- data_set_names
            
            output$magic_colnames_table <- renderDataTable(data_set_names, selection = "none", editable = T, rownames = F)
            
            output$magic_edit_table <- renderDataTable(data_set, selection = "none", editable = T, rownames = F)
            
            output$magic_remove_table <- renderDataTable(data_set, rownames = F)
            
            output$selected_table <- isolate(renderTable({data_set}))
            
            confirmSweetAlert(session = session,
                              inputId = "select_success",
                              title = "Selected Columns Successfully!",
                              type = "success",
                              btn_labels = "OK!",
                              danger_mode = T)
            
            confirmSweetAlert(session = session,
                              inputId = "update_table_cleanup_select_success",
                              title = "Successfully selected columns!",
                              text = "Tables are updated in view tabs",
                              type = "success",
                              btn_labels = "OK!",
                              danger_mode = T)
            
          }
          
        }
        
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
    updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                         choices = colnames(data_set))
    
    updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                         choices = colnames(data_set))
    
    updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                         choices = colnames(data_set), selected = NULL)
    
    
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
    
    # browser() 
    
    data_set_names <- colnames(data_set)
    data_set_names <- as.data.frame(data_set_names)
    # browser()
    colnames(data_set_names) <- "Column Names"
    # browser()
    data_set_names$`Column Names` <- as.character(data_set_names$`Column Names`)
    data_set_names <<- data_set_names
    
    output$magic_edit_table <- renderDataTable(data_set, selection = "none", editable = T, rownames = F)
    
    output$magic_remove_table <- renderDataTable(data_set, rownames = F)
    
    output$magic_colnames_table <- renderDataTable(data_set_names, selection = "none", editable = T, rownames = F)
    
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
  
  
  
  observeEvent(input$magic_colnames_table_cell_edit, {
    
    if(input$magic_colnames_table_cell_edit$value == "") {
      
      colname_proxy <- dataTableProxy("magic_colnames_table")
      colname_info <- input$magic_colnames_table_cell_edit
      colname_row <- colname_info$row
      # colname_column <- colname_info$col
      # colname_value <- colname_info$value
      # data_set_value <- data_set_names[colname_row, 1]
      data_set_names[colname_row, 1] <<- DT:::coerceValue(data_set_names[colname_row, 1], data_set_names[colname_row, 1])
      replaceData(colname_proxy, data_set_names, resetPaging = F)
      
      output$magic_colnames_table <- renderDataTable(data_set_names, selection = "none", editable = T, rownames = F)
      
      output$magic_edit_table <- renderDataTable(data_set, selection = "none", editable = T, rownames = F)
      
      output$magic_remove_table <- renderDataTable(data_set, rownames = F)
      
      output$selected_table <- isolate(renderTable({data_set}))
      
      updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                           choices = colnames(data_set))
      
      updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                           choices = colnames(data_set))
      
      updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                           choices = colnames(data_set), selected = NULL)
      
      confirmSweetAlert(session = session,
                        inputId = "no_edit_column_entry",
                        title = "You need a name for a column!",
                        type = "warning", 
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
    
    # browser()
    colname_proxy <- dataTableProxy("magic_colnames_table")
    colname_info <- input$magic_colnames_table_cell_edit
    colname_row <- colname_info$row
    colname_column <- colname_info$col
    colname_value <- colname_info$value
    data_set_value <- data_set_names[colname_row, 1]
    data_set_names[colname_row, 1] <<- DT:::coerceValue(colname_value, data_set_names[colname_row, 1])
    replaceData(colname_proxy, data_set_names, resetPaging = F)
    
    # browser()
    
    output$magic_colnames_table <- renderDataTable(data_set_names, selection = "none", editable = T, rownames = F)
    
    new_data_set_names <- data_set_names[, 1]
    colnames(data_set) <- new_data_set_names
    data_set <<- data_set
    
    output$magic_edit_table <- renderDataTable(data_set, selection = "none", editable = T, rownames = F)

    output$magic_remove_table <- renderDataTable(data_set, rownames = F)

    output$selected_table <- isolate(renderTable({data_set}))
    
    updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                         choices = colnames(data_set))
    
    updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                         choices = colnames(data_set))
    
    updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                         choices = colnames(data_set), selected = NULL)
    
    confirmSweetAlert(session = session, 
                      inputId = "col_edit_success",
                      title = "Column name change successful!",
                      text = "Check other view tabs to confirm",
                      type = "success",
                      btn_labels = "OK!", 
                      danger_mode = T)
    }
  })
  
  observeEvent(input$magic_edit_table_cell_edit, {
    
    if(input$magic_edit_table_cell_edit$value == "") {
      
      edit_proxy <- dataTableProxy("magic_edit_table")
      edit_info <- input$magic_edit_table_cell_edit
      edit_row <- edit_info$row
      # colname_column <- colname_info$col
      # colname_value <- colname_info$value
      # data_set_value <- data_set_names[colname_row, 1]
      data_set[edit_row, 1] <<- DT:::coerceValue(data_set[edit_row, 1], data_set[edit_row, 1])
      replaceData(edit_proxy, data_set, resetPaging = F)
      
      # output$magic_colnames_table <- renderDataTable(data_set_names, selection = "none", editable = T, rownames = F)
      
      output$magic_edit_table <- renderDataTable(data_set, selection = "none", editable = T, rownames = F)
      
      output$magic_remove_table <- renderDataTable(data_set, rownames = F)
      
      output$selected_table <- isolate(renderTable({data_set}))
      
      updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                           choices = colnames(data_set))
      
      updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                           choices = colnames(data_set))
      
      updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                           choices = colnames(data_set), selected = NULL)
      
      confirmSweetAlert(session = session,
                        inputId = "no_edit_column_entry",
                        title = "You need an entry for a cell!",
                        type = "warning", 
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
      
      # browser()
      edit_proxy <- dataTableProxy("magic_edit_table")
      edit_info <- input$magic_edit_table_cell_edit
      edit_row <- edit_info$row
      edit_column <- edit_info$col
      edit_value <- edit_info$value
      data_set_edit_value <- data_set[edit_row, 1]
      data_set[edit_row, 1] <<- DT:::coerceValue(edit_value, data_set[edit_row, 1])
      replaceData(edit_proxy, data_set, resetPaging = F)
      
      # browser()
      
      # output$magic_colnames_table <- renderDataTable(data_set_names, selection = "none", editable = T, rownames = F)
      
      # new_data_set_names <- data_set_names[, 1]
      # colnames(data_set) <- new_data_set_names
      data_set <<- data_set
      
      output$magic_edit_table <- renderDataTable(data_set, selection = "none", editable = T, rownames = F)
      
      output$magic_remove_table <- renderDataTable(data_set, rownames = F)
      
      output$selected_table <- isolate(renderTable({data_set}))
      
      updateSelectizeInput(session = session, inputId = "arrange_selectize", label = "Arrange by which column?", 
                           choices = colnames(data_set))
      
      updateSelectizeInput(session = session, inputId = "filter_selectize", label = "Choose column to filter through:",
                           choices = colnames(data_set))
      
      updateSelectizeInput(session = session, inputId = "select_selectize", label = "Which columns do you want? (Order matters)",
                           choices = colnames(data_set), selected = NULL)
      
      confirmSweetAlert(session = session, 
                        inputId = "col_edit_success",
                        title = "Cell change successful!",
                        text = "Check other view tabs to confirm",
                        type = "success",
                        btn_labels = "OK!", 
                        danger_mode = T)
    }
  })
  
  observeEvent(input$magic_remove_activate, {
    
    # browser()
    
    if(is.null(input$magic_remove_table_rows_selected)) {
      
      confirmSweetAlert(session = session, 
                        inputId = "col_remove_null",
                        title = "Choose row(s) to delete!",
                        # text = "Check other view tabs to confirm",
                        type = "warning",
                        btn_labels = "OK!", 
                        danger_mode = T)
      
    }else{
      
      # browser()
      
      if(nrow(data_set) == length(input$magic_remove_table_rows_selected)){
        
        confirmSweetAlert(session = session, 
                          inputId = "col_remove_full",
                          title = "Don't delete your entire dataset!",
                          # text = "Check other view tabs to confirm",
                          type = "warning",
                          btn_labels = "OK!", 
                          danger_mode = T)
        
      }else{
    
    data_set <- data_set[-input$magic_remove_table_rows_selected,]
    data_set <<- data_set
   
    output$magic_edit_table <- renderDataTable(data_set, selection = "none", editable = T, rownames = F)
    
    output$magic_remove_table <- renderDataTable(data_set, rownames = F)
    
    output$selected_table <- isolate(renderTable({data_set}))
    
    confirmSweetAlert(session = session,
                      inputId = "magic_removal_success",
                      title = "Successfully deleted row(s)!",
                      type = "success",
                      btn_labels = "OK!", 
                      danger_mode = T)

    }}
    
  })
  
  observeEvent(input$bing_file, {
    withProgress(message = "Loading File", value = 0, {
      incProgress(1, detail = "This may take some time...")
    bing_keywords_upload <- try(read.csv(input$bing_file$datapath, skip = 3, stringsAsFactors = F))
    })
    
    if(class(bing_keywords_upload) == "try-error") {
      
      confirmSweetAlert(session = session, 
                        inputId = "keyword_bing_upload_fail",
                        title = "This is not a keyword export!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T0)
      
    }else{
    
    bing_keywords_upload <<- bing_keywords_upload
    
    bing_keyword_columns <- c("Campaign", "Keyword", "Clicks", "Clicks..Compare.to.")
    
    # browser()
    
    if(all(bing_keyword_columns %in% colnames(bing_keywords_upload))) {
    
    bing_keywords <- bing_keywords_upload %>% 
      select(Keyword, Campaign, Clicks, Impr., Clicks..Compare.to., Impr...Compare.to. ) %>% 
      filter(str_detect(Campaign, "Culligan|culligan")) %>% 
      filter(Impr. != 0) %>% 
      select(Campaign, Keyword, Clicks, Clicks..Compare.to.)
    
    bing_keywords <<- bing_keywords
    
    choices <- as.data.frame(bing_keywords$Campaign)
    choices <- choices %>% 
      distinct()
    
    # browser()
    
    choices$`bing_keywords$Campaign` <- as.character(choices$`bing_keywords$Campaign`)
    
    bing_campaign_options <<- rbind(choices, "All Campaigns")
    colnames(bing_campaign_options) <<- "Select Campaigns (all campaigns also available!)"
    
    # browser()
    
    
    choices <- gsub("^Culligan\\s|\\s\\(.*$|\\sGrouped|\\sMetro", "", choices$`bing_keywords$Campaign`)
    
    # browser()
    
    updateSelectizeInput(session = session, inputId = "bing_campaign", label = "Please input the campaign you want:",
                         choices = bing_campaign_options$`Select Campaigns (all campaigns also available!)`, selected = NULL)
    }else{
      
      bing_keywords_upload <<- NULL
      
      confirmSweetAlert(session = session,
                        inputId = "wrong_bing_keyword_file",
                        title = "This is not a Bing keyword export!",
                        text = "If you are sure it is, make sure you have all relevant columns.",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
    }
    }
  })
  
  observeEvent(input$bing_full_export, {
    
    withProgress(message = "Loading Export", value = 0, {
      
      incProgress(1, detail = "This may take some time...")
      
      bing_lead_export <- try(read.csv(input$bing_full_export$datapath))
      if(class(bing_lead_export) == "try-error") {
        
        confirmSweetAlert(session = session,
                          inputId = "lead_import_bing_fail",
                          title = "There was an issue reading in the file!",
                          text = "This is likely not a raw export.",
                          type = "warning",
                          btn_labels = "OK!",
                          danger_mode = T)
        
      }else{
        
        export_names <- read.csv("~/Desktop/Rob Scripts/Reference Files/Export Names.csv")
        
        export_names <- export_names$names
        
        if(all(export_names %in% colnames(bing_lead_export))) {
          
          bing_lead_export <<- bing_lead_export
          
        }else{
          
          bing_lead_export <<- NULL
          
          confirmSweetAlert(session = session,
                            inputId = "not_current_bing_export",
                            title = "This is not a lead export!",
                            type = "warning",
                            btn_labels = "OK!",
                            danger_mode = T)
          
        }
        
      }
      
      
    })
    
    
  })
  
  observeEvent(input$bing_prior_export, {
    
    withProgress(message = "Loading Export", value = 0, {
      
      incProgress(1, detail = "This may take some time...")
      
      bing_lead_export_prior <- try(read.csv(input$bing_prior_export$datapath))
      
      if(class(bing_lead_export_prior) == "try-error") {
        
        confirmSweetAlert(session = session,
                          inputId = "lead_import_bing_fail2",
                          title = "There was an issue reading in the file!",
                          text = "This is likely not a raw export.",
                          type = "warning",
                          btn_labels = "OK!",
                          danger_mode = T)
        
      }else{
        
        export_names <- read.csv("~/Desktop/Rob Scripts/Reference Files/Export Names.csv")
        
        export_names <- export_names$names
        
        if(all(export_names %in% colnames(bing_lead_export_prior))) {
          
          bing_lead_export_prior <<- bing_lead_export_prior
          
        }else{
          
          bing_lead_export_prior <<- NULL
          
          confirmSweetAlert(session = session,
                            inputId = "not_current_bing_export2",
                            title = "This is not a lead export!",
                            type = "warning",
                            btn_labels = "OK!",
                            danger_mode = T)
          
        }
        
      }
    
      
    })
    
    
  })
  
  # bing_keywords <- read.csv("~/Desktop/bing keywords.csv", skip = 3)
  # 
  # bing_keywords <- bing_keywords %>% 
  #   select(Keyword, Campaign, Clicks, Impr., Clicks..Compare.to., Impr...Compare.to. ) %>% 
  #   filter(str_detect(Campaign, "Culligan|culligan")) %>% 
  #   filter(Impr. != 0) %>% 
  #   select(Campaign, Keyword, Clicks)
  
  
  
  observeEvent(input$bing_generate, {
    
    # browser()
    
    if(class(bing_lead_export) == "try-error" | class(bing_lead_export_prior) == "try-error" | class(bing_keywords_upload) == "try-error" | is.null(bing_keywords_upload) |
     (input$bing_performance_toggle == T & is.null(bing_lead_export)) | (input$bing_performance_toggle == T & is.null(bing_lead_export_prior))) {

      confirmSweetAlert(session = session,
                        title = "Something is wrong with the files!",
                        text = "No powerpoints will be made",
                        inputId = "no_bing_pptx",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
    
    if(is.null(input$bing_file)){
      
      confirmSweetAlert(session = session,
                        inputId = "bing_no_file",
                        title = "Please input keyword export!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
    
    if(is.null(input$bing_campaign)) {
      
      confirmSweetAlert(session = session,
                        inputId = "bing_no_campaign",
                        title = "Please select a campaign!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
      
      if(input$bing_daterange == "" |!str_detect(input$bing_daterange, ".*-.*_.*")) {
        
        # browser()
        
        if(input$bing_daterange == "") {
        
        confirmSweetAlert(session = session,
                          inputId = "bing_no_daterange",
                          title = "Please input date range!",
                          type = "warning",
                          btn_labels = "OK!",
                          danger_mode = T)
          
        }else{
          
          confirmSweetAlert(session = session,
                            inputId = "bing_wrong_daterange",
                            title = "Date format not recognized!",
                            text = "Input date ranges in the format [first period]-[second period]_[year].",
                            type = "warning",
                            btn_labels = "OK!",
                            danger_mode = T)
          
        }
        
      }else{
        
        if((is.null(input$bing_full_export)|is.null(input$bing_prior_export)) & input$bing_performance_toggle == T){
          
          confirmSweetAlert(session = session, 
                            inputId = "no_bing_export_file",
                            title = "Please input a comprehensive export!",
                            type = "warning",
                            btn_labels = "OK!",
                            danger_mode = T)
          
        }else{
    
        # browser()
        
    # bing_keywords <- bing_keywords %>% 
    #   select(Keyword, Campaign, Clicks, Impr.) %>% 
    #   filter(str_detect(Campaign, "Culligan|culligan")) %>% 
    #   filter(Impr. != 0) %>% 
    #   select(Campaign, Keyword, Clicks)
      # View(bing_keywords)
      # browser()
        
      bing_campaign_selection <- input$bing_campaign
      
      if("All Campaigns" %in% bing_campaign_selection) {
        
        # browser()
        
        bing_campaign_selection <- bing_campaign_options %>% 
          filter(`Select Campaigns (all campaigns also available!)` != "All Campaigns")
        
        bing_campaign_selection <- bing_campaign_selection$`Select Campaigns (all campaigns also available!)`
        
        # browser()
        
      }else{
        
        bing_campaign_selection <- bing_campaign_selection
        
      }
      
      # browser()
      
      bing_website <- gsub("^.*\\(|\\).*$", "", bing_campaign_selection)
      
      
      if(length(bing_campaign_selection) > 1) {
        
        # confirmSweetAlert(session = session,
        #                   inputId = "bing_slide_download_success",
        #                   title = "Powerpoint download setup successful!",
        #                   text = "It may take a monent for the powerpoints to download.",
        #                   type = "success",
        #                   btn_labels = "OK!", 
        #                   danger_mode = T)
        
        withProgress(message = "Downloading Powerpoints", value = 0, {
        
        for(i in 1:length(bing_campaign_selection)){
          
          incProgress(1/length(bing_campaign_selection), detail = paste0("Downloading ", i, " of ", length(bing_campaign_selection)))
        
        # browser()
          
          reporting_period <- gsub("_.*", "", input$bing_daterange)
          current_year <- gsub(".*-.*_", "", input$bing_daterange)
          current_year <- as.integer(current_year)
          previous_year <- current_year - 1
          current_year_text <- paste0(reporting_period, " ", current_year, " ", bing_website[i])
          previous_year_text <- paste0(reporting_period, " YOY ", bing_website[i])
          pptx_name <<- gsub("^Culligan\\s|\\s\\(.*$|\\sGrouped|\\sMetro", "", bing_campaign_selection[i]) 
          
          bing_keywords_prior_year <- bing_keywords %>% 
            filter(Campaign == bing_campaign_selection[i]) %>% 
            select(Keyword, Clicks..Compare.to.) %>% 
            filter(Clicks..Compare.to. > 0)
          
       if(nrow(bing_keywords_prior_year) > 0) {

        bing_keywords_current_year <- bing_keywords %>% 
          filter(Campaign == bing_campaign_selection[i]) %>% 
          select(Keyword, Clicks) %>% 
          # arrange(-Clicks) %>% 
          filter(Clicks > 0) 
        
        current_title <- paste0("Top Keywords ", current_year)
        previous_title <- paste0("Top Keywords ", previous_year)
        
        bing_keywords_current_year$Keyword <- gsub("[[:punct:]]|^\\s", "", bing_keywords_current_year$Keyword)
        bing_keywords_current_year$Keyword <- str_to_lower(bing_keywords_current_year$Keyword)
        
        bing_keywords_current_year <- bing_keywords_current_year %>% 
          group_by(Keyword) %>% 
          summarise(Clicks = sum(Clicks)) %>% 
          arrange(-Clicks) %>%
          slice(1:20) 
        
        colnames(bing_keywords_current_year) <- c("Top Keywords Current Period", "Clicks")
        
        bing_keywords_prior_year$Keyword <- gsub("[[:punct:]]|^\\s", "", bing_keywords_prior_year$Keyword)
        bing_keywords_prior_year$Keyword <- str_to_lower(bing_keywords_prior_year$Keyword)
        
        bing_keywords_prior_year <- bing_keywords_prior_year %>% 
          group_by(Keyword) %>% 
          summarize(Clicks = sum(Clicks..Compare.to.)) %>%
          arrange(-Clicks) %>% 
          slice(1:20) 
        
        colnames(bing_keywords_prior_year) <- c("Top Keywords Prior Period", "Clicks")
        
       }else{
            
         bing_keywords_current_year <- bing_keywords %>% 
           filter(Campaign == bing_campaign_selection[i]) %>% 
           select(Keyword, Clicks) %>% 
           # arrange(-Clicks) %>% 
           filter(Clicks > 0) 
         
         bing_keywords_current_year$Keyword <- gsub("[[:punct:]]|^\\s", "", bing_keywords_current_year$Keyword)
         bing_keywords_current_year$Keyword <- str_to_lower(bing_keywords_current_year$Keyword)
         
         bing_keywords_current_year <- bing_keywords_current_year %>% 
           group_by(Keyword) %>% 
           summarise(Clicks = sum(Clicks)) %>% 
           arrange(-Clicks) %>%
           slice(1:20) 
         
         bing_keywords_prior_year$Keyword <- gsub("[[:punct:]]|^\\s", "", bing_keywords_prior_year$Keyword)
         bing_keywords_prior_year$Keyword <- str_to_lower(bing_keywords_prior_year$Keyword)
         
         bing_keywords_prior_year <- bing_keywords_prior_year %>% 
           group_by(Keyword) %>% 
           summarize(Clicks = sum(Clicks..Compare.to.)) %>%
           arrange(-Clicks) %>% 
           slice(1:20) 
         
          }
     
        # browser()
        
        current_keyword_flextable <- regulartable(bing_keywords_current_year) %>% theme_zebra() %>% 
          bg(bg = "#0A3C6E", part = "header") %>% fontsize(i = 1, part = "header", size = 12) %>% 
          fontsize(size = 10, part = "body") %>% color(i = 1, part = "header", color = "white") %>% 
          bold(part = "header") %>% width(width = c(2.5, 1.2)) %>% align(align = "left", part = "all", j = 1) %>% 
          font(part = "all", fontname = "Arial") %>% 
          height(part = "body", height = .05)
        
        prior_keyword_flextable <- regulartable(bing_keywords_prior_year) %>% theme_zebra() %>% 
          bg(bg = "#0A3C6E", part = "header") %>% fontsize(i = 1, part = "header", size = 12) %>% 
          fontsize(size = 10, part = "body") %>% color(i = 1, part = "header", color = "white") %>% 
          bold(part = "header") %>% width(width = c(2.5, 1.2)) %>% align(align = "left", part = "all", j = 1) %>% 
          font(part = "all", fontname = "Arial") %>% 
          height(part = "body", height = .05)
      
        bing_powerpoint <<- read_pptx(path = "~/Desktop/Rob Scripts/Reference Files/BingSampleSlide.pptx")
        
        # browser()
        
        # bing_powerpoint <<- bing_powerpoint %>% 
        #   add_slide(layout = "Bing Keyword", master = "Default Theme") %>% 
        #   ph_with_flextable(type = "tbl", value = current_keyword_flextable)%>% 
        #   ph_with_text(type = "body", str = paste0(reporting_period, " ", current_year, " ", bing_website[i]), index = 10)%>% 
        #   ph_with_text(type = "body", str = paste0("-", bing_campaign_selection[i]), index = 1)
        
        pptx_name <- gsub("^Culligan\\s|\\s\\(.*$|\\sGrouped|\\sMetro", "", bing_campaign_selection[i]) 
        
        if(nrow(bing_keywords_prior_year) > 0) {
          
          # browser()
          
          bing_powerpoint <<- bing_powerpoint %>% 
            add_slide(layout = "Bing Keyword YOY", master = "Default Theme") %>% 
            ph_with_flextable(type = "tbl", value = prior_keyword_flextable, index = 1) %>% 
            ph_with_flextable(type = "tbl", value = current_keyword_flextable, index = 2) %>% 
            ph_with_text(type = "body", str = previous_year_text, index = 23) %>% 
            ph_with_text(type = "body", str = paste0("-", bing_campaign_selection[i]), index = 20)
          
        }else{
          
          # pptx_name <- gsub("^Culligan\\s|\\s\\(.*$|\\sGrouped|\\sMetro", "", input$bing_campaign[i]) 
          
          # browser()
          
          bing_powerpoint <<- bing_powerpoint %>% 
            add_slide(layout = "Bing Keyword No YOY", master = "Default Theme") %>% 
            ph_with_flextable(type = "tbl", value = current_keyword_flextable) %>% 
            ph_with_text(type = "body", str =current_year_text, index = 24) %>% 
            ph_with_text(type = "body", str = paste0("-", bing_campaign_selection[i]), index = 10)
          
          # print("There is no data in the prior year keywords. No YOY table will be generated.")
          
          print(paste0("There is no data in the ", pptx_name, " prior year keyword table. No slide for this date range will be generated."))
          
        }
        
        if(input$bing_performance_toggle == T) {


          # for(i in 1:length(bing_campaign_selection)) {
          # if(is.null(input$bing_lead_export)) {
          #
          #   confirmSweetAlert(session = session,
          #                     inputId = "no_lead_export",
          #                     text = "")
          #
          # }
          bing_keywords_campaign_filtered <-  bing_keywords_upload %>%
            filter(Campaign == bing_campaign_selection[i])

          bing_keywords_campaign_filtered <<- bing_keywords_campaign_filtered

          bing_total_clicks <- bing_keywords_campaign_filtered %>%
            # filter(Campaign == input$bing_campaign) %>%
            select(Clicks) %>%
            summarise("Total Clicks" = sum(Clicks))

          bing_total_clicks <- bing_total_clicks[1, 1]
          # bing_total_clicks <- prettyNum(bing_total_clicks)

          bing_prev_total_clicks <- bing_keywords_campaign_filtered %>%
            # filter(Campaign == input$bing_campaign) %>%
            select(Clicks..Compare.to.) %>%
            summarise("Total Clicks" = sum(Clicks..Compare.to.))

          bing_prev_total_clicks <- bing_prev_total_clicks[1, 1]
          bing_prev_click_entry <- paste0(bing_prev_total_clicks, " Previous Year")

          bing_total_cost <- bing_keywords_campaign_filtered %>%
            select(Spend) %>%
            summarise("Total Cost" = sum(Spend))

          bing_total_cost <- bing_total_cost[1, 1]

          bing_prev_total_cost <- bing_keywords_campaign_filtered %>%
            select(Spend..Compare.to.) %>%
            summarise("Total Cost" = sum(Spend..Compare.to.))

          bing_prev_total_cost <- bing_prev_total_cost[1, 1]

          coop_associations <- read.csv("~/Desktop/Rob Scripts/Reference Files/Bing Associations.csv")
          
          # browser()

          coop_associations <- coop_associations %>%
            filter(Bing_Campaign == bing_campaign_selection[i]) %>%
            select(Dealers) %>%
            distinct()

          coop_associations <- as.character(coop_associations$Dealers)
          
          # if(i == 2) {browser()}

          # coop_associations$Coop <- as.character(coop_associations$Coop)
          #
          # exclusive_names <- gsub("Culligan | \\(.*| Metro| Grouped| Water Residential", "", bing_campaign_selection)
          # exclusive_names <- strsplit(exclusive_names, split = ", ")
          #
          # if(class(exclusive_names) == "list"){
          #
          # exclusive_names <- exclusive_names[[1]]
          #
          # }
          #
          # if("GreenBay" %in% exclusive_names){
          #
          #   exclusive_names <- replace(exclusive_names, exclusive_names == "GreenBay", "Green Bay")
          #
          # }
          #
          # if("La Crosse" %in% exclusive_names) {
          #
          #   exclusive_names <- replace(exclusive_names, exclusive_names == "La Crosse", "La Crosse / Eau Claire")
          #
          # }
          #
          # if("Lincoln Kearney" %in% exclusive_names) {
          #
          #   exclusive_names <- replace(exclusive_names, exclusive_names == "Lincoln Kearney", "Lincoln / Hastings / Kearney")
          #
          # }
          #
          # if("Grand Rapids" %in% exclusive_names) {
          #
          #   exclusive_names <- replace(exclusive_names, exclusive_names == "Grand Rapids", "Grand Rapids / Kalamazoo")
          #
          # }
          #
          # if("Scottsbluff" %in% exclusive_names) {
          #
          #   exclusive_names <- replace(exclusive_names, exclusive_names == "Scottsbluff", "Cheyenne/Scottsbluff")
          #
          # }
          #
          # if("Phoenix" %in% exclusive_names) {
          #
          #   exclusive_names <- replace(exclusive_names, exclusive_names == "Phoenix", "Phoenix Co-op")
          #
          # }
          #
          # if("Tucson" %in% exclusive_names) {
          #
          #   exclusive_names <- replace(exclusive_names, exclusive_names == "Tucson", "Tucson Co-op")
          #
          # }
          #
          # if("Milwaukee" %in% exclusive_names) {
          #
          #   exclusive_names <- replace(exclusive_names, exclusive_names == "Milwaukee", "Milwaukee Co-op")
          #
          # }
          #
          # if(exclusive_names == "McCardel") {
          #
          #   # browser()
          #
          #   coop_associations <- coop_associations %>%
          #     filter(str_detect(Dealers, "Alpena")|str_detect(Dealers, "Big Rapids")|str_detect(Dealers, "Petoskey")|str_detect(Dealers, "Ludington")|str_detect(Dealers, "Traverse City"))
          #
          #    coop_associations <- coop_associations %>%
          #     select(Dealers) %>%
          #     distinct()
          #   coop_associations <- coop_associations$Dealers
          #   coop_associations <- as.character(coop_associations)
          #   coop_associations <<- coop_associations
          #
          # }else{
          #
          # coop_associations <- coop_associations[coop_associations$Coop %in% exclusive_names, ]
          # coop_associations <- coop_associations %>%
          #   select(Dealers) %>%
          #   distinct()
          # coop_associations <- coop_associations$Dealers
          # coop_associations <- as.character(coop_associations)
          # coop_associations <<- coop_associations
          #
          # }

          # browser()

          
          bing_lead_export_filtered <- bing_lead_export[bing_lead_export$SentTo %in% coop_associations, ]

          bing_lead_export_filtered <- bing_lead_export_filtered %>%
            filter(str_detect(PageLink, "promobing")|str_detect(PageLink, "promominnb")|IVR.Number == 8779576577|IVR.Number == 8779361556)

          bing_paid_search_contacts <- nrow(bing_lead_export_filtered)

          bing_lead_export_prior_filtered <- bing_lead_export_prior[bing_lead_export_prior$SentTo %in% coop_associations, ]

          bing_lead_export_prior_filtered <- bing_lead_export_prior_filtered %>%
            filter(str_detect(PageLink, "promobing")|str_detect(PageLink, "promominnb")|IVR.Number == 8779576577|IVR.Number == 8779361556)

          bing_paid_search_contacts_prior <- nrow(bing_lead_export_prior_filtered)

          # browser()

          bing_cpc_current <- bing_total_cost / bing_total_clicks * 1.25
          bing_cpc_prior <- bing_prev_total_cost / bing_prev_total_clicks * 1.25

          bing_current_conversion_rate <- bing_paid_search_contacts / bing_total_clicks * 100
          bing_prior_conversion_rate <- bing_paid_search_contacts_prior / bing_prev_total_clicks * 100

          # browser()

          current_clicks_text <- prettyNum(bing_total_clicks, big.mark = ",")
          prior_clicks_text <- paste0(prettyNum(bing_prev_total_clicks, big.mark = ","), " Prior Year")

          current_cpc_text <- paste0("$", round(bing_cpc_current, 2))
          prior_cpc_text <- paste0("$", round(bing_cpc_prior, 2), " Prior Year")

          paid_search_current <- as.character(bing_paid_search_contacts)
          paid_search_prior <- paste0(bing_paid_search_contacts_prior, " Prior Year")

          current_conv_rate_text <- paste0(round(bing_current_conversion_rate, 1), "%")
          prior_conv_rate_text <- paste0(round(bing_prior_conversion_rate, 1), "% Prior Year")
          
          # browser()

          if(bing_prev_total_cost != 0) {

            # browser()

            # bing_campaign_selection
            # current_year_text

            # layout_properties(bing_powerpoint, layout = "Bing Performance YOY", master = "Default Theme") %>% distinct()

            bing_powerpoint <<- bing_powerpoint %>%
              add_slide(layout = "Bing Performance YOY", master = "Default Theme") %>%
              # ph_with_flextable(type = "tbl", value = current_keyword_flextable) %>%
              ph_with_text(type = "body", str = current_year_text, index = 70) %>%
              ph_with_text(type = "body", str = paste0("-", bing_campaign_selection[i]), index = 40) %>%
              ph_with_text(type = "body", str = paid_search_prior, index = 90) %>%
              ph_with_text(type = "body", str = prior_conv_rate_text, index = 96) %>%
              ph_with_text(type = "body", str = current_clicks_text, index = 60) %>%
              ph_with_text(type = "body", str = current_cpc_text, index = 75) %>%
              ph_with_text(type = "body", str = paid_search_current, index = 80) %>%
              ph_with_text(type = "body", str = current_conv_rate_text, index = 65) %>%
              ph_with_text(type = "body", str = prior_clicks_text, index = 84) %>%
              ph_with_text(type = "body", str = prior_cpc_text, index = 85)

          }else{

            # browser()

            # layout_properties(bing_powerpoint, layout = "Bing Performance No YOY", master = "Default Theme") %>% distinct()

            bing_powerpoint <<- bing_powerpoint %>%
              add_slide(layout = "Bing Performance No YOY", master = "Default Theme") %>%
              # ph_with_flextable(type = "tbl", value = current_keyword_flextable) %>%
              ph_with_text(type = "body", str = current_year_text, index = 80) %>%
              ph_with_text(type = "body", str = paste0("-", bing_campaign_selection[i]), index = 43) %>%
              # ph_with_text(type = "body", str = paid_search_prior, index = 45) %>%
              # ph_with_text(type = "body", str = prior_conv_rate_text, index = 50) %>%
              ph_with_text(type = "body", str = current_clicks_text, index = 61) %>%
              ph_with_text(type = "body", str = current_cpc_text, index = 65) %>%
              ph_with_text(type = "body", str = paid_search_current, index = 70) %>%
              ph_with_text(type = "body", str = current_conv_rate_text, index = 75)
            # ph_with_text(type = "body", str = prior_clicks_text, index = 87) %>%
            # ph_with_text(type = "body", str = prior_cpc_text, index = 90)

          }
        }
        
        if(!dir.exists(paste0("/Volumes/Front/Adam/Reporting/Bing Campaigns/", reporting_period, " ", current_year, " Bing Slides/"))){
          
          dir.create(paste0("/Volumes/Front/Adam/Reporting/Bing Campaigns/", reporting_period, " ", current_year, " Bing Slides/"))
          
        }
        
        powerpoint_name <- paste0("/Volumes/Front/Adam/Reporting/Bing Campaigns/", reporting_period, " ", current_year, " Bing Slides/",
                                  pptx_name, " Bing Slides.pptx")
        
        # browser()
        
        print(bing_powerpoint, powerpoint_name)
        
        }
        
        # browser()
        
        # This only works if you have access immediately to the server
        
       
        
        
        # incProgress(1/length(bing_campaign_selection), detail = paste0("Downloading ", i))
        
        # confirmSweetAlert(session = session,
        #                   inputId = "bing_slide_download_success",
        #                   title = "Powerpoint downloads successful!",
        #                   # text = "It may take a monent for the powerpoints to download.",
        #                   type = "success",
        #                   btn_labels = "OK!", 
        #                   danger_mode = T)
        
        # }
          
          confirmSweetAlert(session = session,
                            inputId = "bing_slide_download_success",
                            title = "Powerpoint downloads successful!",
                            # text = "It may take a monent for the powerpoints to download.",
                            type = "success",
                            btn_labels = "OK!",
                            danger_mode = T)
          
        })
      }else{
      
        reporting_period <- gsub("_.*", "", input$bing_daterange)
        current_year <- gsub(".*-.*_", "", input$bing_daterange)
        current_year <- as.integer(current_year)
        previous_year <- current_year - 1
        current_year_text <- paste0(reporting_period, " ", current_year, " ", bing_website)
        previous_year_text <- paste0(reporting_period, " YOY ", bing_website)
        pptx_name <- gsub("^Culligan\\s|\\s\\(.*$|\\sGrouped|\\sMetro", "", bing_campaign_selection) 
        
        bing_keywords_prior_year <- bing_keywords %>% 
          filter(Campaign == bing_campaign_selection) %>% 
          select(Keyword, Clicks..Compare.to.) %>% 
          filter(Clicks..Compare.to. > 0)
        
        if(nrow(bing_keywords_prior_year) > 0) {
          
          bing_keywords_current_year <- bing_keywords %>% 
            filter(Campaign == bing_campaign_selection) %>% 
            select(Keyword, Clicks) %>% 
            # arrange(-Clicks) %>% 
            filter(Clicks > 0) 
          
          current_title <- paste0("Top Keywords ", current_year)
          previous_title <- paste0("Top Keywords ", previous_year)
          
          bing_keywords_current_year$Keyword <- gsub("[[:punct:]]|^\\s", "", bing_keywords_current_year$Keyword)
          bing_keywords_current_year$Keyword <- str_to_lower(bing_keywords_current_year$Keyword)
          
          bing_keywords_current_year <- bing_keywords_current_year %>% 
            group_by(Keyword) %>% 
            summarise(Clicks = sum(Clicks)) %>% 
            arrange(-Clicks) %>%
            slice(1:20) 
          
          colnames(bing_keywords_current_year) <- c("Top Keywords Current Period", "Clicks")
          
          bing_keywords_prior_year$Keyword <- gsub("[[:punct:]]|^\\s", "", bing_keywords_prior_year$Keyword)
          bing_keywords_prior_year$Keyword <- str_to_lower(bing_keywords_prior_year$Keyword)
          
          bing_keywords_prior_year <- bing_keywords_prior_year %>% 
            group_by(Keyword) %>% 
            summarize(Clicks = sum(Clicks..Compare.to.)) %>%
            arrange(-Clicks) %>% 
            slice(1:20) 
          
          colnames(bing_keywords_prior_year) <- c("Top Keywords Previous Period", "Clicks")
          
        }else{
          
          bing_keywords_current_year <- bing_keywords %>% 
            filter(Campaign == bing_campaign_selection) %>% 
            select(Keyword, Clicks) %>% 
            # arrange(-Clicks) %>% 
            filter(Clicks > 0) 
          
          bing_keywords_current_year$Keyword <- gsub("[[:punct:]]|^\\s", "", bing_keywords_current_year$Keyword)
          bing_keywords_current_year$Keyword <- str_to_lower(bing_keywords_current_year$Keyword)
          
          bing_keywords_current_year <- bing_keywords_current_year %>% 
            group_by(Keyword) %>% 
            summarise(Clicks = sum(Clicks)) %>% 
            arrange(-Clicks) %>%
            slice(1:20) 
          
          bing_keywords_prior_year$Keyword <- gsub("[[:punct:]]|^\\s", "", bing_keywords_prior_year$Keyword)
          bing_keywords_prior_year$Keyword <- str_to_lower(bing_keywords_prior_year$Keyword)
          
          bing_keywords_prior_year <- bing_keywords_prior_year %>% 
            group_by(Keyword) %>% 
            summarize(Clicks = sum(Clicks..Compare.to.)) %>%
            arrange(-Clicks) %>% 
            slice(1:20) 
          
        }
      
      current_keyword_flextable <- regulartable(bing_keywords_current_year) %>% theme_zebra() %>% 
        bg(bg = "#0A3C6E", part = "header") %>% fontsize(i = 1, part = "header", size = 12) %>% 
        fontsize(size = 10, part = "body") %>% color(i = 1, part = "header", color = "white") %>% 
        bold(part = "header") %>% width(width = c(2.5, 1.2)) %>% align(align = "left", part = "all", j = 1) %>% 
        font(part = "all", fontname = "Arial") %>% 
        height(part = "body", height = .05)
      
      prior_keyword_flextable <- regulartable(bing_keywords_prior_year) %>% theme_zebra() %>% 
        bg(bg = "#0A3C6E", part = "header") %>% fontsize(i = 1, part = "header", size = 12) %>% 
        fontsize(size = 10, part = "body") %>% color(i = 1, part = "header", color = "white") %>% 
        bold(part = "header") %>% width(width = c(2.5, 1.2)) %>% align(align = "left", part = "all", j = 1) %>% 
        font(part = "all", fontname = "Arial") %>% 
        height(part = "body", height = .05)
      
      reporting_period <- gsub("_.*", "", input$bing_daterange)
      current_year <- gsub(".*-.*_", "", input$bing_daterange)
      current_year <- as.integer(current_year)
      previous_year <- current_year - 1
      current_year_text <- paste0(reporting_period, " ", current_year, " ", bing_website)
      previous_year_text <- paste0(reporting_period, " YOY ", bing_website)
      pptx_name <- gsub("^Culligan\\s|\\s\\(.*$|\\sGrouped|\\sMetro", "", bing_campaign_selection) 

      bing_powerpoint <<- read_pptx(path = "~/Desktop/Rob Scripts/Reference Files/BingSampleSlide.pptx")
      # bing_powerpoint <<- bing_powerpoint %>% 
      #   add_slide(layout = "Bing Keyword", master = "Default Theme") %>% 
      #   ph_with_flextable(type = "tbl", value = current_keyword_flextable) %>% 
      #   ph_with_text(type = "body", str = current_year_text, index = 10) %>% 
      #   ph_with_text(type = "body", str = paste0("-", bing_campaign_selection), index = 1)
      
      # browser()
      
      if(nrow(bing_keywords_prior_year) > 0) {
        
        # browser()
        # # 
        # layout_properties(bing_powerpoint, layout = "Bing Keyword YOY", master = "Default Theme")
        
        bing_powerpoint <<- bing_powerpoint %>% 
          add_slide(layout = "Bing Keyword YOY", master = "Default Theme") %>% 
          ph_with_flextable(type = "tbl", value = prior_keyword_flextable, index = 1) %>% 
          ph_with_flextable(type = "tbl", value = current_keyword_flextable, index = 2) %>% 
          ph_with_text(type = "body", str = previous_year_text, index = 23) %>% 
          ph_with_text(type = "body", str = paste0("-", bing_campaign_selection), index = 20)
        
        # print(bing_powerpoint, "~/Desktop/test.pptx")
        
      }else{
        
        # layout_properties(bing_powerpoint, layout = "Bing Keyword No YOY", master = "Default Theme")
        
        # browser()
        
        bing_powerpoint <<- bing_powerpoint %>% 
          add_slide(layout = "Bing Keyword No YOY", master = "Default Theme") %>% 
          ph_with_flextable(type = "tbl", value = current_keyword_flextable) %>% 
          ph_with_text(type = "body", str =current_year_text, index = 24) %>% 
          ph_with_text(type = "body", str = paste0("-", bing_campaign_selection), index = 10)
        
        print("There is no data in the prior year keywords. No YOY table will be generated.")
        
      }
      
      if(input$bing_performance_toggle == T) {
        
      # if(is.null(input$bing_lead_export)) {
      #   
      #   confirmSweetAlert(session = session,
      #                     inputId = "no_lead_export",
      #                     text = "")
      #   
      # }        
        bing_keywords_campaign_filtered <-  bing_keywords_upload %>% 
          filter(Campaign == bing_campaign_selection)
        
        bing_keywords_campaign_filtered <<- bing_keywords_campaign_filtered
      
      bing_total_clicks <- bing_keywords_campaign_filtered %>% 
        # filter(Campaign == input$bing_campaign) %>% 
        select(Clicks) %>% 
        summarise("Total Clicks" = sum(Clicks))
      
      bing_total_clicks <- bing_total_clicks[1, 1]
      # bing_total_clicks <- prettyNum(bing_total_clicks)
      
      bing_prev_total_clicks <- bing_keywords_campaign_filtered %>% 
        # filter(Campaign == input$bing_campaign) %>% 
        select(Clicks..Compare.to.) %>% 
        summarise("Total Clicks" = sum(Clicks..Compare.to.))
      
      bing_prev_total_clicks <- bing_prev_total_clicks[1, 1]
      bing_prev_click_entry <- paste0(bing_prev_total_clicks, " Previous Year")
      
      bing_total_cost <- bing_keywords_campaign_filtered %>% 
        select(Spend) %>% 
        summarise("Total Cost" = sum(Spend))
      
      bing_total_cost <- bing_total_cost[1, 1]
      
      bing_prev_total_cost <- bing_keywords_campaign_filtered %>% 
        select(Spend..Compare.to.) %>% 
        summarise("Total Cost" = sum(Spend..Compare.to.))
      
      bing_prev_total_cost <- bing_prev_total_cost[1, 1]
      
      coop_associations <- read.csv("~/Desktop/Rob Scripts/Reference Files/Bing Associations.csv")
      
      # browser()
      
      coop_associations <- coop_associations %>% 
        filter(Bing_Campaign == bing_campaign_selection) %>% 
        select(Dealers) %>% 
        distinct()
      
      coop_associations <- as.character(coop_associations$Dealers)
      
      # coop_associations$Coop <- as.character(coop_associations$Coop)
      # 
      # exclusive_names <- gsub("Culligan | \\(.*| Metro| Grouped| Water Residential", "", bing_campaign_selection)
      # exclusive_names <- strsplit(exclusive_names, split = ", ")
      # 
      # if(class(exclusive_names) == "list"){
      # 
      # exclusive_names <- exclusive_names[[1]]
      # 
      # }
      # 
      # if("GreenBay" %in% exclusive_names){
      #   
      #   exclusive_names <- replace(exclusive_names, exclusive_names == "GreenBay", "Green Bay")
      #                   
      # }
      # 
      # if("La Crosse" %in% exclusive_names) {
      #   
      #   exclusive_names <- replace(exclusive_names, exclusive_names == "La Crosse", "La Crosse / Eau Claire")
      #   
      # }
      # 
      # if("Lincoln Kearney" %in% exclusive_names) {
      #   
      #   exclusive_names <- replace(exclusive_names, exclusive_names == "Lincoln Kearney", "Lincoln / Hastings / Kearney")
      #   
      # }
      # 
      # if("Grand Rapids" %in% exclusive_names) {
      #   
      #   exclusive_names <- replace(exclusive_names, exclusive_names == "Grand Rapids", "Grand Rapids / Kalamazoo")
      #   
      # }
      # 
      # if("Scottsbluff" %in% exclusive_names) {
      #   
      #   exclusive_names <- replace(exclusive_names, exclusive_names == "Scottsbluff", "Cheyenne/Scottsbluff")
      #   
      # }
      # 
      # if("Phoenix" %in% exclusive_names) {
      #   
      #   exclusive_names <- replace(exclusive_names, exclusive_names == "Phoenix", "Phoenix Co-op")
      #   
      # }
      # 
      # if("Tucson" %in% exclusive_names) {
      #   
      #   exclusive_names <- replace(exclusive_names, exclusive_names == "Tucson", "Tucson Co-op")
      #   
      # }
      # 
      # if("Milwaukee" %in% exclusive_names) {
      #   
      #   exclusive_names <- replace(exclusive_names, exclusive_names == "Milwaukee", "Milwaukee Co-op")
      #   
      # }
      # 
      # if(exclusive_names == "McCardel") {
      #   
      #   # browser()
      #   
      #   coop_associations <- coop_associations %>% 
      #     filter(str_detect(Dealers, "Alpena")|str_detect(Dealers, "Big Rapids")|str_detect(Dealers, "Petoskey")|str_detect(Dealers, "Ludington")|str_detect(Dealers, "Traverse City"))
      #  
      #    coop_associations <- coop_associations %>% 
      #     select(Dealers) %>% 
      #     distinct()
      #   coop_associations <- coop_associations$Dealers
      #   coop_associations <- as.character(coop_associations)
      #   coop_associations <<- coop_associations
      #   
      # }else{
      # 
      # coop_associations <- coop_associations[coop_associations$Coop %in% exclusive_names, ]
      # coop_associations <- coop_associations %>% 
      #   select(Dealers) %>% 
      #   distinct()
      # coop_associations <- coop_associations$Dealers
      # coop_associations <- as.character(coop_associations)
      # coop_associations <<- coop_associations
      # 
      # }
      
      # browser()
      
      bing_lead_export <- bing_lead_export[bing_lead_export$SentTo %in% coop_associations, ]
      
      bing_lead_export <- bing_lead_export %>% 
        filter(str_detect(PageLink, "promobing")|str_detect(PageLink, "promominnb")|IVR.Number == 8779576577|IVR.Number == 8779361556)
      
      bing_paid_search_contacts <- nrow(bing_lead_export)
      
      bing_lead_export_prior <- bing_lead_export_prior[bing_lead_export_prior$SentTo %in% coop_associations, ]
      
      bing_lead_export_prior <- bing_lead_export_prior %>% 
        filter(str_detect(PageLink, "promobing")|str_detect(PageLink, "promominnb")|IVR.Number == 8779576577|IVR.Number == 8779361556)
      
      # browser()
      
      bing_paid_search_contacts_prior <- nrow(bing_lead_export_prior)
      
      # browser()
      
      bing_cpc_current <- bing_total_cost / bing_total_clicks * 1.25
      bing_cpc_prior <- bing_prev_total_cost / bing_prev_total_clicks * 1.25
      
      bing_current_conversion_rate <- bing_paid_search_contacts / bing_total_clicks * 100
      bing_prior_conversion_rate <- bing_paid_search_contacts_prior / bing_prev_total_clicks * 100
      
      # browser()
      
      current_clicks_text <- prettyNum(bing_total_clicks, big.mark = ",")
      prior_clicks_text <- paste0(prettyNum(bing_prev_total_clicks, big.mark = ","), " Prior Year")
      
      current_cpc_text <- paste0("$", round(bing_cpc_current, 2))
      prior_cpc_text <- paste0("$", round(bing_cpc_prior, 2), " Prior Year")
      
      paid_search_current <- as.character(bing_paid_search_contacts)
      paid_search_prior <- paste0(bing_paid_search_contacts_prior, " Prior Year")
      
      current_conv_rate_text <- paste0(round(bing_current_conversion_rate, 1), "%")
      prior_conv_rate_text <- paste0(round(bing_prior_conversion_rate, 1), "% Prior Year")
      
      if(bing_prev_total_cost != 0) {
        
        # browser()
        
        # bing_campaign_selection
        # current_year_text
        
        layout_properties(bing_powerpoint, layout = "Bing Performance YOY", master = "Default Theme") %>% distinct()
        
        bing_powerpoint <<- bing_powerpoint %>% 
          add_slide(layout = "Bing Performance YOY", master = "Default Theme") %>% 
          # ph_with_flextable(type = "tbl", value = current_keyword_flextable) %>% 
          ph_with_text(type = "body", str = current_year_text, index = 70) %>% 
          ph_with_text(type = "body", str = paste0("-", bing_campaign_selection), index = 40) %>% 
          ph_with_text(type = "body", str = paid_search_prior, index = 90) %>% 
          ph_with_text(type = "body", str = prior_conv_rate_text, index = 96) %>% 
          ph_with_text(type = "body", str = current_clicks_text, index = 60) %>% 
          ph_with_text(type = "body", str = current_cpc_text, index = 75) %>% 
          ph_with_text(type = "body", str = paid_search_current, index = 80) %>% 
          ph_with_text(type = "body", str = current_conv_rate_text, index = 65) %>% 
          ph_with_text(type = "body", str = prior_clicks_text, index = 84) %>% 
          ph_with_text(type = "body", str = prior_cpc_text, index = 85)
        
      }else{
        
        # browser()
        
        # layout_properties(bing_powerpoint, layout = "Bing Performance No YOY", master = "Default Theme") %>% distinct()
        
        bing_powerpoint <<- bing_powerpoint %>% 
          add_slide(layout = "Bing Performance No YOY", master = "Default Theme") %>% 
          # ph_with_flextable(type = "tbl", value = current_keyword_flextable) %>% 
          ph_with_text(type = "body", str = current_year_text, index = 80) %>% 
          ph_with_text(type = "body", str = paste0("-", bing_campaign_selection), index = 43) %>% 
          # ph_with_text(type = "body", str = paid_search_prior, index = 45) %>% 
          # ph_with_text(type = "body", str = prior_conv_rate_text, index = 50) %>% 
          ph_with_text(type = "body", str = current_clicks_text, index = 61) %>% 
          ph_with_text(type = "body", str = current_cpc_text, index = 65) %>% 
          ph_with_text(type = "body", str = paid_search_current, index = 70) %>% 
          ph_with_text(type = "body", str = current_conv_rate_text, index = 75)  
          # ph_with_text(type = "body", str = prior_clicks_text, index = 87) %>% 
          # ph_with_text(type = "body", str = prior_cpc_text, index = 90)
        
      }
      
      }
      
      if(!dir.exists(paste0("/Volumes/Front/Adam/Reporting/Bing Campaigns/", reporting_period, " ", current_year, " Bing Slides/"))){

        dir.create(paste0("/Volumes/Front/Adam/Reporting/Bing Campaigns/", reporting_period, " ", current_year, " Bing Slides/"))

      }

      powerpoint_name <- paste0("/Volumes/Front/Adam/Reporting/Bing Campaigns/", reporting_period, " ", current_year, " Bing Slides/",
                                pptx_name, " Bing Slides.pptx")

      print(bing_powerpoint, powerpoint_name)

      # print(coop_associations)
      
      confirmSweetAlert(session = session,
                        inputId = "bing_slide_download_success",
                        title = "Powerpoint download successful!",
                        type = "success",
                        btn_labels = "OK!", 
                        danger_mode = T)
      }  
      }
    }
    }
    }
  }
  })
  
  observeEvent(input$exp_voc_file, {
    
    # browser()
    
    gmb_col_names <- c("Store.code", "Business.name", "Address", "Labels", "Total.searches", "Direct.searches", "Discovery.searches",
                       "Discovery.searches", "Total.views", "Search.views", "Maps.views", "Total.actions", "Website.actions",
                       "Directions.actions", "Phone.call.actions")
    
    review_tracker_col_names <- c("Location.ID", "Dealer.ID", "Store.Number..External.ID.", "Location.Name", "Location.Address",
                                  "Location.City", "Location.State", "Location.Zip.Code", "Location.Added.On.Date", "Average.Star.Rating",
                                  "Response.Rate", "Number.of.Reviews")
    
    if(nrow(input$exp_voc_file) != 2) {
      
      # browser()
      
      confirmSweetAlert(session = session,
                        inputId = "exp_voc_file_lessthan_two",
                        title = "Please Input GMB AND ReviewTracker files!",
                        text = "This will not work with less than or more than 2 files.",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
      
      # browser()
      
      voc_data1 <- try(read.csv(input$exp_voc_file$datapath[1]))
      voc_data2 <- try(read.csv(input$exp_voc_file$datapath[2]))
      
      if(class(voc_data1) == "try-error" | class(voc_data2) == "try-error") {
      
      if(class(voc_data1) == "try-error") {
        
        voc_data1 <- read.csv(input$exp_voc_file$datapath[1], stringsAsFactors = T, fileEncoding = "latin1")
        # gmb <- voc_data2
        
        if(all(review_tracker_col_names %in% colnames(voc_data1))) {
          
          review_tracker <<- voc_data1 
          
          if(all(gmb_col_names %in% colnames(voc_data2))) {
            
            gmb <<- voc_data2 
            review_tracker <<- voc_data1 
            
            
          }else{
            
            # browser()
            
            voc_data1 <- NULL
            voc_data2 <- NULL
            
            # browser()
            
            confirmSweetAlert(session = session,
                              inputId = "not_gmb_fail_2",
                              title = "This is not GMB file!",
                              text = "If the export changed to not include some data, update the script.",
                              type = "warning",
                              btn_labels = "OK!",
                              danger_mode = T)
            
          }
          
        }else{
          
          voc_data1 <- NULL
          voc_data2 <- NULL
          
          confirmSweetAlert(session = session,
                            inputId = "not_review_fail_1",
                            title = "This is not a Review Tracker file!",
                            text = "If the export changed to not include some data, update the script.",
                            type = "warning",
                            btn_labels = "OK!",
                            danger_mode = T)
          
        }
        
      }
      
      # if(class(voc_data2))
      
      if(class(voc_data2) == "try-error") {
        
        voc_data2 <- read.csv(input$exp_voc_file$datapath[2], stringsAsFactors = T, fileEncoding = "latin1")
        # gmb <- voc_data2
        
        # browser()
        
        if(all(review_tracker_col_names %in% colnames(voc_data2))) {
          
          
          if(all(gmb_col_names %in% colnames(voc_data1))) {
            
            gmb <<- voc_data1 
            review_tracker <<- voc_data2 
            
            
          }else{
            
            # browser()
            
            voc_data1 <- NULL
            voc_data2 <- NULL
            
            # browser()
            
            confirmSweetAlert(session = session,
                              inputId = "not_gmb_fail_2",
                              title = "This is not GMB file!",
                              text = "If the export changed to not include some data, update the script.",
                              type = "warning",
                              btn_labels = "OK!",
                              danger_mode = T)
            
          }
          
        }else{
          
          voc_data2 <- NULL
          
          confirmSweetAlert(session = session,
                            inputId = "not_review_fail_2",
                            title = "This is not a Review Tracker file!",
                            text = "If the export changed to not include some data, update the script.",
                            type = "warning",
                            btn_labels = "OK!",
                            danger_mode = T)
          
        }
        
      }
    
    # for (i in nrow(input$exp_voc_file)) {
    #   
    #   browser()
    #   
    #   if(gmb_col_names %in% input$exp_voc_file[i]){}
    #   
    #   
    #   
    # }
      
      }else{
        
        if(all(gmb_col_names %in% colnames(voc_data1))) {
          
          gmb <<- voc_data1
          
        }else{
          
          if(all(gmb_col_names %in% colnames(voc_data2))) {
            
            gmb <<- voc_data2
            
          }else{
            
            confirmSweetAlert(session = session,
                              inputId = "weird_voc_no_gmb_file",
                              title = "This is not a GMB file!",
                              text = "There is likely not a ReviewTracker file either!",
                              type = "warning",
                              btn_labels = "OK!",
                              danger_mode = T)
            
          }
          
        }
        
        if(all(review_tracker_col_names %in% colnames(voc_data1))) {
          
          review_tracker <<- voc_data1
          
        }else{
          
          if(all(review_tracker_col_names %in% colnames(voc_data2))) {
            
            review_tracker <<- voc_data2
            
          }else{
            
            confirmSweetAlert(session = session,
                              inputId = "weird_voc_no_review_file",
                              title = "This is not a reviewTracker file!",
                              # text = "There is likely not a ReviewTracker file either!",
                              type = "warning",
                              btn_labels = "OK!",
                              danger_mode = T)
            
          }
          
        }
        
      }
      
    }
    
    # if(gmb_col_names) {}
    
  })
  
  observeEvent(input$exp_voc_gen, {
    
    if(is.null(review_tracker) | is.null(gmb)) {
      
      confirmSweetAlert(session = session,
                        inputId = "stop_voc_generation1",
                        title = "There are not GMB and/or ReviewTracker files to use!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
      
      # browser()
      
      voc_associations <<- read.csv("~/Desktop/Rob Scripts/Reference Files/VOC Associations Final.csv", stringsAsFactors = F)
      
      # browser()
      
      output$voc_all_delete_table <- renderDataTable(datatable(voc_associations, options = list(lengthMenu = list(c(-1, 100), list("All", "100")))))
      output$voc_all_edit_table <- renderDataTable(datatable(voc_associations, editable = T, selection = "none", rownames = F, options = list(lengthMenu = list(c(-1, 100), list("All", "100")))))
      
      review_tracker <- left_join(review_tracker, voc_associations, by = c("Location.Name", "Location.Address"))
      
      review_tracker <- review_tracker %>% 
        mutate(city_state = paste0(Location.City, " ", Location.State))
      
      gmb <- gmb %>% 
        filter(Business.name != "") %>% 
        filter(!str_detect(Business.name, "NOT DOWNLOADABLE")) %>% 
        filter(str_detect(Business.name, "Culligan|culligan"))
      
      gmb <- left_join(gmb, voc_associations, by = c("Business.name", "Address"))
      
      review_tracker <- review_tracker %>% 
        select(Store.code, Location.Name, Business.name, Location.Address, Address, city_state, Average.Star.Rating, Number.of.Reviews)
      review_tracker_filtered <<- review_tracker %>% 
        filter(is.na(Store.code))
      
      gmb <- gmb %>% 
        select(Store.code.y, Location.Name, Business.name, Location.Address, Address, Total.views, Phone.call.actions, Directions.actions, Website.actions) %>% 
        rename("Store.code" = Store.code.y)
      gmb_filtered <<- gmb %>% 
        filter(is.na(Store.code))
    
      full_voc <- full_join(review_tracker, gmb, by = "Store.code")
      
      if(nrow(gmb_filtered) > 0 | nrow(review_tracker_filtered) > 0) {
        
        # updateSelectizeInput(session = session, inputId = "voc_store_code", label = "Please input Corporate ID:", choices = gmb$Store.code, selected = NULL)
        updateSelectizeInput(session = session, inputId = "voc_business_name", label = "Please input GMB name:", choices = gmb$Business.name, selected = NULL)
        updateSelectizeInput(session = session, inputId = "voc_location_name", label = "Please input ReviewTracker name:", choices = review_tracker$Location.Name, selected = NULL)
        updateSelectizeInput(session = session, input = "voc_address", label = "Please input GMB Address:", choices = gmb$Address, selected = NULL)
        updateSelectizeInput(session = session, input = "voc_location_address", label = "Please input ReviewTracker address:", choices = review_tracker$Location.Address, selected = NULL)
        
        confirmSweetAlert(session = session,
                          inputId = "update_voc_association_doc",
                          title = "There appear to be more campaigns on GMB or Review Tracker!",
                          text = "Please update the VOC associations doc.",
                          type = "warning",
                          btn_labels = "OK!",
                          danger_mode = T)
        
      }else{
        
        # full_voc <- full_voc %>%
        #   slice(1:5)
        # 
        # browser()
        # 
        
        dir.create(paste0("/Volumes/Front/Culligan/Local Website Reporting/VOC ", input$exp_voc_date_range))
        
        full_voc <- as.data.frame(lapply(full_voc, as.character), stringsAsFactors = F)
        
        # dat <- full_voc
        # 
        # dat=as.data.frame(lapply(dat, as.character), stringsAsFactors=F)
        
        # withProgress(message = "Downloading Powerpoints", value = 0, {
        #   
        #   for(i in 1:length(bing_campaign_selection)){
        #     
        #     incProgress(1/length(bing_campaign_selection), detail = paste0("Downloading ", i, " of ", length(bing_campaign_selection)))
        
        withProgress(message = "Downloading Powerpoints", value = 0, {
        
        for(i in 1:nrow(full_voc)) {
          
          incProgress(1/nrow(full_voc), detail = paste0("Downloading ", i, " of ", nrow(full_voc)))
          
          # mydoc = pptx(template ="~/shiny-server/shiny_app/MasterData/other_files/Co-op_template.pptx")
          # mydoc=addSlide(mydoc,slide.layout = "VOC Title")
          # mydoc=addSlide(mydoc,slide.layout = "VOC Info")
          # mydoc = addParagraph(mydoc,dat$Total.views[i])
          # mydoc = addParagraph(mydoc,dat$Phone.call.actions[i])
          # mydoc = addParagraph(mydoc,dat$Directions.actions[i])
          # mydoc = addParagraph(mydoc,dat$Website.actions[i])
          # mydoc = addParagraph(mydoc,dat$Average.Star.Rating[i])
          # mydoc = addParagraph(mydoc,dat$Number.of.Reviews[i])
          # mydoc = addParagraph(mydoc,dat$city_state[i])
          # file = paste0("~/Desktop/test/ ",dat$citySt[i], ".pptx")
          # writeDoc(mydoc, file)
          
          voc_powerpoint_doc <- read_pptx(path = "~/shiny-server/shiny_app/MasterData/other_files/Master_template.pptx") %>% 
            add_slide(layout = "VOC Title", master = "Title Slide") %>% 
            add_slide(layout = "VOC Info", master = "Title Slide") %>% 
            ph_with_text(type = "body", str = full_voc$Total.views[i], index = 17) %>% 
            ph_with_text(type = "body", str = full_voc$Phone.call.actions[i], index = 18) %>% 
            ph_with_text(type = "body", str = full_voc$Directions.actions[i], index = 19) %>% 
            ph_with_text(type = "body", str = full_voc$Website.actions[i], index = 3) %>% 
            ph_with_text(type = "body", str = full_voc$Average.Star.Rating[i], index = 4) %>% 
            ph_with_text(type = "body", str = full_voc$Number.of.Reviews[i], index = 5) %>% 
            ph_with_text(type = "body", str = full_voc$city_state[i], index = 6)
          
          # browser()
          save_file <- paste0("/Volumes/Front/Culligan/Local Website Reporting/VOC ", input$exp_voc_date_range, "/", full_voc$city_state[i], ".pptx")
          # save_file <- paste0("~/Desktop/test/", full_voc$city_state[i], ".pptx")
          print(voc_powerpoint_doc, save_file)
            
          
          # mydoc = pptx(template ="~/shiny-server/shiny_app/MasterData/other_files/Co-op_template.pptx")
          # mydoc=addSlide(mydoc,slide.layout = "VOC Title")
          # mydoc=addSlide(mydoc,slide.layout = "VOC Info")
          # mydoc <- mydoc %>%
          #   addParagraph(full_voc$Total.views[i]) %>%
          #   addParagraph(full_voc$Phone.call.actions[i]) %>%
          #   addParagraph(full_voc$Directions.actions[i]) %>%
          #   addParagraph(full_voc$Website.actions[i]) %>%
          #   addParagraph(full_voc$Average.Star.Rating[i]) %>%
          #   addParagraph(full_voc$Number.of.Reviews[i]) %>%
          #   addParagraph(full_voc$city_state[i])
          # mydoc = addParagraph(mydoc,dat$Total.views[i])
          # mydoc = addParagraph(mydoc,dat$Phone.call.actions[i])
          # mydoc = addParagraph(mydoc,dat$Directions.actions[i])
          # mydoc = addParagraph(mydoc,dat$Website.actions[i])
          # mydoc = addParagraph(mydoc,dat$Average.Star.Rating[i])
          # mydoc = addParagraph(mydoc,dat$Number.of.Reviews[i])
          # mydoc = addParagraph(mydoc,dat$city_state[i])
          # file = paste0("~/Desktop/test/", full_voc$city_state[i], ".pptx")
          # writeDoc(mydoc, file)
          # 
          # print("Powerpoint Created")
          
        }
        
      })
        
        confirmSweetAlert(session = session,
                          inputId = "voc_slide_download_complete",
                          title = "VOC slide download complete!",
                          type = "success",
                          btn_labels = "OK!",
                          danger_mode = T)
        
      }
      
    }
    
  })
  
  observeEvent(input$voc_gmb_differences, {
    
    # browser()
    
    # gmb_test <- try(gmb_filtered)
    # review_test <- try(review_tracker_filtered)
    
    if(is.null(input$exp_voc_gen) | is.null(gmb) | is.null(review_tracker)) {
      
      confirmSweetAlert(session = session, 
                        inputId = "voc_upload_file_first",
                        title = "Please upload GMB and ReviewTracker Files first!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
      
      if(is.null(gmb_filtered)) {

        confirmSweetAlert(session = session,
                          inputId = "voc_null_filtered_gmb",
                          title = "There are no discrepencies!",
                          type = "warning",
                          btn_labels = "OK!",
                          danger_mode = T)

        output$voc_gmb_difference_table <- renderDataTable({NULL})

      }else{
        
        if(nrow(gmb_filtered) == 0) {
          
          confirmSweetAlert(session = session,
                            inputId = "voc_zero_filtered_gmb",
                            title = "There are no discrepencies!",
                            type = "warning",
                            btn_labels = "OK!",
                            danger_mode = T)
          
          output$voc_gmb_difference_table <- renderDataTable({NULL})
          
        }else{
          
          # browser()
          
          gmb_filtered_dt <- gmb_filtered %>% 
            select(Business.name, Address) %>% 
            rename("Unmatched Businesses" = Business.name)
          
          # gmb_filtered_dt <- datatable(gmb_filtered)
          
          output$voc_difference_text <- renderText("GMB Association Discrepencies:")
          output$voc_difference_table <- renderTable({gmb_filtered_dt})
          
          # browser()
          
        }
        
      }
      
    }
    
  })
  
  observeEvent(input$voc_review_differences, {
    
    if(is.null(input$exp_voc_gen) | is.null(gmb) | is.null(review_tracker)) {
      
      confirmSweetAlert(session = session, 
                        inputId = "voc_upload_file_first",
                        title = "Please upload GMB and ReviewTracker Files first!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
      
      if(is.null(review_tracker_filtered)) {
        
        confirmSweetAlert(session = session,
                          inputId = "voc_null_filtered_review",
                          title = "There are no discrepencies!",
                          type = "warning",
                          btn_labels = "OK!",
                          danger_mode = T)
        
        output$voc_difference_table <- renderDataTable({NULL})
        
      }else{
        
        if(nrow(review_tracker_filtered) == 0) {
          
          confirmSweetAlert(session = session,
                            inputId = "voc_zero_filtered_review",
                            title = "There are no discrepencies!",
                            type = "warning",
                            btn_labels = "OK!",
                            danger_mode = T)
          
          output$voc_difference_table <- renderDataTable({NULL})
          
        }else{
          
          # browser()
          
          review_tracker_filtered_dt <- review_tracker_filtered %>% 
            select(Location.Name, Location.Address) %>% 
            rename("Unmatched Businesses" = Location.Name) %>% 
            rename("Address" = Location.Address)
          
          # gmb_filtered_dt <- datatable(gmb_filtered)
          
          output$voc_difference_text <- renderText("Review Tracker Association Discrepencies:")
          output$voc_difference_table <- renderTable({review_tracker_filtered_dt})
          
          # browser()
          
        }
        
      }
      
    }
    
  })
  
  observeEvent(input$voc_add_rbind, {
    
    # browser()
    
    if(is.null(input$exp_voc_gen) | is.null(gmb) | is.null(review_tracker) | is.null(input$voc_gmb_differences) |
       is.null(input$voc_review_differences)) {
      
      confirmSweetAlert(session = session, 
                        inputId = "voc_upload_file_first",
                        title = "Please upload GMB and ReviewTracker Files first!",
                        text = "Viewing the differences will let you know what needs to be added to Association Doc.",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
      
      # browser()
      
      if(input$voc_store_code == "") {
        
        confirmSweetAlert(session = session, 
                          inputId = "voc_no_store_code",
                          title = "Please input corporate ID!",
                          text = "This will take some manual searching, but is required since this is how dealers are matched.",
                          type = "warning",
                          btn_labels = "OK!",
                          danger_mode = T)
        
      }else{
        
        if(length(input$voc_business_name) > 1 | length(input$voc_location_name) > 1 | length(input$voc_location_address) > 1| length(input$voc_address) > 1) {
          
          confirmSweetAlert(session = session,
                            inputId = "voc_too_many_entries",
                            title = "There are too many entries in these inputs!",
                            text = "Please only put one name in each field.",
                            type = "warning",
                            btn_labels = "OK!", 
                            danger_mode = T)
          
        }else{
          
        if((!is.null(input$voc_location_name) & is.null(input$voc_location_address)) | (!is.null(input$voc_location_address) & is.null(input$voc_location_name))) {
        
        # if((input$voc_location_name != "" & input$voc_location_address == "") | (input$voc_location_address != "" & input$voc_location_name == "")) {
          
          confirmSweetAlert(session = session, 
                            inputId = "voc_review_missing",
                            title = "Please input both location and address!",
                            text = "Both pieces of information are needed for an addition.",
                            type = "warning",
                            btn_labels = "OK!",
                            danger_mode = T)
          
        }else{
          
          # browser()
          
          if((!is.null(input$voc_business_name) & is.null(input$voc_address)) | (!is.null(input$voc_address) & is.null(input$voc_business_name))) {
          
          # if((input$voc_business_name != "" & input$voc_address == "") | (input$voc_address != "" & input$voc_business_name == "")) {
            
            confirmSweetAlert(session = session, 
                              inputId = "voc_review_missing",
                              title = "Please input both location and address!",
                              text = "Both pieces of information are needed for an addition.",
                              type = "warning",
                              btn_labels = "OK!",
                              danger_mode = T)
            
          }else{
            
            confirmSweetAlert(session = session, 
                              inputId = "voc_confirm_add",
                              title = "Are you sure you want to add the row?",
                              text = "Current association doc will be overwritten.",
                              type = "warning",
                              btn_labels = c("No", "Yes"),
                              danger_mode = T)
            
            # voc_add_row <- c(input$voc_store_code, input$voc_business_name, input$voc_location_name, input$voc_address, input$voc_location_address)
            
            
          }
          
        }
        
      }
      
    }
      
    }
    
  })
  
  observeEvent(input$voc_confirm_add, {
    
    # browser()
    
    if(input$voc_confirm_add == T){
      
      # browser()
      voc_associations <- read.csv("~/Desktop/Rob Scripts/Reference Files/VOC Associations Final.csv", stringsAsFactors = F)
      voc_add_row <- c(input$voc_store_code, input$voc_business_name, input$voc_location_name, input$voc_address, input$voc_location_address)
      voc_add_row <- as.data.frame(t(as.data.frame(voc_add_row)))
      colnames(voc_add_row) <- colnames(voc_associations)
      original_voc_row_number <- nrow(voc_associations)
      test <- voc_associations
      voc_associations <- rbind(voc_associations, voc_add_row)
      voc_associations <- voc_associations %>% 
        distinct()
      after_voc_row_number <- nrow(voc_associations)
      
      # browser()
      
      if(original_voc_row_number == after_voc_row_number) {
        
        confirmSweetAlert(session = session, 
                          inputId = "voc_already_present",
                          title = "This entry is already in the doc!",
                          type = "warning",
                          btn_labels = "OK!",
                          danger_mode = T)
        
      }else{
        
      write.csv(voc_associations, "~/Desktop/Rob Scripts/Reference Files/VOC Associations Final.csv", row.names = F)
      output$voc_all_add_table <- renderTable(voc_associations)
      output$voc_all_delete_table <- renderDataTable(datatable(voc_associations, options = list(lengthMenu = list(c(-1, 100), list("All", "100")))))
      output$voc_all_edit_table <- renderDataTable(datatable(voc_associations, editable = T, selection = "none", rownames = F, options = list(lengthMenu = list(c(-1, 100), list("All", "100")))))
      
      confirmSweetAlert(session = session,
                        inputId = "voc_added_row_confirm",
                        title = "Association now added in doc!",
                        type = "success",
                        btn_labels = "OK!",
                        danger_mode = T)
      
      }
      
    }else{
      
      confirmSweetAlert(session = session,
                        inputId = "voc_reject_add",
                        title = "New row will not be added.",
                        type = "info",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }
    
  })
  
  # output$voc_all_delete_table <- renderDataTable(datatable(voc_associations))
  
  observeEvent(input$voc_delete_row, {
    
    # browser()
    
    if(is.null(input$voc_all_delete_table_rows_selected)) {
      
      # browser()
      
      confirmSweetAlert(session = session,
                        inputId = "no_voc_delete_selected",
                        title = "Please choose row(s) to delete!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
      
      voc_associations <- voc_associations[-input$voc_all_delete_table_rows_selected,]
      output$voc_all_delete_table <- renderDataTable(datatable(voc_associations, options = list(lengthMenu = list(c(-1, 100), list("All", "100")))))
      output$voc_all_add_table <- renderTable(voc_associations)
      output$voc_all_edit_table <- renderDataTable(datatable(voc_associations, editable = T, selection = "none", rownames = F, options = list(lengthMenu = list(c(-1, 100), list("All", "100")))))
      
      write.csv(voc_associations, "~/Desktop/Rob Scripts/Reference Files/VOC Associations Final.csv", row.names = F)
      
      confirmSweetAlert(session = session,
                        inputId = "voc_delete_selected_confirm",
                        title = "Entries Now deleted!",
                        type = "success",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }
    
  })
  
  observeEvent(input$voc_all_edit_table_cell_edit, {
    
    # browser()
    
    if(input$voc_all_edit_table_cell_edit$value == "") {
      
      edit_proxy <- dataTableProxy("voc_all_edit_table")
      edit_info <- input$voc_all_edit_table_cell_edit
      edit_row <- edit_info$row
      edit_column <- edit_info$col
      edit_value <- edit_info$value
      # browser()
      voc_associations[edit_row, edit_column + 1] <<- DT::coerceValue(voc_associations[edit_row, edit_column + 1], voc_associations[edit_row, edit_column + 1])
      replaceData(edit_proxy, voc_associations, resetPaging = F)
      
      output$voc_all_edit_table <- renderDataTable(datatable(voc_associations, editable = T, selection = "none", rownames = F, options = list(lengthMenu = list(c(-1, 100), list("All", "100")))))
      output$voc_all_add_table <- renderTable(voc_associations)
      output$voc_all_delete_table <- renderDataTable(datatable(voc_associations, options = list(lengthMenu = list(c(-1, 100), list("All", "100")))))
      write.csv(voc_associations, "~/Desktop/no rewrite.csv", row.names = F)
      
      confirmSweetAlert(session = session,
                        inputId = "voc_empty_edit",
                        title = "You need to have a value input to make an edit!",
                        text = "If you'd like to remove the row, go to the delete tab.",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
      # browser()
      
    }else{
      
     
      confirmSweetAlert(session = session,
                        inputId = "voc_edit_change",
                        title = "Are you sure you want to make this edit?",
                        text = "You will not be able to revert and have to manually change back if this is not desired.",
                        type = "info",
                        btn_labels = c("No", "Yes"),
                        danger_mode = T)
      
    }
    
  })
  
  observeEvent(input$voc_edit_change, {
    
    # browser()
    
    if(input$voc_edit_change == T) {
    
    edit_proxy <- dataTableProxy("voc_all_edit_table")
    edit_info <- input$voc_all_edit_table_cell_edit
    edit_row <- edit_info$row
    edit_column <- edit_info$col
    edit_value <- edit_info$value
    
    voc_associations[edit_row, edit_column + 1] <<- DT::coerceValue(edit_value, voc_associations[edit_row, edit_column + 1])
    replaceData(edit_proxy, voc_associations, resetPaging = F)
    
    output$voc_all_edit_table <- renderDataTable(datatable(voc_associations, editable = T, selection = "none", rownames = F, options = list(lengthMenu = list(c(-1, 100), list("All", "100")))))
    output$voc_all_add_table <- renderTable(voc_associations)
    output$voc_all_delete_table <- renderDataTable(datatable(voc_associations, options = list(lengthMenu = list(c(-1, 100), list("All", "100")))))
    write.csv(voc_associations, "~/Desktop/Rob Scripts/Reference Files/VOC Associations Final.csv", row.names = F)
    
    }else{
      
      edit_proxy <- dataTableProxy("voc_all_edit_table")
      edit_info <- input$voc_all_edit_table_cell_edit
      edit_row <- edit_info$row
      edit_column <- edit_info$col
      edit_value <- edit_info$value
      # browser()
      voc_associations[edit_row, edit_column + 1] <<- DT::coerceValue(voc_associations[edit_row, edit_column + 1], voc_associations[edit_row, edit_column + 1])
      replaceData(edit_proxy, voc_associations, resetPaging = F)
      
      output$voc_all_edit_table <- renderDataTable(datatable(voc_associations, editable = T, selection = "none", rownames = F, options = list(lengthMenu = list(c(-1, 100), list("All", "100")))))
      output$voc_all_add_table <- renderTable(voc_associations)
      output$voc_all_delete_table <- renderDataTable(datatable(voc_associations, options = list(lengthMenu = list(c(-1, 100), list("All", "100")))))
      write.csv(voc_associations, "~/Desktop/Rob Scripts/Reference Files/VOC Associations Final.csv", row.names = F)
      
    }
    
  })
  
  observeEvent(input$qui_confirm, {
    
    # browser()
    
    if(length(input$qui_slides_order) != 0) {
      
      confirmSweetAlert(session = session, 
                        inputId = "qui_items_in_slides",
                        title = "Please put all of the slides in the order or remove columns!",
                        text = "This feature could be removed since the only relevant column is the order column.",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
    
    # browser()
    shiny_qui_pptx <- read_pptx("~/Desktop/shinyqui test.pptx")
    qui_slide_order <- NULL
    qui_slide_order <- as.data.frame(input$qui_order_order)
    qui_slide_order <- qui_slide_order %>% 
      mutate("slide_order" = rownames(qui_slide_order))
    
    # browser()
    
    qui_remove_slides <- as_data_frame(input$qui_remove_order)
    
    if(nrow(qui_remove_slides) == 0) {
      
      x <- ""
      qui_remove_slides <- data.frame(x)
      qui_remove_slides <- qui_remove_slides %>% 
        filter(x != "")
      
    }
    
    colnames(qui_remove_slides) <- "slide_names"
    
    # browser()
    
    for(i in 1:nrow(qui_slide_order)) {
      
      
      
      # if(i == 1) {browser()}
      # if(i == 2) {browser()}
      # if(i == 3) {browser()}
      # if(i == 4) {browser()}
    
    if("Test 1" %in% qui_slide_order$`input$qui_order_order`[i]){
    
    x <- "Test Slide 1"
    shiny_qui_pptx <<- shiny_qui_pptx %>% 
      add_slide(layout = "Test 1", master = "Office Theme") %>% 
      ph_with_text(type = "body", str = x)
    
    # slide1_placement <- qui_slide_order %>% 
    #   filter(`input$qui_order_order` == "Test 1") %>% 
    #   select(slide_order) %>% 
    #   as.numeric()
    # 
    # slide1_list <- list(test_slide1, slide1_placement)
    
    }
    
    if("Test 2" %in% qui_slide_order$`input$qui_order_order`[i]) {
    
    y <- "Test Slide 2"
    shiny_qui_pptx <<- shiny_qui_pptx %>%
      add_slide(layout = "Test 2", master = "Office Theme") %>%
      ph_with_text(type = "body", str = y)
    
    # slide2_placement <- qui_slide_order %>% 
    #   filter(`input$qui_order_order` == "Test 2") %>% 
    #   select(slide_order) %>% 
    #   as.numeric()
    # 
    # slide2_list <- list(test_slide2, slide2_placement)
    
    }
    
    if("Test 3" %in% qui_slide_order$`input$qui_order_order`[i]) {
    
    z <- "Test Slide 3"
    shiny_qui_pptx <<- shiny_qui_pptx %>%
      add_slide(layout = "Test 3", master = "Office Theme") %>%
      ph_with_text(type = "body", str = z)
    
    # slide3_placement <- qui_slide_order %>% 
    #   filter(`input$qui_order_order` == "Test 3") %>% 
    #   select(slide_order) %>% 
    #   as.numeric()
    # 
    # slide3_list <- list(test_slide3, slide3_placement)
    
    }
    
    if("Test 4" %in% qui_slide_order$`input$qui_order_order`[i]) {
    
    a <- "Test Slide 4"
    shiny_qui_pptx <<- shiny_qui_pptx %>%
      add_slide(layout = "Test 4", master = "Office Theme") %>%
      ph_with_text(type = "body", str = a)
    
    # slide4_placement <- qui_slide_order %>% 
    #   filter(`input$qui_order_order` == "Test 4") %>% 
    #   select(slide_order) %>% 
    #   as.numeric()
    # 
    # slide4_list <- list(test_slide4, slide4_placement)
    
    }
      
    if("Remove" %in% qui_slide_order$`input$qui_order_order`[i]) {
      
      b <- "Remove Slide"
      shiny_qui_pptx <<- shiny_qui_pptx %>%
        add_slide(layout = "Remove", master = "Office Theme") %>%
        ph_with_text(type = "body", str = b)
      
    }
    
    }
    
    # browser()
    
    # qui_list <- list(slide1_list, slide2_list, slide3_list, slide4_list)
    
    print(shiny_qui_pptx, "~/Desktop/QUI TEST.pptx")
    write.csv(qui_slide_order, "~/Desktop/Slide Order.csv", row.names = F)
    write.csv(qui_remove_slides, "~/Desktop/Remove Names.csv", row.names = F)
    
    confirmSweetAlert(session = session, 
                      inputId = "qui_download_success",
                      title = "Slides Successfully Downloaded!",
                      type = "success",
                      btn_labels = "OK!",
                      danger_mode = T)
    
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
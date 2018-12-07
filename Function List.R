# I think it's important that some of the stuff I'm doing gets actually set as functions,
# since it's easier to fix everything in one area instead of several, which has historically
# been a problem with 99.99% of my code.

# So this function is clearly not as good as what is in the shiny app. That being 
# said, I still prefer it to the standard ph_with_text, because it was always so 
# annoying to actually figure out what was the row number of a place holder. This
# wouldn't work well on a presentation with a ton of template slides, but since
# 

better_pptx_text_assignment <- function(x, id, slide_type, text, slide_num) {
  # browser()
  id_labels <- as.data.frame(layout_properties(x))
  
  slide_order <- id_labels %>% 
    select(master_name, name) %>% 
    distinct()
  master_slide <- as.character(slide_order[slide_num, 1])
  layout_slide <- as.character(slide_order[slide_num, 2])
  
  # browser()
  
  id_order <- id_labels %>% 
    filter(type == slide_type) %>% 
    filter(name == layout_slide) %>% 
    filter(master_name == master_slide)
  
  row_number <- which(grepl(id, id_order$id))
  
  if(length(row_number) > 1) {
    
    row_number <- min(row_number)
    
  }
  
  # browser()
  ph_with_text(x = x, str = text, type = slide_type, index = row_number)
  
}
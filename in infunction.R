test <- read.csv("~/Desktop/franklin.csv")
exists("test$Type")

if("SentTo" %in% colnames(test)) {
  print("Good Job!")
}else{
  print("You Suck!")
}

new_web_change <- table$`Forms Web`[index - 1] + table$`Phone Direct Transfer Web`[index - 1]
new_web <- new_web_change / (table$`Forms Web`[index - 2] + table$`Phone Direct Transfer Web`[index - 2])
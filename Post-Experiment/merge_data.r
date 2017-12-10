
read_input<-function(input.dir,file.name){

    ## remove the input.dir from file path
    short.name <- gsub(paste(input.dir,'/data/', sep = ''),'',file.name )
    
    ## determine which parent folder the files is coming from 
    if(sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\1", short.name) == 'generic/'){
      email.type <- 'generic'
    } else if(sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\1", short.name) == 'personal/') {
      email.type <- 'personal'
    } else {stop("Parent dir does not exist.", call.=FALSE)}
    
    
    ## Extract the file type
    format_type <- sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\3", short.name)
    
    ## Extract the file name
    file_name <- sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", short.name)
    
    ## Format type for .csv files
    if(tolower(format_type) == ".csv"){
      input <- read.csv(file.name, header=TRUE, stringsAsFactors=FALSE, na.strings = c("NA","N/A","null", '.'))
      input$file.name <- rep(file_name,nrow(input))
      input$email.type  <- rep(email.type,nrow(input))
    }  else {stop(paste0("Not valide format type, e.g.",as.character(format_type),"."), call.=FALSE)
    
}

  return(input)
}


###  THIS IS THE ONLY LINE THAT SHOULD NEED TO BE ALTERED
input.dir <- "working-directory"

type.list <- list.files(paste(input.dir,'data',sep = '/'))

dir.list <- paste(input.dir,'data',type.list,sep = "/")

file.list <- list.files(dir.list, full.names = TRUE)


list_dfs <- lapply(file.list, read_input,input.dir = input.dir)

merged_df <- do.call(rbind, list_dfs)


######   bring in the randomization document

randomization <- read.csv(paste(input.dir,'Alumni Donation History 11-04-2017.xlsx - randomization.csv',sep = '/'))


randomization <- randomization[,c("Constituent.Number", "Sex","PGSS.Year",
                                  "donor.status","Email.Address","treatment")]

randomization$Email.Address <- tolower(randomization$Email.Address)


merged_df$Supporter.Email <- tolower(merged_df$Supporter.Email)

final_df <- merge(merged_df , randomization, by.x = 'Supporter.Email', 
                  by.y = 'Email.Address')

final_df <- final_df[!is.na(final_df$treatment),]

final_df$click <- ifelse(final_df$Link.Clicked == 'No Click', 0,1)

final_df$open <- ifelse(final_df$Open.Date == '', 0,1)



summary(final_df)


write.csv(final_df, file = "final_df.csv")

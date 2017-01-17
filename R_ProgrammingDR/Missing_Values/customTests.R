# Get the swirl state
getState <- function(){
  # Whenever swirl is running, its callback is at the top of its call stack.
  # Swirl's state, named e, is stored in the environment of the callback.
  environment(sys.function(1))$e
}

# Get the value which a user either entered directly or was computed
# by the command he or she entered.
getVal <- function(){
  getState()$val
}

# Get the last expression which the user entered at the R console.
getExpr <- function(){
  getState()$expr
}

loadDigest <- function(){
  if (!require("digest")) install.packages("digest")
  library(digest)
}

dbs_on_demand <- function(){
  loadDigest()
  selection <- getState()$val
  if(selection == "Yes"){
    course <- "r_missing_values"
    email <- readline("What is your email address? ")
    student_number <- readline("What is your student number? ")
    hash <- digest(paste(course, student_number), "md5", serialize = FALSE)
    
    payload <- sprintf('{  
      "course": "%s",
      "email": "%s",  
      "student_number": "%s",  
      "hash": "%s",  
    }', course, email, student_number, hash)
    url <- paste('http:///results.dbsdataprojects.com/course_results/submit?course=', course, '&hash=', hash, '&email=', email, '&student_number=', student_number, sep='')
  
    respone <- httr::GET(url)
    if(respone$status_code >= 200 && respone$status_code < 300){
      message("Grade submission succeeded!")
    } else {
      message("Grade submission failed.")
      message("Press ESC if you want to exit this lesson and you")
      message("want to try to submit your grade at a later time.")
      return(FALSE)
    }
  }
  TRUE
}

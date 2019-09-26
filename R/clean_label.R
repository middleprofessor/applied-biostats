# function to remove spaces and units from column labels
# Jeff Walker
# November 12, 2018
# to use, add "source("clean_label.R")"

odd <- function(x) x%%2 != 0

even <- function(x) x%%2 == 0

clean_label <- function(x){
  # clean units in parentheses and units with space in front of parenthesis
  x <- gsub(" \\s*\\([^\\)]+\\)","", x)
  # clean units in parentheses and units without space in front of parenthesis
  x <- gsub("\\s*\\([^\\)]+\\)","", x)

  #remove spaces
  x <- gsub(" ", "_", x)

  # replace hash with "n"
  x <- gsub("#", "n_", x)

  # replace % with "perc"
  x <- gsub("%", "perc_", x)

  # replace hyphens
  x <- gsub("-", "_", x)

  # replace forward slash
  x <- gsub("/_", ".", x)

  # replace forward slash
  x <- gsub("/", ".", x)

  # get rid of any double "__"
  x <- gsub("__", "_", x)

  # get rid of ending "_"
  x <- ifelse(substr(x, nchar(x), nchar(x))=="_", substr(x, 1, (nchar(x))-1), x)

  # get rid of "._"
  #x <- gsub("._", "_", x)

  # get rid of "_."
  #x <- gsub("_.", "_", x)

  return(x)
}

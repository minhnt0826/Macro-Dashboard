# library(devtools)
# install_github("mikeasilva/blsAPI")

library(blsAPI)
library(jsonlite)


response <- blsAPI('CUSR0000SA0L12E') 
json <- fromJSON(response) 

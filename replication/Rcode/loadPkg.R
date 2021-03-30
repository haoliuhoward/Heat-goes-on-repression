loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  library(lib, character.only=TRUE)
	}
}
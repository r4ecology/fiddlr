#' Function to detach all loaded packages
#'
#' 
#' @param i 
#'
#' @export
detach_all <- function(){
  
pkgs = names(sessionInfo()$otherPkgs)
pk_list <- pkgs 
 check <- TRUE 
  while(check){
pkgs = names(sessionInfo()$otherPkgs)
  if(length(pkgs) == 0)
    check <- FALSE
if(length(pkgs) > 0){
  pkgs = paste('package:', pkgs, sep = "")

  for(pp in pkgs) 
    detach(pp, character.only = TRUE, unload = TRUE, force = TRUE)
}

}

  if(length(pk_list) > 0){ 
    print("The following packages were successfully detached")
    print(pk_list)} else
      print("No packages available to detach")

}



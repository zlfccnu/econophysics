#' this function can use to generate a beep sound after you finish a long task
#' @param sound numbers to choose different sounds
#' @return invisible 
#' @export
finish_notify=function(sound=1){
  while (TRUE) {
    beepr::beep(sound = sound)
    pause(3)
    system("notify-send \"R script finished running\"")
  }
}
#' @export
#' @import igraph
#' @param rank This is the Rank of the maze.
#' @param score This is the series of connected paths via the black dots.
#' @param nodePosition You need to calculate the nodePosition.
#' @description The ability function returns the weighted score of the individual given his raw score (i.e. the number of black dotes collected).
#' @details The ability function is calculated following the paper in XXXX.
#' @author Aiden Loe and Maria Sanchez
#' @title ability
#'
#' @examples \dontrun{
#'  rank <-3
#'  score<- 8
#'  nodePosition <- colourNodePosition(rank=3,satPercent=0.5,seed=1)
#'  ability(rank,nodePosition,score)
#'}


ability <- function(rank,nodePosition,score){

  if(rank != nodePosition$rank){
    stop("The input rank and the rank to calculate the colour node positions are not the same.")
  }

  #calculates all possible black node Routes
  (totalScore<- blackNodeRoutes(rank,nodePosition))

  #table(totalScore) #frequently of path crossing varying series of connecting dots

  totalScores <- as.data.frame(totalScore)
  names(totalScores)[names(totalScores)=="Var1"] <- "connNodes"

  # Just frequency of path crossing
  Frematrix<-matrix(totalScore)
  Frematrix
  (maxsc<-length(Frematrix))

  # highest frequency of path crossing
  (mostcom<-max(Frematrix))

  m <- which(grepl(mostcom,Frematrix[,1])) #cell value with most frequent path


  #### CHECK THE PAPER ###
  ### THINK HOW TO USE THE CALCULATED SCORE ###
  # To commodate connNodes sometimes starting from 0 or 1.
  if(totalScores$connNodes[1] == 0){
  scoreroutes<-Frematrix[score+1] #nodes starts from 0, not 1.
  }else{
    scoreroutes<-Frematrix[score] #nodes starts from 1.
  }
  scoreroutes


  k <- 1
  if(k == maxsc)  weightedScores <-log(mostcom/Frematrix)
  while(k  < maxsc ){
    weightedScores <-log(mostcom/Frematrix)
    k <- k + 1
  }


  weightedScores <- as.data.frame(weightedScores)
  names(weightedScores)[names(weightedScores)=="V1"] <- "weightedScores"
  print(paste0("The weighted scores for Rank ",rank, " with a saturation of ",nodePosition$satPercent*100,"%"))
  print(cbind.data.frame(totalScores,weightedScores))
  print(paste0("Your weighted score of ",score," connected nodes is ", weightedScores))


  # To commodate connNodes sometimes starting from 0 or 1.
  if(totalScores$connNodes[1] == 0){
      # compute the single weighted score given raw score
      if(score+1 < m){
        out<-NA
        print("Less than most frequent path hence NA. See 1965 paper, p.300. ")
      }else{
        out<-log(mostcom/scoreroutes)
      }

      # Check if series of connected dots exist
      if (score+1 > maxsc){
        out<-'Not a possible score since connected dots does not exist'
      }
      return(out)

    }else{
    if(score < m){
      out<-NA
      print("Less than most frequent path hence NA. See 1965 paper, p.300. ")
    }else{
      out<-log(mostcom/scoreroutes)
    }

    # Check if series of connected dots exist
  if (score > maxsc){
  out<-'Not a possible score since connected dots does not exist'
  }
  return(out)

  }
}
#
#
# rank <-4
# score<- 8
# nodePosition <- colourNodePosition(rank=4,satPercent=0.5,seed=1)
#  ability(rank,nodePosition,1)


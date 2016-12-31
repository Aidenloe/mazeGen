#' @export
#' @import igraph
#' @param path Selecting the specific number of paths to achieve the maximum score.
#' @param rank This is the rank of the maze.
#' @param satPercent This is of saturation percentage ranging from 0-1.
#' @param seed The starting seed to begin searching for the seed with specific paths.
#' @description This generate the solution by searching for the SEED that returns the specific number of paths to achieve the maximum score for a given rank and saturation.
#' @details This might be computationally intensive as the maze size increases. The seed is necessary so that the algorithm does not always begin from the smallest value. Based on the seed value, it will search for the next biggest that returns 1 unique solution.
#' @author Aiden Loe and Maria Sanchez
#' @title genUniqueSolution
#' @examples
#'
#' rank <- 5
#' satPercent <- 0.5
#' seed <- 1
#'
#' #Searches for just one unique solution
#' justOne <- genPathSeed(path=3,rank=rank,satPercent=satPercent,seed=seed)
#' nodePosition <- colourNodePosition(rank,satPercent,seed=justOne)
#' mazeEst(rank,nodePosition)

genPathSeed<-function(path,rank,satPercent,seed){
  if(path==1){
    num <- 1
    count <- 1
  while (num>1){
    seed<-seed+1
    num<-lookUniqueSolution(rank,satPercent,seed)
    count <- count + 1
    if(count > 300){
      num <- 1
      stop("No unique solution can be found. Please reduce saturation percentage.")
    }
   }
  }else{
    num <- 1
    count <- 1
    while(num != path){
      seed<-seed+1
      (num<-pathSolution(rank,satPercent,seed))
      count <- count + 1
      if(count > 500){
        num <- 1
        stop("The desired path cannot be found. Consider lowering number of paths")
      }
    }
  }
  return(seed)
}

# rank <- 4
#  satPercent <- 0.6
#  seed <- 1
#  #Searches for just one unique solution
#  justOne <- genPathSeed(path=3,rank=rank,satPercent=satPercent,seed=seed)
#  justOne
#
#  a<- colourNodePosition(rank,satPercent,seed=4)
# mazeEst(rank,a)


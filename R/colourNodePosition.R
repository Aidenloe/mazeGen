#' @export
#' @import igraph
#' @param rank This is the Rank of the maze.
#' @param satPercent Percentage of saturation.
#' @param seed To always get the same position for a local computer.
#' @details Returns the colour node position. You need to use the node position function first.
#' @description This function will not sample from the first node position. If you consider sampling from the first node, then in javascript, the summing of the black dotes need to begin from 1 rather than 0. To keep it simple, always ensure that the first node is not sampled as a black dot.
#' @author Aiden Loe
#' @title colourNodePosition
#' @examples
#'
#' colourNodePosition(rank=3,satPercent=0.5,seed=1)
#'
#'

#### Colour Node Position #####
colourNodePosition<- function(rank=3, satPercent=0.5, seed=1){
saturation<- ceiling(length(lowerGrid(rank))*satPercent)
set.seed(seed)
nodePosition <- sample(lowerGrid(rank)[-1], saturation, replace=FALSE)

nodePosition <- list(seed = seed,
     rank = rank,
     satPercent= satPercent,
     nodePosition=nodePosition)

class(nodePosition) <- "np"
return(nodePosition)
}




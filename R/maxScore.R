# ' @export
# ' @import igraph
# ' @param nodePosition tells you all the position of the black dots
# ' @description This tells you the maximum score for a given rank and a given colour node positions.
# ' @details The maxScore function returns the maximum score for a given rank and a given colour node positions. You need to use the colour node position function first.
# ' @author Aiden Loe
# ' @title Maximum Score
# ' @examples
# '
# ' np(rank=3,satPercent=0.5,seed=1)
# '
# ' maxScore(nodePosition)
# '


maxScore <- function(nodePosition){


  if("np" %in% class(nodePosition) == FALSE){
    stop("nodePosition must be calculated using the np function.")
  }


  rank <- nodePosition$rank
  nodePosition <- nodePosition$nodePosition



#### Lower Grid Maze Nodes ####
G <- graph(genMaze(rank), directed = TRUE )
lowerGridCombind<- lowerGrid(rank) # lower grid nodes

#### Calculate all Path ####
allPaths <- all_simple_paths(G, 1,lowerGrid(rank))

#### max colour gives you the points for every route on a black dot ####
maxColour <- NULL
for(i in 1:length(allPaths)){
  maxColour[[i]] <- ifelse(as.numeric(allPaths[[i]]) %in% nodePosition,1,0)
}

#### summing up the total score ####
totalScore <- NULL
for(i in 1:length(allPaths)){
  totalScore[i]<- sum(maxColour[[i]])
}

totalScore.df <- as.data.frame(totalScore)
index <- 1:nrow(totalScore.df)
totalScore.df.1<- cbind.data.frame(index,totalScore.df)
optimisedScore <- totalScore.df.1[which(totalScore.df.1$totalScore == max(totalScore.df.1$totalScore, na.rm = TRUE)), ]
# n<-nrow(optimisedScore)
# M<-matrix(unlist(optimisedScore),ncol=n,byrow=TRUE)
#
# maxnu<-M[2,1]
return(optimisedScore)
}


# maxScore(np(rank=3,satPercent=0.5,seed=1))




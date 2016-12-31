#' @export
#' @param rank This is the Rank of the maze.
#' @param nodePosition Tells you all the position of the black dots.
#' @description This returns several results.
#' @details This function calculates the count of all the possible black node routes, the maximum score one can achieve for a given rank of a colour node position, all the minimum routes possible, and all the possible routes.
#' @author Aiden Loe
#' @title Calculate Maze Parameters
#' @examples
#' rank <- 10
#' nodePosition <- colourNodePosition(rank=10,satPercent=0.5,seed=16)
#' c <- mazeEst(rank,nodePosition)


# require(igraph)
mazeEst <- function(rank, nodePosition){
 #rank<- 10
 #nodePosition <- colourNodePosition(rank=10,satPercent=0.2,seed=1)

  if(rank != nodePosition$rank){
    stop("The input rank and the rank to calculate the colour node positions are not the same.")
  }

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

### POSSBLE BLACK NODE ROUTES ####
totalScore.df <- as.data.frame(totalScore)
index <- 1:nrow(totalScore.df)
totalScore.df.1<- cbind.data.frame(index,totalScore.df)
n<-nrow(totalScore.df.1)
M<-matrix(unlist(totalScore.df.1),ncol=n,byrow=TRUE)

LL<-c()
for (j in 1:length(totalScore)){
  M<-matrix(unlist(totalScore.df.1),ncol=n,byrow=TRUE)
  N<-unlist(maxColour[M[1,j]])
  LL<-c(LL,length(N)) #length of every route
}

# We only want routes that is the same as the rank
W<-which( LL == rank)
endScore <- totalScore.df.1[which(totalScore.df.1$index %in% W),]
possibleBlackNodeRoutes<- table(endScore$totalScore)
pbnr <- t(as.matrix(possibleBlackNodeRoutes))
colnames(pbnr) <- paste(colnames(pbnr),"dot(s)" , sep = "_")
rownames(pbnr) <- c('Path')

#number of steps & optimal paths
# totalScore.df <- as.data.frame(totalScore)
# index <- 1:nrow(totalScore.df)
# totalScore.df.1<- cbind.data.frame(index,totalScore.df)

#### MAXIMUM SCORE ####
maxScore <- totalScore.df.1[which(totalScore.df.1$totalScore == max(totalScore.df.1$totalScore, na.rm = TRUE)), ]
totalScore.df.1
n<-nrow(maxScore)
M<-matrix(unlist(maxScore),ncol=n,byrow=TRUE)
maxnu<-M[2,1]

#### MIN STEP ####
LL<-c()
for (j in 1:n){
  M<-matrix(unlist(maxScore),ncol=n,byrow=TRUE)
  N<-unlist(maxColour[M[1,j]])
  LL<-c(LL,length(N))
}

#print("the minimum number of steps for the optimal solution is: ")
minStep <- (min(LL)-1)

# MINIMUM LEG ROUTES ####
#print("The optimium path(s) with minimum legs is: ")
W<-which( LL == min(LL))

allminPath <- allPaths[M[1,W]]
allminPath<- do.call("rbind",allminPath)
m2 <- 1:nrow(allminPath)
rownames(allminPath) <- rownames(m2, do.NULL = FALSE, prefix = "min.Route.")
colnames(allminPath) <- paste("Step_",0:minStep, sep = "")

#print("the minimum number of steps for the optimal solution is: ")
#print(min(LL)-1)
#print(("the number of solutions is: "))
minLegRoutes <- length(W)


#### ALL POSSIBLE PATH ####
#print("The optimal paths are: ")
totalStep <- (max(LL)-1)
allPossiblePaths <-which( LL == rank) #only select those that reaches to the top
allPath <- allPaths[M[1,allPossiblePaths]]
allPath<- do.call("rbind",allPath)
m2 <- 1:nrow(allPath)

rownames(allPath) <- rownames(m2, do.NULL = FALSE, prefix = "pos.Route.")
colnames(allPath) <- paste("Step_",0:totalStep, sep = "")
#print(("the number of solutions is: "))
maxScoreRoutes <- nrow(allPath)


  est <- list(rank = rank,
              nodePosition = nodePosition,
              maxScore=maxnu,
              possibleBlackNodeRoutes=pbnr,
              minStep=minStep,
              minPath = list(allminPath = allminPath,
                             minRoutes=minLegRoutes),
              allPP = list(allPath = allPath,
                           maxScoreRoutes=maxScoreRoutes))


  class(est) <- c("est","aig")

return(est)

}

#
#
#  rank <- 3
# # satPercent <- 0.5
# # genUniqueSolution(rank,satPercent,15)
#  nodePosition <- colourNodePosition(rank=3,satPercent=0.5,seed=1)
#  c <- cal(rank,nodePosition)
#  c

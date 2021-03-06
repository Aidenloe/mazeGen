% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genPathSeed.R
\name{genPathSeed}
\alias{genPathSeed}
\title{Generate Path Seed}
\usage{
genPathSeed(path = 3, rank = 5, satPercent = 0.5, seed = 1,
  runSeed = 500)
}
\arguments{
\item{path}{Selecting the specific number of paths to achieve the maximum score.}

\item{rank}{This is the rank of the maze.}

\item{satPercent}{This is of saturation percentage ranging from 0-1.}

\item{seed}{The starting seed to begin searching for the seed with specific paths.}

\item{runSeed}{This determines the number of searches for the specific paths before stopping.}
}
\description{
This generate the solution by searching for the SEED that returns the specific number of paths to achieve the maximum score for a given rank and saturation.
}
\details{
This might be computationally intensive as the maze size increases. The seed is necessary so that the algorithm does not always begin from the smallest seed value. Based on the starting seed value, it will search for the next seed that returns the desired number of path defined by the user. To limit the search time, The function will stop looking for the seed based on the runSeed value. Using this function does not guarantee that the minimum number of steps will be the same for all possible paths to achieve the maximum score. To ensure that the number of steps are equal across all possible paths for the maximum score, please use the \code{\link{genEMLseed}} function instead.
}
\examples{

rank <- 5
satPercent <- 0.5
seed <- 1

#Search for just one unique path
justOne <- genPathSeed(path=1,rank=rank,satPercent=satPercent,seed=seed)
nodePosition <- np(rank,satPercent,seed=justOne)
mazeEst(nodePosition)

#Search for three path
justThree <- genPathSeed(path=3,rank=rank,satPercent=satPercent,seed=seed, runSeed=300)
nodePosition <- np(rank,satPercent,seed=justThree)
mazeEst(nodePosition)
}
\seealso{
\code{\link{np}},\code{\link{mazeEst}}, \code{\link{genEMLseed}}
}
\author{
Aiden Loe and Maria Sanchez
}

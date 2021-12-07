# mygraph  
<!-- badges: start -->
[![R-CMD-check](https://github.com/panwenchu98/mygraph/actions/workflows/check-release.yaml/badge.svg)](https://github.com/panwenchu98/mygraph/actions/workflows/check-release.yaml)
[![test-coverage](https://github.com/panwenchu98/mygraph/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/panwenchu98/mygraph/actions/workflows/test-coverage.yaml)
<!-- badges: end -->
----

## Overview  
* This package is another version of two graph problems, MST(minimun spanning tree) and SSSP(single-source shortest-path)  
  * For MST, uses the Kruskal algoritm  
  * For SSSP, uses Dijkstra algorithm for undirected graph, and Bellman-Ford algorithm for directed graph  
  * You can learn more about them in *Tutorial.html* in vignette folder.  
----

## installation  
  * install.packages('mygraph') 
  
----
## Usage
```{r}
library(igraph)
library(mygraph)
m=matrix(c(0,1,5,0,2,4,0,3,3,1,5,3,1,4,5,2,5,3,2,6,2,6,3,2,4,1,5,4,7,4,5,7,3,6,7,5)+1,ncol=3,byrow=TRUE)
g1=make_graph(t(m[,1:2]),directed=TRUE)
g2=make_graph(t(m[,1:2]),directed=FALSE)
graph_attr(g1,"weight")=m[,3]
graph_attr(g2,"weight")=m[,3]

mstree(g1)
$IGRAPH d579548 DN-- 8 7 -- 
+ attr: weight (g/c), name (v/c)
+ edges from d579548 (vertex names):
[1] 1->4 2->6 3->6 3->7 5->8 6->8 7->4

sssp(g1,1,7)
$1   3   7 
"1" "3" "7"

sssp(g2,1,7)
$1   4   7 
"1" "4" "7"
```

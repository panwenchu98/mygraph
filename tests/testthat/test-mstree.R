test_that("multiplication works", {
  #on small given graph
  library(igraph)
  m=matrix(c(0,1,5,0,2,4,0,3,3,1,5,3,1,4,5,2,5,3,2,6,2,6,3,2,4,1,5,4,7,4,5,7,3,6,7,5)+1,ncol=3,byrow=TRUE)
  g=make_graph(t(m[,1:2]),directed=TRUE)
  graph_attr(g,"weight")=m[,3]

  tree=mst(g,weights=graph_attr(g,"weight"))
  m1=get.edgelist(mstree(g)); m1=matrix(as.numeric(m1),ncol=2)
  m1=m1[order(m1[,2]),]; m1=m1[order(m1[,1]),]
  m2=get.edgelist(tree); m2=m2[order(m2[,2]),]; m2=m2[order(m2[,1]),]
  expect_equal(as.numeric(m1),as.numeric(m2))

  #on large random graph
  nvert=100   #number of vertices
  nedge=500  #number of edges
  m=matrix(0,nrow=nedge,ncol=3)
  set.seed(1234)
  m[,1]=as.integer(runif(nedge,min=1,max=nvert+0.99))
  m[,2]=as.integer(runif(nedge,min=1,max=nvert+0.99))
  m[,3]=abs(rnorm(nedge))+1
  nvert=max(m[,1:2])
  g1=make_graph(t(m[,1:2]),directed=TRUE)
  graph_attr(g1,"weight")=m[,3]

  tree=mst(g1,weights=graph_attr(g1,"weight"))
  m1=get.edgelist(mstree(g1)); m1=matrix(as.numeric(m1),ncol=2)
  m1=m1[order(m1[,2]),]; m1=m1[order(m1[,1]),]
  m2=get.edgelist(tree); m2=m2[order(m2[,2]),]; m2=m2[order(m2[,1]),]
  expect_equal(as.numeric(m1),as.numeric(m2))
})

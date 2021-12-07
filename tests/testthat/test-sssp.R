test_that("multiplication works", {
  #on small given graph(both directed and indirected)
  library(igraph)
  m=matrix(c(0,1,5,0,2,4,0,3,3,1,5,3,1,4,5,2,5,3,2,6,2,6,3,2,4,1,5,4,7,4,5,7,3,6,7,5)+1,ncol=3,byrow=TRUE)
  g1=make_graph(t(m[,1:2]),directed=TRUE)
  graph_attr(g1,"weight")=m[,3]
  x1=sssp(g1,1,7)
  x2=shortest_paths(g1,from=1,to=7,weights=graph_attr(g1,"weight"))$vpath[[1]]
  expect_equal(as.numeric(x1),as.numeric(x2))

  g2=make_graph(t(m[,1:2]),directed=FALSE)
  graph_attr(g2,"weight")=m[,3]
  x1=sssp(g2,1,7)
  x2=shortest_paths(g2,from=1,to=7,weights=graph_attr(g2,"weight"))$vpath[[1]]
  expect_equal(as.numeric(x1),as.numeric(x2))


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
  x1=sssp(g1,1,nvert)
  x2=shortest_paths(g1,from=1,to=nvert,weights=graph_attr(g1,"weight"))$vpath[[1]]
  expect_equal(as.numeric(x1),as.numeric(x2))

  g2=make_graph(t(m[,1:2]),directed=FALSE)
  graph_attr(g2,"weight")=m[,3]
  x1=sssp(g2,1,nvert)
  x2=shortest_paths(g2,from=1,to=nvert,weights=graph_attr(g2,"weight"))$vpath[[1]]
  expect_equal(as.numeric(x1),as.numeric(x2))

})

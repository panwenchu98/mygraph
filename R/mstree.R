mstree=function(g){#minimun spanning tree using Kruskal
  m=get.edgelist(g)
  vectornames=names(table(m))
  v=length(vectornames)
  dict=1:v
  names(dict)=vectornames
  m=matrix(dict[m],ncol=2)
  m=cbind(m,graph_attr(g,"weight"))
  m=m[order(m[,3]),]

  which_in=1:v
  m=cbind(m,rep(0,nrow(m)))
  inside=rep(0,v)
  l=0
  while ((sum(inside)<v)&(l<nrow(m))){
    l=l+1

    root1=m[l,1]
    while (which_in[root1]!=root1) root1=which_in[root1]
    x1=m[l,1];
    while (which_in[x1]!=root1){
      x2=x1
      x1=which_in[x1]
      which_in[x2]=root1
    }
    root2=m[l,2]
    while (which_in[root2]!=root2) root2=which_in[root2]
    x1=m[l,2];
    while (which_in[x1]!=root2){
      x2=x1
      x1=which_in[x1]
      which_in[x2]=root2
    }

    if (root1!=root2){
      m[l,4]=1
      inside[m[l,1]]=1
      inside[m[l,2]]=1
      which_in[max(root1,root2)]=min(root1,root2)
    }
  }
  names(vectornames)=1:v
  m[,1:2]=matrix(vectornames[m[,1:2]],ncol=2)
  m=m[which(m[,4]==1),]
  m=m[order(m[,2]),]
  m=m[order(m[,1]),]
  ans=make_graph(t(m[,1:2]),directed=TRUE)
}


library(igraph)
m=matrix(c(0,1,5,0,2,4,0,3,3,1,5,3,1,4,5,2,5,3,2,6,2,3,6,2,4,1,5,4,7,4,5,7,3,6,7,5)+1,ncol=3,byrow=T)
g=make_graph(t(m[,1:2]),directed=TRUE)
graph_attr(g,"weight")=m[,3]
V(g)
get.edgelist(g)
graph_attr(g,"weight")
plot(g,edge.label=graph_attr(g,"weight"))
tree=mst(g,weights=graph_attr(g,"weight"))


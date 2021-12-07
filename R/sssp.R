#'@title Single-Source Shortest-Path
#'
#'@description
#'This is a function for generating the single-source shortest-path for directed or undirected graph without negative edge weights
#'
#'@param g The igraph object of the graph
#'@param startpoint Name of the vertex where the path starts
#'@param endpoint Name of the vertex where the path ends
#'
#'@return A set of vertices names of the shortest path
#'
#'@examples
#'library(igraph)
#'library(mygraph)
#'m=matrix(c(0,1,5,0,2,4,0,3,3,1,5,3,1,4,5,2,5,3,2,6,2,6,3,2,4,1,5,4,7,4,5,7,3,6,7,5)+1,ncol=3,byrow=TRUE)
#'g1=make_graph(t(m[,1:2]),directed=TRUE)
#'g2=make_graph(t(m[,1:2]),directed=FALSE)
#'graph_attr(g1,"weight")=m[,3]
#'graph_attr(g2,"weight")=m[,3]
#'sssp(g1,1,7)
#'sssp(g2,1,7)
#'@export
#'



sssp=function(g,startpoint,endpoint){#single-source shortest-path
  m=get.edgelist(g)
  vectornames=names(table(m))
  v=length(vectornames)
  dict=1:v
  names(dict)=vectornames
  m=matrix(dict[as.character(m)],ncol=2)     #change the names of vertices to 1:v
  m=cbind(m,graph_attr(g,"weight"))
  st=dict[as.character(startpoint)]
  en=dict[as.character(endpoint)]


  if (!is_directed(g)){#if not directed, use Dijkstra as it runs faster
    infi=sum(m[,3])+1 #infinity
    m=rbind(m,matrix(c(m[,2],m[,1],m[,3]),ncol=3))
    dist=rep(infi,v)
    inside=rep(0,v)
    #initialization, adding start point
    dist[st]=0
    inside[st]=1
    prior=1:v
    for (j in which(m[,1]==st)){
      if (m[j,3]<dist[m[j,2]]){
        dist[m[j,2]]=m[j,3]
        prior[m[j,2]]=st
      }
    }
    #each step selecting new vertices closest to chosen ones
    while ((min(inside)==0)&&(min(dist[inside==0])<infi)){
      i=min(which((inside==0)&(dist==min(dist[inside==0]))))
      inside[i]=1
      for (j in which(m[,1]==i)){
        if (dist[m[j,2]]>dist[i]+m[j,3]){
          dist[m[j,2]]=dist[i]+m[j,3]
          prior[m[j,2]]=i
        }
      }
    }
  }else{  #if directed, use Bellman-Ford Algorithm`
    dist=rep(sum(m[,3])+1,v)
    dist[st]=0
    prior=1:v
    for (i in 1:(v-1)){ #at most v-1 iteration will finish up, unless there is negative cycle
      done_something=FALSE
      for (j in 1:nrow(m)){
        if (dist[m[j,2]]>dist[m[j,1]]+m[j,3]){
          done_something=TRUE
          dist[m[j,2]]=dist[m[j,1]]+m[j,3]
          prior[m[j,2]]=m[j,1]
        }
      }
      if (!done_something) break;
    }
  }
  #generating shortest path
  paths=c(en)
  temp=en
  while (prior[temp]!=temp){
    temp=prior[temp]
    paths=c(temp,paths)
  }
  names(vectornames)=1:v
  ans=vectornames[paths]
  names(ans)=c()
  return(ans)
}

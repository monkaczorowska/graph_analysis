install.packages("igraph")
install.packages("stats")
library(igraph)
library(stats)

#wczytanie opisu wezlow i krawedzi
path_description = 'WorldCities.clu'
description = read.table(path_description,encoding = 'UTF-8',stringsAsFactors = FALSE, sep='\t')
#wczytanie krajow
path_countries = 'WorldCitiesCountries.clu'
countries = read.table(path_countries,encoding = 'UTF-8',stringsAsFactors = FALSE, sep='\t')
#wczytanie kontynentow
path_continents = 'WorldCitiesContinents.clu'
continents = read.table(path_continents,encoding = 'UTF-8',stringsAsFactors = FALSE, sep='\t')
#wczytanie stolic
path_capitals = 'WorldCitiesCapitals.clu'
capitals = read.table(path_capitals,encoding = 'UTF-8',stringsAsFactors = FALSE, sep='\t')
#wczytanie grafu
path_graph='WorldCities.net'
graph = read_graph(path_graph, format = "pajek")

#wykres grafu
plot(graph)

#oznakowanie danych: miasto, rodzaj firmy
V(graph)$description = description$V1
#oznakowanie danych: kontynent
V(graph)$continents = continents$V1
#oznakowanie danych: kraj
V(graph)$countries = countries$V1
#oznakowanie danych: stolica
V(graph)$capitals = capitals$V1


#elementy statystyki opisowej dla stopnia
#funkcje zwracajace wezly o min, max stopniu, wieksze od mediany i sredniej
minDegree=function(degree){
  min=min(degree)
  min=degree[degree==min]
  return(min)
}

maxDegree=function(degree){
  max=max(degree)
  max=degree[degree==max]
  return(max)
}

medianDegree=function(degree){
  median=median(degree)
  median=degree[degree>median]
  return(median)
}

meanDegree=function(degree){
  mean=mean(degree)
  mean=degree[degree>mean]
  return(mean)
}

varDegree=function(degree){
  var=var(degree)
  return(var)
}

sdDegree=function(degree){
  sd=sd(degree)
  return(sd)
}

#stopien dla kazdego wezla miasto
degreeCities=degree(graph, v=V(graph)$description=="0",mode="all")
sort(degreeCities)
minCities=minDegree(degreeCities)
max_Cities=maxDegree(degreeCities)
median_Cities=medianDegree(degreeCities)
mean_Cities=meanDegree(degreeCities)
var_Cities=varDegree(degreeCities)
sd_Cities=sdDegree(degreeCities)
minCities
max_Cities
median_Cities
mean_Cities
var_Cities
sd_Cities

#rozklad za pomoca liczebnosci wykres
plot(table(degree(graph, v=V(graph)$description=="0",mode="all")))

#stopien dla kazdego wezla firma
degreeCompanies=degree(graph, v=V(graph)$description!="0",mode="all")
sort(degreeCompanies)
minCompanies=minDegree(degreeCompanies)
max_Companies=maxDegree(degreeCompanies)
median_Companies=medianDegree(degreeCompanies)
mean_Companies=meanDegree(degreeCompanies)
var_Companies=varDegree(degreeCompanies)
sd_Companies=sdDegree(degreeCompanies)
minCompanies
max_Companies
median_Companies
mean_Companies
var_Companies
sd_Companies

#rozklad za pomoca liczebnosci wykres
plot(table(degree(graph, v=V(graph)$description!="0",mode="all")))

#statystyka dla stopnia
degreeFeature=function(degree,desc){
 min=minDegree(degree)
 max=maxDegree(degree)
 median=medianDegree(degree)
 mean=meanDegree(degree)
 mean_=mean(degree)
 var=varDegree(degree)
 sd=sdDegree(degree)
 print(desc)
 print("min")
 print(min)
 print("max")
 print(max)
 print("median")
 print(median)
 print("mean")
 print(mean_)
 print(mean)
 print("var")
 print(var)
 print("sd")
 print(sd)
}

#statystyka stopnia dla firm 1 2 3 4 5 6
degreeCompanies=degree(graph, v=V(graph)$description=="6",mode="all")
sort(degreeCompanies)
degreeFeature(degreeCompanies,"6")

#stopien kontynent Europe Asia North America South America Australia and Ocenia
degreeCompaniesCon=degree(graph, v=V(graph)$continents=="Australia and Oceania",mode="all")
sort(degreeCompaniesCon)
degreeFeature(degreeCompaniesCon,"Australia and Oceania")
plot(table(degree(graph, v=V(graph)$continents=="Australia and Oceania",mode="all")))

#stopien firmy dla United States of America
degreeCompaniesCon=degree(graph, v=V(graph)$countries=="United States of America",mode="all")
sort(degreeCompaniesCon)
degreeFeature(degreeCompaniesCon,"United States of America")
plot(table(degree(graph, v=V(graph)$countries=="United States of America",mode="all")))

#sredni stopien dla stolic
degreeCapitalMean=degree(graph, v=V(graph)$capitals=="1",mode="all")
mean(degreeCapitalMean)
#sredni stopien dla miast ktore nie sa stolica
degreeNoCapitalMean=degree(graph, v=V(graph)$capitals=="0",mode="all")
mean(degreeNoCapitalMean)

#centralność stopnia
#centralnosc stopnia wszystkie miasta w danym kontynencie i firma
centralityDegreeBank=function(continent, bank){
 graphCities=induced.subgraph(graph,vids = V(graph)$continents==continent | V(graph)$id==bank)
 #plot(graphCities)
 degreeCities=degree(graphCities, v=V(graphCities), mode="all")
 sort(degreeCities)
 centralityDegree=degreeCities/(length(degreeCities)-1)
 #sort(centralityDegree)
 return (max(centralityDegree))
}

#centralnosc stopnia wszystkie miasta z danego panstwa i firma
centralityDegreeBankCountry=function(country, bank){
  graphCities=induced.subgraph(graph,vids = V(graph)$countries==country | V(graph)$id==bank)
  #plot(graphCities)
  degreeCities=degree(graphCities, v=V(graphCities), mode="all")
  sort(degreeCities)
  centralityDegree=degreeCities/(length(degreeCities)-1)
  #sort(centralityDegree)
  return (max(centralityDegree))
}

#udzial kazdego banku na danym kontynencie: Europe, North America, Asia, Africa
banks=induced.subgraph(graph,vids = V(graph)$description=="3")
banks_names=V(banks)$id
results=c()
for(i in banks_names){
  r=centralityDegreeBank("North America",i)
  results=c(results,r)
}
banks_names
results
shapiro.test(results)

#udzial kazdego banku w danym panstwie
banks=induced.subgraph(graph,vids = V(graph)$description=="3")
banks_names=V(banks)$id
results=c()
for(i in banks_names){
  r=centralityDegreeBankCountry("United States of America",i)
  results=c(results,r)
}
banks_names
results

#centralnosc stopnia wszystkie banki i dane miasto
centralityDegreeBankCapitals=function(desc, city){
  graphCities=induced.subgraph(graph,vids = V(graph)$description==desc | V(graph)$id==city)
  plot(graphCities)
  degreeCities=degree(graphCities, v=V(graphCities), mode="all")
  sort(degreeCities)
  centralityDegree=degreeCities/(length(degreeCities)-1)
  #sort(centralityDegree)
  return (max(centralityDegree))
}

#udzial bankow w stolicach
capitals=induced.subgraph(graph,vids = V(graph)$capitals=="1")
capitals_names=V(capitals)$id
results=c()
for(i in capitals_names){
  r=centralityDegreeBankCapitals("3",i)
  results=c(results,r)
}
capitals_names
results
rr=list()
rr$V=capitals_names
rr$V1=results

#macierz podobienstwa contynent i rodzaj firmy 
similaritiesCountries=function(continent,desc){
 cities=induced.subgraph(graph,vids = V(graph)$continents==continent | V(graph)$description==desc)
 #plot(capitals)
 cities_names=V(cities)$id
 sim=similarity(cities)
 rownames(sim)=cities_names
 colnames(sim)=cities_names
 return(sim)
}

#macierz podobienstwa dwa kontynenty i rodzaj firmy
similaritiesCountriesCon=function(continent1, continent2,desc){
  cities=induced.subgraph(graph,vids = V(graph)$continents==continent1 |V(graph)$continents==continent2 | V(graph)$description==desc)
  #plot(capitals)
  cities_names=V(cities)$id
  sim=similarity(cities)
  rownames(sim)=cities_names
  colnames(sim)=cities_names
  return(sim)
}

# wybranie par miast ktore sa najbardziej podobne
sim=similaritiesCountriesCon("Europe", "North America","3")
rowNames=rownames(sim)
for(j in rowNames){
  for(i in rowNames){
   if(j>i & sim[i,j]>0.8){
     print("--------------------")
     print(i)
     print(j)
    print(sim[i,j])
   }
 }
}

#wyswietlenie sasiadow dla danego wezla
cities=induced.subgraph(graph,vids = V(graph)$continents=="Europe" | V(graph)$description=="4")
plot(graph.neighborhood(cities,nodes = V(cities)["Brussels"])[[1]])
neighbors(capitals, "Tokyo")$id


#analiza dla grafu utworzonego na podstawie macierzy
#tworzenie grafu lacznosci miast na odstawie firm, wezly firm jako krawedzie
#argumenty panstwo oraz rodzaj firmy
connectionBetweenCities=function(country,company){
  
  citiesCompanies = induced.subgraph(graph,vids = V(graph)$countries == country | V(graph)$description == company)
  cities = induced.subgraph(graph,vids = V(graph)$countries == country)
  cities_name=V(cities)$id
  len=length(cities_name)
  # przygotowanie macierzy
  mat=matrix(0, nrow=len,ncol=len) #wypelnienie 0
  rownames(mat) <- cities_name
  colnames(mat) <- cities_name
  for(m in cities_name){ 
    neighbor=neighbors(citiesCompanies, m)$id #miasta, dla kazdego miasta pobierani sa sasiedzi
    for(s in neighbor){
      neighbor2=neighbors(citiesCompanies, s)$id  #dla kazdego sasiada pobierani sa jego sasiedzi
      citiesToMat=intersect(cities_name, neighbor2) # czesc wspolna, wsrod sasiad2 wybierane sa tylko miasta a nie firmy
      for(i in citiesToMat){ #dla kazdego wybranego miasta inskrementacja w tablicy 
        mat[i,m]=mat[i,m]+1
      }
      
    }
  }
  return(mat)
}
#argumenty kontynent oraz rodzaj firmy
connectionBetweenCitiesContinent=function(continent,company){
  
  citiesCompaniesCon = induced.subgraph(graph,vids = V(graph)$continents == continent | V(graph)$description == company)
  citiesCon = induced.subgraph(graph,vids = V(graph)$continents == continent)
  cities_nameCon=V(citiesCon)$id
  len=length(cities_nameCon)
  # przygotowanie macierzy
  mat=matrix(0, nrow=len,ncol=len) #wypelnienie 0
  rownames(mat) <- cities_nameCon
  colnames(mat) <- cities_nameCon
  for(m in cities_nameCon){ 
    neighbor=neighbors(citiesCompaniesCon, m)$id #miasta, dla kazdego miasta pobierani sa sasiedzi
    for(s in neighbor){
      neighbor2=neighbors(citiesCompaniesCon, s)$id  #dla kazdego sasiada pobierani sa jego sasiedzi
      citiesToMat=intersect(cities_nameCon, neighbor2) # czesc wspolna, wsrod sasiad2 wybierane sa tylko miasta a nie firmy
      for(i in citiesToMat){ #dla kazdego wybranego miasta inskrementacja w tablicy 
        mat[i,m]=mat[i,m]+1
      }
      
    }
  }
  return(mat)
}
#argumenty czyStolica oraz rodzaj firmy
connectionBetweenCitiesCapital=function(capital,company){
  
  citiesCompaniesCap = induced.subgraph(graph,vids = V(graph)$capitals == capital | V(graph)$description == company)
  citiesCap = induced.subgraph(graph,vids = V(graph)$capitals == capital)
  cities_nameCap=V(citiesCap)$id
  len=length(cities_nameCap)
  # przygotowanie macierzy
  mat=matrix(0, nrow=len,ncol=len) #wypelnienie 0
  rownames(mat) <- cities_nameCap
  colnames(mat) <- cities_nameCap
  for(m in cities_nameCap){ 
    neighbor=neighbors(citiesCompaniesCap, m)$id #miasta, dla kazdego miasta pobierani sa sasiedzi
    for(s in neighbor){
      neighbor2=neighbors(citiesCompaniesCap, s)$id  #dla kazdego sasiada pobierani sa jego sasiedzi
      citiesToMat=intersect(cities_nameCap, neighbor2) # czesc wspolna, wsrod sasiad2 wybierane sa tylko miasta a nie firmy
      for(i in citiesToMat){ #dla kazdego wybranego miasta inskrementacja w tablicy 
        mat[i,m]=mat[i,m]+1
      }
      
    }
  }
  return(mat)
}
#argumenty panstwo jedna firma
connectionBetweenCitiesID=function(country,company){
  citiesCompanies = induced.subgraph(graph,vids = V(graph)$countries == country | V(graph)$id == company)
  cities = induced.subgraph(graph,vids = V(graph)$countries == country)
  cities_name=V(cities)$id
  len=length(cities_name)
  # przygotowanie macierzy
  mat=matrix(0, nrow=len,ncol=len) #wypelnienie 0
  rownames(mat) <- cities_name
  colnames(mat) <- cities_name
  for(m in cities_name){ 
    neighbor=neighbors(citiesCompanies, m)$id #miasta, dla kazdego miasta pobierani sa sasiedzi
    for(s in neighbor){
      neighbor2=neighbors(citiesCompanies, s)$id  #dla kazdego sasiada pobierani sa jego sasiedzi
      citiesToMat=intersect(cities_name, neighbor2) # czesc wspolna, wsrod sasiad2 wybierane sa tylko miasta a nie firmy
      for(i in citiesToMat){ #dla kazdego wybranego miasta inskrementacja w tablicy 
        mat[i,m]=mat[i,m]+1
      }
      
    }
  }
  return(mat)
}
#argumenty kontynent oraz jedna firma
connectionBetweenCitiesContinentID=function(continent,company){
  
  citiesCompaniesCon = induced.subgraph(graph,vids = V(graph)$continents == continent | V(graph)$id == company)
  citiesCon = induced.subgraph(graph,vids = V(graph)$continents == continent)
  cities_nameCon=V(citiesCon)$id
  len=length(cities_nameCon)
  # przygotowanie macierzy
  mat=matrix(0, nrow=len,ncol=len) #wypelnienie 0
  rownames(mat) <- cities_nameCon
  colnames(mat) <- cities_nameCon
  for(m in cities_nameCon){ 
    neighbor=neighbors(citiesCompaniesCon, m)$id #miasta, dla kazdego miasta pobierani sa sasiedzi
    for(s in neighbor){
      neighbor2=neighbors(citiesCompaniesCon, s)$id  #dla kazdego sasiada pobierani sa jego sasiedzi
      citiesToMat=intersect(cities_nameCon, neighbor2) # czesc wspolna, wsrod sasiad2 wybierane sa tylko miasta a nie firmy
      for(i in citiesToMat){ #dla kazdego wybranego miasta inskrementacja w tablicy 
        mat[i,m]=mat[i,m]+1
      }
      
    }
  }
  return(mat)
}
#argumenty czyStolica oraz rodzaj firmy
connectionBetweenCitiesCapitalID=function(capital,company){
  
  citiesCompaniesCap = induced.subgraph(graph,vids = V(graph)$capitals == capital | V(graph)$id == company)
  citiesCap = induced.subgraph(graph,vids = V(graph)$capitals == capital)
  cities_nameCap=V(citiesCap)$id
  len=length(cities_nameCap)
  # przygotowanie macierzy
  mat=matrix(0, nrow=len,ncol=len) #wypelnienie 0
  rownames(mat) <- cities_nameCap
  colnames(mat) <- cities_nameCap
  for(m in cities_nameCap){ 
    neighbor=neighbors(citiesCompaniesCap, m)$id #miasta, dla kazdego miasta pobierani sa sasiedzi
    for(s in neighbor){
      neighbor2=neighbors(citiesCompaniesCap, s)$id  #dla kazdego sasiada pobierani sa jego sasiedzi
      citiesToMat=intersect(cities_nameCap, neighbor2) # czesc wspolna, wsrod sasiad2 wybierane sa tylko miasta a nie firmy
      for(i in citiesToMat){ #dla kazdego wybranego miasta inskrementacja w tablicy 
        mat[i,m]=mat[i,m]+1
      }
      
    }
  }
  return(mat)
}
#wstawianie 0 na przekatnej aby miasto nie bylo w relacji same z soba
zerosOnDiagonal=function(mat){
  for(i in row.names(mat)){
    mat[i,i]=0
  }
  return(mat)
}

#argumenty kontynent oraz rodzaj firmy
connectionBetweenCitiesContinent2=function(continent1,continent2, company){
  
  citiesCompaniesCon = induced.subgraph(graph,vids = V(graph)$continents == continent1 | V(graph)$continents == continent2  | V(graph)$description == company)
  citiesCon = induced.subgraph(graph,vids = V(graph)$continents == continent1 | V(graph)$continents == continent2 )
  cities_nameCon=V(citiesCon)$id
  len=length(cities_nameCon)
  # przygotowanie macierzy
  mat=matrix(0, nrow=len,ncol=len) #wypelnienie 0
  rownames(mat) <- cities_nameCon
  colnames(mat) <- cities_nameCon
  for(m in cities_nameCon){ 
    neighbor=neighbors(citiesCompaniesCon, m)$id #miasta, dla kazdego miasta pobierani sa sasiedzi
    for(s in neighbor){
      neighbor2=neighbors(citiesCompaniesCon, s)$id  #dla kazdego sasiada pobierani sa jego sasiedzi
      citiesToMat=intersect(cities_nameCon, neighbor2) # czesc wspolna, wsrod sasiad2 wybierane sa tylko miasta a nie firmy
      for(i in citiesToMat){ #dla kazdego wybranego miasta inskrementacja w tablicy 
        mat[i,m]=mat[i,m]+1
      }
      
    }
  }
  return(mat)
}

#polaczenie miedzy miastami Europy i Polnocnej Ameryki, firmy bankowe
matCitiesINEuropeNorthAmerica=connectionBetweenCitiesContinent2("Europe","North America","3")
matCitiesINEuropeNorthAmerica=zerosOnDiagonal(matCitiesINEuropeNorthAmerica)
#konwersja z macierzy do grafu
graphCitiesINEuropeNorthAmerica <- graph_from_adjacency_matrix(matCitiesINEuropeNorthAmerica, mode="undirected" ,weighted=TRUE)
plot(graphCitiesINEuropeNorthAmerica)
#liczba wierzchołków
len=length(V(graphCitiesINEuropeNorthAmerica))
#wagi
E(graphCitiesINEuropeNorthAmerica)$weight
#strength
st=strength(graphCitiesINEuropeNorthAmerica, v=V(graphCitiesINEuropeNorthAmerica),mode="all")
sort(st)
#degree
degreeEuropeAmerica=degree(graphCitiesINEuropeNorthAmerica, mode='all')
degreeEuropeAmerica
sort(degreeEuropeAmerica)
#statystyka dla degree
degreeFeature(degreeEuropeAmerica,"EuropeAmeryka")
#moc przez stopień
StrengthDegree=st/degreeEuropeAmerica
sort(StrengthDegree)
#gestość
edge_density(graphCitiesINEuropeNorthAmerica)
#porownanie gestosci z grafem losowym
res=c()
for(i in c(1:30))
{
  r_graph = random.graph.game(length(V(graphCitiesINEuropeNorthAmerica)),edge_density(graphEurope))
  res=c(res,mean(degree(r_graph,v=V(r_graph))))
  print(mean(degree(r_graph,v=V(r_graph))))
  
}
t.test(res, mu=mean(degree(graphCitiesINEuropeNorthAmerica,v=V(graphCitiesINEuropeNorthAmerica))))
mean(degree(graphCitiesINEuropeNorthAmerica,v=V(graphCitiesINEuropeNorthAmerica)))

#centralność stopnia
centrality=degreeEuropeAmerica/(len-1)
centrality
#kraje najbardziej centralne, najwyzszy wspolczynnik centralnosci
sort(centrality)

#centralnosc globalna
sacaled_degree=(max(degreeEuropeAmerica)-degreeEuropeAmerica)
len=length(degreeEuropeAmerica)
empirical_global_degree_centrality=sum(sacaled_degree)/((len-1)*(len-2))
empirical_global_degree_centrality

#centralnosc posredniczaca
#graf bez wag
graphCitiesINEuropeNorthAmericaNOweights=graphCitiesINEuropeNorthAmerica
E(graphCitiesINEuropeNorthAmericaNOweights)$weight <- 1
paths=betweenness(graphCitiesINEuropeNorthAmericaNOweights)
sort(paths)

#odwrocenie wag
matnoWeighs=matCitiesINEuropeNorthAmerica
for(i in row.names(matnoWeighs)){
  for( j in row.names(matnoWeighs))
    if(matnoWeighs[i,j]!=0)
      matnoWeighs[i,j]=1/matnoWeighs[i,j]
}

#centralnosc posredniczaca
graphCitiesINEuropeNorthAmericaInverseWeights <- graph_from_adjacency_matrix(matnoWeighs, mode="undirected" ,weighted=TRUE)
paths=betweenness(graphCitiesINEuropeNorthAmericaInverseWeights)
sort(paths)

#podobienstwo
sim=similarity(graphCitiesINEuropeNorthAmerica)
sim
#na podstawie macierzy poodbienstwa zostanie obliczona odeleglosc
#macierz odleglosci kazdego z kazdym
distance=dist(sim)
cluster_tree=hclust(distance)
#wykres klastrow
plot(cluster_tree,labels=as_ids(V(graphCitiesINEuropeNorthAmerica)))
#analiza klastrowa
clusters = cutree(cluster_tree, k=5)
#przypisanie wierzcholkow numery klastrow
V(graphCitiesINEuropeNorthAmerica)$cluster=clusters
#wyodrebnienie poszczegolnych klastrow
cluster_1 = induced.subgraph(graphCitiesINEuropeNorthAmerica,vids=V(graphCitiesINEuropeNorthAmerica)$cluster==1)
cluster_2 = induced.subgraph(graphCitiesINEuropeNorthAmerica,vids=V(graphCitiesINEuropeNorthAmerica)$cluster==2)
cluster_3 = induced.subgraph(graphCitiesINEuropeNorthAmerica,vids=V(graphCitiesINEuropeNorthAmerica)$cluster==3)
cluster_4 = induced.subgraph(graphCitiesINEuropeNorthAmerica,vids=V(graphCitiesINEuropeNorthAmerica)$cluster==4)
cluster_5 = induced.subgraph(graphCitiesINEuropeNorthAmerica,vids=V(graphCitiesINEuropeNorthAmerica)$cluster==5)
plot(cluster_1)
#gestosc i srednia stopnia dla kazdego klastra
edge_density(cluster_1)
mean(degree(cluster_1))
edge_density(cluster_2)
mean(degree(cluster_2))
edge_density(cluster_3)
mean(degree(cluster_3))
edge_density(cluster_4)
mean(degree(cluster_4))
edge_density(cluster_5)
mean(degree(cluster_5))

#szukanie podobnych par 
rownames(sim)=rownames(matCitiesINEuropeNorthAmerica)
colnames(sim)=rownames(matCitiesINEuropeNorthAmerica)
rowNames=rownames(sim)
for(j in rowNames){
  for(i in rowNames){
    if(j>i & sim[i,j]>0.9){
      print("--------------------")
      print(i)
      print(j)
      print(sim[i,j])
    }
  }
}


# Jaki procent wszystkich polaczen ma dany kraj to polaczenia z krajem z tego samego klastra?
#relacje Ljubljana (polaczen handlowych) z innymi panstwami w obrebie tego samego klastra
#stopien
degree(cluster_5,V(cluster_5)["Ljubljana"])
# stopien uwzgledniejac wszystkich kraje
degree(graphCitiesINEuropeNorthAmerica,V(graphCitiesINEuropeNorthAmerica)["Ljubljana"])

#relacje Madryt (polaczen handlowych) z innymi panstwami w obrebie tego samego klastra
#stopien
a=degree(cluster_2,V(cluster_2)["Madrid"])
# stopien uwzgledniejac wszystkich kraje
b=degree(graphCitiesINEuropeNorthAmerica,V(graphCitiesINEuropeNorthAmerica)["Madrid"])
a/b

#klaster 1
inter_cluster1 = c()
for (v in V(cluster_1)$name){
  r=degree(cluster_1,V(cluster_1)[v])/degree(graphCitiesINEuropeNorthAmerica,V(graphCitiesINEuropeNorthAmerica)[v])
  inter_cluster1 = c(inter_cluster1,r)
}
mean(inter_cluster1)

#klaster 2
inter_cluster2 = c()
for (v in V(cluster_2)$name){
  r=degree(cluster_2,V(cluster_2)[v])/degree(graphCitiesINEuropeNorthAmerica,V(graphCitiesINEuropeNorthAmerica)[v])
  inter_cluster2 = c(inter_cluster2,r)
}
mean(inter_cluster2)

#klaster 4
inter_cluster4 = c()
for (v in V(cluster_4)$name){
  r=degree(cluster_4,V(cluster_4)[v])/degree(graphCitiesINEuropeNorthAmerica,V(graphCitiesINEuropeNorthAmerica)[v])
  inter_cluster4 = c(inter_cluster4,r)
}
mean(inter_cluster4)

#klaster 5
inter_cluster5 = c()
for (v in V(cluster_5)$name){
  r=degree(cluster_5,V(cluster_5)[v])/degree(graphCitiesINEuropeNorthAmerica,V(graphCitiesINEuropeNorthAmerica)[v])
  inter_cluster5 = c(inter_cluster5,r)
}
mean(inter_cluster5)

# test statystyczny ANOVA aby wykazać różnice statystyczne między grupami
df = data.frame("group"=factor(V(graphCitiesINEuropeNorthAmerica)$cluster), "degree" = degree(graphCitiesINEuropeNorthAmerica))
summary(aov(degree~group,data = df))
aov_res=aov(degree~group,data = df)
#test Tukeya aby zobaczyć między którymi grupami są różnice
tuk_res<-TukeyHSD(aov_res)
plot(tuk_res, las=1)

#wspolczynnik klasteryzacji
edge_density(graph.neighborhood(graphCitiesINEuropeNorthAmerica,nodes = V(graphCitiesINEuropeNorthAmerica)["London"])[[1]])
edge_density(graph.neighborhood(graphCitiesINEuropeNorthAmerica,nodes = V(graphCitiesINEuropeNorthAmerica)["Madrid"])[[1]])
edge_density(graph.neighborhood(graphCitiesINEuropeNorthAmerica,nodes = V(graphCitiesINEuropeNorthAmerica)["Ljubljana"])[[1]])
edge_density(graph.neighborhood(graphCitiesINEuropeNorthAmerica,nodes = V(graphCitiesINEuropeNorthAmerica)["New York"])[[1]])
edge_density(graph.neighborhood(graphCitiesINEuropeNorthAmerica,nodes = V(graphCitiesINEuropeNorthAmerica)["Nicosia"])[[1]])
edge_density(graph.neighborhood(graphCitiesINEuropeNorthAmerica,nodes = V(graphCitiesINEuropeNorthAmerica)["Paris"])[[1]])


#szukanie klik min trzy elementowych
cliques(graphCitiesINEuropeNorthAmerica, min=3) # zwraca liste wszystkich klik ktore spelniaja okreslony warunek
#rozmiar najwiekszej kliki
clique.number(graphCitiesINEuropeNorthAmerica)
#wydobycie najwiekszej kliki
max_clik=cliques(graphCitiesINEuropeNorthAmerica, min=clique.number(graphCitiesINEuropeNorthAmerica))[[1]] #wyciagniecie samej listy wierzcholkow
max_clik
#wyciagniecie maksymalnej kliki z grafu
clique_graph=induced.subgraph(graphCitiesINEuropeNorthAmerica,vids=max_clik)
plot(clique_graph)

#analiza k jader
sort(coreness(graphCitiesINEuropeNorthAmerica))
#analiza komponentow w grafie
comp=components(graphCitiesINEuropeNorthAmerica,mode="weak")
comp

#najkrótsza ścieżka
graph_t=graphCitiesINEuropeNorthAmerica
E(graph_t)$weight = 1

#obliczenie najkrotszych sciezek
path_matrix = shortest.paths(graph_t,v=V(graph_t),to=V(graph_t))
#obliczenie sciezki dla pary
path = shortest_paths(graph_t,V(graph_t)['Krakow'],to=V(graph_t)['Zagreb'])
#pobranie sciezki dla pary wezlow
vertices_in_path = path$vpath[[1]] 

#szukanie sciezek
rowNames=rownames(path_matrix)
for(j in rowNames){
  for(i in rowNames){
    if(j>i & path_matrix[i,j]>=2 & path_matrix[i,j]!=Inf){
      print("--------------------")
      print(i)
      print(j)
      print(path_matrix[i,j])
    }
  }
}

#zapisaywanie grafu
savePath='graphCitiesINEuropeNorthAmerica.net'
write.graph(graphCitiesINEuropeNorthAmerica, savePath,"pajek")

#wyodrebnienei kolejnych grafow
#jeden kraj i wszystkie banki
matCitiesINGermany=connectionBetweenCities("Germany","3")
matCitiesINGermany=zerosOnDiagonal(matCitiesINGermany)
#konwersja z macierzy do grafu
graphCitiesINGermany <- graph_from_adjacency_matrix(matCitiesINGermany, mode="undirected" ,weighted=TRUE)
plot(graphCitiesINGermany)
strength(graphCitiesINGermany, v=V(graphCitiesINGermany),mode="all")

#jeden kontynent i wszystkie banki
matCitiesINAsia=connectionBetweenCitiesContinent("Asia","3")
matCitiesINAsia=zerosOnDiagonal(matCitiesINAsia)
#konwersja z macierzy do grafu
graphCitiesINAsia<- graph_from_adjacency_matrix(matCitiesINAsia, mode="undirected" )
plot(graphCitiesINAsia)

#stolice i wszystkie banki
matCitiesINCapital=connectionBetweenCitiesCapital("1","3")
matCitiesINCapital=zerosOnDiagonal(matCitiesINCapital)
#konwersja z macierzy do grafu
graphCitiesINCapital<- graph_from_adjacency_matrix(matCitiesINCapital, mode="undirected" )
plot(graphCitiesINCapital)

#kraj i jeden wybrany bank
matCitiesINGermany=connectionBetweenCitiesID("Germany","West LB")
matCitiesINGermany=zerosOnDiagonal(matCitiesINGermany)
#konwersja z macierzy do grafu
graphCitiesINGermany <- graph_from_adjacency_matrix(matCitiesINGermany, mode="undirected" )
plot(graphCitiesINGermany)

#kontynent i jeden wybrany bank
matCitiesINAsiaID=connectionBetweenCitiesContinentID("Asia","West LB")
matCitiesINAsia=zerosOnDiagonal(matCitiesINAsia)
#konwersja z macierzy do grafu
graphCitiesINAsia<- graph_from_adjacency_matrix(matCitiesINAsia, mode="undirected" )
plot(graphCitiesINAsia)

#stolice i jeden wybrany bank
matCitiesINCapitalID=connectionBetweenCitiesCapitalID("1","West LB")
matCitiesINCapital=zerosOnDiagonal(matCitiesINCapital)
#konwersja z macierzy do grafu
graphCitiesINCapital<- graph_from_adjacency_matrix(matCitiesINCapital, mode="undirected" )
plot(graphCitiesINCapital)

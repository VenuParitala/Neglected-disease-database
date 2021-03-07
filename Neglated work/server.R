a<-read.csv("Data/abstract-Chagasdise-set_mind.csv")
b<-read.csv("Data/abstract-denguefeve-set_mind.csv")
c<-read.csv("Data/abstract-Dracunculi-set_mind.csv")
d<-read.csv("Data/abstract-Echinococc-set_mind.csv")
e<-read.csv("Data/abstract-Fascilolis-set_mind.csv")
f<-read.csv("Data/abstract-Foodbornet-set_mind.csv")
g<-read.csv("Data/abstract-HumanAfric-set_mind.csv")
h<-read.csv("Data/abstract-Leishmania-set_mind.csv")
i<-read.csv("Data/abstract-Leprosy-set_mind.csv")
j<-read.csv("Data/abstract-Lymphaticf-set_mind.csv")
k<-read.csv("Data/abstract-Mycetoma-set_mind.csv")
l<-read.csv("Data/abstract-Onchocerci-set_mind.csv")
m<-read.csv("Data/abstract-Rabies-set_mind.csv")
n<-read.csv("Data/abstract-Soil-trans-set_mind.csv")
o<-read.csv("Data/abstract-Taeniasisa-set_mind.csv")
p<-read.csv("Data/abstract-Trachoma-set_mind.csv")
q<-read.csv("Data/abstract-Yaws-set_mind.csv")
r<-read.csv("Data/pubmed-Schistosom-set_mind.csv")
s<-read.csv("Data/abstract-buruliulce-set_mind.csv")
t<-read.csv("Data/DISEASE.csv")
u<-read.csv("Data/control.csv")
v<-read.csv("Data/select.csv")
aa<-read.csv("Data/buruli ulcer.csv")
bb<-read.csv("Data/chagas.csv")
cc<-read.csv("Data/dengue.csv")
dd<-read.csv("Data/dracunculiasis disease.csv")
ee<-read.csv("Data/echinococcosis disease.csv")
ff<-read.csv("Data/fascilolisis.csv")
gg<-read.csv("Data/foodbrone.csv")
hh<-read.csv("Data/human african trypanosomiasis.csv")
ii<-read.csv("Data/leishmaniasies.csv")
jj<-read.csv("Data/leprosy.csv")
kk<-read.csv("Data/lymphatic filariasis.csv")
ll<-read.csv("Data/mycetoma.csv")
mm<-read.csv("Data/onchocerciasis.csv")
nn<-read.csv("Data/rabies.csv")
oo<-read.csv("Data/schistosomiasis.csv")
pp<-read.csv("Data/soil transmitted helminthises.csv")
qq<-read.csv("Data/trachoma.csv")
rr<-read.csv("Data/yaws.csv")
ss<-read.csv("Data/(taeniasis and neurocysticercosis).csv")
server<-function(input,output){
 output$Dengue<-renderDataTable({
   b<-as.data.frame(b)
   datatable(b)
 })
 output$plot2<-renderPlot({
     b<-as.data.frame(b)
     b<-head(b,20)
     ggplot(b,aes(b$Gene_symbol,b$Freq))+geom_bar(stat = "identity",fill="green")+
         labs(title = "This Graph Representd by number of articles used top genes",x="Gene_symbol",y="Freq")+theme(axis.text.x = element_text(angle = 90))
 })
 output$chart1<-renderPlot({
     b<-as.data.frame(b)
     b<-ggnetwork(b,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = b,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = b,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Dengue Genes Network")
 })
 output$Rabies<-renderDataTable({
    m<-as.data.frame(m)
    datatable(m)
 })
 output$plot3<-renderPlot({
     m<-as.data.frame(m)
     m<-head(m,20)
     ggplot(m,aes(m$Gene_symbol,m$Freq))+geom_bar(stat = "identity",fill="orange")+
         labs(title = "The graph representd highly used to 20 genes",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
     })
 output$chart2<-renderPlot({
     m<-as.data.frame(m)
     m<-ggnetwork(m,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = m,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = m,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Rabies Genes Network")
 })
 output$Trachoma<-renderDataTable({
    p<-as.data.frame(p)
    datatable(p)
 })
 output$plot4<-renderPlot({
     p<-as.data.frame(p)
     p<-head(p,20)
     ggplot(p,aes(p$Gene_symbol,p$Freq))+geom_bar(stat = "identity",fill="red")+
         labs(title = "The graph represent by top genes used in more abstract",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
     
 })
 output$chart3<-renderPlot({
     p<-as.data.frame(p)
     p<-ggnetwork(p,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = p,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = p,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Teachoma Genes Network")
 })
 output$Yaws<-renderDataTable({
    q<-as.data.frame(q)
    datatable(q)
    
 })
 output$plot5<-renderPlot({
     q<-as.data.frame(q)
     ggplot(q,aes(q$Gene_symbol,q$Freq))+geom_bar(stat = "identity",fill="purple")+
         labs(title = "The plot represent mostly used genes in different articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
 })
 output$chart4<-renderPlot({
     q<-as.data.frame(q)
     q<-ggnetwork(q,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = q,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = q,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Yaws Genes Network")
 })
 output$Leprosy<-renderDataTable({
    i<-as.data.frame(i)
    datatable(i)
 })
 output$plot6<-renderPlot({
     i<-as.data.frame(i)
     i<-head(i,20)
     ggplot(i,aes(i$Gene_symbol,i$Freq))+geom_bar(stat = "identity",fill="green")+
         labs(title = "The plot represnt top genes",x="gene_symbol",y="Freq")+theme(axis.text.x = element_text(angle = 90))
 })
 output$chart5<-renderPlot({
     i<-as.data.frame(i)
     i<-ggnetwork(i,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = i,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = i,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Leprocy Genes Network")
 })
 output$Human_African_trypanosomiasis<-renderDataTable({
    g<-as.data.frame(g)
    datatable(g)
 })
output$plot7<-renderPlot({
    g<-as.data.frame(g)
    g<-head(g,20)
    ggplot(g,aes(g$Gene_symbol,g$Freq))+geom_bar(stat = "identity",fill="purple")+
        labs(title = "This plot represent most of articles used these top genes",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
})
output$chart6<-renderPlot({
    g<-as.data.frame(g)
    g<-ggnetwork(g,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
    ggplot()+
        geom_edges(data = g,
                   aes(x=x,y=y,xend=xend,yend=yend),
                   color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
        geom_nodes(data = g,
                   aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                   alpha=1/2,color="orange")+
        theme_blank()+theme(legend.position = "none")+labs(title = "Human_African_trypanosomiasis Genes Network")
    
})
 output$Leishmaniases<-renderDataTable({
    h<-as.data.frame(h)
    datatable(h)
 })
 output$plot8<-renderPlot({
     h<-as.data.frame(h)
     h<-head(h,20)
     ggplot(h,aes(h$Gene_symbol,h$Freq))+geom_bar(stat = "identity",fill="black")+
         labs(title = "This plot represent most of articles used these top genes",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
 })
 output$chart7<-renderPlot({
     h<-as.data.frame(h)
     h<-ggnetwork(h,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = h,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = h,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Leishmaniases Genes Network")
 })
 output$Taeniasis_and_neurocysticercosis<-renderDataTable({
    o<-as.data.frame(o)
    datatable(o)
 })
 output$plot9<-renderPlot({
     o<-as.data.frame(o)
     o<-head(o,20)
     ggplot(o,aes(o$Gene_symbol,o$Freq))+geom_bar(stat = "identity",fill="green")+
         labs(title = "This plot represent most of articles used these top genes",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
 })
 output$chart8<-renderPlot({
     o<-as.data.frame(o)
     o<-ggnetwork(o,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = o,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = o,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Taeniasis_and_neurocysticercosis Genes Network")
 })
 output$Dracunculiasis<-renderDataTable({
    c<-as.data.frame(c)
    datatable(c)
 })
 output$plot10<-renderPlot({
     c<-as.data.frame(c)
     c<-head(c,20)
     ggplot(c,aes(c$Gene_symbol,c$Freq))+geom_bar(stat = "identity",fill="red")+
         labs(title = "This plot represent most of articles used these top genes",x="gene_symbol",y="freq")+
         theme(axis.text.x = element_text(angle = 90))
 })
 output$chart9<-renderPlot({
     c<-as.data.frame(c)
     c<-ggnetwork(c,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = c,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = c,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Dracunculiasis Genes Network")
 })
 output$Foodborne_trematodiases<-renderDataTable({
    f<-as.data.frame(f)
    datatable(f)
 })
 output$plot11<-renderPlot({
     f<-as.data.frame(f)
     f<-head(f,20)
     ggplot(f,aes(f$Gene_symbol,f$Freq))+geom_bar(stat = "identity",fill="yellow")+
         labs(title = "This plot represent most of articles used these gens",x="gene_symbol",y="Freq")+theme(axis.text.x = element_text(angle = 90))
 })
 output$chart10<-renderPlot({
     f<-as.data.frame(f)
     f<-ggnetwork(f,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = f,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = f,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Foodborne_trematodiases Genes Network")
 })
 output$Echinococcosis<-renderDataTable({
    d<-as.data.frame(d)
    datatable(d)
 })
 output$plot12<-renderPlot({
     d<-as.data.frame(d)
     d<-head(d,20)
     ggplot(d,aes(d$Gene_symbol,d$Freq))+geom_bar(stat = "identity",fill="green")+
         labs(title = "This plot represent most of articles used these genes",x="gene_symbol",y="freq")+
         theme(axis.text.x = element_text(angle = 90))
 })
 output$chart11<-renderPlot({
     d<-as.data.frame(d)
     d<-ggnetwork(q,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = d,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = d,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Echinococcosis Genes Network")
 })
 output$Lymphatic_filariasis<-renderDataTable({
    j<-as.data.frame(j)
    datatable(j)
 })
 output$plot13<-renderPlot({
     j<-as.data.frame(j)
     j<-head(j,20)
     ggplot(j,aes(j$Gene_symbol,j$Freq))+geom_bar(stat = "identity",fill="black")+
         labs(title = "This plot represent most of articles used these genes",x="gene_symbol",y="freq")+
         theme(axis.text.x = element_text(angle = 90))
 })
 output$chart12<-renderPlot({
     j<-as.data.frame(j)
     j<-ggnetwork(q,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = j,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = j,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Lymphatic_filariasis Network")
 })
 output$Onchocerciasis<-renderDataTable({
    l<-as.data.frame(l)
    datatable(l)
 })
 output$plot14<-renderPlot({
     l<-as.data.frame(l)
     l<-head(l,20)
     ggplot(l,aes(l$Gene_symbol,l$Freq))+geom_bar(stat = "identity",fill="green")+
         labs(title = "The plot represent most of articles used these genes",x="gene_symbol",y="freq")+
         theme(axis.text.x = element_text(angle = 90))
 })
 output$chart13<-renderPlot({
     l<-as.data.frame(l)
     l<-ggnetwork(q,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = l,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = l,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Onchocerciasis Genes Network")
 })
 output$Schistosomiasis<-renderDataTable({
    r<-as.data.frame(r)
    datatable(r)
 })
 output$plot15<-renderPlot({
     r<-as.data.frame(r)
     r<-head(r,20)
     ggplot(r,aes(r$Gene_symbol,r$Freq))+geom_bar(stat = "identity",fill="black")+
         labs(title = "This plot represent most of articles used these genes",x="gene_symbol",y="freq")+
         theme(axis.text.x = element_text(angle = 90))
 })
 output$chart14<-renderPlot({
     r<-as.data.frame(r)
     r<-ggnetwork(r,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = r,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = r,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Schistosomiasis Genes Network")
 })
     

 output$Soil_transmitted_helminthiases<-renderDataTable({
    n<-as.data.frame(n)
    datatable(n)
 })
 output$plot16<-renderPlot({
     n<-as.data.frame(n)
     n<-head(n,20)
     ggplot(n,aes(n$Gene_symbol,n$Freq))+geom_bar(stat = "identity",fill="purple")+
         labs(title = "This plot represent most of articles used these gens",x="gene_symbol",y="freq")+
         theme(axis.text.x = element_text(angle = 90))
 })
 output$chart15<-renderPlot({
     n<-as.data.frame(n)
     n<-ggnetwork(n,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = n,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = n,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Soil_transmitted_helminthiases Genes Network")
 })
 output$Mycetoma<-renderDataTable({
    k<-as.data.frame(k)
    datatable(k)
 })
 output$plot17<-renderPlot({
     k<-as.data.frame(k)
     k<-head(k,20)
     ggplot(k,aes(k$Gene_symbol,k$Freq))+geom_bar(stat = "identity",fill="orange")+
         labs(title = "This plot represent most of articles used thes genes",x="gene_symbol",y="freq")+
         theme(axis.text.x = element_text(angle = 90))
 })
 output$chart16<-renderPlot({
     k<-as.data.frame(k)
     k<-ggnetwork(k,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = k,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = k,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Mycetoma Genes Network")
 })
 output$Fascilolisis<-renderDataTable({
    e<-as.data.frame(e)
    datatable(e)
 })
 output$plot18<-renderPlot({
     e<-as.data.frame(e)
     e<-head(e,20)
     ggplot(e,aes(e$Gene_symbol,e$Freq))+geom_bar(stat = "identity",fill="green")+
         labs(title = "This plot represent most of articles used these genes",x="gene_symbol",y="freq")+
         theme(axis.text.x = element_text(angle = 90))
 })
 output$chart17<-renderPlot({
     e<-as.data.frame(e)
     e<-ggnetwork(e,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = e,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = e,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Fascilolisis Genes Network")
 })
 output$Chagasdisease<-renderDataTable({
    a<-as.data.frame(a)
    datatable(a)
 })
 output$plot19<-renderPlot({
     a<-as.data.frame(a)
     a<-head(a,20)
     ggplot(a,aes(a$Gene_symbol,a$Freq))+geom_bar(stat = "identity",fill="purple")+
         labs(title = "The plot represent most of articles used these genes",x="gene_symbol",y="freq")+
         theme(axis.text.x = element_text(angle = 90))
 })
 output$chart18<-renderPlot({
     a<-as.data.frame(a)
     a<-ggnetwork(a,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = a,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = a,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Chagasdisease Genes Network")
 })
 output$Buruli_Ulcer<-renderDataTable({
     s<-as.data.frame(s)
     s<-datatable(s)
 })
 output$plot1<-renderPlot({
     s<-as.data.frame(s)
     ggplot(s,aes(s$Gene_symbol,s$Freq))+geom_bar(stat = "identity",fill="blue")+
         labs(title = "This graph reprsent by number of abstract  at highly used  genes ",x="gene_symbol",y="Freq")+
         theme(axis.text.x = element_text(angle = 90))
 })
 output$chart19<-renderPlot({
     s<-as.data.frame(s)
     s<-ggnetwork(s,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
     ggplot()+
         geom_edges(data = s,
                    aes(x=x,y=y,xend=xend,yend=yend),
                    color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
         geom_nodes(data = s,
                    aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                    alpha=1/2,color="orange")+
         theme_blank()+theme(legend.position = "none")+labs(title = "Buruli_Ulcer Genes Network")
 })
 output$table1<-renderDataTable({
     t<-as.data.frame(t)
     datatable(t,filter = "top")
 })
 
 output$table3<-renderDataTable({
     u<-as.data.frame(u)
     datatable(u,filter = "top")
 })
 output$table2<-renderDataTable({
     v<-as.data.frame(v)
     datatable(v)
 })
 output$plot222<-renderPlot({
     v<-as.data.frame(v)
     ggplot(v,aes(v$Disease.Name,v$Organism.type))+geom_bar(stat = "identity",fill="green")+
         labs(title = "plot is organisms",x="disease name",y="organism type")+theme(axis.text.x = element_text(angle = 90))
 })
 output$tab1<-renderDataTable({
     aa<-as.data.frame(aa)
     datatable(aa)
 })
 output$tab2<-renderDataTable({
     bb<-as.data.frame(bb)
     datatable(bb)
 })
 output$tab3<-renderDataTable({
     cc<-as.data.frame(cc)
     datatable(cc)
 })
 output$tab4<-renderDataTable({
     dd<-as.data.frame(dd)
     datatable(dd)
 })
 output$tab5<-renderDataTable({
     ee<-as.data.frame(ee)
     datatable(ee)
 })
 output$tab6<-renderDataTable({
     ff<-as.data.frame(ff)
     datatable(ff)
 })
 output$tab7<-renderDataTable({
     gg<-as.data.frame(gg)
     datatable(gg)
 })
 output$tab8<-renderDataTable({
     hh<-as.data.frame(hh)
     datatable(hh)
 })
 output$tab9<-renderDataTable({
     ii<-as.data.frame(ii)
     datatable(ii)
 })
 output$tab10<-renderDataTable({
     jj<-as.data.frame(jj)
     datatable(jj)
 })
 output$tab11<-renderDataTable({
     kk<-as.data.frame(kk)
     datatable(kk)
 })
 output$tab12<-renderDataTable({
     ll<-as.data.frame(ll)
     datatable(ll)
 })
 output$tab13<-renderDataTable({
     mm<-as.data.frame(mm)
     datatable(mm)
 })
 output$tab14<-renderDataTable({
     nn<-as.data.frame(nn)
     datatable(nn)
 })
 output$tab15<-renderDataTable({
     oo<-as.data.frame(oo)
     datatable(oo)
 })
 output$tab16<-renderDataTable({
     pp<-as.data.frame(pp)
     datatable(pp)
 })
 output$tab17<-renderDataTable({
     qq<-as.data.frame(qq)
     datatable(qq)
 })
 output$tab18<-renderDataTable({
     rr<-as.data.frame(rr)
     datatable(rr)
 })
 output$tab19<-renderDataTable({
     ss<-as.data.frame(ss)
     datatable(ss)
 })
}

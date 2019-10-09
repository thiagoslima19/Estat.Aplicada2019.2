#Trabalho 2 - Est Aplicada
dados<-avaliacao_da_disciplina_eme_2019_1_respostas_
rm(avaliacao_da_disciplina_eme_2019_1_respostas_)
summary(dados)
install.packages("stringr")
require("stringr")
dados$'Polo ao qual está vinculado:'=as.factor(dados$'Polo ao qual está vinculado:')
dados$`Já havia cursado a disciplina anteriormente?`=as.factor(dados$`Já havia cursado a disciplina anteriormente?`)
summary(dados$`Já havia cursado a disciplina anteriormente?`)
claroselucid<-str_detect(dados$`Como avaliaria os módulos (livros), no que se referem aos exemplos fornecidos? (Pode marcar mais de uma opção.)`,"Claros / elucidativos")
Interessantes<-str_detect(dados$`Como avaliaria os módulos (livros), no que se referem aos exemplos fornecidos? (Pode marcar mais de uma opção.)`,"Interessantes")
Sãoaplicadosnaáreadeformaçãodoaluno<-str_detect(dados$`Como avaliaria os módulos (livros), no que se referem aos exemplos fornecidos? (Pode marcar mais de uma opção.)`,"São aplicados na área de formação do aluno")
Nenhumadasopçõesacimaestádeacordocomminhaopinião<-str_detect(dados$`Como avaliaria os módulos (livros), no que se referem aos exemplos fornecidos? (Pode marcar mais de uma opção.)`,"Nenhuma das opções acima está de acordo com minha opinião")
Nãoseiopinarporquenãoutilizeiosmódulos<-str_detect(dados$`Como avaliaria os módulos (livros), no que se referem aos exemplos fornecidos? (Pode marcar mais de uma opção.)`,"Não sei opinar porque não utilizei os módulos")
contTrue=function(v){
  c=0
  for(i in 1:length(v)){
    if(v[i]==T)
      c=c+1
  }
  return(c)
}
x1=contTrue(claroselucid)
x2=contTrue(Interessantes)
x3=contTrue(Sãoaplicadosnaáreadeformaçãodoaluno)
x4=contTrue(Nenhumadasopçõesacimaestádeacordocomminhaopinião)
x5=contTrue(Nãoseiopinarporquenãoutilizeiosmódulos)
x=c(x1,x2,x3,x4,x5)/sum(x1,x2,x3,x4,x5)
install.packages("RColorBrewer")
require("RColorBrewer")
mypalette<-brewer.pal(9,"Blues")
mypalette
barplot(x,main="Importância do Material",ylab="Frequência",col=mypalette[c(1,2,3,4,7)])
legend(locator(1), xpd=TRUE, ncol=2, legend=c("Claros / elucidativos","Interessantes","São aplicados na área de formação do aluno","Nenhuma das opções","Não sei opinar"),
       fill=mypalette[c(1,2,3,4,7)], bty="n")
legend(x,"topleft",,fill=c("cyan","blue"))
require("DescTools")
LeveneTest(dados$Nota~dados$'Polo ao qual está vinculado:')
oneway.test(dados$Nota~dados$'Polo ao qual está vinculado:',var.equal = F)
x=aov(dados$Nota~dados$'Polo ao qual está vinculado:',dados)
summary(x)
t=PostHocTest(x,method = "bonferroni",main="Intervalos")
t
plot(t) 
require(ggplot2)
require(dplyr)
rr=select(dados,'Nota','Polo ao qual está vinculado:')
rr=rename(rr, Polos=`Polo ao qual está vinculado:`)
ggplot(rr, aes(x=Polos,y=Nota,fill=Polos)) + geom_boxplot()

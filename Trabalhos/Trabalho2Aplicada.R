#Trabalho 2 Estatística Aplicada

dados<-avaliacao_da_disciplina_eme_2019_1_respostas_
rm(avaliacao_da_disciplina_eme_2019_1_respostas_)
summary(dados)
install.packages("stringr")
require("stringr")
dados$'Polo ao qual está vinculado:'=as.factor(dados$'Polo ao qual está vinculado:')
dados$`Já havia cursado a disciplina anteriormente?`=as.factor(dados$`Já havia cursado a disciplina anteriormente?`)
summary(dados$`Já havia cursado a disciplina anteriormente?`)
dados$`Como avaliaria os módulos (livros), no que se refere a clareza da apresentação do conteúdo da disciplina?`=as.factor(dados$`Como avaliaria os módulos (livros), no que se refere a clareza da apresentação do conteúdo da disciplina?`)
summary(dados$`Como avaliaria os módulos (livros), no que se refere a clareza da apresentação do conteúdo da disciplina?`)
claroselucid<-str_detect(dados$`Como avaliaria os módulos (livros), no que se referem aos exemplos fornecidos? (Pode marcar mais de uma opção.)`,"Claros / elucidativos")
Interessantes<-str_detect(dados$`Como avaliaria os módulos (livros), no que se referem aos exemplos fornecidos? (Pode marcar mais de uma opção.)`,"Interessantes")
Sãoaplicadosnaáreadeformaçãodoaluno<-str_detect(dados$`Como avaliaria os módulos (livros), no que se referem aos exemplos fornecidos? (Pode marcar mais de uma opção.)`,"São aplicados na área de formação do aluno")
Nenhumadasopçõesacimaestádeacordocomminhaopinião<-str_detect(dados$`Como avaliaria os módulos (livros), no que se referem aos exemplos fornecidos? (Pode marcar mais de uma opção.)`,"Nenhuma das opções acima está de acordo com minha opinião")
Nãoseiopinarporquenãoutilizeiosmódulos<-str_detect(dados$`Como avaliaria os módulos (livros), no que se referem aos exemplos fornecidos? (Pode marcar mais de uma opção.)`,"Não sei opinar porque não utilizei os módulos")
summary(dados$`Já havia cursado a disciplina anteriormente?`)
summary(dados$`Como avaliaria os Materiais Complementares, no que se refere a contribuição para entendimento do conteúdo da disciplina?`)
dados$`Como avaliaria os Materiais Complementares, no que se refere a contribuição para entendimento do conteúdo da disciplina?`=as.factor(dados$`Como avaliaria os Materiais Complementares, no que se refere a contribuição para entendimento do conteúdo da disciplina?`)
dados$`Como avaliaria os slides, no que se refere a contribuição para entendimento do conteúdo da disciplina?`=as.factor(dados$`Como avaliaria os slides, no que se refere a contribuição para entendimento do conteúdo da disciplina?`)
summary(dados$`Como avaliaria os slides, no que se refere a contribuição para entendimento do conteúdo da disciplina?`)
x1=sum(claroselucid)
x2=sum(Interessantes)
x3=sum(Sãoaplicadosnaáreadeformaçãodoaluno)
x4=sum(Nenhumadasopçõesacimaestádeacordocomminhaopinião)
x5=sum(Nãoseiopinarporquenãoutilizeiosmódulos)
sum(str_detect(dados$`Como avaliaria os Materiais Complementares, no que se referem aos exemplos fornecidos? (Pode marcar mais de uma opção.)`,"Claros"))
sumsum(str_detect(dados$`Como avaliaria os Materiais Complementares, no que se referem aos exemplos fornecidos? (Pode marcar mais de uma opção.)`,"Interessantes"))
sum(str_detect(dados$`Como avaliaria os Materiais Complementares, no que se referem aos exemplos fornecidos? (Pode marcar mais de uma opção.)`,"erros"))


c(x1,x2,x3,x4,x5)
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


porc<-round(frota*100/sum(frota),2) #arredonda a porcentagem para 2 dígitos significativos)
rotulos<-paste("(",porc,"%)",sep="")
pie(frota, main="Frota 2009 - Niterói_RJ",labels=rotulos, col=rainbow(7))
legend(1,1,names(rr$Polos),col = rainbow(17),pch=rep(20,6))







cbind(levels(rr$Polos), cols)
ord <- order(x, decreasing = TRUE)
cols <- cols[ord]
x <- x[ord]
n <- names(x)
rr$Polos <- factor(rr$Polos, levels = n)
a <- barplot(x,
             xaxt = "n",
             las = 1,
             col = cols)
mtext(side = 2, text = "Frequência absoluta", line = 3)
axis(side = 1, at = a, labels = n, las = 2)
box(bty = "L")
barplot(table(rr$Polos),main="Número de Alunos por Polos",axisnames=F,col=rainbow(17),ylim=c(0,35),ylab="Número de Alunos",xlab="Polos")
legend(locator(1), xpd=TRUE, ncol=2, legend=c("Nova Iguaçu","Campo Grande","Nova Friburgo","São Gonçalo","Resende","Itaocara","Itaperuna","Paracambi","Bom Jesus de Itabapoana","Duque de Caxias","Magé","Piraí","Volta Redonda","Angra dos Reis","Macaé","Petrópolis","Três Rios"),
       fill=rainbow(17), bty="n")
box(bty = "L")
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
#Trabalho 2 Estat�stica Aplicada

dados<-avaliacao_da_disciplina_eme_2019_1_respostas_
rm(avaliacao_da_disciplina_eme_2019_1_respostas_)
summary(dados)
install.packages("stringr")
require("stringr")
dados$'Polo ao qual est� vinculado:'=as.factor(dados$'Polo ao qual est� vinculado:')
dados$`J� havia cursado a disciplina anteriormente?`=as.factor(dados$`J� havia cursado a disciplina anteriormente?`)
summary(dados$`J� havia cursado a disciplina anteriormente?`)
dados$`Como avaliaria os m�dulos (livros), no que se refere a clareza da apresenta��o do conte�do da disciplina?`=as.factor(dados$`Como avaliaria os m�dulos (livros), no que se refere a clareza da apresenta��o do conte�do da disciplina?`)
summary(dados$`Como avaliaria os m�dulos (livros), no que se refere a clareza da apresenta��o do conte�do da disciplina?`)
claroselucid<-str_detect(dados$`Como avaliaria os m�dulos (livros), no que se referem aos exemplos fornecidos? (Pode marcar mais de uma op��o.)`,"Claros / elucidativos")
Interessantes<-str_detect(dados$`Como avaliaria os m�dulos (livros), no que se referem aos exemplos fornecidos? (Pode marcar mais de uma op��o.)`,"Interessantes")
S�oaplicadosna�readeforma��odoaluno<-str_detect(dados$`Como avaliaria os m�dulos (livros), no que se referem aos exemplos fornecidos? (Pode marcar mais de uma op��o.)`,"S�o aplicados na �rea de forma��o do aluno")
Nenhumadasop��esacimaest�deacordocomminhaopini�o<-str_detect(dados$`Como avaliaria os m�dulos (livros), no que se referem aos exemplos fornecidos? (Pode marcar mais de uma op��o.)`,"Nenhuma das op��es acima est� de acordo com minha opini�o")
N�oseiopinarporquen�outilizeiosm�dulos<-str_detect(dados$`Como avaliaria os m�dulos (livros), no que se referem aos exemplos fornecidos? (Pode marcar mais de uma op��o.)`,"N�o sei opinar porque n�o utilizei os m�dulos")
summary(dados$`J� havia cursado a disciplina anteriormente?`)
summary(dados$`Como avaliaria os Materiais Complementares, no que se refere a contribui��o para entendimento do conte�do da disciplina?`)
dados$`Como avaliaria os Materiais Complementares, no que se refere a contribui��o para entendimento do conte�do da disciplina?`=as.factor(dados$`Como avaliaria os Materiais Complementares, no que se refere a contribui��o para entendimento do conte�do da disciplina?`)
dados$`Como avaliaria os slides, no que se refere a contribui��o para entendimento do conte�do da disciplina?`=as.factor(dados$`Como avaliaria os slides, no que se refere a contribui��o para entendimento do conte�do da disciplina?`)
summary(dados$`Como avaliaria os slides, no que se refere a contribui��o para entendimento do conte�do da disciplina?`)
x1=sum(claroselucid)
x2=sum(Interessantes)
x3=sum(S�oaplicadosna�readeforma��odoaluno)
x4=sum(Nenhumadasop��esacimaest�deacordocomminhaopini�o)
x5=sum(N�oseiopinarporquen�outilizeiosm�dulos)
sum(str_detect(dados$`Como avaliaria os Materiais Complementares, no que se referem aos exemplos fornecidos? (Pode marcar mais de uma op��o.)`,"Claros"))
sumsum(str_detect(dados$`Como avaliaria os Materiais Complementares, no que se referem aos exemplos fornecidos? (Pode marcar mais de uma op��o.)`,"Interessantes"))
sum(str_detect(dados$`Como avaliaria os Materiais Complementares, no que se referem aos exemplos fornecidos? (Pode marcar mais de uma op��o.)`,"erros"))


c(x1,x2,x3,x4,x5)
x=c(x1,x2,x3,x4,x5)/sum(x1,x2,x3,x4,x5)
install.packages("RColorBrewer")
require("RColorBrewer")
mypalette<-brewer.pal(9,"Blues")
mypalette
barplot(x,main="Import�ncia do Material",ylab="Frequ�ncia",col=mypalette[c(1,2,3,4,7)])
legend(locator(1), xpd=TRUE, ncol=2, legend=c("Claros / elucidativos","Interessantes","S�o aplicados na �rea de forma��o do aluno","Nenhuma das op��es","N�o sei opinar"),
       fill=mypalette[c(1,2,3,4,7)], bty="n")
legend(x,"topleft",,fill=c("cyan","blue"))
require("DescTools")


porc<-round(frota*100/sum(frota),2) #arredonda a porcentagem para 2 d�gitos significativos)
rotulos<-paste("(",porc,"%)",sep="")
pie(frota, main="Frota 2009 - Niter�i_RJ",labels=rotulos, col=rainbow(7))
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
mtext(side = 2, text = "Frequ�ncia absoluta", line = 3)
axis(side = 1, at = a, labels = n, las = 2)
box(bty = "L")
barplot(table(rr$Polos),main="N�mero de Alunos por Polos",axisnames=F,col=rainbow(17),ylim=c(0,35),ylab="N�mero de Alunos",xlab="Polos")
legend(locator(1), xpd=TRUE, ncol=2, legend=c("Nova Igua�u","Campo Grande","Nova Friburgo","S�o Gon�alo","Resende","Itaocara","Itaperuna","Paracambi","Bom Jesus de Itabapoana","Duque de Caxias","Mag�","Pira�","Volta Redonda","Angra dos Reis","Maca�","Petr�polis","Tr�s Rios"),
       fill=rainbow(17), bty="n")
box(bty = "L")
LeveneTest(dados$Nota~dados$'Polo ao qual est� vinculado:')
oneway.test(dados$Nota~dados$'Polo ao qual est� vinculado:',var.equal = F)
x=aov(dados$Nota~dados$'Polo ao qual est� vinculado:',dados)
summary(x)
t=PostHocTest(x,method = "bonferroni",main="Intervalos")
t
plot(t) 
require(ggplot2)
require(dplyr)
rr=select(dados,'Nota','Polo ao qual est� vinculado:')
rr=rename(rr, Polos=`Polo ao qual est� vinculado:`)
ggplot(rr, aes(x=Polos,y=Nota,fill=Polos)) + geom_boxplot()
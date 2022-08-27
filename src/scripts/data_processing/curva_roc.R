
library(tidyverse)

tb_entrega %>% 
  arrange(desc(pred)) %>% 
  mutate(indice=seq(1:nrow(tb_entrega))) %>%
  ggplot(aes(indice,pred)) +
  geom_line()

tb_entrega %>% 
  arrange(desc(pred)) %>%
  ggplot(aes(pred)) +
  geom_density()

tb_entrega %>% 
  arrange(desc(pred)) %>%
  ggplot(aes(pred,fill=factor(clase01))) +
  geom_density(alpha=0.5)


library(pROC)


roc <- roc(tb_entrega$clase01,tb_entrega$pred,grid=T)

plot(roc,col="red",xlim=c(1,0))
roc2 <- roc(tb_entrega$clase01,tb_entrega$pred,grid=T)

plot(roc2,col="green",xlim=c(1,0), add=TRUE)

writexl::write_xlsx()

tb_entrega[,clasif:=ifelse(pred>0.5,1,0)]
table(tb_entrega$clase01,tb_entrega$clasif)

tb_entrega[,clasif:=ifelse(pred>0.8,1,0)]
table(tb_entrega$clase01,tb_entrega$clasif)

tb_entrega[,clasif:=ifelse(pred>0.25,1,0)]
table(realidad=tb_entrega$clase01,prediccion=tb_entrega$clasif)

tb_entrega_plus <- tb_entrega %>% 
  arrange(desc(pred)) %>% 
  mutate(index=1:nrow(tb_entrega)) %>% 
  mutate(positivos=cumsum(clase01))

tb_entrega_plus %>% 
  ggplot(aes(index,positivos)) +
  geom_line() +
  geom_text(data = tb_entrega_plus %>% filter(index %in% seq(1,nrow(tb_entrega),100)),
            aes(label=round(pred,digits = 2))) +
  geom_vline(xintercept = 1000,linetype="dashed") +
  geom_hline(yintercept = 240,linetype="dashed")

tb_entrega[,clasif:=ifelse(pred>0.08,1,0)]
table(realidad=tb_entrega$clase01,prediccion=tb_entrega$clasif)

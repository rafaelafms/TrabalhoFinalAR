library(quantmod)
library(corrplot)
library(triangle)

#variaveis fixas
acoes = 6
amostra = 1000

# ESSE SCRIPT NECESSITA DE CONEXÃO À INTERNET PARA RODAR.

#extracao da tabela completa da serie historica das acoes direto do yahoo finance
vale <- quantmod::getSymbols("VALE3.SA", scr = "yahoo", auto.assign = FALSE, from = '2022-11-22', to = '2023-11-22')
petrobras <- quantmod::getSymbols("PBR", scr = "yahoo", auto.assign = FALSE, from = '2022-11-22', to = '2023-11-22')
bb <- quantmod::getSymbols("BBAS3.SA", scr = "yahoo", auto.assign = FALSE, from = '2022-11-22', to = '2023-11-22')
santander <- quantmod::getSymbols("SAN", scr = "yahoo", auto.assign = FALSE, from = '2022-11-22', to = '2023-11-22')
magalu <- quantmod::getSymbols("MGLU3.SA", scr = "yahoo", auto.assign = FALSE, from = '2022-11-22', to = '2023-11-22')
americanas <- quantmod::getSymbols("AMER3.SA", scr = "yahoo", auto.assign = FALSE, from = '2022-11-22', to = '2023-11-22')

#matriz com o valor do fechamento de cada uma das acoes diariamente (normalmente eh o que se usa para as analises)
resultado <- cbind(vale$VALE3.SA.Close,petrobras$PBR.Close,bb$BBAS3.SA.Close,santander$SAN.Close,magalu$MGLU3.SA.Close,americanas$AMER3.SA.Close)
col_names_vector = c("Vale", "Petrobras", "BB", "Santander", "Magalu", "Americanas")
resultado_limpo <- na.omit(resultado)
colnames(resultado_limpo) <- col_names_vector

#correlacao real
correlacao_real <- cor(resultado_limpo)
corrplot(correlacao_real, method ='number')

#inicio da simulação

#pegamos todos os valores da tabela menos os últimos 30 dias. Os quais usaremos para predição. 
num_dias <- (length(resultado_limpo) / acoes) - 30

#convertemos a matriz para valores numéricos
acoes_num <- matrix(as.numeric(resultado_limpo), ncol = ncol(resultado_limpo))

#primeiramente vamos contar quantas vezes cada ação sobe ou desce no período de 11 meses disponível.
variacoes = matrix(0,2,acoes)
colnames(variacoes) = c("Vale","Petrobras","BB","Santander","Magalu","Americanas")
rownames(variacoes) = c ("Subiu","Desceu")

for (i in 1:acoes){
  for (j in 1:(num_dias - 1)){
    if (acoes_num[j,i] < acoes_num[j+1,i]){
      variacoes[1,i] = variacoes[1,i] + 1
    } 
    else {
      variacoes[2,i] = variacoes[2,i] + 1
    }
  }
}

# variacoes é um matriz que simboliza quantas vezes uma ação subiu ou desceu
# usaremos essa matriz para fazer um sampling com pesos para definir aleatoriamente
# se o valor de uma ação sobe ou desce
predicoes <- resultado_limpo # faremos uma copia dos valores originais para alterar os últimos 30 valores

for(i in 1:acoes){ # para cada ação
  sim_por_acao <- matrix(0,30,1000) # simularemos um mes de previsoes mil vezes
  peso_subiu <- variacoes[1,i] # guardamos o numero de vezes que a acão sobe e desce, para recalculá-los em cada
  peso_desceu <- variacoes[2,i] # ciclo de amostragem
  for(j in 1:amostra){ # mil vezes
    peso_subiu_aux = peso_subiu # estes serão alterados dinamicamente durante a execução
    peso_desceu_aux = peso_desceu
    for(k in 1:30){ # prever os trinta dias 
      fator = sample(1:250,1) # numero aleatorio para simular irregularidades no mercado
      if(fator %% 250 == 0){  # com uma chance de 1 em 250
        fator = rtriangle(1,0.09,0.2,0.09) # admitimos uma taxa de juros muito acima do normal
      } 
      else if (fator %% 50){ # com uma chance de 1 em 50
        fator = rtriangle(1,0.04,0.09,0.04) # admitimos uma taxa relativamente acima do normal
      } 
      else{
        fator = rtriangle(1,0,0.04,0.01)
      }
      if (k == 1){ # se estamos no primeiro elemento
        cotacao_do_dia = resultado_limpo[num_dias,i] # pegamos o valor anterior da tabela original
      } 
      else { # se não
        cotacao_do_dia = sim_por_acao[k-1,j] # pegamos o valor anterior da matriz de simulações
      }
      if(sample(c(1,0),1,prob = c(peso_subiu_aux,peso_desceu_aux)) == 1){ # se o preço da ação subiu
        fator = fator + 1 # somamos o fator a 1 (100%)
        sim_por_acao[k,j] = cotacao_do_dia * fator
        peso_subiu_aux = peso_subiu_aux + 1
      } 
      else{ # se o preço da ação desceu
        fator = 1 - fator # diminuimos o fator de 1
        sim_por_acao[k,j] = cotacao_do_dia * fator
        peso_desceu_aux = peso_desceu_aux + 1
      }
    }
  }
  # media e adicao aqui
  medias_acao <- rowMeans(sim_por_acao)
  for (l in 1:30){
    predicoes[l + num_dias ,i] = medias_acao[l]
  }
}

#correlacao da amostra gerada
correlacao_simulada <- cor(predicoes)
corrplot(correlacao_simulada, method ='number')
# Beluga Data Science Challenge

Nesse repositório está armazenada a implementação em R do algoritmo de previsão de visitas técnicas.

Arquivos:
- Analise.R - analise e algoritmo de implementação
- Analise.xlsx - passo a passo da análise realizada, com interpretações e experimentos realizados
- predict.csv - arquivo com as predições


Processo de análise realizado;
- Analise exploratória univariada: verificação dos tipos de variáveis e suas distribuições no dataset de treino
- Analise exploratória bi-variada:
  - verificação de features que poderiam distiguir os grupos target e não-target (teste de Kruskal-Wallis)
  - nível de associação entre cada uma das features e a variável target (teste V de Cramer)
  - verificação de quais dessas associações possuem significância (teste Chi-Quadrado)
- Modelo: optou-se por utilizar um decision tree, por se tratar de um algoritmo não-paramétrico e que ofereceu bons resultados nos experimentos realizados


Observações:
- Visto que o dataset possui apenas variáveis categóricas, em todo o procedimento de análise dos dados e criação do modelo, optou-se por testes não-paramétricos uma vez que não podemos assumir distribuição normal. O modelo pelo qual foi optado (Decision Tree) também foi escolhido pensando nisto

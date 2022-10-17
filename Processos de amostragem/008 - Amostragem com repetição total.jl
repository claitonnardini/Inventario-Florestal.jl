#Inventário Florestal🌳 
#Amostragem com repetição total
_________________________________________________________________________________________________________________________________________
 
using DataFrames, Statistics, Distributions, CSV, XLSX #Habilitar pacotes
_________________________________________________________________________________________________________________________________________

#Função ART: amostragem com repetição total
function ART(n, Ocasiao_1, Ocasiao_2) #Determina a função

    ###Primeira ocasião####
    Ocasião_1 = (Conversor.*Dados.Ocasiao_1)
    Ocasião_2 = (Conversor.*Dados.Ocasiao_2)
    ART = DataFrame(Unidades = Dados.n, Ocasião_1 = Ocasião_1, Ocasião_2 = Ocasião_2)
    length(Ocasião_1) #Número total de unidades amostradas
    mean(Ocasião_1) #Média
    sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1 #Variância
    sqrt(sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1) #Desvio padrão
    ((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/
    (length(Ocasião_1)))*(1-((length(Ocasião_1))/N1)) #Variância da média
    sqrt(((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1))) #Erro padrão
    quantile(TDist((length(Ocasião_1))-1),1-alpha/2) #Valor de t 
    #Erro da amostragem
    (quantile(TDist((length(Ocasião_1))-1),1-alpha/2))*(sqrt(((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/
    (length(Ocasião_1))-1)/(length(Ocasião_1)))*(1-((length(Ocasião_1))/N1)))) #Absoluto
    (((quantile(TDist((length(Ocasião_1))-1),1-alpha/2))*(sqrt(((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/
    (length(Ocasião_1))-1)/(length(Ocasião_1)))*(1-((length(Ocasião_1))/N1)))))/
    (mean(Ocasião_1)))*100 #Relativo
    #Limite do intervalo de confiança para média
    ((mean(Ocasião_1))-((quantile(TDist((length(Ocasião_1))-1),1-alpha/2))*(sqrt(((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/
    (length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1)))))) #Inferior
    ((mean(Ocasião_1))+((quantile(TDist((length(Ocasião_1))-1),1-alpha/2))*
    (sqrt(((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/
    (length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1)))))) #Superior
    #Total estimado
    (N1*(mean(Ocasião_1))/Conversor)
    #Limite do intervalo de confiança para o total
    (((N1*(mean(Ocasião_1)))-N1*((quantile(TDist((length(Ocasião_1))-1),1-alpha/2))*
    (sqrt(((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1))))))/Conversor) #Inferior
    (((N1*(mean(Ocasião_1)))+N1*((quantile(TDist((length(Ocasião_1))-1),1-alpha/2))*
    (sqrt(((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1))))))/Conversor)#Superior
    Primeira_ocasião = DataFrames.DataFrame(Variáveis=["Média (m³/ha)", "Limite inferior do intervalo de confiança para média (m³/ha)", 
    "Limite superior do intervalo de confiança para média (m³/ha)", "Total da população (m³)", "Limite inferior do intervalo de confiança para o total (m³)", 
    "Limite superior do intervalo de confiança para o total (m³)", "Área da população (ha)", "Erro da amostragem relativo (%)", 
    "Erro da amostragem absoluto (m³/ha)", "Erro padrão (m³/ha)", "Desvio padrão (m³/ha)", "Variância (m³/ha)²", "Variância da média (m³/ha)²", 
    "Número total de unidades", "Nível de de significância (α)"], Valores=[mean(Ocasião_1), ((mean(Ocasião_1))-((quantile(TDist((length(Ocasião_1))-1),1-alpha/2))*
    (sqrt(((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*(1-((length(Ocasião_1))/N1)))))),
    ((mean(Ocasião_1))+((quantile(TDist((length(Ocasião_1))-1),1-alpha/2))*(sqrt(((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/
    (length(Ocasião_1))-1)/(length(Ocasião_1)))*(1-((length(Ocasião_1))/N1)))))), (N1*(mean(Ocasião_1))/Conversor), (((N1*(mean(Ocasião_1)))-
    N1*((quantile(TDist((length(Ocasião_1))-1),1-alpha/2))*(sqrt(((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/
    (length(Ocasião_1)))*(1-((length(Ocasião_1))/N1))))))/Conversor), (((N1*(mean(Ocasião_1)))+N1*((quantile(TDist((length(Ocasião_1))-1),1-alpha/2))*
    (sqrt(((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*(1-((length(Ocasião_1))/N1))))))/Conversor), N1, 
    (((quantile(TDist((length(Ocasião_1))-1),1-alpha/2))*(sqrt(((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1)))))/(mean(Ocasião_1)))*100, (quantile(TDist((length(Ocasião_1))-1),1-alpha/2))*(sqrt(((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/
    (length(Ocasião_1))-1)/(length(Ocasião_1)))*(1-((length(Ocasião_1))/N1)))), sqrt(((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1))), sqrt(sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1), sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1, 
    ((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*(1-((length(Ocasião_1))/N1)), length(Ocasião_1), alpha]) #Tabela de resultados
        
 ###Segunda ocasião####
    length(Ocasião_2) #Número total de unidades amostradas
    mean(Ocasião_2) #Média
    sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1 #Variância
    sqrt(sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1) #Desvio padrão
    ((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)) #Variância da média
    sqrt(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))) #Erro padrão    
    quantile(TDist((length(Ocasião_2))-1),1-alpha/2) #Valor de t 
    #Erro da amostragem
    (quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*(sqrt(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    (length(Ocasião_2))-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)))) #Absoluto
    (((quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*(sqrt(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    (length(Ocasião_2))-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)))))/
    (mean(Ocasião_2)))*100 #Relativo
    #Limite do intervalo de confiança para média
    ((mean(Ocasião_2))-((quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*(sqrt(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    (length(Ocasião_2))-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2)))))) #Inferior
    ((mean(Ocasião_2))+((quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*(sqrt(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    (length(Ocasião_2))-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2)))))) #Superior 
    #Total estimado
    (N2*(mean(Ocasião_2))/Conversor)
    #Limite do intervalo de confiança para o total
    (((N2*(mean(Ocasião_2)))-N2*((quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*
    (sqrt(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2))))))/Conversor) #Inferior
    ((N2*((mean(Ocasião_2)))+N2*((quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*
    (sqrt(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2))))))/Conversor) #Superior
    Segunda_ocasião = DataFrames.DataFrame(Variáveis=["Média (m³/ha)", "Limite inferior do intervalo de confiança para média (m³/ha)", 
    "Limite superior do intervalo de confiança para média (m³/ha)", "Total da população (m³)", "Limite inferior do intervalo de confiança para o total (m³)", 
    "Limite superior do intervalo de confiança para o total (m³)", "Área da população (ha)", "Erro da amostragem relativo (%)", 
    "Erro da amostragem absoluto (m³/ha)", "Erro padrão (m³/ha)", "Desvio padrão (m³/ha)", "Variância (m³/ha)²", "Variância da média (m³/ha)²", 
    "Número total de unidades", "Nível de de significância (α)"], Valores=[mean(Ocasião_2), ((mean(Ocasião_2))-((quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*
    (sqrt(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)))))), 
    ((mean(Ocasião_2))+((quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*(sqrt(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    (length(Ocasião_2))-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)))))), (N2*(mean(Ocasião_2))/Conversor), 
    (((N2*(mean(Ocasião_2)))-N2*((quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*(sqrt(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    (length(Ocasião_2))-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))))/Conversor), ((N2*((mean(Ocasião_2)))+N2*
    ((quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*(sqrt(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2))))))/Conversor), N2, (((quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*(sqrt(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    (length(Ocasião_2))-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)))))/(mean(Ocasião_2)))*100, 
    (quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*(sqrt(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    (length(Ocasião_2))-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)))), sqrt(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))), sqrt(sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1), 
    sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1, ((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)), length(Ocasião_2), alpha]) #Tabela de resultados
        
    ###Crescimento ou mudança####
    length(Dados.n) #Número total de unidades amostradas na primeira e na segunda ocasião
    mean(Ocasião_2)-mean(Ocasião_1) #Média
    sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/(length(Dados.n)-1)
    ((((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))-
    (2*((sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/(length(Dados.n)-1))/
    (length(Dados.n))))#Variância da média
    sqrt(((((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))-
    (2*((sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/(length(Dados.n)-1))/
    (length(Dados.n))))) #Erro padrão
    ((length(Ocasião_1))-1)+((length(Ocasião_2))-1) #Grau de liberdade
    quantile(TDist((((length(Ocasião_1))-1)+((length(Ocasião_2))-1))-1),1-alpha/2) #Valor de t 
    #Erro de amostragem
    quantile(TDist((((length(Ocasião_1))-1)+((length(Ocasião_2))-1))-1),1-alpha/2)*
    (sqrt(((((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))-
    (2*((sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/
    (length(Dados.n)-1))/(length(Dados.n)))))) #Absoluto
    (quantile(TDist((((length(Ocasião_1))-1)+((length(Ocasião_2))-1))-1),1-alpha/2)*
    (sqrt(((((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))-
    (2*((sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/(length(Dados.n)-1))/
    (length(Dados.n))))))/(mean(Ocasião_2)-mean(Ocasião_1)))*100 #Relativo
    #Limite do intervalo de confiança para média
    ((mean(Ocasião_2)-mean(Ocasião_1))-quantile(TDist((((length(Ocasião_1))-1)+((length(Ocasião_2))-1))-1),1-alpha/2)*
    (sqrt(((((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    (length(Ocasião_2))-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))-
    (2*((sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/(length(Dados.n)-1))/
    (length(Dados.n))))))) #Inferior
    ((mean(Ocasião_2)-mean(Ocasião_1))+quantile(TDist((((length(Ocasião_1))-1)+((length(Ocasião_2))-1))-1),1-alpha/2)*
    (sqrt(((((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    (length(Ocasião_2))-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))-
    (2*((sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/(length(Dados.n)-1))/
    (length(Dados.n))))))) #Superior
    #Crescimento total estimado
    N2*(mean(Ocasião_2)-mean(Ocasião_1))
    #Limite do intervalo de confiança para o total
    ((N2*(mean(Ocasião_2)-mean(Ocasião_1)))-N2*quantile(TDist((((length(Ocasião_1))-1)+
    ((length(Ocasião_2))-1))-1),1-alpha/2)*(sqrt(((((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/
    (length(Ocasião_1))-1)/(length(Ocasião_1)))*(1-((length(Ocasião_1))/N1)))+
    (((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2))))-(2*((sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/(length(Dados.n)-1))/
    (length(Dados.n))))))) #Inferior
    ((N2*(mean(Ocasião_2)-mean(Ocasião_1)))+N2*quantile(TDist((((length(Ocasião_1))-1)+
    ((length(Ocasião_2))-1))-1),1-alpha/2)*(sqrt(((((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/
    (length(Ocasião_1))-1)/(length(Ocasião_1)))*(1-((length(Ocasião_1))/N1)))+
    (((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2))))-(2*((sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/(length(Dados.n)-1))/
    (length(Dados.n))))))) #Superior
    Mudança_crescimento = DataFrames.DataFrame(Variáveis=["Crescimento médio (m³/ha)", "Limite inferior do intervalo de confiança para média (m³/ha)", 
    "Limite superior do intervalo de confiança para média (m³/ha)", "Crescimento total estimado (m³)", "Limite inferior do intervalo de confiança para o total (m³)", 
    "Limite superior do intervalo de confiança para o total (m³)", "Área da população (ha)", "Erro da amostragem relativo (%)", 
    "Erro da amostragem absoluto (m³/ha)", "Erro padrão (m³/ha)", "Variância da média (m³/ha)²"], Valores=[mean(Ocasião_2)-mean(Ocasião_1), 
    ((mean(Ocasião_2)-mean(Ocasião_1))-quantile(TDist((((length(Ocasião_1))-1)+((length(Ocasião_2))-1))-1),1-alpha/2)*
    (sqrt(((((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))-
    (2*((sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/(length(Dados.n)-1))/
    (length(Dados.n))))))), ((mean(Ocasião_2)-mean(Ocasião_1))+quantile(TDist((((length(Ocasião_1))-1)+((length(Ocasião_2))-1))-1),1-alpha/2)*
    (sqrt(((((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))-
    (2*((sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/(length(Dados.n)-1))/
    (length(Dados.n))))))), N2*(mean(Ocasião_2)-mean(Ocasião_1)), ((N2*(mean(Ocasião_2)-mean(Ocasião_1)))-N2*quantile(TDist((((length(Ocasião_1))-1)+
    ((length(Ocasião_2))-1))-1),1-alpha/2)*(sqrt(((((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/
    (length(Ocasião_1))-1)/(length(Ocasião_1)))*(1-((length(Ocasião_1))/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2))))-(2*((sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/(length(Dados.n)-1))/
    (length(Dados.n))))))), ((N2*(mean(Ocasião_2)-mean(Ocasião_1)))+N2*quantile(TDist((((length(Ocasião_1))-1)+
    ((length(Ocasião_2))-1))-1),1-alpha/2)*(sqrt(((((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/
    (length(Ocasião_1))-1)/(length(Ocasião_1)))*(1-((length(Ocasião_1))/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2))))-(2*((sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/(length(Dados.n)-1))/
    (length(Dados.n))))))), N2, (quantile(TDist((((length(Ocasião_1))-1)+((length(Ocasião_2))-1))-1),1-alpha/2)*
    (sqrt(((((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))-(2*((sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/(length(Dados.n)-1))/
    (length(Dados.n))))))/(mean(Ocasião_2)-mean(Ocasião_1)))*100, quantile(TDist((((length(Ocasião_1))-1)+((length(Ocasião_2))-1))-1),1-alpha/2)*
    (sqrt(((((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))-(2*((sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/
    (length(Dados.n)-1))/(length(Dados.n)))))), sqrt(((((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))-(2*((sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/(length(Dados.n)-1))/
    (length(Dados.n))))), ((((sum((Ocasião_1.-(mean(Ocasião_1))).^2)/(length(Ocasião_1))-1)/(length(Ocasião_1)))*
    (1-((length(Ocasião_1))/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/(length(Ocasião_2))-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))-(2*((sum((Ocasião_1.-(mean(Ocasião_1))).*(Ocasião_2.-(mean(Ocasião_2))))/(length(Dados.n)-1))/
    (length(Dados.n))))]) #Tabela de resultados   
    XLSX.writetable(("F:/Version_09_07_21/iflorestal.jl/08.xlsx"), Dados=(collect(DataFrames.eachcol(Dados)), 
    DataFrames.names(Dados)), Primeira_ocasião=(collect(DataFrames.eachcol(Primeira_ocasião)), 
    DataFrames.names(Primeira_ocasião)), Segunda_ocasião=(collect(DataFrames.eachcol(Segunda_ocasião)), 
    DataFrames.names(Segunda_ocasião)), Crescimento_ou_mudança=(collect(DataFrames.eachcol(Mudança_crescimento)),     
    DataFrames.names(Mudança_crescimento))) #Exportar para o Excel 
end
_________________________________________________________________________________________________________________________________________

#Processamento do inventário
#Importar dados
Dados = CSV.read("F:/Version_09_07_21/art.csv", DataFrame) 
#Informações necessárias
#Área da população na primeira ocasião
const N1 =1500
#Área da população na segunda ocasião
const N2=1500
#Nível de significância (α)
const alpha = 0.05
#Unidade de medida da variável
Unidade = "m³/1 ha" #Alterar em função do inventário
#Conversor para a unidade de área por hectare
Área_da_parcela=1
Conversor=1/Área_da_parcela
#function ART(n, Ocasião_1, Ocasião_2)
ART(Dados.n, Dados.Ocasiao_1, Dados.Ocasiao_2) #Saída dos dados

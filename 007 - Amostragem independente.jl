#Linguagem Julia verson v.1.5.3
#Modificado: 20/12/2021
#Inventário Florestal🌳
#Amostragem independente
_________________________________________________________________________________________________________________________________________

using DataFrames, Statistics, Distributions, CSV, XLSX #Habilitar pacotes
_________________________________________________________________________________________________________________________________________

#Função independente: amostragem independente
function Independente(n, Ocasiao_1, Ocasiao_2) #Determina a função
    ###Primeira ocasião####
    Ocasião_1 = (Conversor.*Dados.Ocasiao_1)
    Ocasião_2 = (Conversor.*Dados.Ocasiao_2)
    Independente = DataFrame(Unidades = Dados.n, Ocasião_1 = Ocasião_1, Ocasião_2 = Ocasião_2)
    length(Ocasião_1) #Número de unidades 
    mean(Ocasião_1)  #Média
    sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1 #Variância
    sqrt(sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1) #Desvio padrão
    ((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*
    (1-(length(Ocasião_1)/N1)) #Variância da média
    sqrt.(((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*
    (1-(length(Ocasião_1)/N1))) #Erro padrão
    quantile(TDist(length(Ocasião_1)-1),1-alpha/2) #Valor de t 
    #Erro da amostragem
    quantile(TDist(length(Ocasião_1)-1),1-alpha/2)*sqrt.(((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/
    length(Ocasião_1))*(1-(length(Ocasião_1)/N1)))#Absoluto
    (quantile(TDist(length(Ocasião_1)-1),1-alpha/2)*sqrt.(((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/
    length(Ocasião_1))*(1-(length(Ocasião_1)/N1)))/mean(Ocasião_1))*100 #Relativo
    #Limite do intervalo de confiança para média 
    (mean(Ocasião_1)-quantile(TDist(length(Ocasião_1)-1),1-alpha/2)*sqrt.(((sum((Ocasião_1.-mean(Ocasião_1)).^2)/
    length(Ocasião_1)-1)/length(Ocasião_1))*(1-(length(Ocasião_1)/N1)))) #Inferior
    (mean(Ocasião_1)+quantile(TDist(length(Ocasião_1)-1),1-alpha/2)*sqrt.(((sum((Ocasião_1.-mean(Ocasião_1)).^2)/
    length(Ocasião_1)-1)/length(Ocasião_1))*(1-(length(Ocasião_1)/N1)))) #Superior
    #Total estimado
    ((N1*mean(Ocasião_1))/Conversor)
    #Limite do intervalo de confiança para o total
    (((N1*mean(Ocasião_1))-N1*quantile(TDist(length(Ocasião_1)-1),1-alpha/2)*sqrt.(((sum((Ocasião_1.-mean(Ocasião_1)).^2)/
    length(Ocasião_1)-1)/length(Ocasião_1))*(1-(length(Ocasião_1)/N1))))/Conversor) #Inferior
    (((N1*mean(Ocasião_1))+N1*quantile(TDist(length(Ocasião_1)-1),1-alpha/2)*sqrt.(((sum((Ocasião_1.-mean(Ocasião_1)).^2)/
    length(Ocasião_1)-1)/length(Ocasião_1))*(1-(length(Ocasião_1)/N1))))/Conversor) #Superior  
    Primeira_ocasião = DataFrames.DataFrame(Variáveis=["Média (m³/ha)", "Limite inferior do intervalo de confiança para média (m³/ha)", 
    "Limite superior do intervalo de confiança para média (m³/ha)", "Total da população (m³)", "Limite inferior do intervalo de confiança para o total (m³)", 
    "Limite superior do intervalo de confiança para o total (m³)", "Área da população (ha)", "Erro da amostragem relativo (%)", 
    "Erro da amostragem absoluto (m³/ha)", "Erro padrão (m³/ha)", "Desvio padrão (m³/ha)", "Variância (m³/ha)²", "Variância da média (m³/ha)²", 
    "Número total de unidades", "Nível de de significância (α)"], 
    Valores=[ mean(Ocasião_1),  (mean(Ocasião_1)-quantile(TDist(length(Ocasião_1)-1),1-alpha/2)*sqrt.(((sum((Ocasião_1.-mean(Ocasião_1)).^2)/
    length(Ocasião_1)-1)/length(Ocasião_1))*(1-(length(Ocasião_1)/N1)))), (mean(Ocasião_1)+quantile(TDist(length(Ocasião_1)-1),1-alpha/2)*
    sqrt.(((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*(1-(length(Ocasião_1)/N1)))), ((N1*mean(Ocasião_1))/Conversor),
    (((N1*mean(Ocasião_1))-N1*quantile(TDist(length(Ocasião_1)-1),1-alpha/2)*sqrt.(((sum((Ocasião_1.-mean(Ocasião_1)).^2)/
    length(Ocasião_1)-1)/length(Ocasião_1))* (1-(length(Ocasião_1)/N1))))/Conversor), (((N1*mean(Ocasião_1))+N1*quantile(TDist(length(Ocasião_1)-1),1-alpha/2)*
    sqrt.(((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*(1-(length(Ocasião_1)/N1))))/Conversor), N1, 
    (quantile(TDist(length(Ocasião_1)-1),1-alpha/2)*sqrt.(((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/
    length(Ocasião_1))*(1-(length(Ocasião_1)/N1)))/mean(Ocasião_1))*100, quantile(TDist(length(Ocasião_1)-1),1-alpha/2)*sqrt.(((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/
    length(Ocasião_1))*(1-(length(Ocasião_1)/N1))), sqrt.(((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*
    (1-(length(Ocasião_1)/N1))), sqrt(sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1), sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1, 
    ((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*
    (1-(length(Ocasião_1)/N1)), length(Ocasião_1), alpha]) #Tabela de resultados 
    ###Segunda ocasião####
    length(Ocasião_2) #Número de unidades
    mean(Ocasião_2) #Média
    (sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1) #Variância
    sqrt(sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1) #Desvio padrão
    ((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2)) #Variância da média
    sqrt.(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2))) #Erro padrão
    quantile(TDist((length(Ocasião_2))-1),1-alpha/2) #Valor de t 
    #Erro de amostragem
    (quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*sqrt.(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    length(Ocasião_2)-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))) #Absoluto
    ((quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*sqrt.(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    length(Ocasião_2)-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)))/
    (mean(Ocasião_2)))*100 #Relativo
    #Limite do intervalo de confiança para média
    ((mean(Ocasião_2))-(quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*sqrt.(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    length(Ocasião_2)-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)))) #Inferior
    ((mean(Ocasião_2))+(quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*sqrt.(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    length(Ocasião_2)-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)))) #Superior
    #Total estimado
    ((N2*mean(Ocasião_2))/Conversor)
   #Limite do intervalo de confiança para o total
    ((N2*(mean(Ocasião_2)))-N2*((quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*sqrt.(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    length(Ocasião_2)-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))/Conversor) #Inferior
    (((N2*(mean(Ocasião_2)))+N2*(quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*sqrt.(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    length(Ocasião_2)-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))/Conversor) #Superior
    Segunda_ocasião = DataFrames.DataFrame(Variáveis=["Média (m³/ha)", "Limite inferior do intervalo de confiança para média (m³/ha)", 
    "Limite superior do intervalo de confiança para média (m³/ha)", "Total da população (m³)", "Limite inferior do intervalo de confiança para o total (m³)", 
    "Limite superior do intervalo de confiança para o total (m³)", "Área da população (ha)", "Erro da amostragem relativo (%)", 
    "Erro da amostragem absoluto (m³/ha)", "Erro padrão (m³/ha)", "Desvio padrão (m³/ha)", "Variância (m³/ha)²", "Variância da média (m³/ha)²", 
    "Número total de unidades", "Nível de de significância (α)"], Valores=[mean(Ocasião_2), ((mean(Ocasião_2))-(quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*
    sqrt.(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)))), 
    ((mean(Ocasião_2))+(quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*sqrt.(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    length(Ocasião_2)-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)))), ((N2*mean(Ocasião_2))/Conversor), 
    ((N2*(mean(Ocasião_2)))-N2*((quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*sqrt.(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    length(Ocasião_2)-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))/Conversor), (((N2*(mean(Ocasião_2)))+
    N2*(quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*sqrt.(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    length(Ocasião_2)-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))/Conversor), N2, 
    ((quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*sqrt.(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    length(Ocasião_2)-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)))/(mean(Ocasião_2)))*100, 
    (quantile(TDist((length(Ocasião_2))-1),1-alpha/2))*sqrt.(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    length(Ocasião_2)-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))), sqrt.(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/
    length(Ocasião_2)-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))), sqrt(sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1), 
    (sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1), ((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)), length(Ocasião_2), alpha]) #Tabela de resultados
    ###Mudanda ou crescimento####
    mean(Ocasião_2)-mean(Ocasião_1) #Média
    (((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*(1-(length(Ocasião_1)/N1)))+
    (((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2))) #Variância da média
    sqrt((((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*(1-(length(Ocasião_1)/N1)))+
    (((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2)))) #Erro padrão
    quantile(TDist((length(Ocasião_1)-1)+((length(Ocasião_2))-1)-1),1-alpha/2) #Valor t
    #Erro da amostragem
    (length(Ocasião_1)-1)+((length(Ocasião_2))-1)
    ((quantile(TDist((length(Ocasião_1)-1)+((length(Ocasião_2))-1)-1),1-alpha/2))*
    (sqrt((((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*
    (1-(length(Ocasião_1)/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)))))) #Absoluto
    (((quantile(TDist((length(Ocasião_1)-1)+((length(Ocasião_2))-1)-1),1-alpha/2))*
    (sqrt((((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*
    (1-(length(Ocasião_1)/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))))/
    (mean(Ocasião_2)-mean(Ocasião_1)))*100 #Relativo
    #Limite do intervalo de confiança para média
    (mean(Ocasião_2)-mean(Ocasião_1)-((quantile(TDist((length(Ocasião_1)-1)+((length(Ocasião_2))-1)-1),1-alpha/2))*
    (sqrt((((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*
    (1-(length(Ocasião_1)/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))))) #Inferior
    (mean(Ocasião_2)-mean(Ocasião_1)+((quantile(TDist((length(Ocasião_1)-1)+((length(Ocasião_2))-1)-1),1-alpha/2))*
    (sqrt((((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*
    (1-(length(Ocasião_1)/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))))) #Superior
    #Crescimento total estimado
    N2*(mean(Ocasião_2)-mean(Ocasião_1))
    #Limite do intervalo de confiança para o total
    ((N2*(mean(Ocasião_2)-mean(Ocasião_1)))-N2*((quantile(TDist((length(Ocasião_1)-1)+
    ((length(Ocasião_2))-1)-1),1-alpha/2))*(sqrt((((sum((Ocasião_1.-mean(Ocasião_1)).^2)/
    length(Ocasião_1)-1)/length(Ocasião_1))*(1-(length(Ocasião_1)/N1)))+
    (((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2))))))) #Inferior
    ((N2*(mean(Ocasião_2)-mean(Ocasião_1)))+N2*((quantile(TDist((length(Ocasião_1)-1)+
    ((length(Ocasião_2))-1)-1),1-alpha/2))*(sqrt((((sum((Ocasião_1.-mean(Ocasião_1)).^2)/
    length(Ocasião_1)-1)/length(Ocasião_1))*(1-(length(Ocasião_1)/N1)))+
    (((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2))))))) #Superior
    Mudança_crescimento = DataFrames.DataFrame(Variáveis=["Crescimento médio (m³/ha)", "Limite inferior do intervalo de confiança para média (m³/ha)", 
    "Limite superior do intervalo de confiança para média (m³/ha)", "Crescimento total estimado (m³)", "Limite inferior do intervalo de confiança para o total (m³)", 
    "Limite superior do intervalo de confiança para o total (m³)", "Área da população (ha)", "Erro da amostragem relativo (%)", 
    "Erro da amostragem absoluto (m³/ha)", "Erro padrão (m³/ha)", "Variância da média (m³/ha)²"], Valores=[mean(Ocasião_2)-mean(Ocasião_1), 
    (mean(Ocasião_2)-mean(Ocasião_1)-((quantile(TDist((length(Ocasião_1)-1)+((length(Ocasião_2))-1)-1),1-alpha/2))*
    (sqrt((((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*
    (1-(length(Ocasião_1)/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))))), (mean(Ocasião_2)-mean(Ocasião_1)+((quantile(TDist((length(Ocasião_1)-1)+
    ((length(Ocasião_2))-1)-1),1-alpha/2))*(sqrt((((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*
    (1-(length(Ocasião_1)/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))))), N2*(mean(Ocasião_2)-mean(Ocasião_1)), 
    ((N2*(mean(Ocasião_2)-mean(Ocasião_1)))-N2*((quantile(TDist((length(Ocasião_1)-1)+
    ((length(Ocasião_2))-1)-1),1-alpha/2))*(sqrt((((sum((Ocasião_1.-mean(Ocasião_1)).^2)/
    length(Ocasião_1)-1)/length(Ocasião_1))*(1-(length(Ocasião_1)/N1)))+
    (((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2))))))), ((N2*(mean(Ocasião_2)-mean(Ocasião_1)))+N2*((quantile(TDist((length(Ocasião_1)-1)+
    ((length(Ocasião_2))-1)-1),1-alpha/2))*(sqrt((((sum((Ocasião_1.-mean(Ocasião_1)).^2)/
    length(Ocasião_1)-1)/length(Ocasião_1))*(1-(length(Ocasião_1)/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))))), N2, (((quantile(TDist((length(Ocasião_1)-1)+((length(Ocasião_2))-1)-1),1-alpha/2))*
    (sqrt((((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*
    (1-(length(Ocasião_1)/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2))))))/(mean(Ocasião_2)-mean(Ocasião_1)))*100, 
    ((quantile(TDist((length(Ocasião_1)-1)+((length(Ocasião_2))-1)-1),1-alpha/2))*
    (sqrt((((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*
    (1-(length(Ocasião_1)/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/
    (length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)))))), sqrt((((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*
    (1-(length(Ocasião_1)/N1)))+(((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/(length(Ocasião_2)))*
    (1-((length(Ocasião_2))/N2)))), (((sum((Ocasião_1.-mean(Ocasião_1)).^2)/length(Ocasião_1)-1)/length(Ocasião_1))*(1-(length(Ocasião_1)/N1)))+
    (((sum((Ocasião_2.-(mean(Ocasião_2))).^2)/length(Ocasião_2)-1)/(length(Ocasião_2)))*(1-((length(Ocasião_2))/N2)))]) #Tabela de resultados  
    XLSX.writetable(("F:/Version_09_07_21/iflorestal.jl/07.xlsx"), Dados=(collect(DataFrames.eachcol(Independente)), DataFrames.names(Independente)), 
        Primeira_ocasião=(collect(DataFrames.eachcol(Primeira_ocasião)), DataFrames.names(Primeira_ocasião)), 
        Segunda_ocasião=(collect(DataFrames.eachcol(Segunda_ocasião)), DataFrames.names(Segunda_ocasião)), 
        Crescimento_ou_mudança=(collect(DataFrames.eachcol(Mudança_crescimento)), 
        DataFrames.names(Mudança_crescimento))) #Exportar para o Excel
end
_________________________________________________________________________________________________________________________________________

#Processamento do inventário
#Importar dados
Dados = CSV.read("F:/Version_09_07_21/independente.csv", DataFrame) 
#Informações necessárias
#Área da população na primeira ocasião
const N1 = 1500
#Área da população na segunda ocasião
const N2 = 1500
#Nível de significância (α)
const alpha = 0.05
#Unidade de medida da variável
Unidade = "m³/1 ha" #Alterar em função do inventário
#Conversor para a unidade de área por hectare
Área_da_parcela=1
Conversor=1/Área_da_parcela
#function Independente(n, Ocasiao_1, Ocasiao_2)
Independente(Dados.n, Dados.Ocasiao_1, Dados.Ocasiao_2) #Saída dos dados
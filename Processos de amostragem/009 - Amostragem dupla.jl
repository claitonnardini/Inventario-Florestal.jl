#Linguagem Julia verson v.1.5.3
#Modificado: 21/12/2021
#Inventário Florestal🌳
#Amostragem dupla
_________________________________________________________________________________________________________________________________________

using DataFrames, Statistics, Distributions, CSV, XLSX #Habilitar pacotes
_________________________________________________________________________________________________________________________________________

#Função AD: amostragem dupla
function AD(n, Ocasiao_1, Ocasiao_2) #Determina a função

    ###Primeira ocasião###
    Ocasião_1 = (Conversor.*Dados.Ocasiao_1)
    Ocasião_2 = (Conversor.*Dados.Ocasiao_2)
    Unidades = Dados.n
    AD = DataFrame(Unidades = Unidades, Ocasião_1 = Ocasião_1, Ocasião_2 = Ocasião_2)
    length(Unidades) #Número total de unidades
    length(unique(Ocasião_2)) #Unidades permanentes
    length(Unidades)-(length(unique(Ocasião_2))) #Unidades temporárias
    (length(Unidades)-(length(unique(Ocasião_2))))/length(Unidades) #Proporção ideal da subamostra temporária
    (length(unique(Ocasião_2)))/length(Unidades) #Proporção ideal da subamostra permanente
    #Média de unidades temporárias
    j=AD[!, [:Ocasião_1]]
    g=Matrix(j)
    matriz=transpose(g)
    global Xu = 0
    for i in 1:(length(Unidades)-(length(unique(Ocasião_2))))
    global Xu += matriz[i]/(length(Unidades)-(length(unique(Ocasião_2))))
    end 
    println("Média das unidades amostrais temporárias", Xu)
    #Média de unidades permanentes
    global Xm = 0
    for i in ((length(Unidades)-(length(unique(Ocasião_2))))+1):length(Unidades)
    global Xm += matriz[i]/(length(unique(Ocasião_2)))
    end 
    println("Média das unidades amostrais temporárias", Xm)
    (((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm) #Média das unidades totais
    #Variância das unidades temporárias
    global a = 0
    global Sxu² = 0
    for i in 1:(length(Unidades)-(length(unique(Ocasião_2))))
    global a += ((matriz[i].-Xu).^2)
    global Sxu² = a/((length(Unidades)-(length(unique(Ocasião_2))))-1)
    end
    println("Variância das unidades amostrais temporárias", Sxu²)
    #Variância das unidades permanentes
    global b = 0
    global Sxm² = 0
    for i in ((length(Unidades)-(length(unique(Ocasião_2))))+1):length(Unidades)
    global b += ((matriz[i].-Xm).^2)
    global Sxm² = b/((length(unique(Ocasião_2)))-1)
    end
    println("Média das unidades amostrais temporárias", Sxm²)
    sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/
    (length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/
    ((length(Unidades))-1) #Variância das unidades totais
    Sxu=sqrt(Sxu²) #Desvio padrão das unidades temporárias
    Sxm=sqrt(Sxm²) #Desvio padrão das unidades permanentes
    Sx1=sqrt(sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/
    ((length(Unidades))-1)) #Desvio padrão das unidades totais 
    ((sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/((length(Unidades))-1))/
    (length(Unidades)))*(1-((length(Unidades))/N)) #Variância da média
    sqrt(((sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/
    (length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/
    ((length(Unidades))-1))/(length(Unidades)))*(1-((length(Unidades))/N))) #Erro padrão
    quantile(TDist((length(Unidades))-1),1-alpha/2) #Valor de t 
    (0.05*(((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm)) #Limite de erro da amostragem requerido
    ((((quantile(TDist((length(Unidades))-1),1-alpha/2)))^2)*(sum((Ocasião_1.-((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/((length(Unidades))-1)))/
    (((((0.05*(((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))))^2)+
    (((((quantile(TDist((length(Unidades))-1),1-alpha/2)))^2)*(sum((Ocasião_1.-((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/
    ((length(Unidades))-1)))/N)) #Intensidade de amostragem
    #Erro de amostragem
    ((quantile(TDist((length(Unidades))-1),1-alpha/2))*(sqrt(((sum((Ocasião_1.-(((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm)).^2)/((length(Unidades))-1))/
    (length(Unidades)))*(1-((length(Unidades))/N))))) #Absoluto
    (((quantile(TDist((length(Unidades))-1),1-alpha/2))*(sqrt(((sum((Ocasião_1.-(((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm)).^2)/((length(Unidades))-1))/
    (length(Unidades)))*(1-((length(Unidades))/N)))))/(((length(Unidades)-(length(unique(Ocasião_2))))/
    (length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))*100 #Relativo
    #Limite do intervalo de confiança para média
    ((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm)-((quantile(TDist((length(Unidades))-1),1-alpha/2))*
    (sqrt(((sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/((length(Unidades))-1))/
    (length(Unidades)))*(1-((length(Unidades))/N)))))) #Inferior
    ((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm)+((quantile(TDist((length(Unidades))-1),1-alpha/2))*
    (sqrt(((sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/((length(Unidades))-1))/(length(Unidades)))*(1-((length(Unidades))/N)))))) #Superior
    #Total estimado
    (N*(((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm)/Conversor)
    #Limite do intervalo de confiança para o total
    (((N*(((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))-N*((quantile(TDist((length(Unidades))-1),1-alpha/2))*
    (sqrt(((sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/((length(Unidades))-1))/
    (length(Unidades)))*(1-((length(Unidades))/N))))))/Conversor) #Inferior 
    (((N*(((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))+N*((quantile(TDist((length(Unidades))-1),1-alpha/2))*
    (sqrt(((sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/((length(Unidades))-1))/(length(Unidades)))*(1-((length(Unidades))/N))))))/Conversor) #Superior
    Primeira_ocasião = DataFrames.DataFrame(Variáveis=["Média das unidades amostrais totais (m³/ha)", "Média das unidades amostrais temporárias (m³/ha)", 
    "Média das unidades amostrais permanentes (m³/ha)", "Limite inferior do intervalo de confiança para média (m³/ha)", "Limite superior do intervalo de confiança para média (m³/ha)", 
    "Total da população (m³)", "Limite inferior do intervalo de confiança para o total (m³)", "Limite superior do intervalo de confiança para o total (m³)", 
    "Área da população (ha)", "Erro da amostragem relativo (%)", "Erro da amostragem absoluto (m³/ha)", "Erro padrão (m³/ha)", 
    "Desvio padrão das unidades amostrais totais (m³/ha)", "Desvio padrão das unidades amostrais temporárias (m³/ha)", "Desvio padrão das unidades amostrais permanentes (m³/ha)", 
    "Variância das unidades amostrais totais (m³/ha)²", "Variância das unidades amostrais temporárias (m³/ha)²", "Variância das unidades amostrais permanentes (m³/ha)²", 
    "Variância da média (m³/ha)²", "Limite do erro de amostragem requerido", "Intensidade da amostragem", "Número total de unidades amostradas", 
    "Unidades temporárias", "Unidades permanentes", "Proporção ótima da subamostra temporária e substituída na segunda ocasião", 
    "Proporção ótima da subamostra permanente e remedida na segunda ocasião", "Nível de de significância (α)"], Valores=[(((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm), Xu, Xm, ((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm)-((quantile(TDist((length(Unidades))-1),1-alpha/2))*
    (sqrt(((sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/((length(Unidades))-1))/
    (length(Unidades)))*(1-((length(Unidades))/N)))))), ((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm)+((quantile(TDist((length(Unidades))-1),1-alpha/2))*
    (sqrt(((sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/((length(Unidades))-1))/(length(Unidades)))*(1-((length(Unidades))/N)))))), 
    (N*(((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm)/Conversor), 
    (((N*(((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-N*((quantile(TDist((length(Unidades))-1),1-alpha/2))*
    (sqrt(((sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/((length(Unidades))-1))/
    (length(Unidades)))*(1-((length(Unidades))/N))))))/Conversor), (((N*(((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))+N*((quantile(TDist((length(Unidades))-1),1-alpha/2))*
    (sqrt(((sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/((length(Unidades))-1))/(length(Unidades)))*(1-((length(Unidades))/N))))))/Conversor), 
    N, (((quantile(TDist((length(Unidades))-1),1-alpha/2))*(sqrt(((sum((Ocasião_1.-(((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm)).^2)/((length(Unidades))-1))/
    (length(Unidades)))*(1-((length(Unidades))/N)))))/(((length(Unidades)-(length(unique(Ocasião_2))))/
    (length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))*100, ((quantile(TDist((length(Unidades))-1),1-alpha/2))*(sqrt(((sum((Ocasião_1.-(((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm)).^2)/((length(Unidades))-1))/
    (length(Unidades)))*(1-((length(Unidades))/N))))), sqrt(((sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/
    (length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/
    ((length(Unidades))-1))/(length(Unidades)))*(1-((length(Unidades))/N))), sqrt(sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/((length(Unidades))-1)), sqrt(Sxu²), sqrt(Sxm²), sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/
    (length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/((length(Unidades))-1), Sxu², Sxm², ((sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/((length(Unidades))-1))/(length(Unidades)))*(1-((length(Unidades))/N)), 
    (0.05*(((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm)), 
    ((((quantile(TDist((length(Unidades))-1),1-alpha/2)))^2)*(sum((Ocasião_1.-((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/((length(Unidades))-1)))/
    (((((0.05*(((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))))^2)+
    (((((quantile(TDist((length(Unidades))-1),1-alpha/2)))^2)*(sum((Ocasião_1.-((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))).^2)/((length(Unidades))-1)))/N)), length(Unidades), length(Unidades)-(length(unique(Ocasião_2))), 
    length(unique(Ocasião_2)), (length(Unidades)-(length(unique(Ocasião_2))))/length(Unidades), (length(unique(Ocasião_2)))/length(Unidades), 
    alpha]) #Tabela de resultados
        
    ###Segunda ocasião###
    length(unique(Ocasião_2)) #Número de unidades amostradas
    sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))) #Média
    sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))) #Variância
    sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))) #Desvio padrão
    #Estimativa da segunda ocasião por regressão
    j=AD[!, [:Ocasião_1]]
    g=Matrix(j)
    matriz=transpose(g)
    a = [(matriz[i].- Xm) for i in ((length(Unidades)-(length(unique(Ocasião_2))))+1):length(Unidades)]
    h = (skipmissing(Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))))
    c = sum(a.*h)
    Sxy=c/((length(unique(Ocasião_2)))-1)
    Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))) #Coeficiente de correlação
    (Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))))*((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))/Sxm) #Coeficiente angular
    #Resultado da regressão
    (sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/
    (length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm) #Volume médio estimado  
    #Variância da regressão
    z=[(matriz[i].^2) for i in ((length(Unidades)-(length(unique(Ocasião_2))))+1):length(Unidades)]
    Syx²=(1/((length(unique(Ocasião_2))-2))*((sum(skipmissing(Ocasião_2.^2)))-
    (((sum(skipmissing(Ocasião_1.*Ocasião_2))^2)/sum(z))))) 
    (Syx²/(length(unique(Ocasião_2)))+(((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))-Syx²)/(length(Unidades)))) #Variância da média
    sqrt((Syx²/(length(unique(Ocasião_2)))+(((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))-Syx²)/(length(Unidades))))) #Erro padrão
    quantile(TDist((length(unique(Ocasião_2)))-1),1-alpha/2) #Valor de t 
    (0.05*((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))) #Limite dO erro de amostragem requerido
    ((((quantile(TDist((length(unique(Ocasião_2)))-1),1-alpha/2)))^2)*((length(Unidades))*(Syx²)+((length(Unidades))-
    (length(Unidades)-(length(unique(Ocasião_2)))))*((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))-Syx²)))/
    ((length(Unidades))*((((0.05*((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm)))))^2)) #Intenssidade de amostragem
    #Erro da amostragem
    (quantile(TDist((length(unique(Ocasião_2)))-1),1-alpha/2))*
    (sqrt((Syx²/(length(unique(Ocasião_2)))+(((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))-Syx²)/(length(Unidades)))))) #Absoluto
    (((quantile(TDist((length(unique(Ocasião_2)))-1),1-alpha/2))*(sqrt((Syx²/(length(unique(Ocasião_2)))+
    (((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)/(length(Unidades)))))))/((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))))*((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm)))*100 #Relativo
    #Limite do intervalo de confiança para média
    ((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))-
    ((quantile(TDist((length(unique(Ocasião_2)))-1),1-alpha/2))*(sqrt((Syx²/(length(unique(Ocasião_2)))+
    (((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)/(length(Unidades))))))) #Inferior
    ((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/
    (length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))+
    ((quantile(TDist((length(unique(Ocasião_2)))-1),1-alpha/2))*(sqrt((Syx²/(length(unique(Ocasião_2)))+
    (((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)/
    (length(Unidades))))))) #Superior
    #Total estimado
    (N*((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/
    (length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))/Conversor)
    #Limite do intervalo de confiança para o total
    (((N*((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm)))-N*
    ((quantile(TDist((length(unique(Ocasião_2)))-1),1-alpha/2))*(sqrt((Syx²/(length(unique(Ocasião_2)))+
    (((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)/(length(Unidades))))))))/Conversor) #Inferior
    (((N*((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm)))+N*
    ((quantile(TDist((length(unique(Ocasião_2)))-1),1-alpha/2))*(sqrt((Syx²/(length(unique(Ocasião_2)))+
    (((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)/(length(Unidades))))))))/Conversor) #Superior
    Segunda_ocasião = DataFrames.DataFrame(Variáveis=["Média das unidades amostrais permanentes", "Volume médio estimado se a segunda ocasião houvesse todas unidades amostrais", 
    "Limite inferior do intervalo de confiança para média (m³/ha)", "Limite superior do intervalo de confiança para média (m³/ha)", "Total da população (m³)", 
    "Limite inferior do intervalo de confiança para o total (m³)", "Limite superior do intervalo de confiança para o total (m³)", 
    "Área da população (ha)", "Erro da amostragem relativo (%)","Erro da amostragem absoluto (m³/ha)", "Erro padrão (m³/ha)", "Desvio padrão (m³/ha)", 
    "Variância (m³/ha)²", "Variância da regressão (m³/ha)²", "Variância da média (m³/ha)²", "Limite do erro de amostragem requerido", 
    "Intensidade da amostragem", "Número total de unidades amostradas", "Nível de significância (α)"], Valores=[sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))), 
    (sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm), 
    ((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))-((quantile(TDist((length(unique(Ocasião_2)))-1),1-alpha/2))*(sqrt((Syx²/(length(unique(Ocasião_2)))+
    (((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))-Syx²)/(length(Unidades))))))), 
    ((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))+
    ((quantile(TDist((length(unique(Ocasião_2)))-1),1-alpha/2))*(sqrt((Syx²/(length(unique(Ocasião_2)))+
    (((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))-Syx²)/
    (length(Unidades))))))), (N*((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/
    (length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))/Conversor), 
    (((N*((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm)))-N*
    ((quantile(TDist((length(unique(Ocasião_2)))-1),1-alpha/2))*(sqrt((Syx²/(length(unique(Ocasião_2)))+
    (((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)/(length(Unidades))))))))/Conversor), (((N*((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm)))+N*((quantile(TDist((length(unique(Ocasião_2)))-1),1-alpha/2))*(sqrt((Syx²/(length(unique(Ocasião_2)))+
    (((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)/(length(Unidades))))))))/Conversor), N, (((quantile(TDist((length(unique(Ocasião_2)))-1),1-alpha/2))*(sqrt((Syx²/(length(unique(Ocasião_2)))+
    (((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)/(length(Unidades)))))))/((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))))*((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm)))*100, 
    (quantile(TDist((length(unique(Ocasião_2)))-1),1-alpha/2))*
    (sqrt((Syx²/(length(unique(Ocasião_2)))+(((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))-Syx²)/(length(Unidades)))))), sqrt((Syx²/(length(unique(Ocasião_2)))+(((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))-Syx²)/(length(Unidades))))), 
    sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))), 
    sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))), 
    Syx², (Syx²/(length(unique(Ocasião_2)))+(((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))-Syx²)/(length(Unidades)))), 
    (0.05*((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))), ((((quantile(TDist((length(unique(Ocasião_2)))-1),1-alpha/2)))^2)*((length(Unidades))*(Syx²)+((length(Unidades))-
    (length(Unidades)-(length(unique(Ocasião_2)))))*((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))-Syx²)))/((length(Unidades))*((((0.05*((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm)))))^2)), length(unique(Ocasião_2)), alpha]) #Tabela de resultados

    ###Crescimento ou mudança###
    ((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/
    (length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))-
    ((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm)) #Média 
    ((((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)*((1+(Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))))-2))/(((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))))))^2)))/(length(Unidades)))+
    (Syx²/(length(unique(Ocasião_2)))) #Variância da média
    sqrt(((((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)*((1+(Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))-2))/(((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))))))^2)))/(length(Unidades)))+
    (Syx²/(length(unique(Ocasião_2))))) #Erro padrão
    ((length(Unidades))-1)+(length(unique(Ocasião_2))-1) #Grau de liberdade
    quantile(TDist(((((length(Unidades))-1)+(length(unique(Ocasião_2))-1)))-1),1-alpha/2) #Valor de t  
    #Erro de amostragem
    (quantile(TDist(((((length(Unidades))-1)+(length(unique(Ocasião_2))-1)))-1),1-alpha/2))*
    (sqrt(((((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)*((1+(Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))))-2))/(((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))))))^2)))/(length(Unidades)))+
    (Syx²/(length(unique(Ocasião_2)))))) #Absoluto
    (((quantile(TDist(((((length(Unidades))-1)+(length(unique(Ocasião_2))-1)))-1),1-alpha/2))*
    (sqrt(((((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)*((1+(Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))-2))/
    (((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))))))^2)))/(length(Unidades)))+
    (Syx²/(length(unique(Ocasião_2)))))))/(((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))-((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))))*100 #Relativo 
    #Limite do intervalo de confiança para média
    (((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))-((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm)))-((quantile(TDist(((((length(Unidades))-1)+
    (length(unique(Ocasião_2))-1)))-1),1-alpha/2))*(sqrt(((((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)*((1+(Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))-2))/
    (((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))))))^2)))/(length(Unidades)))+
    (Syx²/(length(unique(Ocasião_2))))))) #Inferior
    (((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))-((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm)))+((quantile(TDist(((((length(Unidades))-1)+
    (length(unique(Ocasião_2))-1)))-1),1-alpha/2))*(sqrt(((((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))-Syx²)*
    ((1+(Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))))-2))/(((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))))))^2)))/(length(Unidades)))+
    (Syx²/(length(unique(Ocasião_2))))))) #Superior
    #Total estimado
    N*(((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/
    (length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))-((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm)))
    #Limite do intervalo de confiança para o total
    ((N*(((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))-((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))))-N*((quantile(TDist(((((length(Unidades))-1)+
    (length(unique(Ocasião_2))-1)))-1),1-alpha/2))*
    (sqrt(((((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)*((1+(Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))-2))/
    (((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))))))^2)))/(length(Unidades)))+
    (Syx²/(length(unique(Ocasião_2)))))))) #Inferior 
    ((N*(((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))-((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))))+N*((quantile(TDist(((((length(Unidades))-1)+
    (length(unique(Ocasião_2))-1)))-1),1-alpha/2))*
    (sqrt(((((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)*((1+(Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))-2))/
    (((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))))))^2)))/(length(Unidades)))+
    (Syx²/(length(unique(Ocasião_2)))))))) #Superior
    Mudança_crescimento = DataFrames.DataFrame(Variáveis=["Crescimento médio (m³/ha)", "Limite inferior do intervalo de confiança para média (m³/ha)", 
    "Limite superior do intervalo de confiança para média (m³/ha)", "Total da população (m³)", "Limite inferior do intervalo de confiança para o total (m³)", 
    "Limite superior do intervalo de confiança para o total (m³)", "Área da população (ha)", "Erro da amostragem relativo (%)", 
    "Erro da amostragem absoluto (m³/ha)", "Erro padrão (m³/ha)", "Variância média (m³/ha)²"], Valores=[((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/
    (length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))-((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm)), (((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))-((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm)))-((quantile(TDist(((((length(Unidades))-1)+
    (length(unique(Ocasião_2))-1)))-1),1-alpha/2))*(sqrt(((((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)*((1+(Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))-2))/
    (((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))))))^2)))/(length(Unidades)))+
    (Syx²/(length(unique(Ocasião_2))))))), (((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))-((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm)))+((quantile(TDist(((((length(Unidades))-1)+
    (length(unique(Ocasião_2))-1)))-1),1-alpha/2))*(sqrt(((((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))-Syx²)*
    ((1+(Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))))-2))/(((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))))))^2)))/(length(Unidades)))+
    (Syx²/(length(unique(Ocasião_2))))))), N*(((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/
    (length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))-((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))), ((N*(((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))-((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))))-N*((quantile(TDist(((((length(Unidades))-1)+
    (length(unique(Ocasião_2))-1)))-1),1-alpha/2))*
    (sqrt(((((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)*((1+(Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))-2))/
    (((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))))))^2)))/(length(Unidades)))+
    (Syx²/(length(unique(Ocasião_2)))))))), ((N*(((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))-((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))))+N*((quantile(TDist(((((length(Unidades))-1)+
    (length(unique(Ocasião_2))-1)))-1),1-alpha/2))*
    (sqrt(((((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)*((1+(Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))-2))/
    (((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))))))^2)))/(length(Unidades)))+
    (Syx²/(length(unique(Ocasião_2)))))))), N, (((quantile(TDist(((((length(Unidades))-1)+(length(unique(Ocasião_2))-1)))-1),1-alpha/2))*
    (sqrt(((((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)*((1+(Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))-2))/
    (((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))))))^2)))/(length(Unidades)))+
    (Syx²/(length(unique(Ocasião_2)))))))/(((sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2))))+
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((sqrt(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))/Sxm))*(((((length(Unidades)-(length(unique(Ocasião_2))))/(length(Unidades))*Xu)+
    ((length(unique(Ocasião_2)))/length(Unidades))*Xm))-Xm))-((((length(Unidades)-
    (length(unique(Ocasião_2))))/(length(Unidades))*Xu)+((length(unique(Ocasião_2)))/length(Unidades))*Xm))))*100, 
    (quantile(TDist(((((length(Unidades))-1)+(length(unique(Ocasião_2))-1)))-1),1-alpha/2))*
    (sqrt(((((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)*((1+(Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))))-2))/(((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))))))^2)))/(length(Unidades)))+
    (Syx²/(length(unique(Ocasião_2)))))), sqrt(((((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)*((1+(Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))-2))/(((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))))))^2)))/(length(Unidades)))+
    (Syx²/(length(unique(Ocasião_2))))), ((((sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1))))-Syx²)*((1+(Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1)))))))*
    ((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/(length(unique(Ocasião_2)))))).^2)/
    ((length(unique(Ocasião_2))-1)))))))-2))/(((Sxy/(sqrt(Sxm²*(sum((skipmissing((Ocasião_2.-(sum(skipmissing(Ocasião_2))/
    (length(unique(Ocasião_2)))))).^2)/((length(unique(Ocasião_2))-1))))))))^2)))/(length(Unidades)))+
    (Syx²/(length(unique(Ocasião_2))))]) #Tabela de resultados
    XLSX.writetable(("F:/Version_09_07_21/iflorestal.jl/09.xlsx"), Dados=(collect(DataFrames.eachcol(Dados)), DataFrames.names(Dados)), 
        Primeira_ocasião=(collect(DataFrames.eachcol(Primeira_ocasião)), DataFrames.names(Primeira_ocasião)), 
        Segunda_ocasião=(collect(DataFrames.eachcol(Segunda_ocasião)), DataFrames.names(Segunda_ocasião)), 
        Crescimento_ou_mudança=(collect(DataFrames.eachcol(Mudança_crescimento)), 
        DataFrames.names(Mudança_crescimento))) #Exportar para o Excel
end
_________________________________________________________________________________________________________________________________________

#Processamento do inventário
#Importar dados
Dados = CSV.read("F:/Version_09_07_21/AD.csv", DataFrame) 
#Informações necessárias
#Área da população
const N =500
#Nível de significância (α)
const alpha = 0.05
#Unidade de medida da variável
Unidade = "m³/1 ha" #Alterar em função do inventário
#Conversor para a unidade de área por hectare
Área_da_parcela=1
Conversor=1/Área_da_parcela
#function AD(n, Ocasiao_1, Ocasiao_2)
AD(Dados.n, Dados.Ocasiao_1, Dados.Ocasiao_2) #Saída dos dados
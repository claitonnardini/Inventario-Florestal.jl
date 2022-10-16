#Inventário Florestal🌳
#Amostragem com repetição parcial
_________________________________________________________________________________________________________________________________________

using DataFrames, Statistics, Distributions, CSV, XLSX #Habilitar pacotes
_________________________________________________________________________________________________________________________________________

#Função ARP: amostragem com repetição parcial
function ARP(Unidade, Subamostra, Ocasiao_1, Ocasiao_2) #Determina a função
    Unidades = Dados.Unidade
    Subamostra = Dados.Subamostra
    Ocasião_1 = (Conversor.*Dados.Ocasiao_1)
    Ocasião_2 = (Conversor.*Dados.Ocasiao_2)
    ARP = DataFrame(Unidades = Unidades, Subamostra = Subamostra, Ocasião_1 = Ocasião_1, Ocasião_2 = Ocasião_2)
    ###Informações do inventário###
    length(Unidades) #Número total de unidades
    length(unique(skipmissing(Ocasião_1))) #Número de unidades da primeira ocasião
    length(unique(skipmissing(Ocasião_2))) #Número de unidades da segunda ocasião
    (length(Unidades))-(length(unique(skipmissing(Ocasião_2)))) #Número de subamostras temporárias
    (length(Unidades))-(length(unique(skipmissing(Ocasião_1)))) #Número de subamostras permanentes
    ((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))/
    (length(unique(skipmissing(Ocasião_1)))) #Proporção ideal da subamostra temporária e substituída na segunda ocasião
    ((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))/
    (length(unique(skipmissing(Ocasião_1)))) #Proporção ideal da subamostra permanente e medida novamente na segunda ocasião
    ((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))) #Número de novas subamostras temporárias
    Informações_do_inventário = DataFrames.DataFrame(Variáveis=["Área da população (ha)", "Número total de unidades", 
    "Número de unidades da primeira ocasião", "Número de unidades da segunda ocasião", "Número de subamostras temporárias", 
    "Número de subamostras permanentes", "Número de novas subamostra temporárias", "Proporção ótima da subamostra temporária e substituída na segunda ocasião", 
    "Proporção ótima da subamostra permanente e remedida na segunda ocasião", "Nível de significância (α)"], Valores=[N, length(Unidades), 
    length(unique(skipmissing(Ocasião_1))), length(unique(skipmissing(Ocasião_2))), (length(Unidades))-(length(unique(skipmissing(Ocasião_2)))), 
    (length(Unidades))-(length(unique(skipmissing(Ocasião_1)))), ((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))), 
    ((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))/(length(unique(skipmissing(Ocasião_1)))), 
    ((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))/(length(unique(skipmissing(Ocasião_1)))), alpha])

    ###Primeira ocasião###  
    #Média das unidades de amostragem temporárias
    j=ARP[!, [:Ocasião_1]]
    g=Matrix(j)
    matriz=transpose(g)
    global Xu = 0
    for i in 1:((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))
    global Xu += matriz[i]/(((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))
    end
    println("Média das unidades amostrais temporárias", Xu)
    #Média das unidades de amostragem permanentes
    global Xm = 0
    for i in (((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))+1):(length(unique(skipmissing(Ocasião_1))))
    global Xm += matriz[i]/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))
    end 
    println("Média das unidades amostrais temporárias", Xm)
    sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))) #Média das unidades amostrais totais 
    #Variância das unidades de amostragem temporárias
    global A = 0
    global Sxu² = 0
    for i in 1:((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))
    global A += ((matriz[i].-Xu).^2)
    global Sxu² = A/(((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))-1)
    end
    println("Variância das unidades amostrais temporárias", Sxu²)
    #Variância das unidades de amostragem permanentes
    global C = 0
    global Sxm² = 0
    for i in (((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))+1):(length(unique(skipmissing(Ocasião_1))))
    global C += ((matriz[i].-Xm).^2)
    global Sxm² = C/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1)
    end
    println("Média das unidades amostrais temporárias", Sxm²)
    sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1 #Variância total de unidades amostradas
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*
    (1-((length(unique(skipmissing(Ocasião_1))))/N)) #Variância da média
    sqrt(((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*
    (1-((length(unique(skipmissing(Ocasião_1))))/N))) #Erro padrão
    quantile(TDist((length(unique(skipmissing(Ocasião_1))))-1),1-alpha/2) #Valor de t
    #Erro de amostragem
    (quantile(TDist((length(unique(skipmissing(Ocasião_1))))-1),1-alpha/2))*
    (sqrt(((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*
    (1-((length(unique(skipmissing(Ocasião_1))))/N)))) #Absoluto
    (((quantile(TDist((length(unique(skipmissing(Ocasião_1))))-1),1-alpha/2))*(sqrt(((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*
    (1-((length(unique(skipmissing(Ocasião_1))))/N)))))/(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1))))))*100 #Relativo
    #Limite do intervalo de confiança para média
    ((sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))-((quantile(TDist((length(unique(skipmissing(Ocasião_1))))-1),1-alpha/2))*
    (sqrt(((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*(1-((length(unique(skipmissing(Ocasião_1))))/N)))))) #Inferior
    ((sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))+((quantile(TDist((length(unique(skipmissing(Ocasião_1))))-1),1-alpha/2))*
    (sqrt(((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*(1-((length(unique(skipmissing(Ocasião_1))))/N)))))) #Superior
    #Total estimado
    (N*(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))/Conversor)
    #Limite do intervalo de confiança para o total
    (((N*(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1))))))-N*((quantile(TDist((length(unique(skipmissing(Ocasião_1))))-1),1-alpha/2))*
    (sqrt(((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*
    (1-((length(unique(skipmissing(Ocasião_1))))/N))))))/Conversor) #Inferior
    (((N*(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1))))))+N*((quantile(TDist((length(unique(skipmissing(Ocasião_1))))-1),1-alpha/2))*
    (sqrt(((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*
    (1-((length(unique(skipmissing(Ocasião_1))))/N))))))/Conversor) #Superior 
    Primeira_ocasião = DataFrames.DataFrame(Variáveis=["Média das unidades amostrais totais (m³/ha)", "Média das unidades amostrais temporárias (m³/ha)", 
    "Média das unidades amostrais permanentes (m³/ha)", "Limite inferior do intervalo de confiança para média (m³/ha)", 
    "Limite superior do intervalo de confiança para média (m³/ha)", "Total da população (m³)", 
    "Limite inferior do intervalo de confiança para o total (m³)", "Limite superior do intervalo de confiança para o total (m³)", 
    "Erro da amostragem relativo (%)", "Erro da amostragem absoluto (m³/ha)", "Erro padrão (m³/ha)", "Variância das unidades amostrais totais (m³/ha)²", 
    "Variância das unidades amostrais temporárias (m³/ha)²", "Variância das unidades amostrais permanentes (m³/ha)²", "Variância da média (m³/ha)²"], 
    Valores=[sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))), Xu, Xm, ((sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))-
    ((quantile(TDist((length(unique(skipmissing(Ocasião_1))))-1),1-alpha/2))*(sqrt(((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*
    (1-((length(unique(skipmissing(Ocasião_1))))/N)))))), ((sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))+
    ((quantile(TDist((length(unique(skipmissing(Ocasião_1))))-1),1-alpha/2))*(sqrt(((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*
    (1-((length(unique(skipmissing(Ocasião_1))))/N)))))), (N*(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))/Conversor), 
    (((N*(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1))))))-N*((quantile(TDist((length(unique(skipmissing(Ocasião_1))))-1),1-alpha/2))*
    (sqrt(((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*
    (1-((length(unique(skipmissing(Ocasião_1))))/N))))))/Conversor), (((N*(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1))))))+N*((quantile(TDist((length(unique(skipmissing(Ocasião_1))))-1),1-alpha/2))*
    (sqrt(((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*
    (1-((length(unique(skipmissing(Ocasião_1))))/N))))))/Conversor), (((quantile(TDist((length(unique(skipmissing(Ocasião_1))))-1),1-alpha/2))*
    (sqrt(((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*
    (1-((length(unique(skipmissing(Ocasião_1))))/N)))))/(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1))))))*100, 
    (quantile(TDist((length(unique(skipmissing(Ocasião_1))))-1),1-alpha/2))*(sqrt(((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*
    (1-((length(unique(skipmissing(Ocasião_1))))/N)))), sqrt(((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*
    (1-((length(unique(skipmissing(Ocasião_1))))/N))),  sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1, Sxu², Sxm², ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*
    (1-((length(unique(skipmissing(Ocasião_1))))/N))]) #Tabela de resultados 
    ###Segunda ocasião###
    ((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))/(length(unique(skipmissing(Ocasião_2))))
    ((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))/(length(unique(skipmissing(Ocasião_2))))
    #Média de unidades de amostragem permanentes
    j=ARP[!, [:Ocasião_2]]
    g=Matrix(j)
    matriz=transpose(g)
    global Ym = 0
    for i in (((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))+1):(length(unique(skipmissing(Ocasião_1))))
    global Ym += matriz[i]/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))
    end 
    println("Média das unidades amostrais permanentes", Ym)
    #Média de novas unidades de amostragem temporárias
    j=ARP[!, [:Ocasião_2]]
    g=Matrix(j)
    matriz=transpose(g)
    global Yn = 0
    for i in ((length(unique(skipmissing(Ocasião_1))))+1):(length(Unidades))
    global Yn += matriz[i]/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))
    end
    println("Média das unidades amostrais temporárias", Yn)
    sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))) #Média total das unidades amostradas
    #Variância de unidades de amostragem permanentes
    j=ARP[!, [:Ocasião_2]]
    g=Matrix(j)
    matriz=transpose(g)
    global B = 0
    global Sym² = 0
    for i in (((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))+1):(length(unique(skipmissing(Ocasião_1))))
    global B += ((matriz[i].-Ym).^2)
    global Sym² = B/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1)
    end
    println("Variância das unidades amostrais permanentes", Sym²)
    #Variância de novas unidades de amostragem temporárias
    j=ARP[!, [:Ocasião_2]]
    g=Matrix(j)
    matriz=transpose(g)
    global B = 0
    global Syn² = 0
    for i in ((length(unique(skipmissing(Ocasião_1))))+1):(length(Unidades))
    global B += ((matriz[i].-Yn).^2)
    global Syn² = B/((((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))-1)
    end
    println("Variância das unidades amostrais permanentes", Syn²)
    sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1 #Variância das unidades de amostragem totais
    #Variâncias de unidades permanentes e temporárias
    j=ARP[!, [:Ocasião_1]]
    g=Matrix(j)
    matriz=transpose(g)
    D = [matriz[i].-Xm for i in (((length(Unidades))-
        (length(unique(skipmissing(Ocasião_2)))))+1):(length(unique(skipmissing(Ocasião_1))))]
    j=ARP[!, [:Ocasião_2]]
    g=Matrix(j)
    matriz=transpose(g)
    J = [matriz[i].-Ym for i in (((length(Unidades))-
        (length(unique(skipmissing(Ocasião_2)))))+1):(length(unique(skipmissing(Ocasião_1))))]
    sum(D.*J)/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1)
    (sum(D.*J)/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1))/
    (sqrt(Sxm²)*sqrt(Sym²)) #Coeficiente de correlação entre os volumes das unidades permanentes medidas nas duas ocasiões
    ((sum(D.*J)/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1))/
    (sqrt(Sxm²)*sqrt(Sym²)))*(sqrt(Sym²)/sqrt(Sxm²))
    #Constantes "a" e "b" 
    a=((((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))*(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_2)))))/(length(unique(skipmissing(Ocasião_1))))))/
    ((length(unique(skipmissing(Ocasião_2))))-((((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))/
    (length(unique(skipmissing(Ocasião_1)))))*((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))*((((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²))))^2))))*
    (((sum(D.*J)/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*
    (sqrt(Sym²)/sqrt(Sxm²))) ##Constante a
    c=((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))/((length(unique(skipmissing(Ocasião_2))))-
    ((((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))/(length(unique(skipmissing(Ocasião_1)))))*
    (((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))*((((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²))))^2))) #Constante c
    #Média corrente
    ((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)
    ((a^2)*(sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)*
    ((1/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+(1/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))))+((c^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))+(((1-c)^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))-((2*a*c*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²))))*((sqrt(Sym²)*sqrt(Sxm²))/
    ((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))) #Variância da média
    sqrt(((a^2)*(sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)*((1/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+(1/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))))+((c^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))+(((1-c)^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))-((2*a*c*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²))))*((sqrt(Sym²)*sqrt(Sxm²))/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))) #Erro padrão 
    quantile(TDist((length(unique(skipmissing(Ocasião_2))))-1),1-alpha/2) #Valor de t
    #Erro de amostrage
    (quantile(TDist((length(unique(skipmissing(Ocasião_2))))-1),1-alpha/2))*
    (sqrt(((a^2)*(sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)*((1/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+(1/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))))+((c^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))+(((1-c)^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))-((2*a*c*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²))))*((sqrt(Sym²)*sqrt(Sxm²))/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))))) #Absoluto
    (((quantile(TDist((length(unique(skipmissing(Ocasião_2))))-1),1-alpha/2))*(sqrt(((a^2)*(sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)*((1/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_2))))))+(1/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))))+((c^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))+(((1-c)^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))-((2*a*c*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²))))*((sqrt(Sym²)*sqrt(Sxm²))/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))))))/
    (((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)))*100 #Relativo
    #Intervalo de confiança para a média
    ((((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn))-((quantile(TDist((length(unique(skipmissing(Ocasião_2))))-1),1-alpha/2))*
    (sqrt(((a^2)*(sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)*((1/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+(1/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))))+((c^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))+(((1-c)^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))-((2*a*c*((sum(D.*J)/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²))))*
    ((sqrt(Sym²)*sqrt(Sxm²))/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))))))) #Inferior 
    ((((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn))+((quantile(TDist((length(unique(skipmissing(Ocasião_2))))-1),1-alpha/2))*
    (sqrt(((a^2)*(sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)*((1/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+(1/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))))+((c^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))+(((1-c)^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))-((2*a*c*((sum(D.*J)/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1))/
    (sqrt(Sxm²)*sqrt(Sym²))))*((sqrt(Sym²)*sqrt(Sxm²))/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))))))) #Superior
    #Total estimado
    (N*(((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn))/Conversor)
    #Intervalo de confiança para o total
    (((N*(((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)))-N*((quantile(TDist((length(unique(skipmissing(Ocasião_2))))-1),1-alpha/2))*
    (sqrt(((a^2)*(sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)*((1/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+(1/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))))+((c^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))+(((1-c)^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))-((2*a*c*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²))))*((sqrt(Sym²)*sqrt(Sxm²))/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))))))/Conversor) #Inferior
    (((N*(((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)))+N*((quantile(TDist((length(unique(skipmissing(Ocasião_2))))-1),1-alpha/2))*
    (sqrt(((a^2)*(sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)*((1/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+(1/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))))+((c^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))+(((1-c)^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))-((2*a*c*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²))))*((sqrt(Sym²)*sqrt(Sxm²))/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))))))/Conversor) #Superior

    Segunda_ocasião = DataFrames.DataFrame(Variáveis=["Média das unidades amostrais totais (m³/ha)", "Média das unidades amostrais permanente (m³/ha)", 
    "Média das novas unidades amostrais temporárias (m³/ha)", "Média corrente (m³/ha)", "Limite inferior do intervalo de confiança para média (m³/ha)", 
    "Limite superior do intervalo de confiança para média (m³/ha)", "Total da população (m³)", 
    "Limite inferior do intervalo de confiança para o total (m³)", "Limite superior do intervalo de confiança para o total (m³)", 
    "Erro da amostragem relativo (%)", "Erro da amostragem absoluto (m³/ha)", "Erro padrão (m³/ha)", 
    "Variância das unidades amostrais totais (m³/ha)²", "Variância das novas unidades amostrais temporárias (m³/ha)²", 
    "Variância das unidades amostrais permanentes (m³/ha)²", "Variância da média (m³/ha)²", "Coeficiente de correlação", "Constante a", 
    "Constante c"], Valores=[sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))), Yn, Ym, ((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn), 
    ((((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn))-((quantile(TDist((length(unique(skipmissing(Ocasião_2))))-1),1-alpha/2))*
    (sqrt(((a^2)*(sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)*((1/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+(1/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))))+((c^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))+(((1-c)^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))-((2*a*c*((sum(D.*J)/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²))))*
    ((sqrt(Sym²)*sqrt(Sxm²))/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))))))), 
    ((((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn))+((quantile(TDist((length(unique(skipmissing(Ocasião_2))))-1),1-alpha/2))*
    (sqrt(((a^2)*(sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)*((1/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+(1/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))))+((c^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))+(((1-c)^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))-((2*a*c*((sum(D.*J)/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1))/
    (sqrt(Sxm²)*sqrt(Sym²))))*((sqrt(Sym²)*sqrt(Sxm²))/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))))))), 
    (N*(((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn))/Conversor), (((N*(((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)))-N*((quantile(TDist((length(unique(skipmissing(Ocasião_2))))-1),1-alpha/2))*
    (sqrt(((a^2)*(sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)*((1/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+(1/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))))+((c^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))+(((1-c)^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))-((2*a*c*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²))))*((sqrt(Sym²)*sqrt(Sxm²))/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))))))/Conversor), (((N*(((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)))+N*((quantile(TDist((length(unique(skipmissing(Ocasião_2))))-1),1-alpha/2))*
    (sqrt(((a^2)*(sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)*((1/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+(1/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))))+((c^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))+(((1-c)^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))-((2*a*c*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²))))*((sqrt(Sym²)*sqrt(Sxm²))/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))))))/Conversor), (((quantile(TDist((length(unique(skipmissing(Ocasião_2))))-1),1-alpha/2))*(sqrt(((a^2)*(sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)*((1/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_2))))))+(1/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))))+((c^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))+(((1-c)^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))-((2*a*c*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²))))*((sqrt(Sym²)*sqrt(Sxm²))/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))))))/
    (((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)))*100, (quantile(TDist((length(unique(skipmissing(Ocasião_2))))-1),1-alpha/2))*
    (sqrt(((a^2)*(sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)*((1/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+(1/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))))+((c^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))+(((1-c)^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))-((2*a*c*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²))))*((sqrt(Sym²)*sqrt(Sxm²))/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))))), sqrt(((a^2)*(sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)*((1/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+(1/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))))+((c^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))+(((1-c)^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))-((2*a*c*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²))))*((sqrt(Sym²)*sqrt(Sxm²))/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))), 
    sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1, Syn², Sym², ((a^2)*(sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)*((1/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+(1/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))))+((c^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))+(((1-c)^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))-((2*a*c*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²))))*((sqrt(Sym²)*sqrt(Sxm²))/
    ((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))), (sum(D.*J)/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1))/
    (sqrt(Sxm²)*sqrt(Sym²)), a, c]) #Tabela de resultados
        
    ###Crescimento ou mudança###
    #Estimativa direta 
    #É necessário achar os coeficientes "A" e "B" para calcular a crescimento médio 
    A=(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))/((length(unique(skipmissing(Ocasião_2))))-
    ((((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))/(length(unique(skipmissing(Ocasião_1)))))*((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))*((sum(D.*J)/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))^2)))+
    (((((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))*(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))/(length(unique(skipmissing(Ocasião_1))))))/
    ((length(unique(skipmissing(Ocasião_2))))-((((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))/(length(unique(skipmissing(Ocasião_1)))))*
    ((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))^2)))*
    (((sum(D.*J)/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*(sqrt(Sxm²)/sqrt(Sym²))))
    B=((-((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))*(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_2)))))/(length(unique(skipmissing(Ocasião_1))))))/
    ((length(unique(skipmissing(Ocasião_2))))-((((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))/
    (length(unique(skipmissing(Ocasião_1)))))*((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))^2)))*
    (((sum(D.*J)/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*
    (sqrt(Sym²)/sqrt(Sxm²)))-(((length(unique(skipmissing(Ocasião_2))))*(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))/(length(unique(skipmissing(Ocasião_1))))))/
    ((length(unique(skipmissing(Ocasião_2))))-((((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))/
    (length(unique(skipmissing(Ocasião_1)))))*((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))^2)))
    #Crescimento médio
    (A*Ym)+((1-A)*Yn)+(B*Xm)-((1+B)*Xu) 
    #Melhor estimativa da média direta da primeira ocasião
     #Para isso é necessário encontrar os coeficientes "b" "c" dados pelas seguintes equações:
    b=((length(unique(skipmissing(Ocasião_2))))*(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))/
    (length(unique(skipmissing(Ocasião_1))))))/((length(unique(skipmissing(Ocasião_2))))-
    ((((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))/(length(unique(skipmissing(Ocasião_1)))))*
    ((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))^2))
    c=((-((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))*(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))/(length(unique(skipmissing(Ocasião_1))))))/
    ((length(unique(skipmissing(Ocasião_2))))-((((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))/
    (length(unique(skipmissing(Ocasião_1)))))*((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))^2)))*(sqrt(Sxm²)/sqrt(Sym²))
    #A partir deste ponto, encontra-se a média direta da primeira ocasião
    X=((1-b)*Xu)+(b*Xm)+(c*Ym)-(c*Yn) 
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*
    (1-((((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))*((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))^2)/
    (((length(unique(skipmissing(Ocasião_1))))*(length(unique(skipmissing(Ocasião_2)))))-
    (((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))*((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/
    (sqrt(Sxm²)*sqrt(Sym²)))^2)))) #Variância da média
    sqrt(((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*
    (1-((((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))*((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))^2)/
    (((length(unique(skipmissing(Ocasião_1))))*(length(unique(skipmissing(Ocasião_2)))))-
    (((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))*((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/
    (sqrt(Sxm²)*sqrt(Sym²)))^2))))) #Erro padrão
    ((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)-X #Estimativa de crescimento
    (A^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+((1-A)^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+(B^2)*
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+((1+B)^2)*
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+2*A*B*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*((sqrt(Sxm²)*sqrt(Sym²))/
    ((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))) #Variâcia da média
    sqrt((A^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+((1-A)^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+(B^2)*
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+((1+B)^2)*
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+2*A*B*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*((sqrt(Sxm²)*sqrt(Sym²))/
    ((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))) #Erro padrão
    ((length(unique(skipmissing(Ocasião_1))))-1)+((length(unique(skipmissing(Ocasião_2))))-1) #Grau de liberdade
    quantile(TDist((((length(unique(skipmissing(Ocasião_1))))-1)+((length(unique(skipmissing(Ocasião_2))))-1))-1),1-alpha/2) #Valor t
    #Erro de amostragem
    (quantile(TDist((((length(unique(skipmissing(Ocasião_1))))-1)+((length(unique(skipmissing(Ocasião_2))))-1))-1),1-alpha/2))*(sqrt((A^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+((1-A)^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+(B^2)*
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+((1+B)^2)*
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+2*A*B*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*((sqrt(Sxm²)*sqrt(Sym²))/
    ((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))) #Absoluto
    (((quantile(TDist((((length(unique(skipmissing(Ocasião_1))))-1)+((length(unique(skipmissing(Ocasião_2))))-1))-1),1-alpha/2))*(sqrt((A^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+((1-A)^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+(B^2)*
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+((1+B)^2)*
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+2*A*B*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*((sqrt(Sxm²)*sqrt(Sym²))/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))))/
    (((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)-X))*100 #Relativo
    #Limite do intervalo de confiança para média
    ((((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)-X)-((quantile(TDist((((length(unique(skipmissing(Ocasião_1))))-1)+
    ((length(unique(skipmissing(Ocasião_2))))-1))-1),1-alpha/2))*(sqrt((A^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+((1-A)^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+(B^2)*((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+((1+B)^2)*((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_2))))))+2*A*B*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*((sqrt(Sxm²)*sqrt(Sym²))/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))))) #Inferior
    ((((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)-X)+((quantile(TDist((((length(unique(skipmissing(Ocasião_1))))-1)+
    ((length(unique(skipmissing(Ocasião_2))))-1))-1),1-alpha/2))*(sqrt((A^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+((1-A)^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+(B^2)*((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+((1+B)^2)*((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_2))))))+2*A*B*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*((sqrt(Sxm²)*sqrt(Sym²))/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))))) #Superior
    #Crescimento total estimado
    N*(((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)-X)
    #Limite do intervalo de confiança para o total
    ((N*(((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)-X))-N*((quantile(TDist((((length(unique(skipmissing(Ocasião_1))))-1)+
    ((length(unique(skipmissing(Ocasião_2))))-1))-1),1-alpha/2))*(sqrt((A^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+((1-A)^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+(B^2)*((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+((1+B)^2)*((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_2))))))+2*A*B*((sum(D.*J)/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*
    ((sqrt(Sxm²)*sqrt(Sym²))/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))))) #Inferior
    ((N*(((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)-X))+N*((quantile(TDist((((length(unique(skipmissing(Ocasião_1))))-1)+
    ((length(unique(skipmissing(Ocasião_2))))-1))-1),1-alpha/2))*(sqrt((A^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+((1-A)^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+(B^2)*((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+((1+B)^2)*((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_2))))))+2*A*B*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*((sqrt(Sxm²)*sqrt(Sym²))/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))))) #superior
    
    Mudança_crescimento = DataFrames.DataFrame(Variáveis=["Crescimento médio (m³/ha)", "Média direta da primeira ocasião (m³/ha)",
    "Limite inferior do intervalo de confiança para média (m³/ha)", "Limite superior do intervalo de confiança para média (m³/ha)", 
    "Crescimento total estimado (m³)", "Limite inferior do intervalo de confiança para o total (m³)", 
    "Limite superior do intervalo de confiança para o total (m³)", "Erro da amostragem relativo (%)", "Erro da amostragem absoluto (m³/ha)", 
    "Erro padrão (m³/ha)", "Variância da média", "Coeficientes A", "Coeficiente B", "Coeficientes b", "Coeficiente c"], 
    Valores=[(A*Ym)+((1-A)*Yn)+(B*Xm)-((1+B)*Xu), X, ((((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)-X)-((quantile(TDist((((length(unique(skipmissing(Ocasião_1))))-1)+
    ((length(unique(skipmissing(Ocasião_2))))-1))-1),1-alpha/2))*(sqrt((A^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+((1-A)^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+(B^2)*((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+((1+B)^2)*((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_2))))))+2*A*B*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*((sqrt(Sxm²)*sqrt(Sym²))/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))))), (((quantile(TDist((((length(unique(skipmissing(Ocasião_1))))-1)+((length(unique(skipmissing(Ocasião_2))))-1))-1),1-alpha/2))*(sqrt((A^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+((1-A)^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+(B^2)*
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+((1+B)^2)*
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+2*A*B*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*((sqrt(Sxm²)*sqrt(Sym²))/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))))/
    (((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)-X))*100, N*(((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)-X), ((N*(((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)-X))-N*((quantile(TDist((((length(unique(skipmissing(Ocasião_1))))-1)+
    ((length(unique(skipmissing(Ocasião_2))))-1))-1),1-alpha/2))*(sqrt((A^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+((1-A)^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+(B^2)*((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+((1+B)^2)*((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_2))))))+2*A*B*((sum(D.*J)/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*
    ((sqrt(Sxm²)*sqrt(Sym²))/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))))), ((N*(((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)-X))+N*((quantile(TDist((((length(unique(skipmissing(Ocasião_1))))-1)+
    ((length(unique(skipmissing(Ocasião_2))))-1))-1),1-alpha/2))*(sqrt((A^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+((1-A)^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/
    (length(unique(skipmissing(Ocasião_2)))))).^2)/(length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+(B^2)*((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1))))))+((1+B)^2)*((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/
    (length(unique(skipmissing(Ocasião_1)))))).^2)/(length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_2))))))+2*A*B*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*((sqrt(Sxm²)*sqrt(Sym²))/((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))))))), (((quantile(TDist((((length(unique(skipmissing(Ocasião_1))))-1)+((length(unique(skipmissing(Ocasião_2))))-1))-1),1-alpha/2))*(sqrt((A^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+((1-A)^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+(B^2)*
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+((1+B)^2)*
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+2*A*B*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*((sqrt(Sxm²)*sqrt(Sym²))/((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))))/
    (((a*Xu)-(a*Xm)+(c*Ym)+(1-c)*Yn)-X))*100, (quantile(TDist((((length(unique(skipmissing(Ocasião_1))))-1)+((length(unique(skipmissing(Ocasião_2))))-1))-1),1-alpha/2))*(sqrt((A^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+((1-A)^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+(B^2)*
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+((1+B)^2)*
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+2*A*B*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*((sqrt(Sxm²)*sqrt(Sym²))/
    ((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))))), sqrt((A^2)*((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+((1-A)^2)*
    ((sum((skipmissing(Ocasião_2).-(sum(skipmissing(Ocasião_2))/(length(unique(skipmissing(Ocasião_2)))))).^2)/
    (length(unique(skipmissing(Ocasião_2))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+(B^2)*
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))+((1+B)^2)*
    ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/((length(Unidades))-(length(unique(skipmissing(Ocasião_2))))))+2*A*B*((sum(D.*J)/(((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))*((sqrt(Sxm²)*sqrt(Sym²))/
    ((length(Unidades))-(length(unique(skipmissing(Ocasião_1))))))), ((sum((skipmissing(Ocasião_1).-(sum(skipmissing(Ocasião_1))/(length(unique(skipmissing(Ocasião_1)))))).^2)/
    (length(unique(skipmissing(Ocasião_1))))-1)/(length(unique(skipmissing(Ocasião_1)))))*(1-((((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))*((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))*((sum(D.*J)/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1))/(sqrt(Sxm²)*sqrt(Sym²)))^2)/
    (((length(unique(skipmissing(Ocasião_1))))*(length(unique(skipmissing(Ocasião_2)))))-(((length(Unidades))-(length(unique(skipmissing(Ocasião_2)))))*((length(Unidades))-
    (length(unique(skipmissing(Ocasião_1)))))*((sum(D.*J)/(((length(Unidades))-(length(unique(skipmissing(Ocasião_1)))))-1))/
    (sqrt(Sxm²)*sqrt(Sym²)))^2)))), A, B, b, c]) #Tabela de resultados
    XLSX.writetable(("F:/Version_09_07_21/iflorestal.jl/10.xlsx"), Dados=(collect(DataFrames.eachcol(ARP)), DataFrames.names(ARP)), 
    Informações_do_inventário=(collect(DataFrames.eachcol(Informações_do_inventário)), DataFrames.names(Informações_do_inventário)), 
    Primeira_ocasião=(collect(DataFrames.eachcol(Primeira_ocasião)), DataFrames.names(Primeira_ocasião)), 
    Segunda_ocasião=(collect(DataFrames.eachcol(Segunda_ocasião)), DataFrames.names(Segunda_ocasião)), 
    Crescimento_ou_mudança=(collect(DataFrames.eachcol(Mudança_crescimento)), DataFrames.names(Mudança_crescimento))) #Exportar para o Excel
end 
_________________________________________________________________________________________________________________________________________

#Processamento do inventário
#Importar dados
Dados = CSV.read("F:/Version_09_07_21/ARP.csv", DataFrame) 
#Informações necessárias
#Área da população
const N = 1582
#Nível de significância (α)
const alpha = 0.05
#Unidade de medida da variável
Unidade = "m³/1 ha" #Alterar em função do inventário
#Conversor para a unidade de área por hectare
Área_da_parcela=1
Conversor=1/Área_da_parcela
#function ARP(Unidade, Subamostra, Ocasiao_1, Ocasiao_2)
ARP(Dados.Unidade, Dados.Subamostra, Dados.Ocasiao_1, Dados.Ocasiao_2) #Saída dos dados

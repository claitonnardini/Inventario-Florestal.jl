#Linguagem Julia verson v.1.5.3
#Modificado: 17/12/2021
#Inventário Florestal🌳
#Amostragem sistemática com múltiplos inícios aleátorios
_________________________________________________________________________________________________________________________________________

using DataFrames, Statistics, Distributions, CSV, XLSX #Habilitar pacotes
_________________________________________________________________________________________________________________________________________

#Função Multiplos_inicios_aleatorios: amostragem sistemática com múltiplos inícios aleátorios
function Multiplos_inicios_aleatorios(Dados) #Determina a função

    Conjunto_de_dados = (Conversor.*Dados)
    #Tabela com estatísticas descritivas por conglomerados
    Tabela=transform(Conjunto_de_dados, AsTable(:) .=> ByRow.([I -> count(!ismissing, I), sum, mean, var]).=>[:n, :Soma, :Média, :Variância])
    length(Tabela.n) #Número de conglomerados
    first((first(unique((Tabela.n))))) #Número de subunidades
    sum(Tabela.Média)/(length(Tabela.n)) #Média
    sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))
    sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)) #Variância dentro dos conglomerados
    sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/((length(Tabela.n))-1)
    ((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))) #Variância entre conglomerados
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))+
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))) #Variância total
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
    ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))+(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))) #Coeficiente de correlação intraconglomerado
    (0.1*(sum(Tabela.Média)/(length(Tabela.n)))) #Limite de erro da amostragem requerido
    quantile(TDist((length(Tabela.n))-1),1-alpha/2) #Vallor de t 
    (((((quantile(TDist((length(Tabela.n))-1),1-alpha/2))^2)*((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))+
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))))./
    ((((0.1*(sum(Tabela.Média)/(length(Tabela.n))))).^2).*((first(unique((Tabela.n))))))).*(1 .+((((sum(((first(unique((Tabela.n))))).*
    (Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
    ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./
    ((first(unique((Tabela.n))))))+(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1)))))*(((first(unique((Tabela.n))))).-1)) #Fração da amostragem 
    (((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))+
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./((length(Tabela.n))*
    ((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/
    (length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
    ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))+
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))))*
    (((first(unique((Tabela.n))))).-1)) #Variância da média
    sqrt.((((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))+
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./((length(Tabela.n))*
    ((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/
    (length(Tabela.n)))).^2)/((length(Tabela.n))-1)).-
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./
    ((first(unique((Tabela.n))))))/((((sum(((first(unique((Tabela.n))))).*
    (Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))+
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))))*
    (((first(unique((Tabela.n))))).-1))) #Erro padrão
    #Erro da amostragem
    (quantile(TDist((length(Tabela.n))-1),1-alpha/2))*(sqrt.((((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1)))+(((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/
    (length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./((length(Tabela.n))*
    ((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*
    (Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/((length(Tabela.n))-1)).-
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./
    ((first(unique((Tabela.n))))))/((((sum(((first(unique((Tabela.n))))).*
    (Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/((length(Tabela.n))-1)).-
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./
    ((first(unique((Tabela.n))))))+(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1)))))*(((first(unique((Tabela.n))))).-1))))  #Absoluto
    (((quantile(TDist((length(Tabela.n))-1),1-alpha/2))*(sqrt.((((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1)))+(((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/
    (length(Tabela.n)))).^2)/((length(Tabela.n))-1)).-
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./((length(Tabela.n))*
    ((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/
    (length(Tabela.n)))).^2)/((length(Tabela.n))-1)).-
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
    ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))+
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))))*
    (((first(unique((Tabela.n))))).-1)))))/(sum(Tabela.Média)/(length(Tabela.n))))*100 #Relativo
    #Limite do intervalo de confiança para média 
    ((sum(Tabela.Média)/(length(Tabela.n))).-((quantile(TDist((length(Tabela.n))-1),1-alpha/2))*
    (sqrt.((((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))+
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./
    ((length(Tabela.n))*((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*
    (Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
    ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))+
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))))*
    (((first(unique((Tabela.n))))).-1)))))) #Inferior
    ((sum(Tabela.Média)/(length(Tabela.n))).+((quantile(TDist((length(Tabela.n))-1),1-alpha/2))*
    (sqrt.((((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))+
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./
    ((length(Tabela.n))*((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*
    (Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
    ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./
    ((first(unique((Tabela.n))))))+(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1)))))*
    (((first(unique((Tabela.n))))).-1)))))) #Superior
    #Total estimado
    ((N*((first(unique((Tabela.n)))))*(sum(Tabela.Média)/(length(Tabela.n))))/Conversor)
    #Limite do intervalo de confiança para o total
    (((N*((first(unique((Tabela.n)))))*(sum(Tabela.Média)/(length(Tabela.n))))-(N*((first(unique((Tabela.n))))).*
    ((quantile(TDist((length(Tabela.n))-1),1-alpha/2))*(sqrt.((((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))+
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./
    ((length(Tabela.n))*((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*
    (Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
    ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))+
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1)))))*
    (((first(unique((Tabela.n))))).-1)))))))/Conversor) #Inferior
    (((N*((first(unique((Tabela.n)))))*(sum(Tabela.Média)/(length(Tabela.n))))+(N*((first(unique((Tabela.n))))).*
    ((quantile(TDist((length(Tabela.n))-1),1-alpha/2))*(sqrt.((((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))+
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./
    ((length(Tabela.n))*((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*
    (Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
    ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))+
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1)))))*
    (((first(unique((Tabela.n))))).-1)))))))/Conversor) #Superior
    if  (((quantile(TDist((length(Tabela.n))-1),1-alpha/2))*(sqrt.((((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
        (((first(unique((Tabela.n))))).-1)))+(((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/
        (length(Tabela.n)))).^2)/((length(Tabela.n))-1)).-
        (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
        (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./((length(Tabela.n))*
        ((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/
        (length(Tabela.n)))).^2)/((length(Tabela.n))-1)).-
        (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
        (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
        ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
        ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
        ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))+
        (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))))*
        (((first(unique((Tabela.n))))).-1)))))/(sum(Tabela.Média)/(length(Tabela.n))))*100 > EAR
        Observação = "Diante do exposto, conclui-se que os resultados obtidos na amostragem não satisfazem as exigências de
        precisão estabelecidas para o inventário, ou seja, um erro de amostragem máximo de ±10% da média  para confiabilidade designada. 
        O erro estimado foi maior que o limite fixado, sendo recomendado incluir mais unidades amostrais no inventário."
        println(Observação)
        elseif  (((quantile(TDist((length(Tabela.n))-1),1-alpha/2))*(sqrt.((((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
            (((first(unique((Tabela.n))))).-1)))+(((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/
            (length(Tabela.n)))).^2)/((length(Tabela.n))-1)).-
            (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
            (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./((length(Tabela.n))*
            ((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/
            (length(Tabela.n)))).^2)/((length(Tabela.n))-1)).-
            (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
            (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
            ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
            ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
            ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))+
            (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))))*
            (((first(unique((Tabela.n))))).-1)))))/(sum(Tabela.Média)/(length(Tabela.n))))*100 ≤ EAR
                Observação  = "Diante do exposto, conclui-se que os resultados obtidos na amostragem satisfazem as exigências de
                precisão estabelecidas para o inventário, ou seja, um erro de amostragem máximo de ±10% da média para confiabilidade designada. 
                O erro estimado foi menor que o limite fixado, assim as unidades amostrais são suficientes para o inventário."
                println(Observação)
    end
    Resultados = DataFrames.DataFrame(Variaiáveis=["Média (m³/ha)", "Limite inferior do intervalo de confiança para média (m³/ha)", 
    "Limite superior do intervalo de confiança para média (m³/ha)", "Total da população (m³)", "Limite inferior do intervalo de confiança para o total (m³)", 
    "Limite superior do intervalo de confiança para o total (m³)", "Área da população (ha)", "Erro da amostragem relativo (%)", "Erro da amostragem absoluto (m³/ha)", 
    "Erro padrão (m³/ha)", "Variância dentro dos conglomerados (m³/ha)²", "Variância entre conglomerados (m³/ha)²", "Variância total (m³/ha)²", "Variância da média (m³/ha)²", 
    "Coeficiente de correlação intraconglomerados", "Fração da amostragem", "Limite do erro de amostragem requerido", "Número de conglomerados", 
    "Número de subunidades", "Nível de significância (α)", "Observação"], 
    Valores=[sum(Tabela.Média)/(length(Tabela.n)), ((sum(Tabela.Média)/(length(Tabela.n))).-((quantile(TDist((length(Tabela.n))-1),1-alpha/2))*
    (sqrt.((((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))+
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./
    ((length(Tabela.n))*((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*
    (Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
    ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))+
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))))*
    (((first(unique((Tabela.n))))).-1)))))), ((sum(Tabela.Média)/(length(Tabela.n))).+((quantile(TDist((length(Tabela.n))-1),1-alpha/2))*
    (sqrt.((((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))+
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./
    ((length(Tabela.n))*((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*
    (Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
    ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./
    ((first(unique((Tabela.n))))))+(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1)))))*
    (((first(unique((Tabela.n))))).-1)))))), ((N*((first(unique((Tabela.n)))))*(sum(Tabela.Média)/(length(Tabela.n))))/Conversor), 
    (((N*((first(unique((Tabela.n)))))*(sum(Tabela.Média)/(length(Tabela.n))))-(N*((first(unique((Tabela.n))))).*
    ((quantile(TDist((length(Tabela.n))-1),1-alpha/2))*(sqrt.((((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))+
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./
    ((length(Tabela.n))*((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*
    (Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
    ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))+
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1)))))*
    (((first(unique((Tabela.n))))).-1)))))))/Conversor), (((N*((first(unique((Tabela.n)))))*(sum(Tabela.Média)/(length(Tabela.n))))+(N*((first(unique((Tabela.n))))).*
    ((quantile(TDist((length(Tabela.n))-1),1-alpha/2))*(sqrt.((((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))+
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./
    ((length(Tabela.n))*((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*
    (Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
    ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))+
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1)))))*
    (((first(unique((Tabela.n))))).-1)))))))/Conversor), area, (((quantile(TDist((length(Tabela.n))-1),1-alpha/2))*(sqrt.((((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1)))+(((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/
    (length(Tabela.n)))).^2)/((length(Tabela.n))-1)).-
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./((length(Tabela.n))*
    ((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/
    (length(Tabela.n)))).^2)/((length(Tabela.n))-1)).-
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
    ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))+
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))))*
    (((first(unique((Tabela.n))))).-1)))))/(sum(Tabela.Média)/(length(Tabela.n))))*100, (quantile(TDist((length(Tabela.n))-1),1-alpha/2))*(sqrt.((((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1)))+(((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/
    (length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./((length(Tabela.n))*
    ((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*
    (Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/((length(Tabela.n))-1)).-
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./
    ((first(unique((Tabela.n))))))/((((sum(((first(unique((Tabela.n))))).*
    (Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/((length(Tabela.n))-1)).-
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./
    ((first(unique((Tabela.n))))))+(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1)))))*(((first(unique((Tabela.n))))).-1)))), sqrt.((((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))+
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./((length(Tabela.n))*
    ((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/
    (length(Tabela.n)))).^2)/((length(Tabela.n))-1)).-
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1))))./
    ((first(unique((Tabela.n))))))/((((sum(((first(unique((Tabela.n))))).*
    (Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))+
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))))*
    (((first(unique((Tabela.n))))).-1))), sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)), 
    ((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))), (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))+
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))), (((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))+
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))./((length(Tabela.n))*
    ((first(unique((Tabela.n)))))))*(1 .+((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/
    (length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
    ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))+
    (sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))))*
    (((first(unique((Tabela.n))))).-1)), (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
    ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))+(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))), (((((quantile(TDist((length(Tabela.n))-1),1-alpha/2))^2)*((sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/
    ((length(Tabela.n))*(((first(unique((Tabela.n))))).-1)))+
    (((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n)))))))))./
    ((((0.1*(sum(Tabela.Média)/(length(Tabela.n))))).^2).*((first(unique((Tabela.n))))))).*(1 .+((((sum(((first(unique((Tabela.n))))).*
    (Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./((first(unique((Tabela.n))))))/
    ((((sum(((first(unique((Tabela.n))))).*(Tabela.Média.-(sum(Tabela.Média)/(length(Tabela.n)))).^2)/
    ((length(Tabela.n))-1)).-(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1))))./
    ((first(unique((Tabela.n))))))+(sum(Tabela.Variância.*(((first(unique((Tabela.n))))).-1))/((length(Tabela.n))*
    (((first(unique((Tabela.n))))).-1)))))*(((first(unique((Tabela.n))))).-1)), (0.1*(sum(Tabela.Média)/(length(Tabela.n)))), 
    length(Tabela.n), first((first(unique((Tabela.n))))), alpha, Observação]) #Tabela de resultados        
    XLSX.writetable(("F:/Version_09_07_21/iflorestal.jl/06.xlsx"), 
        Dados=(collect(DataFrames.eachcol(Dados)), DataFrames.names(Dados)), 
        Analise_descritiva=(collect(DataFrames.eachcol(Tabela)), DataFrames.names(Tabela)), 
        Resultados=(collect(DataFrames.eachcol(Resultados)), DataFrames.names(Resultados))) #Exportar para o Excel 
end
_________________________________________________________________________________________________________________________________________

#Processamento do inventário
#Importar dados
Dados = CSV.read("F:/Version_09_07_21/mult_inic_alea.csv", DataFrame) 
#Informações necessárias
#Área da população
const  area = 13000
const N = 6.500 
##Nível de significância (α)
const alpha = 0.05 
const EAR = 10 #Erro da amostragem requerido
#Unidade de medida da variável
Unidade = "m³/0.25 ha" #Alterar em função do inventário
#Conversor para a unidade de área por hectare
Área_da_parcela=0.25
Conversor=1/Área_da_parcela
#function Multiplos_inicios_aleatorios(Dados)
Multiplos_inicios_aleatorios(Dados) #Saída dos dados
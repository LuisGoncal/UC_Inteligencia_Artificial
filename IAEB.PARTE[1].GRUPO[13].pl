%Trabalho 1 Inteligência Artificial

:- set_prolog_flag( unknown,fail ).
:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).


:- op( 900,xfy,'::' ).

:- dynamic '-'/1.
:- dynamic avaliar/3.
:- dynamic comparar/4.
:- dynamic siL/2.
:- dynamic utente/6.
:- dynamic evolucao/1.
:- dynamic ato/8.
:- dynamic excecao/1.
:- dynamic marcador/7.
:- dynamic pertence/2.



% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).


% Extensao do meta-predicado si: Questao,Resposta -> {Verdadeiro,Falso,Desconhecido}

si( Questao,verdadeiro ) :-
    Questao.
si( Questao,falso ) :-
    -Questao.
si( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).
    

% Extensao do meta-predicado siL: [Questao],[Resposta] -> {Verdadeiro, Falso, Desconhecido}
siL([X|T1],[Y|T2]):-
    si(X,Y),
    siL(T1,T2).
siL([],[]).


% Extensao do meta-predicado siC: [Q | T], Resposta -> {Verdadeiro, Falso, Desconhecido}

siC([], verdadeiro).

siC([Q|T], verdadeiro) :- 
    si(Q, verdadeiro),
    siC(T, verdadeiro).

siC([Q|T], desconhecido) :-
    si(Q, desconhecido),
    nao( siC(T, falso)).

siC([Q|T], desconhecido) :-
    nao( si(Q, falso)),
    siC(T, desconhecido).

siC([Q|T], falso) :- 
    si(Q, falso).

siC([Q|T], falso) :-
    siC(T, falso).


% Extensao do meta-predicado siD: [Q | T], Resposta -> {verdadeiro, falso, desconhecido}

siD([], falso).

siD([Q|T], verdadeiro) :- 
    si(Q, verdadeiro).

siD([Q|T], verdadeiro) :-
    siD(T, verdadeiro).

siD([Q|T], desconhecido) :-
    si(Q, desconhecido),
    nao( siD(T, verdadeiro)).

siD([Q|T], desconhecido) :-
    nao( si(Q, verdadeiro)),
    siD(T, desconhecido).

siD([Q|T], falso) :- 
    si(Q, falso),
    siD(T, falso).



%Formalização do PMF ______________________________

-utente(I,N,D,M,A,G):-
    nao(utente(I,N,D,M,A,G)),
    nao(excecao(utente(I,N,D,M,A,G))).
-ato(Ida,D,Idu,I,C,Pu,Prd,Prs):-
    nao(ato(Ida,D,Idu,I,C,Pu,Prd,Prs)),
    nao(excecao(ato(Ida,D,Idu,I,C,Pu,Prd,Prs))).
-marcador(Idm,A,Imin,Imax,G,Vmin,Vmax):-
    nao(marcador(Idm,A,Imin,Imax,G,Vmin,Vmax)),
    nao(excecao(marcador(Idm,A,Imin,Imax,G,Vmin,Vmax))).


%Dados--------------------------------------------------------------------------------------------------


%Extensão do predicado utente: idUtente, Nome, Dia de nascimento, Mês de nascimento, Ano de nascimento, Género -> {V,F,D}

utente(123 456 780,antonio,30,06,1990,masculino).
utente(987 654 321,beatriz,30,07,1985,feminino).
utente(135 246 978,carlos,30,08,1987,masculino).
utente(598 792 742,abel,29,02,1969,masculino).
utente(100 101 110,doofenshmirtz,04,08,1975,masculino).
utente(123 456 789,josefina,11,11,2002,feminino).
utente(valor_desconhecido01,mateus,17,10,2001,masculino).
utente(178 900 100,valor_desconhecido02,12,03,1995,feminino).
utente(145 457 910,carolina,06,05,2003,valor_interdito).
-utente(156 345 781,manuela,09,09,1999,masculino).
utente(123 678 902,ines,12,11,2004,feminino).



%Extensão do predicado ato: IdAto, Data do ato, IdUtente, Idade, Colesterol, Pulsação, Pressão diastólica, Pressão sistólica -> {V,F,D}


ato(gmr5148,16/05/2019,100 101 110,47,140,70,65,133).
ato(gmr02067,30/06/2020,123 456 780,32,140,70,60,123).
ato(gmr2183,30/07/2021,987 654 321,37,190,60,70,142).
ato(gmr2297,30/06/2022,123 456 780,35,230,90,65,151).
ato(gmr3457,17/11/2022,123 456 789,20,150,70,70,111).
ato(gmr4200,14/12/2020,598 792 742,53,250,120,50,90).
ato(gmr6100,18/02/2021,987 654 321,36,valor_interdito,123,66,145).
-ato(gmr7207,09/12/2020,850 759 592,86,157,99,78,154).
ato(valor_desconhecido03,11/11/2022,123 678 902,18,135,80,68,134).


%Extensão do predicado marcador: Idmarcador, Analise, Idade Mínima, 
Idade Máxima, Género, Valor Normal Mínimo, Valor Nomral Máximo -> {V,F,D}


marcador(ctm01,colesterol,18,30,masculino,0,170).
marcador(ctf02,colesterol,18,30,feminino,0,160).
marcador(ctm03,colesterol,31,45,masculino,0,190).
marcador(ctf04,colesterol,31,45,feminino,0,180).
marcador(ctm05,colesterol,46,150,masculino,0,200).
marcador(ctf06,colesterol,46,150,feminino,0,195).
marcador(psm07,pulsacao,18,25,masculino,56,73).
marcador(psf08,pulsacao,18,25,feminino,61,78).
marcador(psm09,pulsacao,26,35,masculino,55,74).
marcador(psf10,pulsacao,26,35,feminino,60,76).
marcador(psm11,pulsacao,36,45,masculino,57,75).
marcador(psf12,pulsacao,36,45,feminino,60,78).
marcador(psm13,pulsacao,46,55,masculino,58,76).
marcador(psf14,pulsacao,46,55,feminino,61,77).
marcador(psm15,pulsacao,56,65,masculino,57,75).
marcador(psf16,pulsacao,56,65,feminino,60,77).
marcador(psm17,pulsacao,65,150,masculino,56,73).
marcador(psf18,pulsacao,65,150,feminino,60,76).
marcador(pam19,pressaoarterial,18,39,masculino,70,119).
marcador(paf20,pressaoarterial,18,39,feminino,68,110).
marcador(pam21,pressaoarterial,40,59,masculino,77,124).
marcador(paf22,pressaoarterial,40,59,feminino,74,122).
marcador(pam23,pressaoarterial,60,150,masculino,69,133).
marcador(paf24,pressaoarterial,60,150,feminino,69,139).


%Excecoes---------------------------------------------------------------------------------------------------------

excecao(utente(I, N, D, M, A, G)) :- utente(valor_desconhecido01, N, D, M, A, G).

excecao(utente(I,N,D,M,A,G)) :- utente(I,valor_desconhecido02,D,M,A,G).

excecao(utente(167 888 234,jose,23,04,1993,masculino)).

excecao(utente(167 888 234,carlos,23,04,1993,masculino)).

excecao(utente(I,N,D,M,A,G)):- utente(I,N,D,M,A,valor_interdito).

excecao(ato(Id_ato,Data_ato,Id_utente,Idade,C,P,Prd,Prs)):-
    ato(Id_ato,Data_ato,Id_utente,Idade,C,P,Prd,Prs).

excecao(ato(gmr6789, 15/12/2022, 123 456 789, 20, 70, 60, 70, 142)).

excecao(ato(gmr6789, 15/12/2022,123 456 789,20,100,60,70,142)).

excecao(ato(Id_ato,Data_ato,Id_utente,Idade,C,P,Prd,Prs)):-
    ato(Id_ato,Data_ato,Id_utente,Idade,valor_interdito,P,Prd,Prs).





%Predicados para avaliacao da normalidade dos resultados das analises-------------------------------------------------------------


%Extensão do predicado avaliar: IdUtente, DataAto, Análise -> {V,F,D}
    
avaliar(Idutente,Data,Analise):-
    utente(Idutente,N,D,M,A,G),
    ato(T,Data,Idutente,I,C,P,Prd,Prs),
    marcador(Id,Analise,Imin,Imax,G,Vmin,Vmax),
    Imin=<I,
    I=<Imax,
    comparar(Id,Analise,Idutente,T).


%Extensão do predicado comparar: IdMarcador, Análise, IdUtente, IdAto -> {V,F,D}

comparar(Id,pulsacao,Idutente,T):-
    ato(T,Data,Idutente,I,C,P,Prd,Prs),
    marcador(Id,pulsacao,Imin,Imax,G,Vmin,Vmax),
    Vmin=<P,
    P=<Vmax.

comparar(Id,colesterol,Idutente,T):-
    ato(T,Data,Idutente,I,C,P,Prd,Prs),
    marcador(Id,colesterol,Imin,Imax,G,Vmin,Vmax),
    Vmin=<C,
    C=<Vmax.

comparar(Id,pressaoarterial,Idutente,T):-
    ato(T,Data,Idutente,I,C,P,Prd,Prs),
    marcador(Id,pressaoarterial,Imin,Imax,G,Vmin,Vmax),
    Vmin*0.95=<Prd,
    Prd=<Vmin*1.05,
    Vmax*0.95=<Prs,
    Prs=<Vmax*1.05.



%Predicados auxiliares evolucao do conhecimento --------------------------------------------------------------------


%Extensão do predicado comprimento: Lista, Comprimento -> {V,F}

comprimento(S,N):- length(S,N).


%Extensão do predicado inserção: Termo -> {V,F}

insercao(Termo):- assert(Termo).
insercao(Termo):- retract(Termo),!,fail.

%Extensão do predicado remoção: Termo -> {V,F}

remocao(Termo):- retract(Termo).

%Extensão do predicado teste: Lista -> {V,F}

teste([]).
teste([R|LR]):- R,teste(LR). 


%Extensão do predicado removeTermos: ListaTermos -> {V,F}

removeTermos( [] ).
removeTermos( [X] ) :-
	retract(X).
removeTermos( [X|L] ) :-
	retract(X),
	removeTermos( L ).

%Extensão do predicado interdito: Valor Interdito -> {V,F}

interdito(valor_interdito).


%Extensão do predicado pertence: Valor, Lista -> {V,F}

pertence(X,[X|_]).
pertence(X,[_|T]):- pertence(X,T).


%Extensão do predicado is_list: Variável -> {V,F}

is_list(X) :-
        var(X), !,
        fail.
is_list([]).
is_list([_|T]) :-
        is_list(T).



%Evolucao do conhecimento ----------------------------------------------------------------------------------------


%Extensão do predicado evolucaoU: Termo -> {V,F}

evolucaoU(Termo):-
    is_list(Termo),
    verificaEvolucaoImpU(Termo).

evolucaoU(utente(I,N,D,M,A,G)):-
    is_list(I),
    verificaEvolucaoImpU(utente(I,N,D,M,A,G)).

evolucaoU(Termo):-
    findall(Invariante,+ Termo :: Invariante, Lista),
    insercao(Termo),
    verificaEvolucaoU(Termo),
    teste(Lista).

%Extensão do predicado verificaEvolucaoU: utente(IdUtente,Nome,Dia,Mês,Ano,Género) -> {V,F}

verificaEvolucaoU( utente(I,N,D,M,A,G) ) :-
    findall(B,utente(B,N,D,M,A,G),F),
    comprimento(F,R),
    R==1,
    findall(excecao(utente(I,No,D,M,A,G)),excecao(utente(I,Nome,D,M,A,G)),S),
    comprimento(S,K),
    K=0.
    
verificaEvolucaoU(utente(I,N,D,M,A,G)) :-
    findall(Nome,excecao(utente(I,Nome,D,M,A,G)),S),
    pertence(N,S),
    findall(excecao(utente(I,No,D,M,A,G)),excecao(utente(I,Nome,D,M,A,G)),W),
    removeTermos(W).

verificaEvolucaoU( utente(I,N,D,M,A,G) ) :-
	findall(B,utente(B,N,D,M,A,G),S2),
    comprimento(S2,Size),
    Size==2,
    removeTermos([utente(valor_desconhecido01,N,D,M,A,G)]).

verificaEvolucaoU(utente(I,N,D,M,A,G)) :-
    findall(X,utente(I,X,D,M,A,G),S),
    comprimento(S,Size2),
    Size2==2,
    removeTermos([utente(I,valor_desconhecido02,D,M,A,G)]).



%Extensão do predicado verificaEvolucaoImpU: Termo -> {V,F}

verificaEvolucaoImpU(utente([H,T],N,D,M,A,G)):-
    H =="-",
    findall(Invariante,+ utente(T,N,D,M,A,G) :: Invariante, Lista),
    findall(B,utente(B,N,D,M,A,G),S2),
    comprimento(S2,Size),
    Size==1,
    insercao(excecao(utente(X,N,D,M,A,G)):-X=<T),
    removeTermos([utente(valor_desconhecido01,N,D,M,A,G)]),
    teste(Lista),
    findall(Invariante,+ excecao(utente(T,N,D,M,A,G)) :: Invariante, Lista2),
    teste(Lista2).

verificaEvolucaoImpU(utente([H,T],N,D,M,A,G)):-
    T=="+",
    findall(Invariante,+ utente(H,N,D,M,A,G) :: Invariante, Lista),
    findall(B,utente(B,N,D,M,A,G),S2),
    comprimento(S2,Size),
    Size==1,
    removeTermos([utente(valor_desconhecido01,N,D,M,A,G)]),
    insercao(excecao(utente(X,N,D,M,A,G)):-X>=H),
    teste[Lista],
    findall(Invariante,+ excecao(utente(T,N,D,M,A,G)) :: Invariante, Lista2),
    teste(Lista2).

verificaEvolucaoImpU(utente([H,T],N,D,M,A,G)):-
    findall(Invariante,+ utente(H,N,D,M,A,G) :: Invariante, Lista),
    findall(Invariante,+ utente(T,N,D,M,A,G) :: Invariante, Lista2),
    verificaEvolucaoU(utente(H,N,D,M,A,G)),
    insercao(excecao(utente(X,N,D,M,A,G)):-(X>=H, X=<T)),
    findall(B,utente(B,N,D,M,A,G),S2),
    comprimento(S2,Size),
    Size==1,
    removeTermos([utente(valor_desconhecido01,N,D,M,A,G)]),
    teste(Lista),
    teste(Lista2),
    findall(Invariante,+ excecao(utente(H,N,D,M,A,G)) :: Invariante, Lista3),
    teste(Lista3).

verificaEvolucaoImpU([H|T]):-
    findall(Invariante,+ H :: Invariante, Lista),
    verificaEvolucaoU(H),
    insercao(excecao(H)),
    verificaEvolucaoImpU(T),
    teste(Lista),
    findall(Invariante,+ excecao(utente(H,N,D,M,A,G)) :: Invariante, Lista3),
    teste(Lista3).

verificaEvolucaoImpU([]).


%Extensão do predicado evolucaoA: Termo -> {V,F}

evolucaoA(Termo):-
    is_list(Termo),
    verificaEvolucaoImpA(Termo), 
    removeTermos([ato(valor_desconhecido03,D,Idu,I,C,Pu,Prd,Prs)]).

evolucaoA(Termo):-
    findall(Invariante,+Termo :: Invariante,Lista),
    insercao(Termo),
    verificaEvolucaoA(Termo),
    teste(Lista).


%Extensão do predicado verificaEvolucaoA: ato(IdAto,Data,IdUtente,Idade,Colesterol,Pulsação,Pressão Diastólica,Pressão Sistólica) -> {V,F}

verificaEvolucaoA(ato(Ida,D,Idu,I,C,Pu,Prd,Prs)):-
    findall(U,ato(U,D,Idu,I,C,Pu,Prd,Prs),J),
    comprimento(J,X),
    X==1,
    findall(excecao(ato(U,D,Idu,I,Co,Pu,Prd,Prs)),excecao(ato(U,D,Idu,I,Colesterol,Pu,Prd,Prs)),S),
    comprimento(S,K),
    K=0.

verificaEvolucaoA(ato(Ida,D,Idu,I,C,Pu,Prd,Prs)):-
    findall( B,excecao(ato(Ida,D,Idu,I,B,Pu,Prd,Prs)),S),
    pertence(C,S),
    findall(excecao(ato(Ida,D,Idu,I,B,Pu,Prd,Prs)),excecao(ato(Ida,D,Idu,I,B,Pu,Prd,Prs)),S2),
    removeTermos(S2).

verificaEvolucaoA(ato(Ida,D,Idu,I,C,Pu,Prd,Prs)):-
    findall(X,ato(X,D,Idu,I,C,Pu,Prd,Prs),S),
    comprimento(S,Size2),
    Size2==2,
    removeTermos([ato(valor_desconhecido03,D,Idu,I,C,Pu,Prd,Prs)]).
    

%Extensão do predicado verificaEvolucaoImpA: Termo -> {V,F}

verificaEvolucaoImpA([H|T]):-
    insercao(excecao(H)),
    findall(Invariante, + excecao(H) :: Invariante, Lista),
    teste(Lista),
    verificaEvolucaoImpA(T).

verificaEvolucaoImpA([]).

    
%Extensão do predicado evolucaoM: Termo -> {V,F}

evolucaoM(Termo):-
    findall(Invariante,+Termo :: Invariante,Lista), 
    insercao(Termo),
    verificaEvolucaoM(Termo),
    teste(Lista).

%Extensão do predicado verificaEvolucaoM : marcador(IdMarcador,Analise,IdadeMinima, IdadeMaxima, Genero, ValorMinimo, ValorMaximo) -> {V,F}

verificaEvolucaoM( marcador(Idm,A,Imin,Imax,G,Vmin,Vmax)) :-
	findall( B,marcador(B,A,Imin,Imax,G,Vmin,Vmax),S),
	comprimento( S,X ),
	X == 1.


%Extensão do predicado involucaoU: Termo -> {V,F}

involucaoU(Termo):-
    findall(Invariante, -Termo:: Invariante,Lista),
    remocao(Termo),
    teste(Lista).


%Extensão do predicado involucaoA: Termo ->{V,F}

involucaoA(Termo):-
    findall(Invariante, - Termo::Invariante,Lista),
    remocao(Termo),
    teste(Lista).


%Extensão do predicado involucaoM: Termo -> {V,F}

involucaoM(Termo):-
    findall(Invariante, - Termo::Invariante,Lista),
    remocao(Termo),
    teste(Lista).




%Invariantes------------------------------------------------------------------------------------------------------------

%Invariante que verifica que não se adiciona um utente com Id repetido
+utente(I,N,D,M,A,G)::(findall(I,(utente(I,Nome,Dia,Mes,Ano,Genero)),S),comprimento(S,Z),Z==1).

%Invariante que verifica que não se adiciona um ato com Id repetido
+ato(Ida,D,Idu,I,C,Pu,Prd,Prs)::(findall(Ida,(ato(Ida,Data,Idutente,Idade,Colesterol,Pulsacao,Pressaod,Pressaos)),S),comprimento(S,Z),Z==1).

%Invariante que verifica que não se adiciona um marcador com Id repetido
+marcador(Idm,A,Imin,Imax,G,Vmin,Vmax)::(findall(Idm,(marcador(Idm,Analise,Idademin,Idademax,Genero,Valormin,Valormax)),S),comprimento(S,Z),Z==1).

%Invariante que não permite que se insira conhecimento quando se tem conhecimento perfeito negativo oposto
+utente( I,N,D,M,A,G ) :: nao( -utente( I,N,D,M,A,G ) ).
+ato(Ida,D,Idu,I,C,Pu,Prd,Prs) :: nao(-ato(Ida,D,Idu,I,C,Pu,Prd,Prs)).
+marcador(Idm,Analise,Imin,Imax,G,Vmin,Vmax) :: nao(-marcador(Idm,Analise,Imin,Imax,G,Vmin,Vmax)).

%Invariante que não permite adicionar excecoes a conhecimento perfeito positivo
+excecao( utente( I,N,D,M,A,G ) ) :: nao( utente( I,N,D,M,A,G ) ).
+excecao(ato(Ida,D,Idu,I,C,Pu,Prd,Prs)) :: nao(ato(Ida,D,Idu,I,C,Pu,Prd,Prs)).
+excecao(marcador(Idm,Analise,Imin,Imax,G,Vmin,Vmax)) :: nao(marcador(Idm,Analise,Idmin,Idmax,G,Vmin,Vmax)).

%Invariante que não permite a evolucao do conhecimento interdito
+utente(I,N,D,M,A,G)::(findall(X,(utente(I,N,D,M,A,X),nao(interdito(X))),S),comprimento(S,L),L==1).
+ato(Ida,D,Idu,I,C,Pu,Prd,Prs)::(findall(X,(ato(Ida,D,Idu,I,X,Pu,Prd,Prs),nao(interdito(X))),S),comprimento(S,L),L==1).

%Invariante que não permite a remoção de utentes com atos associados
-utente(Idu,N,D,M,A,G)::(findall(X,ato(X,Data,Idu,I,C,Pu,Prd,Prs),J),comprimento(J,L),L==0).

%Invariante que não deixa inserir atos relacionados a um utente que não exita.
+ato(Ida,D,Idu,I,C,Pu,Prd,Prs)::(findall(Idu,utente(Idu,N,Dia,M,A,G),S),comprimento(S,L),L==1).

%Invariante que não deixa inserir exceções com Idato repetido
+excecao(ato(Ida,D,Idu,I,C,Pu,Prd,Prs))::(findall(Ida,(ato(Ida,Data,Idutente,Idade,Colesterol,Pulsacao,Pressaod,Pressaos)),S),comprimento(S,Z),Z==0).









    






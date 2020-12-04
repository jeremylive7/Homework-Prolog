


kaka(M,Z,R):-reverse(M,B),getTwo(B,Z),delete(B,K),reverse(K,R).
getTwo([X,Y|W],Z):-Z=[X,Y].
delete([X,Y|H],R):-R=H.



coca(M,N):-M==1,N=11.
coca(M,N):-M==2,N=22.




moved(ptm(izq,I,_,_,_,M,N,_),Carga):-M==2,N==1,getLower(I,Carga,R),N=0,I=R,!.
getLower(M,Z,R):-reverse(M,B),getTwo(B,Z),delete(B,K),reverse(K,R).
delete([X,Y|H],R):-R=H.


% findall(X,persona(Y,X),L),reverse(L,L2),member(L3,L2).
%  [(Alberto, 1),(Beatriz, 2),(Carlos, 5),(Dora, 10),(Emilio, 15)]

persona(alberto, 1).
persona(beatriz, 2).
persona(carlos, 5).
persona(dora, 10).
persona(emilio, 15).



insert_sort(List,Sorted):-i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([H|T],Acc,Sorted):-insert(H,Acc,NAcc),i_sort(T,NAcc,Sorted).
   
insert(X,[Y|T],[Y|NT]):-X>Y,insert(X,T,NT).
insert(X,[Y|T],[X,Y|T]):-X=<Y.
insert(X,[],[X]).



insert_sort2(List,Sorted):-i_sort2(List,[],Sorted).
i_sort2([],Acc,Acc).
i_sort2([(O,X)|T],Acc,Sorted):-insert2((O,X),Acc,NAcc),i_sort2(T,NAcc,Sorted).
   
insert2((O,X),[(P,Y)|T],[(P,Y)|NT]):-X>Y,insert2((O,X),T,NT).
insert2((O,X),[(P,Y)|T],[(O,X),(P,Y)|T]):-X=<Y.
insert2((O,X),[],[(O,X)]).



getMasRapido([(X,Z)|Xs],M) :- getMasRapido(Xs,Z,M).
getMasRapido([(X,Z)|Xs],Y,M) :- Y >= Z, getMasRapido(Xs,Y,M).
getMasRapido([(X,Z)|Xs],Y,M) :- Y < Z,  getMasRapido(Xs,Z,M).
getMasRapido([],M,M).



getMasLento([(X,Z)|Xs],M) :- getMasLento(Xs,Z,M).
getMasLento([(X,Z)|Xs],Y,M) :- Y >= Z, getMasLento(Xs,Y,M).
getMasLento([(X,Z)|Xs],Y,M) :- Y < Z,  getMasLento(Xs,Z,M).
getMasLento([],M,M).



select1(X,[X|Xs],Xs).
select1(X,[Y|Xs],[Y|Zs]) :- X \= Y, select1(X,Xs,Zs).

select2(X,[X|Xs],Xs).
select2(X,[Y|Xs],[Y|Zs]) :- select2(X,Xs,Zs).

select3(_,[],[]).
select3(X,[X|Xs],Zs)     :- select3(X,Xs,Zs).
select3(X,[Y|Xs],[Y|Zs]) :- X \= Y, select3(X,Xs,Zs).

select4(_,[],[]).
select4(X,[Y|Xs],[Y|Zs]) :- select4(X,Xs,Zs).
select4(X,[X|Xs],Zs)     :- select4(X,Xs,Zs).


reverse1([],[]).
reverse1([X|Xs],Z):-reverse1(Xs,Ys),append(Ys,[X],Z).

reverse2(Xs,Ys):-reverse2(Xs,[],Ys).
reverse2([X|Xs],Acc,Ys):-reverse2(Xs,[X|Acc],Ys).
reverse2([],Ys,Ys).

% mcd(X,Y,Z): Z es el divisor común máximo de los enteros X y Y
mcd(I,0,I).
mcd(I,J,Mcd) :- J>0, R is I mod J, mcd(J,R,Mcd).

% factorial1(N,F): F es el factorial del entero N
factorial1(N,F):- N > 0, N1 is N-1, factorial1(N1,F1), F is N*F1.
factorial1(0,1).

% factorial2(N,F): F es el factorial del entero N (con recursión de cola)
factorial2(N,F) :- factorial2(0,N,1,F).
factorial2(I,N,T,F) :- I < N,
                       I1 is I + 1,
                       T1 is T * I1,
                       factorial2(I1,N,T1,F).
factorial2(N,N,F,F).

% factorial3(N,F): F es el factorial del entero N(con otra forma de recursión de cola)
factorial3(N,F) :- factorial3(N,1,F).
factorial3(N,T,F) :- N > 0,
                     T1 is T * N,
                     N1 is N - 1,
                    factorial3(N1,T1,F).
factorial3(0,F,F).

%entre(I,J,K): K es un entero que está entre los enteros I y J, incluyéndolos
entre(I,J,I) :- J >= I.
entre(I,J,K) :- I < J,
                I1 is I + 1,
                entre(I1,J,K).

% sumarLista1(Lista,Suma): Suma es la suma de la lista de enteros Lista
sumarLista1([I|Is],Suma) :- sumarLista1(Is,SumaIs),
                            Suma is I + SumaIs.
sumarLista1([],0).

% sumarLista2(Lista,Suma):  Suma es la suma de la lista de enteros Lista (con recursión de cola)
sumarLista2(Lista,Suma) :- sumarLista2(Lista,0,Suma).
sumarLista2([I|Is],Acum,Suma) :-
                    Acum1 is Acum + I,
                    sumarLista2(Is,Acum1,Suma).
sumarLista2([],Suma,Suma).

%prodint1(Xs,Ys,Valor): Valor es el producto interno de los vectores representados por las listas de enteros Xs y Ys
prodint1([X|Xs],[Y|Ys],PI) :- prodint1(Xs,Ys,PI1),
                              PI is X*Y + PI1.
prodint1([],[],0).

% prodint2(Xs,Ys,Valor): Valor es el producto interno de los vectores representados por las listas de enteros Xs y Ys (con recursión de cola)
prodint2(Xs,Ys,PI) :- prodint2(Xs,Ys,0,PI).
prodint2([X|Xs],[Y|Ys],Acum,PI) :-
                             Acum1 is Acum + X * Y,
                             prodint2(Xs,Ys,Acum1,PI).
prodint2([],[],PI,PI).

% maximo(Xs,Max): Max es el máximo de una lista de enteros Xs.
maximo([X|Xs],M) :- maximo(Xs,X,M).
maximo([X|Xs],Y,M) :- Y >= X, maximo(Xs,Y,M).
maximo([X|Xs],Y,M) :- Y < X,  maximo(Xs,X,M).
maximo([],M,M).

%length1(Xs,N): N es la longitud de una lista Xs
length1([X|Xs],N) :- length1(Xs,N1), N is N1 + 1.
length1([],0).



% rango(M,,N,Ns): Ns es la lista de enteros entre M y N incluyéndolos
rango(M,N,[M|Ns]) :- M < N, M1 is M + 1, rango(M1,N,Ns).
rango(N,N,[N]).




move(ptm(izq,I,_,_,_,2,_,_),Carga):-moves(I,Carga).

moves([],[]).
moves([U|Cadena],Focus):- getTuplas(U,Cadena,Z),
                          moves(Cadena,Return),
                          fusion(Z,Return,Focus).

getTuplas(U,[],[]).
getTuplas(U,[N|Nodo],[(U,N)|Return]):- getTuplas(U,Nodo,(Return)).

fusion([],Tolerant,Tolerant).
fusion([W|Wubo],Tolerant,[W|Return]):- fusion(Wubo,Tolerant,Return).



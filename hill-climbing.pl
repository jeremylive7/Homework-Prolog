
solve_hill_climb(State,_,[]) :-
    final_state(State).

solve_hill_climb(State,History,[Move|Moves]) :-
    hill_climb(State,Move),      % generar una nueva Movida en el orden heurístico
    update(State,Move,State1),   % calcula nuevo estado usando Movida
    legal(State1),               % nuevo estado debe ser legal
    %insert_sort00(State1,State11),
    not(member(State11,History)), % debe ser primera vez que se llega al nuevo estado
    solve_hill_climb(State11,[State1|History],Moves).   % continuar a partir de nuevo estado

hill_climb(State,Move) :-
    findall(M,move(State,M),Moves),         % Encuentra todas las movidas posibles
    evaluate_and_order(Moves,State,[],MVs), % Evalúa con la heurística todas las movidas y las ordena.
    member((Move,_),MVs).                   % Escoge movidas en orden de heurística

evaluate_and_order([Move|Moves],State,MVs,OrderedMVs) :-
    insert_sort00(State,State0),
    update(State0,Move,State1),         % obtiene nuevo estado usando movida
    value(State1,Value),               % calcula el valor heurísico del nuevo estado
    insertPair((Move,Value),MVs,MVs1), % inserta en orden el par (movida,valor) en lista de movidas
    evaluate_and_order(Moves,State0,MVs1,OrderedMVs).  % procesa recursivamente el resto de movidas
    
evaluate_and_order([],_,MVs,MVs).

insertPair(MV,[],[MV]).
insertPair((M,V),[(M1,V1)|MVs],[(M,V),(M1,V1)|MVs]) :-
    V >= V1.
insertPair((M,V),[(M1,V1)|MVs],[(M1,V1)|MVs1]) :-
    V < V1,insertPair((M,V),MVs,MVs1).

test_hill_climb(Problem,Moves) :-
   initial_state(Problem,State),           % obtener un Estado inicial dado Problema
   solve_hill_climb(State,[State],Moves).  % inicia resolución desde Estado


%                        0          1      2     3           4           5     6
%Caso#1,3.. Parametros(Bote,Izquierda,Derecha,LimitTime,ActualTime,Cantidad,listaOriginal)
%initial_state(ptm,ptm(izq,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15)],[],28,0,2,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15)])).
%final_state(ptm(der,[],[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15)],28,28,2,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15)])).

%Caso#3.
initial_state(ptm,ptm(izq,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15), (julio, 20)],[],42,0,2,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15), (julio, 20)])).
final_state(ptm(der,[],[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15), (julio, 20)],42,42,2,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15), (julio, 20)])).




%Derecha a Izquierda simpre va a pasarse el primero, como esta ordenada la lista entonces es el mas rapido.
move(ptm(der,_,D,_,_,M,_),Carga):-M==2,select(Carga,D,C2).

%Izquierda a derecha.
%Tener caso para cuando necesito los dos mas lentos y los dos mas rapidos por medio de una bandera.
%agarro los dos mas rapidos, osea los dos primeros.
move(ptm(izq,I,_,_,_,M,O),Carga):-M==2,getFaster(I,Carga),esRapido(O,Carga),!.
getFaster(M,Z):-getTwo(M,Z).
getTwo([X,Y|W],Z):-Z=[X,Y].

%Caso final.
move(ptm(izq,[X,Y],_,_,_,M,O),Carga):-M==2,Carga=[X,Y],!.

%agarro los dos mas lentos, osea los dos que estan al final.
move(ptm(izq,I,_,_,_,M,O),Carga):-M==2,getLower(I,Carga),reverse(O,B),isSlow(B,Carga),!.
getLower(M,Z):-reverse(M,B),getTwoReverse(B,Z).
getTwoReverse([X,Y|W],Z):-Z=[Y,X].
isSlow([N,M|W],[X,Y]):-X==M,Y==N.

%agarro los mas lentos habiendo elimino los los mas lentos
move(ptm(izq,I,_,_,_,M,O),Carga):-M==2,getLower(I,Carga),reverse(O,B),delete(B,L),isSlow(L,Carga),!.




update(ptm(B,I,D,TT,TA,M,O),Carga,ptm(B1,I1,D1,TT,TAA,M,O)):-
      update_Bote(B,B1),
      update_margenes(Carga,B,I,D,I1,D1,TA,TAA,M,O).

update_Bote(izq,der).
update_Bote(der,izq).

update_margenes(Carga,der,I,D,I1,D1,TA,TAA,2,O):-
      select(Carga,D,D1),
      insert(Carga,I,I1,O),
      sumarTiempo(Carga,TA,TAA).

update_margenes(Carga,izq,I,D,I1,D1,TA,TAA,2,O):-
      selectTupla(Carga,I,I1),
      insertTupla(Carga,D,D1,O,I),
      sumarTiempoTupla(Carga,TA,TAA).



sumarTiempo((U,X),TA,TAA):-TAA is TA+X.
sumarTiempoTupla([X,(U,Y)],TA,TAA):-TAA is TA+Y.

select(X,[X|Xs],Xs).
select(X,[Y|Ys],[Y|Zs]):-select(X,Ys,Zs).

insert(X,[Y|Ys],[X,Y|Ys],O):-verdaderoTupla2(X,[Y|Ys],O).
insert(X,[Y|Ys],[Y|Zs],O):-verdaderoTupla2(X,Y,O),insert(X,Ys,Zs,O). 
insert(X,[],[X],O).

selectTupla([X,Z],[X,Z|Xs],Xs).
selectTupla([X,Z],[Y|Ys],[Y|Zs]):-selectTupla([X,Z],Ys,Zs).

insertTupla([X,Z],[Y|Ys],[X,Z,Y|Ys],O,I):-verdaderoTupla1([X,Z],[Y|Ys],O,I).
insertTupla([X,Z],[Y|Ys],[Y|Zs],O,I):-verdaderoTupla1([X,Z],[Y|Ys],O,I),insertTupla([X,Z],Ys,Zs,O,I).
insertTupla([X,Z],[],[X,Z],O,I).




%De izquierda a derecha
%Digo si la primera movida es valida, es la mas rapida?
verdaderoTupla1([X,Y],[],O,_):-esRapido(O,[X,Y]),!.
%Es cierto si la movida es la mas rapida
esRapido([N,M|W],[X,Y]):-X==N,Y==M.

%Digo si la tercera movida es valida, son los mas lentos?
verdaderoTupla1([X,Y],[_],O,_):-reverse(O,B),esLento(B,[X,Y]),!.
%Es cierto si la movida es la mas lenta
esLento([N,M|W],[X,Y]):-X==M,Y==N.

%Digo si la quinta movida es valida, son los mas rapidos los de la izq? y los de la derecha son los mas lentos?
verdaderoTupla1([X,Y],[N,M],O,_):-esRapido(O,[X,Y]),reverse(O,B),esLento(B,[N,M]),!.

%Caso final del caso 1 y 3
verdaderoTupla1([X,Y],_,_,[J,K]):-X==J,Y==K,!.

%Movida de cuando escojo los dos mas lentos pero habiendo pasado ya los primeros dos mas lentos.
verdaderoTupla1([X,Y],_,O,_):-esLower(O,V),esLento(V,[X,Y]),!.

esLower(M,V):-reverse(M,B),delete(B,V).

delete([X,Y|H],R):-R=H.

%De derecha a izquierda
verdaderoTupla2(X,_,O):-esElMasRapido1(O,X),!.
verdaderoTupla2(X,_,O):-esElMasRapido2(O,X),!.

esElMasRapido1([V,Y|W],X):-X==V.

esElMasRapido2([V,Y|W],X):-X==Y.

%verdaderoTupla1([X,Y,Z],_,_,[J,K,W]):-X==J,Y==K,!.



legal(ptm(izq,_,D,TT,TA,M,P)):-not(ilegal(TT,TA)).
legal(ptm(der,I,_,TT,TA,M,P)):-not(ilegal(TT,TA)).

ilegal(TT,TA):-TA>TT.




insert_sort00(ptm(izq,I,D,TT,TA,M,P),L):-insert_sort2(I,I2),L=ptm(izq,I2,D,TT,TA,M,P).
insert_sort00(ptm(der,I,D,TT,TA,M,P),L):-insert_sort2(D,D2),L=ptm(der,I,D2,TT,TA,M,P).

insert_sort2(List,Sorted):-i_sort2(List,[],Sorted).
i_sort2([],Acc,Acc).
i_sort2([(O,X)|T],Acc,Sorted):-insertar((O,X),Acc,NAcc),i_sort2(T,NAcc,Sorted).

insertar((O,X),[(P,Y)|T],[(P,Y)|NT]):-X>Y,insertar((O,X),T,NT).
insertar((O,X),[(P,Y)|T],[(O,X),(P,Y)|T]):-X=<Y.
insertar((O,X),[],[(O,X)]).





esElMasRapido([N,M|W],X):-X==M,!.

esMuyRapido([N,M|W],[X|H]):-X==N,!.
esMuyRapido([N,M|W],[X|H]):-X==M.


%Valido que sea mejor pasar de der a izq cuando hayan mas elementos al lado derecho y que los primeros dos sean los dos mas rapidos
value(ptm(der,_,[X,Y,_,_,_],_,_,2,O),7):-esRapido(O,[X,Y]),!.
value(ptm(der,_,[X,Y,_],_,_,2,O),5):-esRapido(O,[X,Y]),!.
value(ptm(der,_,[X,Y],_,_,2,O),3):-esRapido(O,[X,Y]),!.
%Valido que sean los dos mas rapidos del lado izq y que sea mejor que en el lado derecho haya uno o mas elementos.
value(ptm(izq,[X,Y],[V|M],_,_,2,O),7):-esRapido(O,[X,Y]),!.
value(ptm(izq,[X,Y|Z],[V],_,_,2,O),6):-esRapido(O,[X,Y]),!.
value(ptm(izq,[X,Y],[],_,_,2,O),5):-esRapido(O,[X,Y]),!.
%value(ptm(izq, [(alberto, 1),  (carlos, 5),  (dora, 10),  (emilio, 15)], [(beatriz, 2)], 28, 3, 2,
value(ptm(der,M,[X],_,_,_,2,_),1):-esElMasRapido(O,X),!.
value(ptm(der,M,[X|_],_,_,_,2,_),1):-esMuyRapido(O,X),!.


value(ptm(_,_,_,_,_,_,_,_),0).




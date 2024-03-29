
solve_best([punto(State,Path,_)|_],_,Moves) :-
    final_state(State),reverse(Path,Moves).
 
solve_best([punto(State,Path,_)|Frontier],History,FinalPath) :-
    findall(M,move(State,M),Moves),     % obtiene movidas del mejor estado
    updates(Moves,Path,State,States),   % obtiene los nuevos estados usando movidas
    legals(States,States1),             % escoge los nuevos estados que son legales
    news(States1,History,States2),      % elimina nuevos estados ya incluidos en historial
    evaluates(States2,Values),          % calcula valores heur�sticos de los nuevos estados
    inserts(Values,Frontier,Frontier1), % inserta en orden los nuevos puntos en la frontera
    solve_best(Frontier1,[State|History],FinalPath). % continuar a partir de nueva frontera

updates([M|Ms],Path,S,[(S2,[M|Path])|Ss]) :-
    update(S,M,S1),         % obtiene el estado al que se llega por una movida
    insert_sort00(S1,S2),
    updates(Ms,Path,S,Ss).  % procesa recursivamente las siguientes movidas
updates([],_,_,[]).

legals([(S,P)|States],[(S,P)|States1]) :-
    legal(S),
    legals(States,States1).
    
legals([(S,_)|States],States1) :-
    not(legal(S)),
    legals(States,States1).
    
legals([],[]).

news([(S,_)|States],History,States1) :-
    member(S,History),
    news(States,History,States1).

news([(S,P)|States],History,[(S,P)|States1]) :-
    not(member(S,History)),
    news(States,History,States1).
    
news([],_,[]).

evaluates([(S,P)|States],[punto(S,P,V)|Values]) :-
    value(S,V),                % calcula valor heur�stico del estado S
    evaluates(States,Values).  % procesa resto de estados
evaluates([],[]).

inserts([Punto|Puntos],Frontier,Frontier1) :-
    insertPoint(Punto,Frontier,Frontier0),  % inserta primer punto
    inserts(Puntos,Frontier0,Frontier1).    % recursivamente inserta los dem�s puntos
inserts([],Frontier,Frontier).

insertPoint(Point,[],[Point]).

insertPoint(Point,[Point1|Points],[Point1,Point|Points]) :-
    less_than(Point1,Point).

insertPoint(Point,[Point1|Points],[Point|Points]) :-
    equals(Point,Point1).

insertPoint(Point,[Point1|Points],[Point1|Points1]) :-
    less_than(Point,Point1),
    insertPoint(Point,Points,Points1).

insertPoint(Point,[Point1|Points],[Point,Point1|Points]) :-
    same(Point,Point1).

equals(punto(S,_,V),punto(S,_,V)).

less_than(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 < V2.

same(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 = V2.

test_best_search(Problem,Moves) :-
   initial_state(Problem,State),   % obtener un Estado inicial dado Problema
   value(State,Value),             % calcula el valor heur�stico del estado incial
   solve_best([punto(State,[],Value)],[State],Moves). % inicializa frontera e historial,
                                                      % inicia resoluci�n

%-------------------------------------------------------------------------------------------------------------------------------------------


%                        0          1      2     3           4           5     6
%Caso#1.. Parametros(Bote,Izquierda,Derecha,LimitTime,ActualTime,Cantidad,listaOriginal)
%initial_state(ptm,ptm(izq,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15)],[],28,0,2,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15)])).
%final_state(ptm(der,[],_,28,28,2,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15)])).

%Caso#3.
%initial_state(ptm,ptm(izq,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15), (julio, 20)],[],42,0,2,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15), (julio, 20)])).
%final_state(ptm(der,[],_,42,42,2,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15), (julio, 20)])).

%Caso#2.
initial_state(ptm,ptm(izq,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15)],[],21,0,3,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15)])).
final_state(ptm(der,[],_,21,21,3,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15)])).

%Caso#4.
%initial_state(ptm,ptm(izq,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15), (julio, 20)],[],30,0,3,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15), (julio, 20)])).
%final_state(ptm(der,[],_,30,30,3,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15), (julio, 20)])).


%Derecha a Izquierda simpre va a pasarse el primero, como esta ordenada la lista entonces es el mas rapido. Para todos los casos.
move(ptm(der,_,D,_,_,_,_),Carga):-select(Carga,D,C2),!.



%Izquierda a derecha.
%Caso final.
move(ptm(izq,[X,Y],_,_,_,M,O),Carga):-M==2,Carga=[X,Y],!.

%Tener caso para cuando necesito los dos mas lentos y los dos mas rapidos por medio de una bandera.
%agarro los dos mas rapidos, osea los dos primeros.
move(ptm(izq,I,_,_,_,M,O),Carga):-M==2,getFaster(I,Carga),esRapido(O,Carga),!.

%Obtengo los dos primeros
getFaster(M,Z):-getTwo(M,Z).
getTwo([X,Y|W],Z):-Z=[X,Y].

%Es cierto si la movida es la mas rapida
esRapido([N,M|W],[X,Y]):-X==N,Y==M.


%agarro los dos mas lentos, osea los dos que estan al final.
move(ptm(izq,I,_,_,_,M,O),Carga):-M==2,getLower(I,Carga),reverse(O,B),isSlow(B,Carga),!.

%Obtengo los dos mas lentos
getLower(M,Z):-reverse(M,B),getTwoReverse(B,Z).
getTwoReverse([X,Y|W],Z):-Z=[Y,X].
isSlow([N,M|W],[X,Y]):-X==M,Y==N.


%agarro los mas lentos habiendo elimino los dos mas lentos
move(ptm(izq,I,_,_,_,M,O),Carga):-M==2,getLower(I,Carga),reverse(O,B),delete(B,L),isSlow(L,Carga),!.

%Elimino los dos primeros
delete([X,Y|H],R):-R=H.




%Caso de tres elementos.
%Caso final cuando quedan tres.
move(ptm(izq,[X,Y,Z],_,_,_,M,O),Carga):-M==3,Carga=[X,Y,Z],!.

%Caso final cuando quedan dos.
move(ptm(izq,[X,Y],_,_,_,M,O),Carga):-M==3,Carga=[X,Y],!.

%Valida que sean los tres mas rapidos en base a la lista original.
move(ptm(izq,I,_,_,_,M,O),Carga):-M==3,getFaster2(I,Carga),esRapido2(O,Carga),!.

%Obtengo los tres mas rapidos
getFaster2(M,Z):-getThree(M,Z).
getThree([X,Y,W|M],Z):-Z=[X,Y,W].

%agarro los tres mas lentos, osea los tres que estan al final.
move(ptm(izq,I,_,_,_,M,O),Carga):-M==3,getLower2(I,Carga),reverse(O,B),isSlow2(B,Carga),!.

%Obtengo los tres mas lentos.
getLower2(M,Z):-reverse(M,B),getThreeReverse(B,Z).
getThreeReverse([X,Y,K|W],Z):-Z=[K,Y,X].
isSlow2([N,M,Z|W],[X,Y,K]):-X==Z,Y==M,K==N.

%agarro los mas lentos habiendo elimino los tres mas lentos
move(ptm(izq,I,_,_,_,M,O),Carga):-M==2,getLower2(I,Carga),reverse(O,B),delete2(B,L),isSlow2(L,Carga),!.

%Es cierto si la movida es la mas rapida
esRapido2([N,M,W|O],[X,Y,R]):-X==N,Y==M,W=R.

%Elimino los tres primeros
delete2([X,Y,Z|H],R):-R=H.



%Logica central de actualizar, se actualiza el estado.
update(ptm(B,I,D,TT,TA,M,O),Carga,ptm(B1,I1,D1,TT,TAA,M,O)):-
      update_Bote(B,B1),
      update_margenes(Carga,B,I,D,I1,D1,TA,TAA,M,O).

%Actualiza donde se encuentra el bote.
update_Bote(izq,der):-!.
update_Bote(der,izq).

%Casos en que pasen dos elementos
update_margenes(Carga,der,I,D,I1,D1,TA,TAA,2,O):-
      select(Carga,D,D1),
      insert(Carga,I,I1,O),
      sumarTiempo(Carga,TA,TAA),!.

update_margenes(Carga,izq,I,D,I1,D1,TA,TAA,2,O):-
      selectTupla(Carga,I,I1),
      insertTupla(Carga,D,D1,O,I),
      sumarTiempoTupla(Carga,TA,TAA).

%Casos en que pasen tres elementos
update_margenes(Carga,der,I,D,I1,D1,TA,TAA,3,O):-
      select(Carga,D,D1),
      insert(Carga,I,I1,O),
      sumarTiempo(Carga,TA,TAA),!.

update_margenes(Carga,izq,I,D,I1,D1,TA,TAA,3,O):-
      selectTripleta(Carga,I,I1),
      insertTripleta(Carga,D,D1,O,I),
      sumarTiempoTripleta(Carga,TA,TAA),!.

update_margenes(Carga,izq,[I0,I2],D,I1,D1,TA,TAA,3,O):-
      selectTupla(Carga,[I0,I2],I1),
      insertTripletaFinal(Carga,D,D1,O,[I0,I2]),
      sumarTiempoTupla(Carga,TA,TAA).




%Actualiza el tiempo actual cuando son dos elementos
sumarTiempo((U,X),TA,TAA):-TAA is TA+X.
sumarTiempoTupla([X,(U,Y)],TA,TAA):-TAA is TA+Y.

%Actualiza el tiempo actual cuando son tres elementos
sumarTiempoTripleta([X,U,(O,Y)],TA,TAA):-TAA is TA+Y.



%Caso general.Cuando pasa solo un elementos de derecha a izquierda
select(X,[X|Xs],Xs).
select(X,[Y|Ys],[Y|Zs]):-select(X,Ys,Zs).

insert(X,[Y|Ys],[X,Y|Ys],O):-verdaderoDerIzq(X,[Y|Ys],O).
insert(X,[Y|Ys],[Y|Zs],O):-verdaderoDerIzq(X,Y,O),insert(X,Ys,Zs,O). 
insert(X,[],[X],O).


%Casos en que pasen dos elementos.
selectTupla([X,Z],[X,Z|Xs],Xs).
selectTupla([X,Z],[Y|Ys],[Y|Zs]):-selectTupla([X,Z],Ys,Zs).

insertTupla([X,Z],[Y|Ys],[X,Z,Y|Ys],O,I):-verdaderoTupla([X,Z],[Y|Ys],O,I).
insertTupla([X,Z],[Y|Ys],[Y|Zs],O,I):-verdaderoTupla([X,Z],[Y|Ys],O,I),insertTupla([X,Z],Ys,Zs,O,I).
insertTupla([X,Z],[],[X,Z],O,I).


%Casos en que pasen tres elementos.
selectTripleta([X,Z,U],[X,Z,U|Xs],Xs).                        
selectTripleta([X,Z,U],[Y|Ys],[Y|Zs]):-selectTripleta([X,Z,U],Ys,Zs).   

insertTripleta([X,Z,U],[Y|Ys],[X,Z,U,Y|Ys],O,I):-verdaderoTupla([X,Z,U],[Y|Ys],O,I).
insertTripleta([X,Z,U],[Y|Ys],[Y|Zs],O,I):-verdaderoTupla([X,Z,U],[Y|Ys],O,I),insertTripleta([X,Z,U],Ys,Zs,O,I).  
insertTripleta([X,Z,U],[],[X,Z,U],O,I).                           

insertTripletaFinal([X,Z],[Y|Ys],[X,Z,Y|Ys],O,I):-verdaderoTupla([X,Z],[Y|Ys],O,I).
insertTripletaFinal([X,Z],[Y|Ys],[Y|Zs],O,I):-verdaderoTupla([X,Z],[Y|Ys],O,I),insertTripletaFinal([X,Z],Ys,Zs,O,I).  
insertTripletaFinal([X,Z],[],[X,Z],O,I).   



%Caso general. De derecha a izquierda
verdaderoDerIzq(X,_,O):-esElMasRapido1(O,X),!.

%Es el mas rapido de los dos mas rapidos.
esElMasRapido1([V,Y|W],X):-X==V,!.
esElMasRapido1([V,Y|W],X):-X==Y,!.



%De izquierda a derecha
%Caso final del caso 1, 3 y 4.
verdaderoTupla([X,Y],_,_,[J,K]):-X==J,Y==K,!.

%Cuando son tres elementos la movida. Caso final del caso 2
verdaderoTupla([X,Y,Z],_,_,[J,K,L]):-X==J,Y==K,Z==L,!.

%Cuando son tres elementos son los mas lentos? Cuando a la derecha hay dos elementos.
verdaderoTupla([X,Y,Z],[J,K],O,_):-reverse(O,B),esLento2(B,[X,Y,Z]),!.

%Digo si la primera movida es valida, es la mas rapida?
verdaderoTupla([X,Y],[],O,_):-esRapido(O,[X,Y]),!.

%Tercera movida valida
verdaderoTupla([X,Y],_,O,_):-reverse(O,B),esLento(B,[X,Y]),!.

%Digo si la quinta movida es valida, son los mas rapidos los de la izq? y los de la derecha son los mas lentos?
verdaderoTupla([X,Y],[N,M],O,_):-esRapido(O,[X,Y]),reverse(O,B),esLento(B,[N,M]),!.

%Movida de cuando escojo los dos mas lentos pero habiendo pasado ya los primeros dos mas lentos.
verdaderoTupla([X,Y],_,O,_):-esLower(O,V),esLento(V,[X,Y]),!.

%Hace un actualizar a la lista con la que se va a verificar para saber si son los mas lentos.
esLower(M,V):-reverse(M,B),delete(B,V).

%Es cierto si la movida es la mas lenta
esLento([N,M|W],[X,Y]):-X==M,Y==N.

%Valida que sean los tres mas lentos.
esLento2([N,M,K|W],[X,Y,Z]):-X==K,Y==M,Z==N.




%Verifica que el tiempo actaul no se salga de rango del tiempo estimado.
legal(ptm(izq,_,D,TT,TA,M,P)):-not(ilegal(TT,TA)).
legal(ptm(der,I,_,TT,TA,M,P)):-not(ilegal(TT,TA)).

ilegal(TT,TA):-TA>TT.



%Ordeno el estado de menor a mayor, osea del mas rapido al mas lento.
insert_sort00(ptm(izq,I,D,TT,TA,M,P),L):-insert_sort2(I,I2),L=ptm(izq,I2,D,TT,TA,M,P).
insert_sort00(ptm(der,I,D,TT,TA,M,P),L):-insert_sort2(D,D2),L=ptm(der,I,D2,TT,TA,M,P).

insert_sort2(List,Sorted):-i_sort2(List,[],Sorted).
i_sort2([],Acc,Acc).
i_sort2([(O,X)|T],Acc,Sorted):-insertar((O,X),Acc,NAcc),i_sort2(T,NAcc,Sorted).

insertar((O,X),[(P,Y)|T],[(P,Y)|NT]):-X>Y,insertar((O,X),T,NT).
insertar((O,X),[(P,Y)|T],[(O,X),(P,Y)|T]):-X=<Y.
insertar((O,X),[],[(O,X)]).





%Clausulas que se usan en value
esElMasRapido([N,M|W],X):-X==M,!.

esMuyRapido([N,M|W],[X|H]):-X==N,!.
esMuyRapido([N,M|W],[X|H]):-X==M.

esElMasRapido2([N,M|W],X):-X==N,!.
esElMasRapido2([N,M|W],X):-X==M,!.

noEsElMasRapido2([N,M|W],X):-X\=N,!.
noEsElMasRapido2([N,M|W],X):-X\=M,!.


%Caso de parada.
value(ptm(der,[],_,_,_,_,_),10):-!.

%Valido que sea mejor pasar de der a izq cuando hayan mas elementos al lado derecho y que los primeros dos sean los dos mas rapidos
value(ptm(der,_,[X,_,_,_,_],_,_,_,O),7):-esElMasRapido2(O,X),!.
value(ptm(der,_,[X,Y,_,_],_,_,_,O),7):-esRapido(O,[X,Y]),!.
value(ptm(der,_,[X,_,_],_,_,_,O),5):-esElMasRapido2(O,X),!.
value(ptm(der,_,[X,Y],_,_,_,O),3):-esRapido(O,[X,Y]),!.

%Valido que sean los dos mas rapidos del lado izq y que sea mejor que en el lado derecho haya uno o mas elementos.
value(ptm(izq,[X,Y|_],[V|_],_,_,_,O),7):-esRapido(O,[X,Y]),!.
value(ptm(izq,[X|_],_,_,_,_,O),7):-esElMasRapido2(O,X),!.
value(ptm(izq,[X,Y|_],[],_,_,_,O),5):-esRapido(O,[X,Y]),!.
value(ptm(izq,[X,Y|_],[_],_,_,_,O),1):-esRapido(O,[X,Y]),!.

%Valida que sean del lado derecho el primero el rapido o los dos primeros los dos mas rapidos.
value(ptm(izq,_,[V],_,_,_,O),7):-esElMasRapido2(O,V),!.
value(ptm(izq,_,[V,M|_],_,_,_,O),7):-esElMasRapido2(O,V),noEsElMasRapido2(O,M),!.
value(ptm(izq,_,[X,Y|_],_,_,_,O),1):-esRapido(O,[X,Y]),!.
value(ptm(izq,_,[X,Y],_,_,_,O),1):-esRapido(O,[X,Y]),!.

%Ejm: value(ptm(izq, [(alberto, 1),  (carlos, 5),  (dora, 10),  (emilio, 15)], [(beatriz, 2)], 28, 3, 2..
value(ptm(der,_,[X],_,_,_,_,_),1):-esElMasRapido(O,X),!.
value(ptm(der,_,[X|_],_,_,_,_,_),1):-esMuyRapido(O,X),!.

%Valida que es ilogico que mueva al mas rapido al lado izquierdo habiendo ya completado todo.
value(ptm(izq,[X],[Y|_],_,_,_,O),1):-esElMasRapido2(O,[X,Y]),!.

%Ver cuando solo tengo elementos justos en el lado derecho para mover de una vez
value(ptm(der,[],[X|Xs],_,_,_,O),0):-!.

%Caso por defecto.
value(ptm(_,_,_,_,_,_,_,_),0).

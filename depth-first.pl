
solve_dfs(Estado,_,[]) :- final_state(Estado).

solve_dfs(Estado,Historia,[Movida|Movidas]) :-
      move(Estado,Movida),
      update(Estado,Movida,Estado2),
      legal(Estado2),
      insert_sort00(Estado2,Estado22),
      not(member(Estado22,Historia)),
      solve_dfs(Estado22,[Estado22|Historia],Movidas).

test_dfs(Problema,Movidas) :-
      initial_state(Problema,Estado),
      solve_dfs(Estado,[Estado],Movidas).
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
%Caso final del caso 1, 3.
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


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


solve_dfs(Estado,_,[]) :- final_state(Estado).

solve_dfs(Estado,Historia,[Movida|Movidas]) :-
      move(Estado,Movida),               % generar una nueva Movida
      update(Estado,Movida,Estado2),     % calcula nuevo estado usando Movida
      legal(Estado2),                    % nuevo estado debe ser legal
      insert_sort0(Estado2,Estado22),
      not(member(Estado22,Historia)),     % debe ser primera vez que se llega al nuevo estado
      solve_dfs(Estado22,[Estado22|Historia],Movidas).   % continuar a partir de nuevo estado

test_dfs(Problema,Movidas) :-
      initial_state(Problema,Estado),      % obtener un Estado inicial dado Problema
      solve_dfs(Estado,[Estado],Movidas).  % inicia resolución desde Estado



initial_state(ptm,ptm(izq,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15), (julio,20)],[],30,0)).
final_state(ptm(der,[],[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15), (julio,20)],30,30)).

move(ptm(izq,[I1,I2],_,_,_),Carga):-Carga=[I1,I2],!.
move(ptm(izq,I,_,_,_),Carga):-select2(Carga,I,C2).
move(ptm(der,_,D,_,_),Carga):-select(Carga,D,C2).
move(ptm(_,_,_,_,_),solo).

update(ptm(B,I,D,TT,TA),Carga,ptm(B1,I1,D1,TT,TAA)):-
      update_Bote(B,B1),                   
      update_margenes(Carga,B,I,D,I1,D1,TA,TAA).   

update_Bote(izq,der).
update_Bote(der,izq).

update_margenes(Carga,izq,[I1,I2],D,I11,D1,TA,TAA):-
      select22(Carga,[I1,I2],I11),       
      insert22(Carga,D,D1),        
      sumarTiempo22(Carga,TA,TAA),!.

sumarTiempo22([X,(O,Y)],TA,TAA):-TAA is TA+Y.

update_margenes(Carga,izq,I,D,I1,D1,TA,TAA):-
      select2(Carga,I,I1),       
      insert2(Carga,D,D1),        
      sumarTiempo2(Carga,TA,TAA).

sumarTiempo2([X,U,(O,Y)],TA,TAA):-TAA is TA+Y.

update_margenes(Carga,der,I,D,I1,D1,TA,TAA):-
      select(Carga,D,D1),    
      insert(Carga,I,I1),      
      sumarTiempo(Carga,TA,TAA).

sumarTiempo((U,X),TA,TAA):-TAA is TA+X.

insert(X,[Y|Ys],[X,Y|Ys]):-precedes(X,[Y|Ys]).
insert(X,[Y|Ys],[Y|Zs]):-precedes(X,Y),insert(X,Ys,Zs). 
insert(X,[],[X]).                        

insert2([X,Z,U],[Y|Ys],[X,Z,U,Y|Ys]):-precedes2([X,Z,U],[Y|Ys]).
insert2([X,Z,U],[Y|Ys],[Y|Zs]):-precedes2([X,Z,U],[Y|Ys]),insert2([X,Z,U],Ys,Zs).  
insert2([X,Z,U],[],[X,Z,U]).                           

insert22([X,Z],[Y|Ys],[X,Z,Y|Ys]):-precedes2([X,Z],[Y|Ys]).
insert22([X,Z],[Y|Ys],[Y|Zs]):-precedes2([X,Z],[Y|Ys]),insert2([X,Z],Ys,Zs).  
insert22([X,Z],[],[X,Z]).     

select(X,[X|Xs],Xs).                         
select(X,[Y|Ys],[Y|Zs]):-select(X,Ys,Zs).     

select2([X,Z,U],[X,Z,U|Xs],Xs).                        
select2([X,Z,U],[Y|Ys],[Y|Zs]):-select2([X,Z,U],Ys,Zs).                       

select22([X,Z],[X,Z|Xs],Xs).                        
select22([X,Z],[Y|Ys],[Y|Zs]):-select2([X,Z],Ys,Zs).    

precedes((alberto,1),[(dora,10),(emilio,15),(julio,20)]).
precedes((beatriz,2),[(alberto,1)]).
precedes2([(alberto,1),(beatriz,2),(carlos,5)],[]).
precedes2([(dora,10),(emilio,15),(julio,20)],[(beatriz,2),(carlos,5)]).
precedes2([(alberto,1),(beatriz,2)],[(carlos,5),(dora,10),(emilio,15),(julio,20)]).

legal(ptm(izq,_,D,TT,TA)):-not(ilegal(TT,TA)). 
legal(ptm(der,I,_,TT,TA)):-not(ilegal(TT,TA)).

ilegal(TT,TA):-TA>TT.

insert_sort0(ptm(izq,I,D,TT,TA),L):-insert_sort2(I,I2),L=ptm(izq,I2,D,TT,TA).
insert_sort0(ptm(der,I,D,TT,TA),L):-insert_sort2(D,D2),L=ptm(der,I,D2,TT,TA).

insert_sort2(List,Sorted):-i_sort2(List,[],Sorted).
i_sort2([],Acc,Acc).
i_sort2([(O,X)|T],Acc,Sorted):-insert22((O,X),Acc,NAcc),i_sort2(T,NAcc,Sorted).
   
insert22((O,X),[(P,Y)|T],[(P,Y)|NT]):-X>Y,insert22((O,X),T,NT).
insert22((O,X),[(P,Y)|T],[(O,X),(P,Y)|T]):-X=<Y.
insert22((O,X),[],[(O,X)]).

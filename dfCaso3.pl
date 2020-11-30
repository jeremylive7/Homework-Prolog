
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


initial_state(ptm,ptm(izq,[1,2,5,10,15,20],[],42,0)).
final_state(ptm(der,[],[1,2,5,10,15,20],42,42)).

move(ptm(izq,I,_,_,_),Carga):-member(Carga1,I),member(Carga2,I),Carga1\=Carga2,Carga=[Carga1,Carga2].
move(ptm(der,_,D,_,_),Carga):-select(Carga,D,C2).
move(ptm(_,_,_,_,_),solo).

update(ptm(B,I,D,TT,TA),Carga,ptm(B1,I1,D1,TT,TAA)):-
      update_Bote(B,B1),                   
      update_margenes(Carga,B,I,D,I1,D1,TA,TAA).   

update_Bote(izq,der).
update_Bote(der,izq).

update_margenes(Carga,izq,I,D,I1,D1,TA,TAA):-
      select2(Carga,I,I1),       
      insert2(Carga,D,D1),        
      sumarTiempo2(Carga,TA,TAA).

sumarTiempo2([X,Y],TA,TAA):-TAA is TA+Y.

update_margenes(Carga,der,I,D,I1,D1,TA,TAA):-
      select(Carga,D,D1),    
      insert(Carga,I,I1),      
      sumarTiempo(Carga,TA,TAA).

sumarTiempo(X,TA,TAA):-TAA is TA+X.

insert(X,[Y|Ys],[X,Y|Ys]):-precedes(X,[Y|Ys]).
insert(X,[Y|Ys],[Y|Zs]):-precedes(X,Y),insert(X,Ys,Zs). 
insert(X,[],[X]).                        

insert2([X,Z],[Y|Ys],[X,Z,Y|Ys]):-precedes2([X,Z],[Y|Ys]).
insert2([X,Z],[Y|Ys],[Y|Zs]):-precedes2([X,Z],[Y|Ys]),insert2([X,Z],Ys,Zs).  
insert2([X,Z],[],[X,Z]).                           

select(X,[X|Xs],Xs).                         
select(X,[Y|Ys],[Y|Zs]):-select(X,Ys,Zs).     

select2([X,Z],[X,Z|Xs],Xs).                        
select2([X,Z],[Y|Ys],[Y|Zs]):-select2([X,Z],Ys,Zs).    

precedes(1,[5,10,15,20]).
precedes(2,[1,5,10]).
precedes(1,[5,10]).
precedes(2,[1]).
precedes2([1,2],[]).
precedes2([15,20],[2]).
precedes2([1,2],[15,20]).
precedes2([5,10],[2,15,20]).
precedes2([1,2],[5,10,15,20]).

legal(ptm(izq,_,D,TT,TA)):-not(ilegal(TT,TA)). 
legal(ptm(der,I,_,TT,TA)):-not(ilegal(TT,TA)).

ilegal(TT,TA):-TA>TT.

insert_sort0(ptm(izq,I,D,TT,TA),L):-insert_sort(I,I2),L=ptm(izq,I2,D,TT,TA).
insert_sort0(ptm(der,I,D,TT,TA),L):-insert_sort(D,D2),L=ptm(der,I,D2,TT,TA).

insert_sort(List,Sorted):-i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([H|T],Acc,Sorted):-insert00(H,Acc,NAcc),i_sort(T,NAcc,Sorted).
   
insert00(X,[Y|T],[Y|NT]):-X>Y,insert00(X,T,NT).
insert00(X,[Y|T],[X,Y|T]):-X=<Y.
insert00(X,[],[X]).

%precedes([5,10,15],[1,2]).
%precedes([1,5,10,15],[2]).
%precedes([1,5],[2,10,15]).
%precedes([],[1,2,5,10,15]).

%update_margenes(solo,_,I,D,I,D,_,_).

%initial_state(ptm,ptm(izq,[1,2,5,10,15,20],[],42,0)).

%final_state(ptm(der,[],[1,2,5,10,15,20],42,42)).
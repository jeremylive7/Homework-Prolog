/*
 *  Estructura de datos
 */
 estructura1(
    persona(Alberto, 1),
    persona(Beatriz, 2),
    persona(Carlos, 5),
    persona(Dora, 10),
    persona(Emilio, 15)
    ).

/*
 *  Ejemplo de obtener el mas rapido
 */
getmax1([(X,Y)|Xs],M) :- getmax1(Xs,X,M).

/*
 *  Ejemplo de obtener los dos mas lentos
 */



 /*
 *  Ejemplo de obtener los dos mas rapidos
 */

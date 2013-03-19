%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Proyecto 2. Prolog - Sopa de Letras
%% Realizado por:
%% Oriana Gomez 09-10336
%% Carla Urrea 09-11215
%% Fecha: 22/03/13
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% CARGAR LISTA PALABRA

% Predicado para cargar las listas de palabras

cargarListaPalabra(Archivo, Alfabeto,Palabras):-
  see(Archivo),
  read(Palabras),
  seen,
  transformaEnAscii(Palabras,Alfabeto),
  !.

% Predicado para llevar las listas y el alfabeto a Ascii

transformaEnAscii([],_).

transformaEnAscii([C|R], Alf):-
  atom_codes(T,Alf),
  atom_codes(T,AlfabetoAscii),
  atom_chars(C,Temp),
  atom_codes(X,Temp),
  atom_codes(X,CabezaAscii),
  comparar(CabezaAscii,AlfabetoAscii),
  transformaEnAscii(R,Alf),
  !.

% Predicado para comparar que las palabras estan formadas por
% el alfabeto

comparar([],_).

comparar([C|R],Alf):-
  member(C,Alf),
  comparar(R,Alf),
  !.

%% SOPA LETRA

% Predicado que devuelve todas las posibles sopas de letras

%sopaLetra(Alfabeto,Tam,Aceptadas,Rechazadas):-

% Predicado que genera las filas en la sopa de letras

generarFilas(Alfabeto,N,Fila):-
  L = [],
  agregarElem(Alfabeto,L,N,ListaFila),
  Fila = ListaFila.

% Predicado que agrega los elementos a una fila

agregarElem(_,L,N,ListaFila):-
  length(L,Long),
  Long >= N,
  L = ListaFila,
  !.
  
agregarElem(Alfabeto,L,N,ListaFila):-
  randomElem(Alfabeto,Elem),
  write('elemento  '),write(Elem),nl,
  write('lista  '),write(L),nl,
  Fila = [Elem|L],
  write('fila  '),write(Fila),nl,
  agregarElem(Alfabeto,Fila,N,ListaFila).
  
% Predicado que reemplaza el elemento en una posicion de la fila
% por otro

reemplazar([_ | Resto], 0, Elem, [Elem | Resto]).
reemplazar([C | R], Pos, Elem, [C | F]) :-
  Pos > 0,
  Pos1 is Pos - 1,
  reemplazar(R, Pos1, Elem, F),
  !.

% Predicado que reemplaza todos los elementos requeridos e una fila dada

reemplazarEnFila([], FilaTmp, _, NuevaFila):-
  FilaTmp = NuevaFila.

reemplazarEnFila([PrimeraLetra|RestoLetras], Fila, Pos, NuevaFila) :-
  Pos >= 0,
  length(Fila, N), 
  Pos < N,
  reemplazar(Fila, Pos, PrimeraLetra, FilaTmp),                             
  Pos1 is Pos + 1,
  reemplazarEnFila(RestoLetras, FilaTmp, Pos1, NuevaFila),
  !.

% Predicado que añade las palabras en forma horizontal 

addHorizontal(Palabra, Rechazadas, SopaLetras, Result) :-
  length(SopaLetras, Tam),
  random(0, Tam, PosFila),                             %Obtengo las posicion donde se agregara la palabra
  random(0, Tam, PosCol),
  nth0(PosFila, SopaLetras, Fila),                         %Obtengo la Sublista (Fila) en la que agregare la palabra
  atom_chars(Palabra, Letras),               %Guardo la palabra como una lista de atomos en Tmp
  length(Letras, Long),                              %Obtengo la longitud de la palabra
  Long =< Tam,                                       %Verifico que la palabra quepa en el tablero
  reemplazarEnFila(Letras, Fila, PosCol, FilaNueva),
  reemplazar(SopaLetras,PosFila,FilaNueva,SopaLetrasAct),
  Result = SopaLetrasAct.
  
%% Predicado que genera un elemento random del alfabeto

randomElem([], []).
randomElem(Lista, Elem) :-
  length(Lista, Long),
  random(0, Long, Pos),
  nth0(Pos, Lista, Elem).
  
%% MOSTRAR SOPA.
  
% Predicado que muestra las sopas de letra en pantalla

mostrarSopa([C|R]):-
  separar(C),
  mostrarSopa(R),
  !.

mostrarSopa([]).

% Predicado que imprime las lineas de la sopa de letras

separar([C|R]):-
  write(C),
  write(' '),
  separar(R),
  !.

separar([]):- 
  nl.

separar(A):-
  write(A),
  nl.

%% GENERADOR SOPA

% Predicado Principal: generadorSopa

generadorSopa:-
  write('GENERADOR DE SOPAS DE LETRAS'),
  nl,nl,
  write('Nota: Debe terminar con punto todo lo que ingrese por consola.'),
  nl,nl,
  write('Indique el tamaño de la sopa de letras:'),
  nl,
  read(Tam),
  write('Indique el afabeto a utilizar:'),
  nl,
  read(Alfabeto),
  write('Indique el nombre de archivo de palabras aceptadas:'),
  nl,
  read(ArchivoA),
  write('Indique el nombre de archivo de palabras rechazadas:'),
  nl,
  read(ArchivoR),
  cargarListaPalabra(ArchivoA,Alfabeto,Aceptadas),
  cargarListaPalabra(ArchivoR,Alfabeto,Rechazadas),
  write(Aceptadas),
  write(Rechazadas).
  %sopaLetra(Alfabeto,Tam,Aceptadas,Rechazadas).
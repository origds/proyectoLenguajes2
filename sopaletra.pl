%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Proyecto 2 - Sopa de Letras
%% Realizado por:
%% Oriana Gomez 09-10336
%% Carla Urrea 09-11215
%% Fecha: 22/03/13
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Funcion para cargar las listas de palabras

cargarListaPalabra(Archivo, Alfabeto):-
see(Archivo),
read(Lista),
seen,
transformaEnAscii(Lista,Alfabeto).

% Funcion auxiliar para llevar las listas y el alfabeto a Ascii

transformaEnAscii([],_).

transformaEnAscii([C|R], Alf):-
atom_codes(Temp,Alf),
name(Temp, AlfabetoAscii),
name(C, CabezaAscii),
comparar(CabezaAscii,AlfabetoAscii),
transformaEnAscii(R,Alf).

% Funcion para comparar que las palabras estan formadas por
% el alfabeto

comparar([],_).

comparar([C|R],Alf):-
member(C,Alf),
comparar(R,Alf).


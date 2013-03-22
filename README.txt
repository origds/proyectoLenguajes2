Universidad Simon Bolivar
Deparamento de Computacion y Tecnologia de la Informacion
Laboratorio de Lenguajes I CI-3661

Proyecto 2. Prolog

Autores.
  Carla Urrea 09-11215
  Oriana Gomez 09-10336

El proyecto funciona en su totalidad permitiendo generar todas las sopas de letras
posibles para un determinado tamano, palabras aceptadas y palabras rechazadas. 

La implementacion del proyecto esta hecha de manera tal que en un principio se tiene
una sopa de letras llena de simbolos de dolar ($), la cual es pasada como argumento
en los predicados encargados de verificar que las palabras aceptadas puedan estar en
la sopa de letras. La posicion en la que las palabras se agregan, se van generando en 
el orden siguiente: Horizontal, HorizontalReverse (verificar que la palabra pueda estar
en la sopa de letras pero escrita alreves), Vertical, VerticalReverse, Diagonal, 
DiagonalReverse, DiagonalInv (verificar que la palabra este en la diagonal secundaria 
de la 'matriz') y DiagonalInvReverse. La llamada a estos predicados se va haciendo 
gracias al backtracking de Prolog. 




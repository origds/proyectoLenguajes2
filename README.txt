Universidad Simon Bolivar
Deparamento de Computacion y Tecnologia de la Informacion
Laboratorio de Lenguajes I CI-3661

Proyecto 2. Prolog. Sopa de Letras

Autores.
  Carla Urrea 09-11215
  Oriana Gomez 09-10336

El proyecto funciona en su totalidad permitiendo generar todas las sopas de letras
posibles para un determinado tamaño, palabras aceptadas y palabras rechazadas. 

La implementación del proyecto esta hecha de manera tal que en un principio se tiene
una sopa de letras llena de símbolos de dolar ($), que sabemos que por ser un caracter
especial, no estara dentro del alfabeto. La sopa de letras es pasada como argumento en
los predicados encargados de verificar que las palabras aceptadas puedan estar en
la misma. La posición en la que las palabras se agregan, se van generando en el orden
siguiente: Horizontal, HorizontalReverse (verificar que la palabra pueda estar en la 
sopa de letras pero escrita alreves), Vertical, VerticalReverse, Diagonal, 
DiagonalReverse, DiagonalInv (verificar que la palabra este en la diagonal secundaria 
de la 'matriz') y DiagonalInvReverse. La llamada a estos predicados se va haciendo 
gracias al backtracking de Prolog. Una vez que se generan las posibilidades, se 
rellenan aleatoriamente los espacios faltantes con los átomos del alfabeto, y finalmente
se chequea por backtracking si alguna de las palabras rechazadas se generó al terminar 
de llenar la matriz, de ser asi entonces la sopa de letras se descarta y se genera 
la siguiente, solo se muestran aquellas sopas que no tienen las palabras rechazadas.




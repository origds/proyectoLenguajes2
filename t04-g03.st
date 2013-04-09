"Tarea 4. Smalltalk"
"Realizado por: Oriana Gomez 09-10336
                Carla Urrea 09-11215"
"Fecha de modificacion: 08/04/13"

"Implementacion. Parte 1."

"Pregunta 1. Extienda OrderedCollection, tal que toda instancia
de la clase responda al mensaje subCollectionFrom:to:"

OrderedCollection extend [
    subCollectionFrom: ini to: fin [
      |nuevo i|
      i:= ini.
      nuevo:= OrderedCollection new.
      ini to: fin do: [:x | nuevo addLast: (self at: x)].
      ^nuevo 
    ]
]

"Pregunta 2. Extienda OrderedCollection, tal que toda instancia
de la clase responda al mensaje filter:"

OrderedCollection extend [
  filter: condicion [
    | aux i nuevo tam seCumple|
    aux := OrderedCollection new.
    tam := self size.
    i := 1.
    [i <= tam] whileTrue: [
      seCumple := condicion value: (self at: i).      
      seCumple ifTrue: [
        aux addLast: (self at: i).
      ].
      i := i +1
    ].
    ^aux
  ]
]

"Pregunta 3. Extienda la clase Array para implementar el método
cuyo mensaje es esCuadradoMagicoNormal."

"Cuadrado Magico"

Array extend [

  esCuadradoMagicoNormal [
    | magicoNormal |
    (self esCuadrada) ifTrue: [
      magicoNormal:= (self sumarFilas) & (self sumarColumnas) 
        & (self sumarDiagonalPpal) & (self sumarDiagonalSec).
    ] ifFalse: [
      magicoNormal:= false.
    ].
    ^magicoNormal.
  ]

  sumarFilas [
    | sumF sumD1 sumD2 tam elem ctte esMagico subElem |
    tam := self size.
    sumF := 0.
    sumD1 := 0.
    sumD2 := 0.
    ctte :=  (tam*(tam*tam +1)) / 2.
    esMagico := true.
    
    1 to: tam do: [:i | 
      subElem := self at: i. 
        1 to: tam do: [:j |
          elem := subElem at: j.
          sumF := sumF + elem.
        ].
      (sumF ~= ctte) ifTrue: [
        esMagico := false.
      ].
      sumF := 0
    ].
    ^esMagico
  ]

  sumarColumnas [
    |i j bool tam ctte esMagico arreglo sum elem|
    tam := self size.
    sum:= 0.
    ctte := (tam*(tam*tam +1)) / 2.
    esMagico:= true.

    1 to: tam do: [:j |
      1 to: tam do: [:i |
        arreglo:= self at: j.
        elem := arreglo at: i.
        sum := sum + elem.
      ].
      (sum ~= ctte) ifTrue: [
        esMagico:= false
      ].
      sum := 0.
    ].
    ^esMagico
  ]

  sumarDiagonalPpal [
    |i bool tam ctte arreglo sum|
    i:= 1.
    tam:= self size.
    ctte := (tam*(tam*tam +1)) / 2.
    bool:= true.
    sum:= 0.
    [i<=tam] whileTrue: [
      arreglo:= self at: i.
      sum:= sum + (arreglo at: i).
      i:= i+1.
    ].
    (sum=ctte) ifTrue: [ bool:= true. ] ifFalse: [ bool:= false. ].
    ^bool
  ]
  
  sumarDiagonalSec [
    |i bool tam ctte arreglo sum|
    tam:= self size.
    ctte := (tam*(tam*tam +1)) / 2.
    i:= tam.
    bool:= true.
    sum:= 0.
    [i>=1] whileTrue: [
      arreglo:= self at: i.
      sum:= sum + (arreglo at: i).
      i:= i-1.
    ].
    (sum = ctte) ifTrue: [bool:= true.] ifFalse: [bool:= false.].
    ^bool
  ]

  esCuadrada [
    |i elem tam tam1 esIgual|
    i := 1.
    tam := self size.
    tam1:=0.
    esIgual := true.

    [i <= tam] whileTrue: [ 
      elem := self at: i.
      tam1 := elem size.

      (tam = tam1) ifTrue: [
        esIgual := true & esIgual.
      ] ifFalse: [
        esIgual := false.
      ].
      i:= i + 1.
    ].
    ^esIgual
  ]

].

"Implementacion. Parte 2"

"Pregunta 1. Implementacion Clase Coleccion"

Object subclass: Coleccion [

  | coleccion |
   
  init [
    coleccion := OrderedCollection new.
  ] 
  
  getColeccion [
    ^coleccion.
  ]
  
].

Coleccion class extend [

  inicializar [
    |obj|
    obj := super new.
    obj init.
    ^obj
  ]
  
].

Coleccion extend [

  printOn: stream [
    stream nextPutAll: 'Coleccion: '.
    coleccion printOn: stream.
  ]
  
].

"Pregunta 2. Clase Pila implementada como Coleccion"

Coleccion subclass: #Pila.

Pila class extend [
  
  | pila |
  
  new [
    pila:= super inicializar.
    ^pila
  ]
  
].

Pila extend [

  tamano [
    |tam|
    tam:= (super getColeccion) size.
    ^tam
  ]

  empilar: elem [
    (super getColeccion) addFirst: elem.
  ]
  
  tope [
    |tam elemento|
    tam:= self tamano.
    (tam = 0) ifTrue: [
      ^(self error: 'La pila se encuentra vacia.')
    ].
    elemento:= (super getColeccion) first.
    ^elemento
  ]  
  
  desempilar [
    |tam elemento|
    tam:= self tamano.
    (tam = 0) ifTrue: [
      ^(self error: 'La pila se encuentra vacia. No puede desempilar ningun elemento.')
    ].
    elemento:= self tope.
    (super getColeccion) removeFirst.
    ^self
  ]
  
  desempilarHasta: bloque [
    |tam elemento|
    tam:= self tamano.
    (tam = 0) ifTrue: [
      ^(self error: 'La pila se encuentra vacia. No puede desempilar ningun elemento.')
    ].
    1 to: tam do: 
      [ :x | (bloque value: (self tope)) ifFalse: [self desempilar] ]
  ]
  
].

"Pregunta 3. Clase Cola implementada como Coleccion"

Coleccion subclass: #Cola.

Cola class extend [
  
  | cola |
  
  new [
    cola:= super inicializar.
    ^cola
  ]
  
].

Cola extend [

  tamano [
    |tam|
    tam:= (super getColeccion) size.
    ^tam
  ]

  encolar: elem [
    (super getColeccion) addLast: elem.
  ]

  proximo [
    |tam elemento|
    tam:= self tamano.
    (tam = 0) ifTrue: [
      ^(self error: 'Error: La cola se encuentra vacia')
    ].
    elemento:= (super getColeccion) first.
    ^elemento
  ]

  desencolar [
    |tam elemento|
    tam := self tamano.
    (tam = 0) ifTrue: [
      ^(self error: 'Error: La cola se encuentra vacia')
    ].
    elemento := (super getColeccion) removeFirst.
    ^self
  ] 

].  

"Pregunta 4. Implementacion de Cola Memoria"

Cola subclass: ColaMemoria [
  
  |desencolados|

  inicializar [
    desencolados := Set new.
  ]
  
]

ColaMemoria class extend [
        
  new [
    ^(super new inicializar)
  ]

]

ColaMemoria extend [
  
  tamano [
    ^(super tamano)
  ]

  proximo [
    ^(super proximo)
  ]

  encolar: elemento [
    ^(super encolar: elemento)
  ]

  desencolar [
    |elemento|
    elemento := super proximo.
    super desencolar.
    desencolados add: elemento.
    ^self
  ]

  obtenerDesencolados [
    ^desencolados
  ]
  
].

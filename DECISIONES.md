# Decisiones de diseño

Los ADR siguientes describen decisiones tomadas para este proyecto y las justifican con referencia al código existente.

## D1. Representación del mundo, los identificadores y las salidas

**Contexto:** El motor debe guardar un conjunto finito de salas y objetos, localizar elementos por su identificador y recorrer el grafo de salidas sin que la lógica imponga un orden de almacenamiento. También debe distinguir tres tipos de información: dónde está el jugador, qué objetos lleva y qué objetos están en cada sala.

**Opciones:** Se consideraron usar listas para todas las colecciones y usar mapas para salas y objetos. Para los identificadores se consideró conservarlos como `String` o envolverlos en `newtype`s separados.

**Decisión:** Se usan `Data.Map.Strict.Map String Sala` y `Data.Map.Strict.Map String Objeto` en `Motor.Tipos.Mundo`. Los identificadores siguen siendo `String`, pero se validan en `Motor.Mundo.identificadorValido` como letras ASCII minúsculas, letras minúsculas acentuadas, dígitos y `_`. Las direcciones son el tipo algebraico `Direccion`; una `Salida` contiene dirección, destino y un posible objeto requisito en `salidaRequisito`.

**Consecuencias:** Los mapas evitan búsquedas lineales y garantizan unicidad lógica por clave al construirlos con `Map.fromList`. El tipo algebraico impide direcciones desconocidas dentro de `Motor.Nucleo`, aunque no impide que un `String` sea un identificador inexistente: esa relación se verifica semánticamente. El uso de `String` hace el formato, los mensajes y las pruebas sencillos, pero no crea tipos distintos para identificadores de sala y objeto. Si fuera necesario se podrían reemplazar por dos `newtype`s sin cambiar la idea general de los mapas.

## D2. Cómo se representa el Estado y dónde quedan los objetos

**Contexto:** Las acciones pueden cambiar la posición, el inventario y los objetos colocados en las salas. El núcleo debía ser puro y persistir estas modificaciones sin consultar archivos ni usar `IO`.

**Opciones:** Se consideró representar únicamente la posición y el inventario, calculando después qué objetos quedan en las salas; copiar el mundo completo dentro de `Estado`; o mantener varios mapas de cambios separados.

**Decisión:** Se eligió `Estado` con `posicion`, `inventario` y `mundo` completo en `Motor.Tipos`. `estadoInicial` usa el mundo recibido y comienza con inventario vacío. Cuando `tomar` o `soltar` modifican una sala, `Motor.Nucleo.actualizarSala` reemplaza esa sala dentro del mapa `salas (mundo estado)`. Los objetos retirados dejan de aparecer en `salObjectos` y pasan al `inventario`; al soltarlos ocurre la operación inversa.

**Consecuencias:** Cada `Estado` contiene una imagen completa y autocontenida del mundo mutable. Esto simplifica `ejecutar`, hace las pruebas puras y permite que `Estado` pueda mostrarse y leerse con `Show`/`Read` para guardar partidas. El costo es copiar o reconstruir el mapa al cambiar una sala, y llevar el mundo completo dentro de una partida. Se aceptó porque los mundos son pequeños y la separación con `IO` es más clara.

## D3. Cómo se representan las salidas bloqueadas

**Contexto:** Una salida puede exigir que el jugador tenga un objeto, pero no debe moverlo ni aceptar la orden si el objeto no está en el inventario.

**Opciones:** Representar el bloqueo como un estado booleano separado, guardar una clave en la propia salida o expresar el requisito como un `Maybe String`.

**Decisión:** `Salida` tiene `salidaRequisito :: Maybe String`. `Nothing` representa una salida libre y `Just objeto` representa una salida bloqueada. En `Motor.Nucleo.mover`, se busca primero la salida de la dirección indicada y después se comprueba que el requisito pertenezca al inventario.

**Consecuencias:** El diseño evita estados adicionales y mantiene el requisito junto con el destino en `Motor.Tipos.Salida`. Cruzar la salida no cambia permanentemente su definición: mientras el jugador tenga el objeto sigue pudiendo pasar; si luego lo suelta, vuelve a quedar bloqueada. Esa interpretación coincide con el formato y con el hecho de que el archivo no conserva una bandera de “ya abierto”. Se pierde la posibilidad de una salida que se abra solo una vez, pero no se requiere por el enunciado.

## D4. Cómo se reportan todos los errores de validación

**Contexto:** El motor debe reunir errores V1-V10 en lugar de detenerse al encontrar el primero. Encadenar operaciones con `Either` mediante `do` corta la ejecución en el primer `Left`.

**Opciones:** Interrumpir en el primer error, recopilar funciones individuales que devuelven `Maybe ErrorMundo` y agregarlas, o usar una estructura adicional como `Validation`.

**Decisión:** Se usan listas de `ErrorMundo` como acumuladores. `leerMundo` primero devuelve `Left [ErrorMundo]` si falla el análisis sintáctico o estructural; si la sintaxis es válida, `validarSemantica` concatena los resultados V2-V10 y V10 usa el estado variante. `procesarBloques` también concatena los errores de cada bloque.

**Consecuencias:** Se informa un conjunto amplio de problemas sin depender de `do` sobre `Either`. La segunda fase no se ejecuta ante errores de sintaxis, que es una decisión razonable permitida por el enunciado. Para V2-V10 la representación por listas evita que un primer fallo oculte el resto. Se perdió una abstracción general de validación, pero las funciones actuales son pequeñas y se pueden seguir utilizando en pruebas directas y semántica.

## D5. Cómo se diseñó el reconocedor

**Contexto:** El archivo de mundo requiere reconocer líneas completas, pares `CLAVE: valor`, separadores y estructuras repetidas sin usar librerías externas de reconocimiento.

**Opciones:** Usar una implementación sobre `ReadS`, envolverlo directamente en una instancia de `ReadS`, o usar un `newtype` con instancias propias.

**Decisión:** Se implementó `newtype Reconocedor a` sobre una función `String -> [(a, String)]`, con instancias `Functor`, `Applicative` y `Monad` en `Motor.Reconocedor`. Sus combinadores básicos permiten reconocer caracteres, cadenas, espacios, cero o más repeticiones y líneas. `claveValor` y `separadorBloque` reconocen partes específicas del formato.

**Consecuencias:** La aplicación secuencial es legible con `do`, y el resultado conserva el resto sin consumir. La representación devuelve una lista de resultados para mantener explícita la posibilidad de ambigüedad. Las funciones de alto nivel suelen aceptar exactamente un resultado y exigen que el resto sea vacío; así, por ejemplo, `parsearLineaClave` rechaza texto sobrante. El número de línea se obtiene numerando las líneas originales con `zip [1..] (lines input)` antes de agruparlas. Comentarios y líneas vacías conservan los números originales porque se filtran después de construir esa numeración.

## D6. Variantes de comandos aceptados

**Contexto:** El enunciado permite decidir sinónimos y abreviaturas; conviene tener una gramática simple y predecible.

**Opciones:** Permitir artículos y nombres escritos con variaciones, aceptar solo los identificadores exactos, o usar una combinación limitada de mayúsculas, sinónimos y nombres completos.

**Decisión:** `Motor.Comandos.leerComando` convierte el comando y sus argumentos a minúsculas. Se aceptan `coger` como sinónimo de `tomar`, `inv` como sinónimo de `inventario` y nombres de objeto formados por varias palabras, como `Tomar llave vieja`. Las direcciones deben escribirse completas: no se añadieron abreviaturas como `n` o `s`.

**Consecuencias:** Los identificadores de objetos escritos en español pueden contener espacios como valor del comando, pero internamente deben coincidir exactamente con el identificador convertido a minúsculas. Esta coincidencia evita depender de artículos especiales. La lectura rechaza argumentos adicionales en comandos sin argumentos y formatos incompletos como `combinar a b`. `guardar` y `cargar` aceptan rutas con espacios porque toman el resto de la línea como `FilePath`.

## D7. Cómo se implementó la variante 3: Combinar

**Contexto:** La variante 3 permite combinar dos objetos de interés en el inventario. El mundo debe validar las recetas y el motor debe impedir combinaciones que no tengan ambos ingredientes o una receta definida.

**Opciones:** Mezclar la combinación directamente en `Motor.Nucleo`, usar ingredientes como una lista dentro de cada objeto o representar recetas explícitamente en `Mundo`.

**Decisión:** Se añadió `RECETA: a + b -> c` a la cabecera y el tipo `Receta` con los dos ingredientes y el resultado. `Mundo.receta` almacena todas las recetas. `leerComando` reconoce `combinar a con b`; `Motor.Nucleo.combinar` acepta las recetas en cualquier orden porque compara ambos ingredientes permutando los operandos.

**Consecuencias:** La combinación reemplaza una copia de cada ingrediente por una copia del resultado. `objetosConseguibles` usa búsqueda sobre estados de objetos e incluye en el conjunto los objetos obtenibles por una o varias recetas para la validación V10. También se valida que los tres identificadores existan y que el resultado esté definido. Se permite que el resultado aparezca además en una sala, pero no se permite contar resultados por recetas si no existe el objeto correspondiente. La decisión hace la combinación útil y evita errores internos al cargar recetas incompletas.

## D8. Formato y validez de las partidas guardadas

**Contexto:** `guardar` y `cargar` deben funcionar sin dar lugar a partidas corruptas o de otro mundo.

**Opciones:** Guardar solo posición e inventario, guardar todos los cambios respecto al mundo original o guardar el `Estado` completo usando `Show`/`Read`.

**Decisión:** `Motor.Persistencia` usa `show estado` al guardar y `readMaybe` al cargar. Después verifica que el mundo guardado coincida esencialmente con el mundo actual: título, inicio, meta, objetos, recetas y estructura compatible de las salas.

**Consecuencias:** La implementación es breve y conserva objetos soltados y otras modificaciones de sala dentro del mapa del `Estado`. Una partida corrupta queda como un texto que `readMaybe` no puede interpretar. Si el mundo cambia su descripción o sus salidas, `cargarPartida` rechaza la partida en lugar de mezclar dos versiones incompatibles. Se perdió la estabilidad de un formato propio versionado y legible, pero se ganó una implementación consistente con los tipos algebraicos.

## D9. Entrada redirigida y fin de entrada

**Contexto:** El programa debe seguir funcionando con guiones redirigidos y terminar sin excepción cuando se alcanza EOF.

**Opciones:** Leer todo el programa con `getContents`, leer una línea con `getLine` hasta que falle, o usar `isEOF` antes de cada iteración.

**Decisión:** `app/Main.hs` utiliza `isEOF` en el bucle principal y `getLine` únicamente cuando hay entrada disponible. El indicador `> ` se escribe antes de leer cada línea y se fuerza con `hFlush stdout`.

**Consecuencias:** La entrada interactiva y la entrada redirigida usan el mismo flujo. Al llegar a EOF, el bucle termina normalmente sin intentar leer una línea inexistente. `isEOF/getLine` hace que el comportamiento sea explícito y fácil de probar; no se usa evaluación perezosa de `getContents` en esta implementación. Por ello no se aprovecha una lectura perezosa completa de un archivo de entrada, pero se evita el riesgo de excepciones al finalizar el flujo.

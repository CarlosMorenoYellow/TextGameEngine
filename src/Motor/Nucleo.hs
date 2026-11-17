module Motor.Nucleo
  ( estadoInicial
  , ejecutar
  , metaCumplida
  ) where

import qualified Data.Map.Strict as Map
import Data.List (find)
import Motor.Tipos

estadoInicial :: Mundo -> Estado
estadoInicial mundoActual =
  Estado
    { posicion = inicio mundoActual
    , inventario = []
    , mundo = mundoActual
    }

metaCumplida :: Estado -> Bool
metaCumplida estado =
  case meta (mundo estado) of
    LlegarASala destino ->
      posicion estado == destino

    LlevarObjetoASala objeto destino ->
      posicion estado == destino && objeto `elem` inventario estado

ejecutar :: Comando -> Estado -> ([String], Estado)
ejecutar comando estado =
  let (mensajes, estadoNuevo) = ejecutarBase comando estado
      ganoAhora = not (metaCumplida estado) && metaCumplida estadoNuevo
      mensajeVictoria =
        if ganoAhora then ["*** ¡Has ganado! ***"] else []
  in (mensajes ++ mensajeVictoria, estadoNuevo)

ejecutarBase :: Comando -> Estado -> ([String], Estado)
ejecutarBase comando estado =
  case comando of
    Mirar ->
      (mirar estado, estado)

    Ir direccion ->
      mover direccion estado

    Tomar objeto ->
      tomar objeto estado

    Soltar objeto ->
      soltar objeto estado

    Examinar objeto ->
      examinar objeto estado

    Inventario ->
      (mostrarInventario estado, estado)

    Combinar objeto1 objeto2 ->
      combinar objeto1 objeto2 estado

    Ayuda ->
      ( [ "Comandos: ir <direccion>, mirar, tomar <objeto>, soltar <objeto>"
        , "          examinar <objeto>, inventario, combinar <a> con <b>"
        , "          guardar <archivo>, cargar <archivo>, ayuda, salir"
        ]
      , estado
      )

    Salir ->
      (["Hasta luego."], estado)

    Guardar _ ->
      (["Guardar se procesa desde la interfaz del programa."], estado)

    Cargar _ ->
      (["Cargar se procesa desde la interfaz del programa."], estado)

salaActual :: Estado -> Maybe Sala
salaActual estado =
  Map.lookup (posicion estado) (salas (mundo estado))

mirar :: Estado -> [String]
mirar estado =
  case salaActual estado of
    Nothing -> ["Error: la posición actual no existe en el mundo."]
    Just sala ->
      let mundoActual = mundo estado
          nombresObjetos = salObjectos sala
          nombresSalidas = map describirSalida (salidas sala)
          objetos =
            [ descripcionObjeto mundoActual objeto
            | objeto <- nombresObjetos
            ]
      in [salDesc sala]
         ++ ["Objetos: " ++ listaOninguno objetos]
         ++ ["Salidas: " ++ listaOninguno nombresSalidas]

describirSalida :: Salida -> String
describirSalida salida =
  nombreDireccion (salidaDir salida)
    ++ " -> "
    ++ salidaDestino salida
    ++ case salidaRequisito salida of
         Nothing -> ""
         Just objeto -> " (requiere " ++ objeto ++ ")"

nombreDireccion :: Direccion -> String
nombreDireccion direccion =
  case direccion of
    Norte  -> "norte"
    Sur    -> "sur"
    Este   -> "este"
    Oeste  -> "oeste"
    Arriba -> "arriba"
    Abajo  -> "abajo"

listaOninguno :: [String] -> String
listaOninguno [] = "ninguno"
listaOninguno xs = unwords xs

descripcionObjeto :: Mundo -> String -> String
descripcionObjeto mundoActual identificador =
  case Map.lookup identificador (objectos mundoActual) of
    Nothing -> identificador
    Just objeto -> objDesc objeto

mover :: Direccion -> Estado -> ([String], Estado)
mover direccion estado =
  case salaActual estado of
    Nothing -> (["Error: la posición actual no existe."], estado)
    Just sala ->
      case find ((== direccion) . salidaDir) (salidas sala) of
        Nothing ->
          (["No hay salida en esa dirección."], estado)

        Just salida ->
          case salidaRequisito salida of
            Just objeto | objeto `notElem` inventario estado ->
              (["La salida está bloqueada; necesitas " ++ objeto ++ "."], estado)

            _ ->
              let estadoNuevo = estado { posicion = salidaDestino salida }
              in (["Te mueves a " ++ salidaDestino salida ++ "."], estadoNuevo)

tomar :: String -> Estado -> ([String], Estado)
tomar objeto estado =
  case salaActual estado of
    Nothing -> (["Error: la posición actual no existe."], estado)
    Just sala
      | objeto `notElem` salObjectos sala ->
          (["Ese objeto no está en esta sala."], estado)
      | otherwise ->
          case quitarUno objeto (salObjectos sala) of
            Nothing -> (["No se pudo tomar el objeto."], estado)
            Just objetosRestantes ->
              let salaNueva = sala { salObjectos = objetosRestantes }
                  estadoConSala = actualizarSala estado salaNueva
                  estadoNuevo =
                    estadoConSala { inventario = objeto : inventario estado }
              in (["Tomaste " ++ objeto ++ "."], estadoNuevo)

soltar :: String -> Estado -> ([String], Estado)
soltar objeto estado
  | objeto `notElem` inventario estado =
      (["No tienes ese objeto."], estado)
  | otherwise =
      case salaActual estado of
        Nothing -> (["Error: la posición actual no existe."], estado)
        Just sala ->
          let inventarioNuevo = quitarPrimera objeto (inventario estado)
              salaNueva = sala { salObjectos = objeto : salObjectos sala }
              estadoConSala = actualizarSala estado salaNueva
              estadoNuevo = estadoConSala { inventario = inventarioNuevo }
          in (["Soltaste " ++ objeto ++ "."], estadoNuevo)

examinar :: String -> Estado -> ([String], Estado)
examinar objeto estado =
  case salaActual estado of
    Nothing -> (["Error: la posición actual no existe."], estado)
    Just sala
      | objeto `notElem` inventario estado
          && objeto `notElem` salObjectos sala ->
            (["No tienes ese objeto y no está en esta sala."], estado)
      | otherwise ->
          ([descripcionObjeto (mundo estado) objeto], estado)

mostrarInventario :: Estado -> [String]
mostrarInventario estado =
  case inventario estado of
    [] -> ["No llevas objetos."]
    objetosLlevados ->
      [ "Llevas: "
          ++ unwords
               [ descripcionObjeto (mundo estado) objeto
               | objeto <- objetosLlevados
               ]
      ]

combinar :: String -> String -> Estado -> ([String], Estado)
combinar objeto1 objeto2 estado
  | objeto1 `notElem` inventario estado
      || objeto2 `notElem` inventario estado =
      (["Necesitas tener ambos objetos en el inventario."], estado)
  | otherwise =
      case find coincide (receta (mundo estado)) of
        Nothing ->
          (["No existe una receta para combinar esos objetos."], estado)

        Just recetaEncontrada ->
          let resultado = recResultado recetaEncontrada
              objetos = objectos (mundo estado)
          in if Map.notMember resultado objetos
               then (["Error: el resultado de la receta no está definido."], estado)
               else
                 case quitarUno objeto1 (inventario estado)
                        >>= quitarUno objeto2 of
                   Nothing ->
                     (["No tienes suficientes copias de esos objetos."], estado)
                   Just inventarioRestante ->
                     let estadoNuevo =
                           estado
                             { inventario = resultado : inventarioRestante }
                     in (["Combinaste los objetos y obtuviste " ++ resultado ++ "."],
                         estadoNuevo)
  where
    coincide recetaActual =
      (recIng1 recetaActual == objeto1
        && recIng2 recetaActual == objeto2)
      || (recIng1 recetaActual == objeto2
        && recIng2 recetaActual == objeto1)

actualizarSala :: Estado -> Sala -> Estado
actualizarSala estado salaNueva =
  let mundoActual = mundo estado
      salasNuevas =
        Map.insert (salId salaNueva) salaNueva (salas mundoActual)
  in estado { mundo = mundoActual { salas = salasNuevas } }

quitarUno :: Eq a => a -> [a] -> Maybe [a]
quitarUno _ [] = Nothing
quitarUno buscado (x:xs)
  | buscado == x = Just xs
  | otherwise = (x :) <$> quitarUno buscado xs

quitarPrimera :: Eq a => a -> [a] -> [a]
quitarPrimera buscado xs =
  case quitarUno buscado xs of
    Nothing -> xs
    Just resto -> resto
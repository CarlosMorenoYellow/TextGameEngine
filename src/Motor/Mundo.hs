module Motor.Mundo 
( 

) where

import Motor.Tipos
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map 
import qualified Data.Set as Set
import Data.Char (isSpace, toLower)
import Data.List (foldl', partition, (\\))
import Data.Maybe (mapMaybe, isJust, fromMaybe)
import qualified Data.Foldable as Set
import Motor.Tipos (Direccion)

-- Estructuras Intermedias de Parsing

type NumLinea = Int

data LineaClave = LineaClave
    { numeroLinea :: NumLinea,
    lcClave :: String,
    lcValor :: String
    } deriving(Show)

data BloqueData = BloqueData {
    bloqueLineaInicial :: NumLinea,
    bloqueTipo :: String,
    bloqueId :: Maybe String,
    bloqueCampos :: [LineaClave]
} deriving (Show)

-- Analisis Sintactico y Deteccion de V1

esIgnorable :: String -> Bool
esIgnorable str =
  let s = dropWhile isSpace str
  in null s || head s == '#'

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

agruparBloques :: [(NumLinea, String)] -> [[(NumLinea, String)]]
agruparBloques [] = []
agruparBloques lineas =
  let (grupo, resto) = break (esSeparador . snd) lineas
      restoSinSeparador = dropWhile (esSeparador . snd) resto
  in if null grupo
       then agruparBloques restoSinSeparador
       else grupo : agruparBloques restoSinSeparador
  where
    esSeparador s = trim s == "-"

procesarBloques :: [[(NumLinea, String)]] -> ([ErrorMundo], [BloqueRaw])
procesarBloques bloques =
  let resultados = map procesarBloque bloques
      errs = concatMap lefts' resultados
      blqs = mapMaybe rights' resultados
  in (errs, blqs)
  where
    lefts' (Left e)  = e
    lefts' (Right _) = []
    rights' (Right b) = Just b
    rights' (Left _)  = Nothing

procesarBloque :: [(NumLinea, String)] -> Either [ErrorMundo] BloqueRaw
procesarBloque [] = Left [ErrorSintaxis 0 "Bloque vacío"]
procesarBloque ((numLin, primeraLinea):resto) =
  case break (== ':') primeraLinea of
    (clave, ':':valor) ->
      let claveTrim = trim clave
          valTrim   = trim valor
      in case claveTrim of
        "MUNDO" ->
          let (errs, campos) = parsearCampos resto
          in if not (null errs)
               then Left errs
               else Right $ BloqueRaw numLin "MUNDO" (Just valTrim) campos
        "OBJETO" ->
          let (errs, campos) = parsearCampos resto
          in if not (null errs)
               then Left errs
               else Right $ BloqueRaw numLin "OBJETO" (Just valTrim) campos
        "SALA" ->
          let (errs, campos) = parsearCampos resto
          in if not (null errs)
               then Left errs
               else Right $ BloqueRaw numLin "SALA" (Just valTrim) campos
        _ -> Left [ErrorSintaxis numLin ("Encabezado de bloque desconocido: " ++ claveTrim)]
    _ -> Left [ErrorSintaxis numLin "Línea de encabezado sin el formato CLAVE: valor"]

parsearCampos :: [(NumLinea, String)] -> ([ErrorMundo], [LineaClave])
parsearCampos lineas =
  let resultados = map parsearLineaClave lineas
      errs = mapMaybe left' resultados
      camps = mapMaybe right' resultados
  in (errs, camps)
  where
    left' (Left e)  = Just e
    left' (Right _) = Nothing
    right' (Right c) = Just c
    right' (Left _)  = Nothing

parsearLineaClave :: (NumLinea, String) -> Either ErrorMundo LineaClave
parsearLineaClave (n, str) =
  case break (== ':') str of
    (clave, ':':valor) ->
      let c = trim clave
          v = trim valor
      in if null c
           then Left (ErrorSintaxis n "Clave vacía")
           else Right (LineaClave n c v)
    _ -> Left (ErrorSintaxis n "Línea sin formato CLAVE: valor")

clasificarBloques :: [BloqueRaw] -> Either [ErrorMundo] (BloqueRaw, [BloqueRaw], [BloqueRaw])
clasificarBloques bloques =
  let (cabeceras, resto) = partition (\b -> brTipo b == "MUNDO") bloques
      (objetos, salas)   = partition (\b -> brTipo b == "OBJETO") resto
  in case cabeceras of
    []  -> Left [ErrorSintaxis 1 "Falta el bloque MUNDO"]
    [c] -> Right (c, objetos, salas)
    _   -> Left [ErrorSintaxis (brLineaInicio (cabeceras !! 1)) "Bloque MUNDO duplicado"]

parsearSintaxis :: String -> Either [ErrorMundo] (ErrorMundo, [BloqueData], [BloqueData])
parsearSintaxis input = 
    let lineasNumeradas = zip [1..] (lines input)
        -- Ignorar lineas vacias y comentarios (#)
        lineasFiltradas = filter (not . esIgnorable . snd) lineasNumeradas
        bloquesData = agruparbloques lineasFiltradas

        --Validar sintaxis de cada bloque
        (errores, bloquesValidos) = procesarBloques bloquesData
    in if not (null errores)
        then Left errores 
        else clasificarVloques bloquesValidados

-- Funciones auxiliares para validacion semantica

duplicados :: Ord a => [a] -> [a]
duplicados xs = Set.toList (dups xs Set.empty Set.empty)
    where
        dups [] _ acc = acc 
        dups (y:ys) vistos acc
            | y `Set.member` vistos = dups ys vistos (Set.insert y acc)
            | otherwise             = dups ys (Set.insert y vistos) acc

parsearDireccion :: String -> Maybe Direccion
parsearDireccion s = case map toLower s of
  "norte"  -> Just Norte
  "sur"    -> Just Sur
  "este"   -> Just Este
  "oeste"  -> Just Oeste
  "arriba" -> Just Arriba
  "abajo"  -> Just Abajo
  _        -> Nothing

parsearSalida :: String -> LineaClave -> Maybe (String, String, Maybe String, Direccion, LineaClave)
parsearSalida salaID (LineaClave n _ val) = let w = words va; in case w of
    (direccionString : "->" : destinoString : "requiere" : objetoRequerimiento : _) ->
        case parsearDireccion direccionString of
            Just d -> Just (salaId, destinoString, Just objetoRequerimiento, d, LineaClave n "" val)
            Nothing -> Nothing
    (direccionString : "->" : destinoString : _) ->
        case parsearDireccion direccionString of
            Just b ->  Just (salaId, destinoString, Nothing, d, LineaClave n "" val)
            Nothing -> Nothing
    _ -> Nothing

validarDireccionesDuplicadas :: BloqueData -> [ErrorMundo]
validarDireccionesDuplicadas s =
  let salaId = fromMaybe "" (bloqueId s)
      salidas = filter (\c -> lcClave c == "SALIDA") (bloqueCampos s)
      dirs = mapMaybe (\c -> parsearDir (head (words (lcValor c)))) salidas
  in map (ErrorSalidaDireccionDuplicada salaId) (duplicados dirs)

parsearMetaVal :: String -> Maybe Meta
parsearMetaVal val =
  let w = words val
  in case w of
    ("llegar" : "a" : s : _) -> Just (LlegarASala s)
    ("llevar" : obj : "a" : s : _) -> Just (LlevarObjetoASala obj s)
    _ -> Nothing

validarMetaExistencia :: Meta -> Set.Set String -> Set.Set String -> [ErrorMundo]
validarMetaExistencia meta setSalas setObjetos = case meta of
  LlegarASala s ->
    if s `Set.member` setSalas then [] else [ErrorMetaInexistente meta]
  LlevarObjetoASala obj s ->
    let sOk   = s `Set.member` setSalas
        objOk = obj `Set.member` setObjetos
    in if sOk && objOk then [] else [ErrorMetaInexistente meta]

bfsAlcanzables :: String -> [BloqueData] -> Set.Set String
bfsAlcanzables inicio salas = go (Set.singleton inicio) [inicio]
  where
    mapaSalidas = Map.fromList [ (fromMaybe "" (bloqueId s), obtenerDestinos s) | s <- salas ]
    obtenerDestinos s = mapMaybe (\c -> case words (lcValor c) of
                                         (_ : "->" : d : _) -> Just d
                                         _ -> Nothing
                                 ) (filter (\c -> lcClave c == "SALIDA") (bloqueCampos s))
    
    go vistos [] = vistos
    go vistos (curr:queue) =
      let vecinos = fromMaybe [] (Map.lookup curr mapaSalidas)
          nuevos  = filter (`Set.notMember` vistos) vecinos
          vistos' = foldl' (flip Set.insert) vistos nuevos
      in go vistos' (queue ++ nuevos)

lookupCampo :: String -> [LineaClave] -> Maybe LineaClave
lookupCampo k campos = case filter (\c -> lcClave c == k) campos of
  (c:_) -> Just c
  []    -> Nothing

-- funcion Principal


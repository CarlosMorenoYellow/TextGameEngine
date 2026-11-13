module Motor.Mundo (
    leerMundo
) where

import Motor.Tipos
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (isAsciiLower, isSpace, toLower)
import Data.List (partition, sort)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import qualified Motor.Reconocedor as Reconocedor

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
  case dropWhile isSpace str of
    []     -> True
    '#':_  -> True
    _      -> False

agruparBloques :: [(NumLinea, String)] -> [[(NumLinea, String)]]
agruparBloques [] = []
agruparBloques lineas =
  let (grupo, resto) = break (esSeparador . snd) lineas
      restoSinSeparador = dropWhile (esSeparador . snd) resto
  in if null grupo
       then agruparBloques restoSinSeparador
       else grupo : agruparBloques restoSinSeparador
  where
    esSeparador s =
      not (null (Reconocedor.runReconocedor Reconocedor.separadorBloque s))

procesarBloques :: [[(NumLinea, String)]] -> ([ErrorMundo], [BloqueData])
procesarBloques bloques =
  let resultados = map procesarBloque bloques
  in (concatMap fst resultados, mapMaybe snd resultados)

procesarBloque :: [(NumLinea, String)] -> ([ErrorMundo], Maybe BloqueData)
procesarBloque [] = ([ErrorSintaxis 0 "Bloque vacío"], Nothing)
procesarBloque ((numLin, primeraLinea):resto) =
  case parsearLineaClave (numLin, primeraLinea) of
    Right (LineaClave _ clave valor) ->
      case clave of
        "MUNDO" ->
          let (errs, campos) = parsearCampos resto
          in (errs, Just $ BloqueData numLin "MUNDO" (Just valor) campos)
        "OBJETO" ->
          let (errs, campos) = parsearCampos resto
          in (errs, Just $ BloqueData numLin "OBJETO" (Just valor) campos)
        "SALA" ->
          let (errs, campos) = parsearCampos resto
          in (errs, Just $ BloqueData numLin "SALA" (Just valor) campos)
        _ -> ([ErrorSintaxis numLin ("Encabezado de bloque desconocido: " ++ clave)], Nothing)
    Left err -> ([err], Nothing)

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

parsearRecetaVal :: String -> Maybe Receta
parsearRecetaVal val =
  case words val of
    [a, "+", b, "->", c] -> Just (Receta a b c)
    _                    -> Nothing

parsearLineaClave :: (NumLinea, String) -> Either ErrorMundo LineaClave
parsearLineaClave (n, str) =
  case Reconocedor.runReconocedor Reconocedor.claveValor str of
    [((clave, valor), resto)] | null resto && not (null clave) ->
      Right (LineaClave n clave valor)
    _ -> Left (ErrorSintaxis n "Línea sin formato CLAVE: valor")

clasificarBloques :: [BloqueData] -> Either [ErrorMundo] (BloqueData, [BloqueData], [BloqueData])
clasificarBloques bloques =
  case bloques of
    [] -> Left [ErrorSintaxis 1 "Falta el bloque MUNDO"]
    cabecera:resto
      | bloqueTipo cabecera /= "MUNDO" ->
          Left [ErrorSintaxis (bloqueLineaInicial cabecera) "El primer bloque debe ser MUNDO"]
      | otherwise ->
          let cabeceras = filter ((== "MUNDO") . bloqueTipo) resto
              (objetos, salas) = partition ((== "OBJETO") . bloqueTipo) resto
          in case cabeceras of
            c2:_ -> Left [ErrorSintaxis (bloqueLineaInicial c2) "Bloque MUNDO duplicado"]
            [] -> Right (cabecera, objetos, salas)

validarEstructuraBloque :: BloqueData -> [ErrorMundo]
validarEstructuraBloque b =
  errId ++ case bloqueTipo b of
    "MUNDO"  -> validarMundo b
    "OBJETO" -> validarObjeto b
    "SALA"   -> validarSala b
    _        -> []
  where
    errId = case bloqueId b of
      Nothing -> [ErrorSintaxis (bloqueLineaInicial b) ("Falta identificador en " ++ bloqueTipo b)]
      Just "" -> [ErrorSintaxis (bloqueLineaInicial b) ("Identificador vacío en " ++ bloqueTipo b)]
      Just ident
        | bloqueTipo b /= "MUNDO" && not (identificadorValido ident) ->
            [ErrorSintaxis (bloqueLineaInicial b) ("Identificador inválido: " ++ ident)]
        | otherwise -> []

identificadorValido :: String -> Bool
identificadorValido [] = False
identificadorValido ident = all caracterIdentificador ident
  where
    caracterIdentificador c =
      isAsciiLower c || c `elem` "áéíóúñü" || (c >= '0' && c <= '9') || c == '_'

validarMundo :: BloqueData -> [ErrorMundo]
validarMundo b =
  validarClavesPermitidas b ["INICIO", "META", "RECETA"]
  ++ validarCampoUnico b "INICIO"
  ++ validarCampoUnico b "META"
  ++ errInicioFormato
  ++ errMetaFormato
  ++ errRecetasFormato
  where
    errInicioFormato = case filter (\c -> lcClave c == "INICIO") (bloqueCampos b) of
        [c] | not (identificadorValido (lcValor c)) ->
            [ErrorSintaxis (numeroLinea c) "INICIO debe ser un identificador de sala válido"]
        _ -> []

    errMetaFormato = case filter (\c -> lcClave c == "META") (bloqueCampos b) of
        [c] | isNothing (parsearMetaVal (lcValor c)) ->
            [ErrorSintaxis (numeroLinea c) ("Formato de META inválido: " ++ lcValor c)]
        _ -> []

    errRecetasFormato =
        [ErrorSintaxis (numeroLinea c) ("Formato de RECETA inválido (esperado 'a + b -> c'): " ++ lcValor c)
        | c <- bloqueCampos b, lcClave c == "RECETA", isNothing (parsearRecetaVal (lcValor c))
        ]


validarObjeto :: BloqueData -> [ErrorMundo]
validarObjeto b =
  validarClavesPermitidas b ["DESC"]
  ++ validarCampoUnico b "DESC"

validarSala :: BloqueData -> [ErrorMundo]
validarSala b =
  validarClavesPermitidas b ["DESC", "SALIDA", "OBJETO"]
  ++ validarCampoUnico b "DESC"
  ++ errSalidas
  ++ errObjetosSala
  where
    errSalidas =
      [ ErrorSintaxis (numeroLinea c) ("Formato de SALIDA inválido: " ++ lcValor c)
      | c <- bloqueCampos b
      , lcClave c == "SALIDA"
      , isNothing (parsearSalidaEstructurada c)
      ]

    errObjetosSala =
      [ ErrorSintaxis (numeroLinea c) ("Identificador de OBJETO inválido: " ++ lcValor c)
      | c <- bloqueCampos b
      , lcClave c == "OBJETO"
      , not (identificadorValido (lcValor c))
      ]

validarCampoUnico :: BloqueData -> String -> [ErrorMundo]
validarCampoUnico b clave =
  case filter (\c -> lcClave c == clave) (bloqueCampos b) of
    []       -> [ErrorSintaxis (bloqueLineaInicial b) ("Falta el campo obligatorio '" ++ clave ++ "' en " ++ bloqueTipo b)]
    [c]      -> if null (lcValor c)
                  then [ErrorSintaxis (numeroLinea c) ("El campo '" ++ clave ++ "' no puede estar vacío")]
                  else []
    (_:c2:_) -> [ErrorSintaxis (numeroLinea c2) ("Campo '" ++ clave ++ "' duplicado en " ++ bloqueTipo b)]

validarClavesPermitidas :: BloqueData -> [String] -> [ErrorMundo]
validarClavesPermitidas b permitidas =
  [ ErrorSintaxis (numeroLinea c) ("Clave desconocida '" ++ lcClave c ++ "' en bloque " ++ bloqueTipo b)
  | c <- bloqueCampos b
  , lcClave c `notElem` permitidas
  ]

parsearSintaxis :: String -> Either [ErrorMundo] (BloqueData, [BloqueData], [BloqueData])
parsearSintaxis input =
  let lineasNumeradas = zip [1..] (lines input)
      lineasFiltradas = filter (not . esIgnorable . snd) lineasNumeradas
      bloquesData = agruparBloques lineasFiltradas
      (erroresLineas, bloquesValidos) = procesarBloques bloquesData
  in case clasificarBloques bloquesValidos of
       Left errClasif -> Left (erroresLineas ++ errClasif)
       Right (cabecera, objetos, salas) ->
         let erroresEstructura = concatMap validarEstructuraBloque (cabecera : objetos ++ salas)
             errores = erroresLineas ++ erroresEstructura
         in if not (null errores)
              then Left errores
              else Right (cabecera, objetos, salas)

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
parsearSalida salaID (LineaClave n _ val) = let w = words val in case w of
    [direccionString, "->", destinoString, "requiere", objetoRequerimiento]
      | identificadorValido destinoString && identificadorValido objetoRequerimiento ->
        case parsearDireccion direccionString of
            Just d -> Just (salaID, destinoString, Just objetoRequerimiento, d, LineaClave n "" val)
            Nothing -> Nothing
    [direccionString, "->", destinoString]
      | identificadorValido destinoString ->
        case parsearDireccion direccionString of
            Just d ->  Just (salaID, destinoString, Nothing, d, LineaClave n "" val)
            Nothing -> Nothing
    _ -> Nothing

validarDireccionesDuplicadas :: BloqueData -> [ErrorMundo]
validarDireccionesDuplicadas s =
  let salaId = fromMaybe "" (bloqueId s)
      salidas = filter (\c -> lcClave c == "SALIDA") (bloqueCampos s)
      dirs = mapMaybe (\c ->
        case words (lcValor c) of
        direccion : _ -> parsearDireccion direccion
        []            -> Nothing
        ) salidas
  in map (ErrorSalidaDireccionDuplicada salaId) (duplicados dirs)

parsearMetaVal :: String -> Maybe Meta
parsearMetaVal val =
  let w = words val
  in case w of
    ["llegar", "a", s] | identificadorValido s -> Just (LlegarASala s)
    ["llevar", obj, "a", s]
      | identificadorValido obj && identificadorValido s ->
          Just (LlevarObjetoASala obj s)
    _ -> Nothing

validarMetaExistencia :: Meta -> Set.Set String -> Set.Set String -> [ErrorMundo]
validarMetaExistencia meta setSalas setObjetos = case meta of
  LlegarASala s ->
    if s `Set.member` setSalas then [] else [ErrorMetaOnexistente meta]
  LlevarObjetoASala obj s ->
    let sOk   = s `Set.member` setSalas
        objOk = obj `Set.member` setObjetos
    in if sOk && objOk then [] else [ErrorMetaOnexistente meta]

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

-- | Explora las combinaciones posibles conservando la cantidad de cada objeto.
objetosConseguibles :: [String] -> [Receta] -> Set.Set String
objetosConseguibles disponibles recetas =
  explorar [sort disponibles] Set.empty Set.empty
  where
    explorar [] _ obtenibles = obtenibles
    explorar (estado:pendientes) visitados obtenibles
      | estado `Set.member` visitados =
          explorar pendientes visitados obtenibles
      | otherwise =
          let visitados' = Set.insert estado visitados
              obtenibles' = Set.union obtenibles (Set.fromList estado)
              siguientes = mapMaybe (aplicarReceta estado) recetas
          in explorar (siguientes ++ pendientes) visitados' obtenibles'

    aplicarReceta estado (Receta ingrediente1 ingrediente2 resultado) = do
      sinIngrediente1 <- quitar ingrediente1 estado
      sinIngredientes <- quitar ingrediente2 sinIngrediente1
      pure (sort (resultado : sinIngredientes))

    quitar _ [] = Nothing
    quitar objeto (x:xs)
      | objeto == x = Just xs
      | otherwise = (x :) <$> quitar objeto xs

--Validacion de V2 a V10

validarSemantica :: BloqueData -> [BloqueData] -> [BloqueData] -> [ErrorMundo]
validarSemantica cabecera objetosData salasData =
  let -- Extracción de IDs
      idsSalas   = mapMaybe bloqueId salasData
      idsObjetos = mapMaybe bloqueId objetosData
      setSalas   = Set.fromList idsSalas
      setObjetos = Set.fromList idsObjetos
      recetas = mapMaybe (parsearRecetaVal . lcValor) (filter (\c -> lcClave c == "RECETA") (bloqueCampos cabecera))

      -- V2: Identificadores duplicados
      errV2_salas   = map ErrorIdDuplicadoSala (duplicados idsSalas)
      errV2_objetos = map ErrorIdDuplicadoObjecto (duplicados idsObjetos)
      errVariante3 =
        [ ErrorRecetaObjetoInexistente obj
        | Receta a b c <- recetas,
        obj <- [a, b, c],
        obj `Set.notMember` setObjetos
        ]

      -- V3, V4, V5, V6: Salidas y Objetos en salas
      salidasData = concatMap (\s -> zip (repeat (fromMaybe "" (bloqueId s))) (filter (\c -> lcClave c == "SALIDA") (bloqueCampos s))) salasData
      salidasParseadas = mapMaybe (uncurry parsearSalida) salidasData

      errV3 = [ ErrorSalidaSalaInexistente s dest
              | (s, dest, _, _, _) <- salidasParseadas, dest `Set.notMember` setSalas ]

      errV4 = [ ErrorSalidaRequisitoInexistente s req
              | (s, _, Just req, _, _) <- salidasParseadas, req `Set.notMember` setObjetos ]

      errV5 = concatMap validarDireccionesDuplicadas salasData

      objetosEnSalasWithRoom = concatMap (\s -> map (\c -> (fromMaybe "" (bloqueId s), lcValor c))
                                                    (filter (\c -> lcClave c == "OBJETO") (bloqueCampos s))) salasData

      errV6 = [ ErrorSalaObjetoIndefinido s obj
              | (s, obj) <- objetosEnSalasWithRoom, obj `Set.notMember` setObjetos ]

      -- V7: Objeto colocado en más de una sala (o dos veces)
      todosObjetosColocados = map snd objetosEnSalasWithRoom
      objetosDuplicados = duplicados todosObjetosColocados
      errV7 = [ ErrorObjetoEnMultiplesSalas sala obj
              | (sala, obj) <- objetosEnSalasWithRoom
              , obj `elem` objetosDuplicados
              ]

      -- V8: INICIO o META con sala/objeto inexistente
      campoInicio = lookupCampo "INICIO" (bloqueCampos cabecera)
      campoMeta   = lookupCampo "META" (bloqueCampos cabecera)

      errV8_inicio = case campoInicio of
        Just (LineaClave _ _ val) -> if val `Set.member` setSalas then [] else [ErrorInicioInexistente val]
        Nothing -> []

      metaParsed = case campoMeta of
        Just (LineaClave _ _ val) -> parsearMetaVal val
        Nothing -> Nothing

      errV8_meta = case metaParsed of
        Just meta -> validarMetaExistencia meta setSalas setObjetos
        Nothing -> []

      -- V9: Alcanzabilidad desde INICIO
      errV9 = case campoInicio of
        Just (LineaClave _ _ inicio) | inicio `Set.member` setSalas ->
          let alcanzables = bfsAlcanzables inicio salasData
              inalcanzables = Set.toList (setSalas `Set.difference` alcanzables)
          in map ErrorSalaInalcanzable inalcanzables
        _ -> []

      -- V10: Objetos necesarios no colocados en ninguna sala
      setConseguibles = objetosConseguibles todosObjetosColocados recetas

      objsRequeridosSalidas = mapMaybe (\(_, _, mReq, _, _) -> mReq) salidasParseadas
      objsRequeridosMeta    = case metaParsed of
        Just (LlevarObjetoASala obj _) -> [obj]
        _ -> []
      objsNecesarios = Set.toList (Set.fromList (objsRequeridosSalidas ++ objsRequeridosMeta))
      errV10 =  [ ErrorObjectoClaveNoColocado obj
                | obj <- objsNecesarios, obj `Set.notMember` setConseguibles
                ]

  in errV2_salas ++ errV2_objetos ++ errV3 ++ errV4 ++ errV5 ++ errV6
     ++ errV7 ++ errV8_inicio ++ errV8_meta ++ errV9 ++ errV10 ++ errVariante3

-- Construccion del AST Final

construirMundo :: BloqueData -> [BloqueData] -> [BloqueData] -> Mundo
construirMundo cabecera objetosData salasData =
  let camposCabecera = bloqueCampos cabecera
      tituloVal = fromMaybe "" (bloqueId cabecera)
      inicioVal = fromMaybe "" (lcValor <$> lookupCampo "INICIO" camposCabecera)
      recetasVal = mapMaybe (parsearRecetaVal . lcValor)
                     (filter (\c -> lcClave c == "RECETA") (bloqueCampos cabecera))
      metaVal = fromMaybe (LlegarASala "") $
        lookupCampo "META" camposCabecera >>= parsearMetaVal . lcValor
      mapaObjetos = Map.fromList [ (fromMaybe "" (bloqueId o), construirObjeto o) | o <- objetosData ]
      mapaSalas   = Map.fromList [ (fromMaybe "" (bloqueId s), construirSala s)   | s <- salasData ]
  in Mundo tituloVal inicioVal metaVal mapaSalas mapaObjetos recetasVal

construirObjeto :: BloqueData -> Objeto
construirObjeto b =
  let objId' = fromMaybe "" (bloqueId b)
      desc'  = fromMaybe "" (lcValor <$> lookupCampo "DESC" (bloqueCampos b))
  in Objeto objId' desc'

construirSala :: BloqueData -> Sala
construirSala b =
  let salaId' = fromMaybe "" (bloqueId b)
      desc'   = fromMaybe "" (lcValor <$> lookupCampo "DESC" (bloqueCampos b))
      objs    = map lcValor (filter (\c -> lcClave c == "OBJETO") (bloqueCampos b))
      salidasData = filter (\c -> lcClave c == "SALIDA") (bloqueCampos b)
      salidasParsed = mapMaybe parsearSalidaEstructurada salidasData
  in Sala salaId' desc' salidasParsed objs

parsearSalidaEstructurada :: LineaClave -> Maybe Salida
parsearSalidaEstructurada campo = do
  (_, destino, requisito, direccion, _) <- parsearSalida "" campo
  pure (Salida direccion destino requisito)

-- funcion Principal

-- | Lee y valida el archivo de mundo en dos fases: sintaxis (V1) y semántica (V2-V10).
leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo input =
  case parsearSintaxis input of
    Left erroresV1 -> Left erroresV1
    Right (cabecera, objetosData, salasData) ->
        let erroresSemanticos = validarSemantica cabecera objetosData salasData
        in if not (null erroresSemanticos)
            then Left erroresSemanticos
        else --error aqui (doble Then)
            Right (construirMundo cabecera objetosData salasData)

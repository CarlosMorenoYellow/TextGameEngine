module Motor.Comandos (leerComando) where

import Data.Char (toLower)
import Motor.Tipos

leerComando :: String -> Maybe Comando
leerComando entrada =
  case words entrada of
    [] -> Nothing
    (comando:argumentos) ->
      let nombre = map toLower comando
          args = map (map toLower) argumentos
          objeto = unwords argumentos
      in case nombre of
        "ir" ->
          case args of
            [direccion] -> Ir <$> leerDireccion direccion
            _            -> Nothing

        "mirar" ->
          if null args then Just Mirar else Nothing

        "tomar"  -> objetoNoVacio Tomar objeto
        "coger"  -> objetoNoVacio Tomar objeto
        "soltar" -> objetoNoVacio Soltar objeto
        "examinar" -> objetoNoVacio Examinar objeto

        "inventario" -> comandoSinArgumentos Inventario args
        "inv"         -> comandoSinArgumentos Inventario args
        "ayuda"       -> comandoSinArgumentos Ayuda args
        "salir"       -> comandoSinArgumentos Salir args

        "guardar" -> ruta Guardar argumentos
        "cargar"  -> ruta Cargar argumentos

        "combinar" -> leerCombinacion argumentos args

        _ -> Nothing
  where
    objetoNoVacio constructor objeto
      | null (words objeto) = Nothing
      | otherwise = Just (constructor objeto)

    comandoSinArgumentos constructor args
      | null args = Just constructor
      | otherwise = Nothing

    ruta constructor argumentos
      | null argumentos = Nothing
      | otherwise = Just (constructor (unwords argumentos))

leerDireccion :: String -> Maybe Direccion
leerDireccion palabra =
  case palabra of
    "norte"  -> Just Norte
    "sur"    -> Just Sur
    "este"   -> Just Este
    "oeste"  -> Just Oeste
    "arriba" -> Just Arriba
    "abajo"  -> Just Abajo
    _        -> Nothing

leerCombinacion :: [String] -> [String] -> Maybe Comando
leerCombinacion argumentos argumentosMinuscula =
  case break (== "con") argumentosMinuscula of
    ([], _) -> Nothing
    (_, []) -> Nothing
    (antes, "con":despues)
      | null despues
          || "con" `elem` antes
          || "con" `elem` despues -> Nothing
      | otherwise ->
          Just (Combinar (unwords antes) (unwords despues))
    _ -> Nothing

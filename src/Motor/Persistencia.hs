module Motor.Persistencia
  ( guardarPartida
  , cargarPartida
  ) where

import Control.Exception (IOException, evaluate, try)
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)
import Motor.Tipos

guardarPartida :: FilePath -> Estado -> IO (Either String ())
guardarPartida ruta estado = do
  resultado <-
    (try (writeFile ruta (show estado))
      :: IO (Either IOException ()))
  pure (either (Left . show) Right resultado)

cargarPartida :: FilePath -> Mundo -> IO (Either String Estado)
cargarPartida ruta mundoActual = do
  lectura <-
    (try $ do
      contenido <- readFile ruta
      _ <- evaluate (length contenido)
      pure contenido
      :: IO (Either IOException String))

  case lectura of
    Left errorIO ->
      pure (Left (show errorIO))

    Right contenido ->
      case readMaybe contenido of
        Nothing ->
          pure (Left "El archivo de partida está corrupto o tiene un formato inválido.")

        Just estado
          | not (partidaEsDelMundo estado mundoActual) ->
              pure (Left "La partida pertenece a otro mundo.")
          | Map.notMember (posicion estado) (salas mundoActual) ->
              pure (Left "La partida contiene una posición inexistente.")
          | any (`Map.notMember` objectos mundoActual) (inventario estado) ->
              pure (Left "La partida contiene objetos inexistentes en el inventario.")
          | inventarioDuplicado (inventario estado) ->
              pure (Left "La partida contiene objetos repetidos en el inventario.")
          | objetosDeSalasInvalidos estado mundoActual ->
              pure (Left "La partida contiene objetos inválidos o repetidos en las salas.")
          | otherwise ->
              pure (Right estado)

partidaEsDelMundo :: Estado -> Mundo -> Bool
partidaEsDelMundo estado mundoActual =
  let mundoGuardado = mundo estado
  in titulo mundoGuardado == titulo mundoActual
     && inicio mundoGuardado == inicio mundoActual
     && meta mundoGuardado == meta mundoActual
     && objectos mundoGuardado == objectos mundoActual
     && receta mundoGuardado == receta mundoActual
     && salasCompatibles (salas mundoGuardado) (salas mundoActual)

salasCompatibles :: Map.Map String Sala -> Map.Map String Sala -> Bool
salasCompatibles guardadas actuales =
  Map.size guardadas == Map.size actuales
  && all existeYEsCompatible (Map.toList guardadas)
  where
    existeYEsCompatible (identificador, salaGuardada) =
      case Map.lookup identificador actuales of
        Nothing -> False
        Just salaActual ->
          salId salaGuardada == salId salaActual
          && salDesc salaGuardada == salDesc salaActual
          && salidas salaGuardada == salidas salaActual

inventarioDuplicado :: [String] -> Bool
inventarioDuplicado [] = False
inventarioDuplicado (objeto:resto)
  | objeto `elem` resto = True
  | otherwise = inventarioDuplicado resto

objetosDeSalasInvalidos :: Estado -> Mundo -> Bool
objetosDeSalasInvalidos estado mundoActual =
  let objetosGuardados = concatMap salObjectos (Map.elems (salas (mundo estado)))
  in any (`Map.notMember` objectos mundoActual) objetosGuardados
     || inventarioDuplicado objetosGuardados
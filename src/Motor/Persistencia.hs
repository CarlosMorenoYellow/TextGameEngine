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
          | mundo estado /= mundoActual ->
              pure (Left "La partida pertenece a otro mundo.")
          | Map.notMember (posicion estado) (salas mundoActual) ->
              pure (Left "La partida contiene una posición inexistente.")
          | any (`Map.notMember` objectos mundoActual) (inventario estado) ->
              pure (Left "La partida contiene objetos inexistentes en el inventario.")
          | otherwise ->
              pure (Right estado)
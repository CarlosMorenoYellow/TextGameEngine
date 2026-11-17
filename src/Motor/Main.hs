module Main (main) where

import Control.Exception (IOException, evaluate, try)
import Motor.Comandos (leerComando)
import Motor.Mundo (leerMundo)
import Motor.Nucleo (ejecutar, estadoInicial, metaCumplida)
import Motor.Persistencia (cargarPartida, guardarPartida)
import Motor.Tipos
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, isEOF, stdout)

main :: IO ()
main = do
  argumentos <- getArgs
  case argumentos of
    [archivo] -> iniciar False archivo
    ["--validar", archivo] -> iniciar True archivo
    _ -> do
      putStrLn "Uso: aventura [--validar] MUNDO"
      exitFailure

iniciar :: Bool -> FilePath -> IO ()
iniciar soloValidar archivo = do
  lectura <-
    (try $ do
      contenido <- readFile archivo
      _ <- evaluate (length contenido)
      pure contenido
      :: IO (Either IOException String))

  case lectura of
    Left errorIO -> do
      putStrLn ("Error: " ++ show errorIO)
      exitFailure

    Right contenido ->
      case leerMundo contenido of
        Left errores -> do
          mapM_ (putStrLn . ("Error: " ++) . show) errores
          exitFailure

        Right mundoActual
          | soloValidar -> do
              putStrLn "Mundo válido."
              exitSuccess

          | otherwise -> do
              putStrLn (titulo mundoActual)
              let estado = estadoInicial mundoActual
                  (mensajes, _) = ejecutar Mirar estado
              mapM_ putStrLn mensajes
              if metaCumplida estado
                then putStrLn "*** ¡Has ganado! ***"
                else ciclo mundoActual estado

ciclo :: Mundo -> Estado -> IO ()
ciclo mundoActual estado = do
  fin <- isEOF
  if fin
    then pure ()
    else do
      putStr "> "
      hFlush stdout
      entrada <- getLine
      case leerComando entrada of
        Nothing -> do
          putStrLn "Comando inválido. Escribe 'ayuda' para ver los comandos."
          ciclo mundoActual estado

        Just Salir ->
          putStrLn "Hasta luego."

        Just (Guardar ruta) -> do
          resultado <- guardarPartida ruta estado
          case resultado of
            Left errorTexto -> putStrLn ("Error al guardar: " ++ errorTexto)
            Right () -> putStrLn "Partida guardada."
          ciclo mundoActual estado

        Just (Cargar ruta) -> do
          resultado <- cargarPartida ruta mundoActual
          case resultado of
            Left errorTexto -> do
              putStrLn ("Error al cargar: " ++ errorTexto)
              ciclo mundoActual estado
            Right estadoNuevo -> do
              putStrLn "Partida cargada."
              if metaCumplida estadoNuevo
                then putStrLn "*** ¡Has ganado! ***"
                else ciclo mundoActual estadoNuevo

        Just comando -> do
          let (mensajes, estadoNuevo) = ejecutar comando estado
          mapM_ putStrLn mensajes
          if metaCumplida estadoNuevo
            then pure ()
            else ciclo mundoActual estadoNuevo

module Main (main) where

import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import Motor.Comandos (leerComando)
import Motor.Nucleo (ejecutar, estadoInicial, metaCumplida)
import Motor.Tipos
import System.Exit (exitFailure)

main :: IO ()
main = do
  let pruebas =
        [ ("mirar", leerComando "mirar" == Just Mirar)
        , ("direccion", leerComando "ir norte" == Just (Ir Norte))
        , ("tomar", leerComando "tomar llave" == Just (Tomar "llave"))
        , ("sinonimo coger", leerComando "coger llave" == Just (Tomar "llave"))
        , ("soltar", leerComando "soltar llave" == Just (Soltar "llave"))
        , ("examinar", leerComando "examinar llave" == Just (Examinar "llave"))
        , ("inventario", leerComando "inventario" == Just Inventario)
        , ("sinonimo inv", leerComando "inv" == Just Inventario)
        , ("ayuda", leerComando "ayuda" == Just Ayuda)
        , ("salir", leerComando "salir" == Just Salir)
        , ("guardar", leerComando "guardar partida.txt" == Just (Guardar "partida.txt"))
        , ("cargar", leerComando "cargar partida.txt" == Just (Cargar "partida.txt"))
        , ("combinar", leerComando "combinar a con b" == Just (Combinar "a" "b"))
        , ("comando inválido", leerComando "volar" == Nothing)
        , ("combinación inválida", leerComando "combinar a b" == Nothing)
        , ("estado inicial", posicion (estadoInicial mundoPrueba) == "plaza")
        , ("bloqueo sin llave", fst (ejecutar (Ir Norte) (estadoInicial mundoPrueba)) /= [])
        , ("tomar llave", inventario estadoConLlave == ["llave"])
        , ("mover con llave", posicion estadoEnBiblioteca == "biblioteca")
        , ("meta de llevar objeto", metaCumplida estadoEnBiblioteca)
        , ("combinar receta", "c" `elem` inventario estadoCombinado)
        ]

  resultados <- mapM comprobar pruebas
  unless (and resultados) exitFailure

comprobar :: (String, Bool) -> IO Bool
comprobar (nombre, resultado) = do
  putStrLn ((if resultado then "OK: " else "FALLÓ: ") ++ nombre)
  pure resultado

estadoConLlave :: Estado
estadoConLlave =
  snd (ejecutar (Tomar "llave") (estadoInicial mundoPrueba))

estadoEnBiblioteca :: Estado
estadoEnBiblioteca =
  snd (ejecutar (Ir Norte) estadoConLlave)

estadoCombinado :: Estado
estadoCombinado =
  snd (ejecutar (Combinar "a" "b") (estadoInicial mundoPrueba))

mundoPrueba :: Mundo
mundoPrueba =
  Mundo
    { titulo = "Mundo de prueba"
    , inicio = "plaza"
    , meta = LlevarObjetoASala "llave" "biblioteca"
    , salas = Map.fromList
        [ ("plaza", Sala "plaza" "Una plaza."
            [Salida Norte "biblioteca" (Just "llave")]
            ["llave", "a", "b"])
        , ("biblioteca", Sala "biblioteca" "Una biblioteca." [] [])
        ]
    , objectos = Map.fromList
        [ ("llave", Objeto "llave" "Una llave.")
        , ("a", Objeto "a" "Objeto a.")
        , ("b", Objeto "b" "Objeto b.")
        , ("c", Objeto "c" "Objeto c.")
        ]
    , receta = [Receta "a" "b" "c"]
    }
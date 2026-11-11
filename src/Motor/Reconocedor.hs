import Data.Char(isSpace)


-- Envoltorio ligero para ReadS a

newtype Reconocedor a = Reconocedor {
    runReconocedor :: String -> [(a, String)]
}

-- transforma resultado final de un reconocedor mediante
-- una funcion f sin alterar el comportamiento ni el resto
instance Functor Reconocedor where
    fmap f (Reconocedor r) = Reconocedor $ \input ->
        [(f val, resto) | (val, resto) <- r input]

-- Permite ejecucion secuencial de varios reconocedores y
-- combina sus resultados cuando los pasos siguientes no
-- dependen del valor obtenido en los pasos anteriores
instance Applicative Reconocedor where
    pure x = Reconocedor $ \input -> [(x, input)]
    (Reconocedor rf) <*> (Reconocedor rx) = Reconocedor $ \input ->
        [ (f x, resto2)
        | (f, resto1) <- rf input
        , (x, resto2) <- rx resto1
        ]

-- Habilita la notacion 'do' de Haskell para encadenar reconocedores
-- secuenciales. El resultado del primer paso determina que parser
-- se debe ejecutar despues.
instance Monad Reconocedor where
    return = pure
    (Reconocedor rx) >>= f = Reconocedor $ \input ->
        [ res
        | (x, resto1) <- rx input,
        res <- runReconocedor (f x) resto1
        ]


-- Seccion de combinadores basicos
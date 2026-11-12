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

-- Reconoce un caracter que cumpla con un predicado
satisface ::(Char -> Bool) -> Reconocedor Char
satisface p = Reconocedor $ \input -> case input of
    (c:cs) | p c -> [(c, cs)]
    _            -> []

-- Reconoce un caracter especifico
caracter :: Char -> Reconocedor Char
caracter c = satisface (== c)

-- Reconoce una cadena exacta
cadena :: String -> Reconocedor String
cadena []           = pure []
cadena (c:cs) = do
    _ <- caracter c
    _ <- cadena cs
    pure (c:cs)

-- Ignora espacios en blanco (sin saltos de linea)
espacio :: Reconocedor ()
espacio = Reconocedor $ \input ->
    [((), dropWhile (\c -> isSpace c && c /= '\n') input)]

-- Ejecuta un reconocedor y devuelve Nothing si falla
opcional :: Reconocedor a -> Reconocedor (Maybe a)
opcional (Reconocedor r) = Reconocedor $ \input ->
    case r input of
        []  -> [(Nothing, input)]
        xs  -> [(Just val, resto) | (val, resto) <- xs]

-- Aplica un reconocedor cero o mas veces
muchos :: Reconocedor a -> Reconocedor [a]
muchos r = alMenosUno r `oBien` pure []
    where
        oBien (Reconocedor r1) (Reconocedor r2) = Reconocedor $ \input ->
            case r1 input of
                [] -> r2 input
                res -> res

-- Aplica un reconocedor una o mas veces
alMenosUno :: Reconocedor a -> Reconocedor [a]
alMenosUno r = do
    x <- r
    xs <- muchos r
    pure (x:xs)

-- Seccion de combinadores especificos para el formato de Mundo


import Data.Map.Strict(Map)
import Data.Char (GeneralCategory(Surrogate))
import GHC.Exts.Heap (ClosureType(SMALL_MUT_ARR_PTRS_DIRTY))
import Data.String (IsString)

data Direccion = Norte
            | Sur
            | Este 
            | Oeste 
            | Arriba 
            | Abajo 
            deriving (Eq, Ord, Show, Read)

data Objeto = Objeto {
    objId :: String,
    objDesc :: String
} deriving (Eq, Show, Read)

data Salida = Salida {
    salidaDir :: Direccion,
    salidaDestino :: String,
    salidaRequisito :: Maybe String
} deriving(Eq,Show, Read)

data Sala = Sala {
    salId :: String,
    salDesc :: String,
    salidas :: [Salida],
    salObjectos :: [String]
} deriving(Eq,Show,Read)

data Meta = LlegarASala String
        | LlevarObjectoASala String String
        deriving (Eq, Show, Read)

data Mundo = Mundo {
    titulo :: String,
    inicio :: String,
    meta :: Meta,
    salas :: Map String Sala,
    objectos :: Map String Objeto
} deriving (Eq, Show, Read)

data Estado = Estado {
    posicion :: String,
    inventario :: [String],
    mundo :: Mundo
} deriving(Eq, Show, Read)

data Comando = Ir Direccion
            | Mirar 
            | Tomar String
            | Soltar String
            | Examinar String
            | Inventario
            | Guardar FilePath
            | Cargar FilePath
            | Ayuda
            | Salir 
            deriving(Eq,Show,Read)

data ErrorMundo = ErrorSintaxis Int String
                | ErrorIdDuplicadoSala String
                | ErrorIdDuplicadoObjecto String
                | ErrorSalidaSalaInexistente String String
                | ErrorSalidaRequisitoInexistente String String
                | ErrorSalidaDireccionDuplicada String Direccion
                | ErrorSalaObjetoIndefinido String String
                | ErrorObjetoEnMultiplesSalas String String
                | ErrorInicioInexistente String
                | ErrorMetaOnexistente Meta
                | ErrorSalaInalcanzable String
                | ErrorObjectoClaveNoColocado String
                deriving(Eq, Show) 

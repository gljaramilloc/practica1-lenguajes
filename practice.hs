import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)
import System.Directory (doesFileExist)

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    identificacion :: String,
    nombre :: String,
    fechaIngreso :: UTCTime,
    horaSalida :: Maybe UTCTime  
} deriving (Show, Read)

-- Función para registrar la entrada de un estudiante
registrarIngreso :: String -> String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarIngreso identificacionEstudiante nombreEstudiante tiempo estudiantes =
    Estudiante identificacionEstudiante nombreEstudiante tiempo Nothing : estudiantes

-- Función para registrar la salida de un estudiante
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida identificacionEstudiante tiempo =
    map (\e -> if identificacionEstudiante == identificacion e then e { horaSalida = Just tiempo } else e)

-- Función para buscar un estudiante
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante identificacionEstudiante =
    find (\e -> identificacionEstudiante == identificacion e)

-- Función para calcular el tiempo que un estudiante permaneció en la universidad
tiempoEnUniversidad :: Estudiante -> IO NominalDiffTime
tiempoEnUniversidad estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (fechaIngreso estudiante)

-- Función para guardar la información de los estudiantes
guardarEstudiantes :: [Estudiante] -> IO ()
guardarEstudiantes estudiantes = do
    withFile "estudiantes.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map show estudiantes))
    putStrLn "Información de estudiantes guardada."

-- Función para cargar estudiantes desde el archivo
cargarEstudiantes :: IO [Estudiante]
cargarEstudiantes = do
    existe <- doesFileExist "estudiantes.txt"
    if not existe
        then return []
        else catch (withFile "estudiantes.txt" ReadMode $ \h -> do
                contenido <- hGetContents h
                contenido `deepseq` return (map read (lines contenido)))
            (\e -> do
                putStrLn $ "Error al leer el archivo: " ++ show (e :: SomeException)
                return [])

-- Función para mostrar un estudiante
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante id nombre fechaIngreso horaSalida) =
    "ID: " ++ id ++ ", Nombre: " ++ nombre ++ 
    ", Ingreso: " ++ show fechaIngreso ++ 
    ", Salida: " ++ maybe "No ha salido" show horaSalida

-- Función para listar los estudiantes
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes registrados."
listarEstudiantes estudiantes = do
    putStrLn "Lista de estudiantes:"
    mapM_ (putStrLn . mostrarEstudiante) estudiantes

-- Función principal del programa
main :: IO ()
main = do
    estudiantes <- cargarEstudiantes
    putStrLn "¡Bienvenido al Sistema de Gestión de Estudiantes!"
    cicloPrincipal estudiantes

-- Ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal estudiantes = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar ingreso"
    putStrLn "2. Registrar salida"
    putStrLn "3. Buscar estudiante"
    putStrLn "4. Listar estudiantes"
    putStrLn "5. Salir"
    opcion <- getLine

    case opcion of
        "1" -> do
            putStrLn "Ingrese la identificación del estudiante:"
            idEst <- getLine
            putStrLn "Ingrese el nombre del estudiante:"
            nombreEst <- getLine
            tiempoActual <- getCurrentTime
            let nuevosEstudiantes = registrarIngreso idEst nombreEst tiempoActual estudiantes
            putStrLn $ "Estudiante " ++ idEst ++ " registrado."
            guardarEstudiantes nuevosEstudiantes
            cicloPrincipal nuevosEstudiantes

        "2" -> do
            putStrLn "Ingrese la identificación del estudiante para registrar salida:"
            idEst <- getLine
            tiempoActual <- getCurrentTime
            let nuevosEstudiantes = registrarSalida idEst tiempoActual estudiantes
            putStrLn $ "Salida registrada para el estudiante " ++ idEst ++ "."
            guardarEstudiantes nuevosEstudiantes
            cicloPrincipal nuevosEstudiantes

        "3" -> do
            putStrLn "Ingrese la identificación del estudiante a buscar:"
            idEst <- getLine
            case buscarEstudiante idEst estudiantes of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnUniversidad estudiante
                    putStrLn $ "Estudiante encontrado: " ++ nombre estudiante
                    putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado."
            cicloPrincipal estudiantes

        "4" -> do
            listarEstudiantes estudiantes
            cicloPrincipal estudiantes

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida, intente nuevamente."
            cicloPrincipal estudiantes



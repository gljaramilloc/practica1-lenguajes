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
    fechaEgreso :: Maybe UTCTime  -- Usamos Maybe para representar que el estudiante aún está en la universidad o ya egresó
} deriving (Show, Read)

-- Función para registrar la entrada de un estudiante a la universidad
registrarIngreso :: String -> String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarIngreso identificacionEstudiante nombreEstudiante tiempo estudiantes =
    Estudiante identificacionEstudiante nombreEstudiante tiempo Nothing : estudiantes

-- Función para registrar la salida de un estudiante de la universidad
registrarEgreso :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEgreso identificacionEstudiante tiempo estudiantes =
    map (\e -> if identificacionEstudiante == identificacion e then e { fechaEgreso = Just tiempo } else e) estudiantes

-- Función para buscar un estudiante por su identificación en la universidad
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante identificacionEstudiante estudiantes =
    find (\e -> identificacionEstudiante == identificacion e && isNothing (fechaEgreso e)) estudiantes
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un estudiante permaneció en la universidad
tiempoEnUniversidad :: Estudiante -> IO NominalDiffTime
tiempoEnUniversidad estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (fechaIngreso estudiante)

-- Función para guardar la información de los estudiantes en un archivo de texto
guardarEstudiantes :: [Estudiante] -> IO ()
guardarEstudiantes estudiantes = do
    withFile "estudiantes.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarEstudiante estudiantes))
    putStrLn "Información de estudiantes guardada en el archivo estudiantes.txt."

-- Función para cargar la información de los estudiantes desde un archivo de texto
cargarEstudiantes :: IO [Estudiante]
cargarEstudiantes = do
    existe <- doesFileExist "estudiantes.txt"
    if not existe
        then return []
        else do
            contenido <- withFile "estudiantes.txt" ReadMode $ \h -> do -- Profe lo quiero mucho, ayudenos a pasar la materia >: 
                contenido <- hGetContents h
                contenido `deepseq` return contenido
            let lineas = lines contenido
            return (map leerEstudiante lineas)
    where
        leerEstudiante linea = read linea :: Estudiante

-- Función para mostrar la información de un estudiante como cadena de texto
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante identificacion nombre fechaIngreso fechaEgreso) =
    "Estudiante {identificacion = \"" ++ identificacion ++ "\", nombre = \"" ++ nombre ++ "\", fechaIngreso = " ++ show fechaIngreso ++ ", fechaEgreso = " ++ maybe "Nothing" show fechaEgreso ++ "}"

-- Función para listar los estudiantes en la universidad
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes en la universidad."
listarEstudiantes estudiantes = do
    putStrLn "Estudiantes en la universidad:"
    mapM_ (putStrLn . mostrarEstudiante) estudiantes

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar los estudiantes desde el archivo de texto
    estudiantes <- cargarEstudiantes
    putStrLn "¡Bienvenido al Sistema de Gestión de Estudiantes de la Universidad!"

    -- Ciclo principal del programa
    cicloPrincipal estudiantes

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal estudiantes = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar ingreso de estudiante"
    putStrLn "2. Registrar egreso de estudiante"
    putStrLn "3. Buscar estudiante por identificación"
    putStrLn "4. Listar estudiantes"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la identificación del estudiante:"
            identificacionEstudiante <- getLine
            putStrLn "Ingrese el nombre del estudiante:"
            nombreEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let estudiantesActualizados = registrarIngreso identificacionEstudiante nombreEstudiante tiempoActual estudiantes
            putStrLn $ "Estudiante con identificación " ++ identificacionEstudiante ++ " ingresado a la universidad."
            guardarEstudiantes estudiantesActualizados
            cicloPrincipal estudiantesActualizados

        "2" -> do
            putStrLn "Ingrese la identificación del estudiante a egresar:"
            identificacionEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let estudiantesActualizados = registrarEgreso identificacionEstudiante tiempoActual estudiantes
            putStrLn $ "Estudiante con identificación " ++ identificacionEstudiante ++ " egresado de la universidad."
            guardarEstudiantes estudiantesActualizados
            cicloPrincipal estudiantesActualizados

        "3" -> do
            putStrLn "Ingrese la identificación del estudiante a buscar:"
            identificacionEstudiante <- getLine
            case buscarEstudiante identificacionEstudiante estudiantes of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnUniversidad estudiante
                    putStrLn $ "El estudiante con identificación " ++ identificacionEstudiante ++ " se encuentra en la universidad."
                    putStrLn $ "con nombre: " ++ nombre estudiante
                    putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la universidad."
            cicloPrincipal estudiantes

        "4" -> do
            listarEstudiantes estudiantes
            cicloPrincipal estudiantes

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal estudiantes
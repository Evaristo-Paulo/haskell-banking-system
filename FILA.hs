module FILA where
import System.IO
import Data.Char (toUpper);
import Data.Time;
import Numeric;
import Data.List;

-- EVARISTO DOMINGOS PAULO
-- 3º ANO - 2021

-- Menu fila de espera
fila :: IO ()
fila = do 
  putStrLn "\n\n\n"
  putStrLn "                      Banco PJ | Fila de espera               "
  putStrLn "      --------------------------------------------------------"
  putStrLn "      -                                                      -"
  putStrLn "      -                   A. Criação de conta                -"
  putStrLn "      -                   B. Depósito                        -"
  putStrLn "      -                   C. Levantamento                    -"
  putStrLn "      -                   D. Transferência                   -"
  putStrLn "      -                   E. Extracto                        -"
  putStrLn "      -                   F. Consulta                        -"
  putStrLn "      -                                                      -"
  putStrLn "      --------------------------------------------------------\n"
  putStr   "                    Por favor, escolha uma opção: "
  fila_op <- getLine;
  putStrLn "\n"
  case (string_minuscula_para_maiscula fila_op) of
    "A" -> do
      putStrLn "                      Banco PJ | Criação de conta             "
      putStrLn "      --------------------------------------------------------"
      tirar_senha ('A')
      fila
    "B" -> do
      putStrLn "                        Banco PJ | Depósito                     "
      putStrLn "      --------------------------------------------------------"
      tirar_senha ('B')
      fila
    "C" -> do
      putStrLn "                        Banco PJ | Levantamento             "
      putStrLn "      --------------------------------------------------------"
      tirar_senha ('C')
      fila
    "D" -> do
      putStrLn "                        Banco PJ | Transferência             "
      putStrLn "      --------------------------------------------------------"
      tirar_senha ('D')
      fila
    "E" -> do
      putStrLn "                        Banco PJ | Extracto             "
      putStrLn "      --------------------------------------------------------"
      tirar_senha ('E')
      fila
    "F" -> do
      putStrLn "                          Banco PJ | Consulta             "
      putStrLn "      --------------------------------------------------------"
      tirar_senha ('F')
      fila
    otherwise -> do
      putStrLn "\n       Não conseguimos localizar esta opção, tenta novamente"
      fila

-- Transformar uma string minúsculo para maísculo
string_minuscula_para_maiscula :: String -> String
string_minuscula_para_maiscula cs = [ toUpper c | c <- cs ];

-- Tirar senha e entrar na fila de espera
tirar_senha :: (Char ) -> IO ()
tirar_senha (servico) = do
    hora_chegada <- getCurrentTime; 
    putStrLn("\n                          Sua senha é "++([servico])++""++((take 2 (drop 17 (show hora_chegada)))));

    r_arquivo <- openFile "fila" ReadMode;
    contents <- hGetContents r_arquivo;

    if ( length contents ) == 0 then do 
        guardar_senha (show ([servico])++""++((take 2 (drop 17 (show hora_chegada)))));
    else do
        guardar_senha (","++show ([servico])++""++((take 2 (drop 17 (show hora_chegada)))));

-- Guardar a senha no ficheiro fila
guardar_senha :: String -> IO ()
guardar_senha senha = do
    arquivo <- openFile "fila" AppendMode;
    hPutStr arquivo senha;
    hFlush arquivo;
    hClose arquivo;

-- Pegar o ano do sistema
anoActual :: IO (Integer,Int,Int)
anoActual = getCurrentTime >>= return . toGregorian . utctDay

-- Substituir espacos em branco por vírgula
substituir_espaco_por_virgula [] = []
substituir_espaco_por_virgula (x:xs) = 
     if x == ' ' 
     then ',' : substituir_espaco_por_virgula xs 
     else x : substituir_espaco_por_virgula xs
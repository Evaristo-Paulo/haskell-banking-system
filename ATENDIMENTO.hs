module ATENDIMENTO where
import FILA;
import System.IO
import qualified Data.Map as Map
import Data.Map ((!))
import System.Random(randomRIO)
import Data.List(delete)
import Data.List

-- EVARISTO DOMINGOS PAULO
-- 3º ANO - 2021

-- Menu para os usuários autorizados
atendimento_geral_autenticado :: IO ()
atendimento_geral_autenticado = do 
  putStrLn "\n\n\n"
  putStrLn "                     Banco PJ | Atendimento geral               "
  putStrLn "      --------------------------------------------------------"
  putStrLn "      -                                                      -"
  putStrLn "      -                   1. Chamar                          -"
  putStrLn "      -                   2. Sair                            -"
  putStrLn "      -                                                      -"
  putStrLn "      --------------------------------------------------------\n"
  putStr   "                    Por favor, escolha um serviço: "
  atendimento_op <- getLine;
  f_relatorios <- openFile "relatorios" ReadMode;
  d_relatorios <- hGetContents f_relatorios;
    -- Actualizar ficheiro relatorios-copia
  f_relatorio_copia <- openFile "relatorios-copia" WriteMode;
  if (length d_relatorios) == 0 then do
    hPutStr f_relatorio_copia (read (show "[]"))
  else do
    hPutStr f_relatorio_copia (read (show d_relatorios))

  hFlush f_relatorio_copia;
  hClose f_relatorio_copia;
  hClose f_relatorios;
  case (atendimento_op) of
    "1" -> do
      -- Chamar o ficheiro que contém as senhas geradas
      senhas_fila <- readFile "fila"
      -- Gerar um valor aleatório de zero até ao tamanho da lista de senhas
      v_aleatorio <- randomRIO (0, ( length( string_para_lista senhas_fila ) - 1)::Int)
      if ( length senhas_fila ) == 0 then do 
        putStrLn $ "\n                  Nenhum cliente na fila de espera"
        atendimento_geral_autenticado
      else do
        putStrLn $ "\n                    Clientes na fila de espera: " ++ show ( length( string_para_lista senhas_fila ))
      
      putStrLn $ "\n     Cliente com a senha "++ (head(drop v_aleatorio (string_para_lista senhas_fila))) ++ " , por favor, dirige-se ao balcão"
      putStrLn $ "\n\n\n"
      case (( take 3 (head(drop v_aleatorio  (string_para_lista senhas_fila))))) of
        "\"A\""-> do
          putStrLn "                     Banco PJ | Criação de conta              "
          putStrLn "      --------------------------------------------------------\n"
          putStr $ "                      Nome completo: "
          cl_nome <- getLine
          putStrLn "                          Data de Nascimento"
          putStr $ "                                Dia: "
          cl_nasc_dia <- getLine
          putStr $ "                         Mes (1/12): "
          cl_nasc_mes <- getLine
          putStr $ "                                Ano: "
          cl_nasc_ano <- getLine
          putStr $ "                                 ID: "
          cl_id <- getLine
          putStr $ "                           Telefone: "
          cl_telefone <- getLine
          putStrLn   "\n\n                           Confirma os dados              "
          putStrLn "             ---------------------------------------------"
          putStrLn $ "                      Nome completo: " ++ cl_nome
          putStrLn $ "                 Data de nascimento: " ++ cl_nasc_dia ++ "-" ++ cl_nasc_mes ++ "-" ++ cl_nasc_ano
          putStrLn $ "                                 ID: " ++ cl_id
          putStrLn $ "                           Telefone: " ++ cl_telefone
          putStrLn "             ---------------------------------------------\n"

          f_clientes <- openFile "clientes" ReadMode;
          d_clientes <- hGetContents f_clientes;
          -- Actualizar ficheiro clientes-copia
          f_clientes_copia <- openFile "clientes-copia" WriteMode;

          if (length d_clientes) == 0 then do
            hPutStr f_clientes_copia (read (show "[]"))
          else do
            hPutStr f_clientes_copia (read (show d_clientes))
          
          hFlush f_clientes_copia;
          hClose f_clientes_copia;
          hClose f_clientes;

          putStrLn "                            1. Confirma"
          putStrLn "                            2. Cancela\n"
          putStr "                      Por favor, escolha a opção: "
          c_op <- getLine
          case ( c_op ) of
            "1" -> do
              (year, month, day) <- anoActual
              let idade = ((year)-(read cl_nasc_ano))
              if idade > 17 then do
                -- Gerar um número aleatório entre 1000 até 5999
                cl_conta <- randomRIO (1000,5999::Int)
                -- Chamar função criar conta
                criar_conta ( cl_nome, (read cl_nasc_dia), (read cl_nasc_mes), (read cl_nasc_ano), cl_id, cl_telefone, show(cl_conta) ++ "PJ", read "0",day, month, read( show year))
                putStrLn $ "\n               Atenção, aponte o teu nº conta " ++ show cl_conta ++ "PJ"
                putStrLn "\n                       Conta criada com sucesso"
                remover_senha_cliente_atendido (unwords (delete (head(drop v_aleatorio  (string_para_lista senhas_fila))) (string_para_lista senhas_fila)));
              else do
                putStrLn "             Deve ter mais de 17 anos para abrir conta"
                atendimento_geral_autenticado
            "2" -> do
              atendimento_geral_autenticado
            otherwise -> do
              putStrLn "\n       Não conseguimos localizar esta opção, tenta novamente"
              atendimento_geral_autenticado

          atendimento_geral_autenticado
        "\"B\""-> do
          putStrLn "                         Banco PJ | Depósito                  "
          putStrLn "      --------------------------------------------------------\n"
          putStr "                             Nº conta: "
          n_conta <- getLine
          putStr "                                Valor: "
          v_depositar <- getLine
          f_clientes <- openFile "clientes" ReadMode
          dado <- hGetContents f_clientes
          if (length dado) == 0 then do
            putStrLn "\n                           Não há cliente"
            atendimento_geral_autenticado
          else do
            putStrLn "\n"
          if length (actualizar_dado_cliente ( (read dado) , n_conta )) > 0 then do
            depositar ((actualizar_dado_cliente ( (read dado) , n_conta )), (read v_depositar))
            putStrLn $ "\n   Foi efectuado um depósito no valor de " ++ v_depositar ++ ",00 kz na conta " ++ mensagem_operacao_efectuada (actualizar_dado_cliente ( (read dado) , n_conta )) ++ "\n"
            remover_senha_cliente_atendido (unwords (delete (head(drop v_aleatorio  (string_para_lista senhas_fila))) (string_para_lista senhas_fila)));
          else do
            putStrLn "\n                 Não conseguimos localizar esta conta"
          atendimento_geral_autenticado
        "\"C\""-> do
          putStrLn "                        Banco PJ | Levantamento               "
          putStrLn "      --------------------------------------------------------\n"
          putStr "                             Nº conta: "
          n_conta <- getLine
          putStr "                                Valor: "
          v_levantar <- getLine
          f_clientes <- openFile "clientes" ReadMode
          dado <- hGetContents f_clientes
          if (length dado) == 0 then do
            putStrLn "\n                           Não há cliente"
            atendimento_geral_autenticado
          else do
            putStrLn "\n"
          if length (actualizar_dado_cliente ( (read dado) , n_conta )) > 0 then do
            levantar ((actualizar_dado_cliente ( (read dado) , n_conta )), (read v_levantar))
            putStrLn $ "\n   Foi efectuado um levantamento no valor de " ++ v_levantar ++ ",00 kz na conta " ++ mensagem_operacao_efectuada (actualizar_dado_cliente ( (read dado) , n_conta )) ++ "\n"
            remover_senha_cliente_atendido (unwords (delete (head(drop v_aleatorio  (string_para_lista senhas_fila))) (string_para_lista senhas_fila)));
          else do
            putStrLn "\n                 Não conseguimos localizar esta conta"

          atendimento_geral_autenticado
        "\"D\""-> do
          putStrLn "                       Banco PJ | Transferência               "
          putStrLn "      --------------------------------------------------------\n"
          putStr "                           Nº conta origem: "
          n_conta_o <- getLine
          putStr "                          Nº conta destino: "
          n_conta_d <- getLine
          putStr "                                     Valor: "
          v_transferir <- getLine
          f_clientes <- openFile "clientes" ReadMode
          dado <- hGetContents f_clientes
          if (length dado) == 0 then do
            putStrLn "\n                           Não há cliente"
            atendimento_geral_autenticado
          else do
            putStrLn "\n"
          if length (actualizar_dado_cliente ( (read dado) , n_conta_o )) > 0 then do
            if length (actualizar_dado_cliente ( (read dado) , n_conta_d )) > 0 then do
              transferir ((actualizar_dado_cliente ( (read dado) , n_conta_o ) ++ actualizar_dado_cliente ( (read dado) , n_conta_d ) ), (read v_transferir))
              putStrLn $ "\nFoi efectuado uma transferência no valor de " ++ v_transferir ++ ",00 kz na conta " ++ mensagem_operacao_efectuada (actualizar_dado_cliente ( (read dado) , n_conta_o )) ++ " para a conta " ++ mensagem_operacao_efectuada (actualizar_dado_cliente ( (read dado) , n_conta_d )) ++ "\n"
              remover_senha_cliente_atendido (unwords (delete (head(drop v_aleatorio  (string_para_lista senhas_fila))) (string_para_lista senhas_fila)));
            else do
              putStrLn "\n               Não conseguimos localizar o nº conta destino"
          else do
            putStrLn "\n               Não conseguimos localizar o nº conta origem"

          atendimento_geral_autenticado
        "\"E\""-> do
          putStrLn "                         Banco PJ | Extracto                  "
          putStrLn "      --------------------------------------------------------\n"
          putStr "                             Nº conta: "
          n_conta <- getLine
          putStrLn "\n"
          f_relatorios <- openFile "relatorios" ReadMode
          dado <- hGetContents f_relatorios
          if (length dado) == 0 then do
            putStrLn "\n                            Não há relatório"
            atendimento_geral_autenticado
          else do
            putStrLn "\n"

          if length (extrato ( (read dado), n_conta )) > 0 then do
            putStrLn $ "\n                  "++ show (length (extrato ( (read dado), n_conta ))) ++" resultado(s) encontrado(s)\n"
            visualizar_relatorio_geral (extrato ( (read dado), n_conta))
            remover_senha_cliente_atendido (unwords (delete (head(drop v_aleatorio  (string_para_lista senhas_fila))) (string_para_lista senhas_fila)));
            atendimento_geral_autenticado
          else do
          putStrLn "\n                Não há relatório vinculada a esta conta"
          atendimento_geral_autenticado
        "\"F\""-> do
          putStrLn "                         Banco PJ | Consulta                  "
          putStrLn "      --------------------------------------------------------\n"
          putStr "                             Nº conta: "
          n_conta <- getLine
          f_clientes <- openFile "clientes" ReadMode
          dado <- hGetContents f_clientes
          if (length dado) == 0 then do
            putStrLn "\n                         Não há relatório"
            atendimento_geral_autenticado
          else do
            putStrLn "\n"
          putStr "\n\n\n"
          if length (actualizar_dado_cliente ( (read dado) , n_conta )) > 0 then do
            consultar (actualizar_dado_cliente ( (read dado) , n_conta ))
            remover_senha_cliente_atendido (unwords (delete (head(drop v_aleatorio  (string_para_lista senhas_fila))) (string_para_lista senhas_fila)));
          else do
            putStrLn "\n                 Não conseguimos localizar esta conta"
          atendimento_geral_autenticado
    "2" -> do
      atendimento
    otherwise -> do
      putStrLn "\n       Não conseguimos localizar esta opção, tenta novamente"
      atendimento_geral_autenticado

-- Menu de relatório diário
relatorio_diario :: IO ()
relatorio_diario = do 
  putStrLn "\n\n\n"
  putStrLn "                      Banco PJ | Relatório diário            "
  putStrLn "      --------------------------------------------------------"
  putStrLn "      -                                                      -"
  putStrLn "      -                   1. Criação de conta                -"
  putStrLn "      -                   2. Depósito                        -"
  putStrLn "      -                   3. Levantamento                    -"
  putStrLn "      -                   4. Transferência                   -"
  putStrLn "      -                   5. Estatística                     -"
  putStrLn "      -                   6. Voltar                          -"
  putStrLn "      -                                                      -"
  putStrLn "      --------------------------------------------------------\n"
  putStr   "                    Por favor, escolha uma opção: "
  atendimento_op <- getLine;
  putStrLn   "\n\n\n"
  case (atendimento_op) of
    "1" -> do
      putStrLn "                      Banco PJ | Criação de conta             "
      putStrLn "      --------------------------------------------------------\n"
      putStr "                              Dia: "
      rd_dia <- getLine
      putStr "                        Mes(1/12): "
      rd_mes <- getLine
      putStr "                              Ano: "
      rd_ano <- getLine
      putStrLn "\n"
      f_clientes <- openFile "clientes" ReadMode
      dado <- hGetContents f_clientes
      if (length dado) == 0 then do
        putStrLn "\n                            Não há relatório"
        relatorio_diario
      else do
        putStrLn "\n"
      if length (relatorio_diario_criacao_conta ((read dado), (read rd_dia), (read rd_mes), (read rd_ano))) > 0 then do
        putStrLn $ "\n                  "++ show (length (relatorio_diario_criacao_conta ((read dado), (read rd_dia), (read rd_mes), (read rd_ano)))) ++" resultado(s) encontrado(s)\n"
        visualizar_relatorio_geral_criacao_conta (relatorio_diario_criacao_conta ((read dado), (read rd_dia), (read rd_mes), (read rd_ano)))
        relatorio_diario
      else do
        putStrLn "\n          Não há relatório de criação de conta nesta data"
        relatorio_diario
    "2" -> do
      putStrLn "                        Banco PJ | Depósito                     "
      putStrLn "      --------------------------------------------------------\n"
      putStr "                              Dia: "
      rd_dia <- getLine
      putStr "                        Mes(1/12): "
      rd_mes <- getLine
      putStr "                              Ano: "
      rd_ano <- getLine
      putStrLn "\n"
      f_relatorios <- openFile "relatorios" ReadMode
      dado <- hGetContents f_relatorios
      if (length dado) == 0 then do
        putStrLn "\n                            Não há relatório"
        relatorio_diario
      else do
        putStrLn "\n"
      if length (relatorio_diario_tds ( (read dado), "D", (read rd_dia), (read rd_mes), (read rd_ano) )) > 0 then do
        putStrLn $ "\n                  "++ show (length (relatorio_diario_tds ( (read dado), "D", (read rd_dia), (read rd_mes), (read rd_ano) ))) ++" resultado(s) encontrado(s)\n"
        visualizar_relatorio_geral (relatorio_diario_tds ( (read dado), "D", (read rd_dia), (read rd_mes), (read rd_ano) ))
        relatorio_diario
      else do
        putStrLn "\n              Não há relatório de depósito nesta data"
        relatorio_diario
    "3" -> do
      putStrLn "                        Banco PJ | Levantamento             "
      putStrLn "      --------------------------------------------------------\n"
      putStrLn "\n"
      f_relatorios <- openFile "relatorios" ReadMode
      dado <- hGetContents f_relatorios
      putStr "                              Dia: "
      rd_dia <- getLine
      putStr "                        Mes(1/12): "
      rd_mes <- getLine
      putStr "                              Ano: "
      rd_ano <- getLine
      putStrLn "\n"
      f_relatorios <- openFile "relatorios" ReadMode
      dado <- hGetContents f_relatorios
      if (length dado) == 0 then do
        putStrLn "\n                            Não há relatório"
        relatorio_diario
      else do
        putStrLn "\n"
      if length (relatorio_diario_tds ( (read dado), "L", (read rd_dia), (read rd_mes), (read rd_ano) )) > 0 then do
        putStrLn $ "\n                  "++ show (length (relatorio_diario_tds ( (read dado), "L", (read rd_dia), (read rd_mes), (read rd_ano) ))) ++" resultado(s) encontrado(s)\n"
        visualizar_relatorio_geral (relatorio_diario_tds ( (read dado), "L", (read rd_dia), (read rd_mes), (read rd_ano) ))
        relatorio_diario
      else do
        putStrLn "\n              Não há relatório de levantamento nesta data"
        relatorio_diario
    "4" -> do
      putStrLn "                        Banco PJ | Transferência             "
      putStrLn "      --------------------------------------------------------\n"
      putStrLn "\n"
      f_relatorios <- openFile "relatorios" ReadMode
      dado <- hGetContents f_relatorios
      putStr "                              Dia: "
      rd_dia <- getLine
      putStr "                        Mes(1/12): "
      rd_mes <- getLine
      putStr "                              Ano: "
      rd_ano <- getLine
      putStrLn "\n"
      f_relatorios <- openFile "relatorios" ReadMode
      dado <- hGetContents f_relatorios
      if (length dado) == 0 then do
        putStrLn "\n                            Não há relatório"
        relatorio_diario
      else do
        putStrLn "\n"
      if length (relatorio_diario_tds ( (read dado), "T", (read rd_dia), (read rd_mes), (read rd_ano) )) > 0 then do
        putStrLn $ "\n                  "++ show (length (relatorio_diario_tds ( (read dado), "T", (read rd_dia), (read rd_mes), (read rd_ano) ))) ++" resultado(s) encontrado(s)\n"
        visualizar_relatorio_geral (relatorio_diario_tds ( (read dado), "T", (read rd_dia), (read rd_mes), (read rd_ano) ))
        relatorio_diario
      else do
        putStrLn "\n              Não há relatório de transferência nesta data"
        relatorio_diario
    "5" -> do
      putStrLn "                        Banco PJ | Estatística             "
      putStrLn "      --------------------------------------------------------\n"
      putStr "                              Em construção... "
      relatorio_diario
    "6" -> do
      atendimento
    otherwise -> do
      putStrLn "\n       Não conseguimos localizar esta opção, tenta novamente"
      relatorio_diario

-- Menu de relatorio geral
relatorio_geral :: IO ()
relatorio_geral = do 
  putStrLn "\n\n\n"
  putStrLn "                       Banco PJ | Relatório geral             "
  putStrLn "      --------------------------------------------------------"
  putStrLn "      -                                                      -"
  putStrLn "      -                   1. Criação de conta                -"
  putStrLn "      -                   2. Depósito                        -"
  putStrLn "      -                   3. Levantamento                    -"
  putStrLn "      -                   4. Transferência                   -"
  putStrLn "      -                   5. Estatística                     -"
  putStrLn "      -                   6. Voltar                          -"
  putStrLn "      -                                                      -"
  putStrLn "      --------------------------------------------------------\n"
  putStr   "                    Por favor, escolha uma opção: "
  atendimento_op <- getLine;
  putStrLn   "\n\n\n"
  case (atendimento_op) of
    "1" -> do
      putStrLn "                      Banco PJ | Criação de conta             "
      putStrLn "      --------------------------------------------------------\n"
      putStrLn "\n"
      f_clientes <- openFile "clientes" ReadMode
      dado <- hGetContents f_clientes
      if (length dado) == 0 then do
        putStrLn "\n                            Não há relatório"
        relatorio_geral
      else do
        putStrLn "\n"
      if length (relatorio_geral_criacao_conta (read dado)) > 0 then do
        putStrLn $ "\n                  "++ show (length (relatorio_geral_criacao_conta (read dado))) ++" resultado(s) encontrado(s)\n"
        visualizar_relatorio_geral_criacao_conta (relatorio_geral_criacao_conta (read dado))
        relatorio_geral
      else do
        putStrLn "\n                            Não há relatório"
        relatorio_geral
    "2" -> do
      putStrLn "                        Banco PJ | Depósito                     "
      putStrLn "      --------------------------------------------------------\n"
      putStrLn "\n"
      f_relatorios <- openFile "relatorios" ReadMode
      dado <- hGetContents f_relatorios
      if (length dado) == 0 then do
        putStrLn "\n                            Não há relatório"
        relatorio_geral
      else do
        putStrLn "\n"

      if length (relatorio_geral_tds ( (read dado), "D" )) > 0 then do
        putStrLn $ "\n                  "++ show (length (relatorio_geral_tds ( (read dado), "D" ))) ++" resultado(s) encontrado(s)\n"
        visualizar_relatorio_geral (relatorio_geral_tds ( (read dado), "D"))
        relatorio_geral
      else do
        putStrLn "\n                    Não há relatório de depósito"
        relatorio_geral
    "3" -> do
      putStrLn "                        Banco PJ | Levantamento             "
      putStrLn "      --------------------------------------------------------\n"
      putStrLn "\n"
      f_relatorios <- openFile "relatorios" ReadMode
      dado <- hGetContents f_relatorios
      if (length dado) == 0 then do
        putStrLn "\n                            Não há relatório"
        relatorio_geral
      else do
        putStrLn "\n"
      if length (relatorio_geral_tds ( (read dado), "L" )) > 0 then do
        putStrLn $ "\n                  "++ show (length (relatorio_geral_tds ( (read dado), "L" ))) ++" resultado(s) encontrado(s)\n"
        visualizar_relatorio_geral (relatorio_geral_tds ( (read dado), "L"))
        relatorio_geral
      else do
        putStrLn "\n                    Não há relatório de levantamento"
        relatorio_geral
    "4" -> do
      putStrLn "                        Banco PJ | Transferência             "
      putStrLn "      --------------------------------------------------------\n"
      putStrLn "\n"
      f_relatorios <- openFile "relatorios" ReadMode
      dado <- hGetContents f_relatorios
      if (length dado) == 0 then do
        putStrLn "\n                            Não há relatório"
        relatorio_geral
      else do
        putStrLn "\n"
      if length (relatorio_geral_tds ( (read dado), "T" )) > 0 then do
        putStrLn $ "\n                  "++ show (length (relatorio_geral_tds ( (read dado), "T" ))) ++" resultado(s) encontrado(s)\n"
        visualizar_relatorio_geral (relatorio_geral_tds ( (read dado), "T"))
        relatorio_geral
      else do
        putStrLn "\n                    Não há relatório de transferência"
        relatorio_geral
    "5" -> do
      putStrLn "                        Banco PJ | Estatística             "
      putStrLn "      --------------------------------------------------------\n"
      putStr "                              Em construção... "
      relatorio_geral
    "6" -> do
      atendimento
    otherwise -> do
      putStrLn "\n       Não conseguimos localizar esta opção, tenta novamente"
      relatorio_geral

-- Menu atendimento
atendimento :: IO()
atendimento = do
  putStrLn "\n\n\n"
  putStrLn "                       Banco PJ | Atendimento                 "
  putStrLn "      --------------------------------------------------------"
  putStrLn "      -                                                      -"
  putStrLn "      -                   1. Atendimento geral               -"
  putStrLn "      -                   2. Relatório diário                -"
  putStrLn "      -                   3. Relatório geral                 -"
  putStrLn "      -                                                      -"
  putStrLn "      --------------------------------------------------------\n"
  putStr "                     Por favor, escolha uma opção: "
  at_op <- getLine
  case at_op of
    "1" -> do
      atendimento_geral
    "2" -> do
      relatorio_diario
    "3" -> do
      relatorio_geral
    otherwise -> do
      putStrLn "\n       Não conseguimos localizar esta opção, tenta novamente"
      atendimento_geral


-- Menu atendimento geral, é necessário autenticação
atendimento_geral :: IO()
atendimento_geral = do
  putStrLn "\n\n\n"
  putStrLn "                       Banco PJ | Atendimento                 "
  putStrLn "      --------------------------------------------------------\n"
  putStr $ "                         Usuário: "
  usuario <- getLine
  putStr $ "                           Senha: "
  senha <-getLine
-- Abrir o ficheiro dos usuários autorizados
  f_usuario <- openFile "usuario" ReadMode
  dado <- hGetContents f_usuario
  if length (pesquisar_usuario ((read dado), usuario)) > 0 then do
    if autenticar_usuario ((read dado), usuario) == senha then do
      atendimento_geral_autenticado
    else do
      putStrLn "\n                    Usuário ou senha incorrecta\n"
      atendimento_geral
  else do
    putStrLn "\n                    Usuário ou senha incorrecta\n"
    atendimento_geral

-- Mostrar dados do relatório geral das contas criadas
visualizar_relatorio_geral_criacao_conta xs  = mapM_ (\(a,b,c,d,e,f,g,h,i,j,k) -> putStrLn $ "                                ID: " ++ e++ "\n                           Cliente: " ++ a ++ "\n                Data de nascimento: " ++ show b ++ "-"++ show c ++ "-"++ show d ++ "\n"++ "                             Conta: " ++ g ++ "\n                          Telefone: " ++ f ++ "\n                             Saldo: " ++ show h ++ ",00 kz\n                   Data de criação: " ++ show i ++ "-"++ show j ++ "-"++ show k ++ "\n" ) xs

-- Função que retorna todas contas criadas
relatorio_geral_criacao_conta :: [(String, Int, Int, Int, String, String, String, Int, Int, Int, Int)] ->  [(String, Int, Int, Int, String, String, String, Int, Int, Int, Int)]
relatorio_geral_criacao_conta  listaTupas = do
  filter (\(a,b,c,d,e,f,g,h,i,j,k)-> h > 0) listaTupas

-- Função que retorna todas contas criadas a partir de uma data
relatorio_diario_criacao_conta :: ([(String, Int, Int, Int, String, String, String, Int, Int, Int, Int)], Int, Int, Int) ->  [(String, Int, Int, Int, String, String, String, Int, Int, Int, Int)]
relatorio_diario_criacao_conta  (listaTupas, dia, mes, ano) = do
  filter (\(a,b,c,d,e,f,g,h,i,j,k)-> i == dia && j == mes && k == ano ) listaTupas

-- Mostrar dados do relatório geral das demais opções, excepto criação de conta
visualizar_relatorio_geral xs  = mapM_ (\(a,b,c,d,e,f,g) -> putStrLn $ "                           Cliente: " ++ a ++ "\n                          Operação: " ++ c ++ "\n                          Montante: " ++ show d ++ ",00 kz\n                              Data: " ++ show e ++ "-"++ show f ++ "-"++ show g ++ "\n") xs

-- Função que pesquisa na lista de tuplas, um cliente pelo seu número de conta e retorna uma lista com as tuplas correspondente ou uma  lista vazia
relatorio_geral_tds :: ([(String, String, String, Int, Int, Int, Int)], String ) ->  [(String, String, String, Int, Int, Int, Int)]
relatorio_geral_tds  (listaTupas, operacao) = do
  filter (\(a,b,c,d,e,f,g)-> c == operacao) listaTupas

-- Função que retorna os extractos de um cliente, pesquisando pelo nº conta
extrato :: ([(String, String, String, Int, Int, Int, Int)], String ) ->  [(String, String, String, Int, Int, Int, Int)]
extrato  (listaTupas, n_conta) = do
  filter (\(a,b,c,d,e,f,g)-> b == n_conta ) listaTupas

-- Função que retorna todos os relatórios, a partir de uma data
relatorio_diario_tds :: ([(String, String, String, Int, Int, Int, Int)], String, Int, Int, Int ) ->  [(String, String, String, Int, Int, Int, Int)]
relatorio_diario_tds  (listaTupas, operacao, dia, mes, ano ) = do
  filter (\(a,b,c,d,e,f,g)-> c == operacao && e == dia && f == mes && g == ano) listaTupas

-- Função que regista um relatório, isto é, quando uma operação foi feita (levantamento, depósito, transferência)
criar_relatorio :: (String, String, String, Int, Int, Int, Int ) -> IO ()
criar_relatorio (cl_nome, cl_conta, operacao, cl_saldo, dia, mes, ano ) = do
  let item = [( cl_nome, cl_conta, operacao, cl_saldo, dia, mes, ano )]
  f_relatorios_copia <- openFile "relatorios-copia" ReadMode
  d_relatorios_copia <- hGetContents f_relatorios_copia
  f_relatorios <- openFile "relatorios" WriteMode
  let novoRelatorioLista = item ++ (read d_relatorios_copia)
  hPutStr f_relatorios (show novoRelatorioLista)
  hFlush f_relatorios
  hClose f_relatorios

-- Função que pesquisa na lista de tuplas, um cliente pelo seu número de conta e retorna uma lista com apenas 1 tupla correspondente ou uma  lista vazia
actualizar_dado_cliente :: ([(String, Int, Int, Int, String, String, String, Int, Int, Int, Int)], String ) ->  [(String, Int, Int, Int, String, String, String, Int, Int, Int, Int)]
actualizar_dado_cliente  (listaTupas, generico) = do
  filter (\(a,b,c,d,e,f,g,h,i,j,k)-> g == generico) listaTupas
  
-- Fynção que autentica o usuário que deseja logar
autenticar_usuario :: ([(String, String)], String) -> String
autenticar_usuario (usuarios, nome) = Map.fromList usuarios ! nome

-- Transformar String em lista, com base num caracter existente (vírgula, neste caso)
-- Ex.: "A,B,C" ==> ["A","B","C"]
string_para_lista :: String -> [String]
string_para_lista [] = [""]
string_para_lista (c:cs)
   | c == ','  = "" : resto
   | otherwise = (c : head resto) : tail resto
 where
   resto = string_para_lista cs

-- Função que cria uma conta cliente
criar_conta :: (String, Int, Int, Int, String, String, String, Int, Int, Int, Int ) -> IO ()
criar_conta (cl_nome, cl_nasc_dia, cl_nasc_mes, cl_nasc_ano, cl_id, cl_telefone, cl_conta, cl_saldo, cliente_dta_dia,cliente_dta_mes,cliente_dta_ano ) = do
  let item = [( cl_nome, cl_nasc_dia, cl_nasc_mes, cl_nasc_ano, cl_id, cl_telefone, cl_conta, cl_saldo , cliente_dta_dia,cliente_dta_mes,cliente_dta_ano)]
  f_clientes_copia <- openFile "clientes-copia" ReadMode;
  d_clientes_copia <- hGetContents f_clientes_copia;
  f_clientes <- openFile "clientes" WriteMode;
  let novaListaTupla = item ++ (read d_clientes_copia)
  hPutStr f_clientes (show novaListaTupla)
  hFlush f_clientes;
  hClose f_clientes;

-- Mostrar notificação quando uma operação é feita
mensagem_operacao_efectuada :: [(String, Int, Int, Int, String, String, String, Int, Int, Int, Int)] -> String
mensagem_operacao_efectuada  cliente = do
  let cl_nome = (cliente_nome ( head cliente))
  cl_nome

-- Função que deposita na conta de um cliente
depositar :: ([(String, Int, Int, Int, String, String, String, Int, Int, Int, Int)], Int ) -> IO()
depositar  ( cliente, valor ) = do
  if valor > 0 then do
    putStrLn ""
  else do
    putStrLn "\n                   Atenção, deposita um valor positivo"
    atendimento_geral_autenticado

  f_clientes <- openFile "clientes" ReadMode;
  d_clientes <- hGetContents f_clientes;
  let items =(read d_clientes)
  -- Retornar o indice do cliente na lista de clientes
  case (elemIndex ( head cliente) items) of
    Just n -> do
      -- Somar o saldo anterior com o valor depositado
      hClose f_clientes
      let cl_saldo_actual = (cliente_saldo ( head cliente)) + valor
      let cl_nome = (cliente_nome ( head cliente))
      let cl_nasc_dia = (cliente_nasc_dia ( head cliente))
      let cl_nasc_mes = (cliente_nasc_mes ( head cliente))
      let cl_nasc_ano = (cliente_nasc_ano ( head cliente))
      let cl_id = (cliente_id ( head cliente))
      let cl_telefone = (cliente_telefone ( head cliente))
      let cl_conta = (cliente_conta ( head cliente))
      (year, month, day) <- anoActual
      -- Criar lista com os dados da nova tupla
      let novaTupla = [(cl_nome, cl_nasc_dia, cl_nasc_mes, cl_nasc_ano, cl_id, cl_telefone, cl_conta, cl_saldo_actual, day, month, read( show year))]
      let novaListaTupla = novaTupla ++ (take n items ++ drop (1 + n) items)
      criar_relatorio ( cl_nome, cl_conta, "D", valor, day, month, read( show year))
      -- Guardar a nova lista gerada no ficheiro
      f_clientes <- openFile "clientes" WriteMode;
      hPutStr f_clientes (show novaListaTupla)
      hFlush f_clientes;
      hClose f_clientes;
    Nothing -> do
      putStr $ "Esta conta não existe"

-- Função que levanta um valor na conta 
levantar :: ([(String, Int, Int, Int, String, String, String, Int, Int, Int, Int)], Int ) -> IO()
levantar  ( cliente, valor ) = do
  if valor > 0 then do
    putStrLn ""
  else do
    putStrLn "\n                   Atenção, levanta um valor positivo"
    atendimento_geral_autenticado

  f_clientes <- openFile "clientes" ReadMode;
  d_clientes <- hGetContents f_clientes;
  let items =(read d_clientes)
  -- Retornar o indice do cliente na lista de clientes
  case (elemIndex ( head cliente) items) of
    Just n -> do
      -- Somar o saldo anterior com o valor depositado
      hClose f_clientes
      let cl_saldo_actual = (cliente_saldo ( head cliente)) - valor
      -- Verificar se o valor é válido para depositar
      if cl_saldo_actual >= 0 then do
        putStrLn ""
      else do
        putStrLn "\n                           Saldo insuficiente"
        atendimento_geral_autenticado
        
      let cl_nome = (cliente_nome ( head cliente))
      let cl_nasc_dia = (cliente_nasc_dia ( head cliente))
      let cl_nasc_mes = (cliente_nasc_mes ( head cliente))
      let cl_nasc_ano = (cliente_nasc_ano ( head cliente))
      let cl_id = (cliente_id ( head cliente))
      let cl_telefone = (cliente_telefone ( head cliente))
      let cl_conta = (cliente_conta ( head cliente))
      (year, month, day) <- anoActual
      -- Criar lista com os dados da nova tupla
      let novaTupla = [(cl_nome, cl_nasc_dia, cl_nasc_mes, cl_nasc_ano, cl_id, cl_telefone, cl_conta, cl_saldo_actual,day, month, read( show year))]
      let novaListaTupla = novaTupla ++ (take n items ++ drop (1 + n) items)
      
      criar_relatorio ( cl_nome, cl_conta, "L", valor, day, month, read( show year))
      -- Guardar a nova lista gerada no ficheiro
      f_clientes <- openFile "clientes" WriteMode;
      hPutStr f_clientes (show novaListaTupla)
      hFlush f_clientes;
      hClose f_clientes;
    Nothing -> do
      putStr $ "Esta conta não existe"

-- Função que transfere valor numa conta
transferir :: ([(String, Int, Int, Int, String, String, String, Int, Int, Int, Int)], Int ) -> IO()
transferir  ( clientes, valor ) = do
  let cl_nome = (cliente_nome            ( head clientes))
  let cl_conta = (cliente_conta          ( head clientes))
  let cl_conta_o = (cliente_conta          ( last clientes))

  if (cl_conta == cl_conta_o) then do
    putStrLn "\n           Nº contas iguais, tentou transferir valor na mesma conta"
    atendimento_geral_autenticado
  else do
    putStrLn ""

  levantar ([head clientes], valor)
  depositar ([last clientes], valor)
  let cl_saldo_actual =   (cliente_saldo ( head clientes)) + valor
  (year, month, day) <- anoActual
  criar_relatorio ( cl_nome, cl_conta, "T", valor, day, month, read( show year))

-- Mostrar dados de um cliente 
consultar :: ([(String, Int, Int, Int, String, String, String, Int, Int, Int, Int)]) -> IO()
consultar  ( cliente) = do
  putStrLn $ "                                ID: " ++ ((cliente_id ( head cliente)))
  putStrLn $ "                           Cliente: " ++ ((cliente_nome ( head cliente)))
  putStrLn $ "                Data de nascimento: " ++ show ((cliente_nasc_dia ( head cliente))) ++"-"++ show ((cliente_nasc_mes ( head cliente)))++"-"++ show ((cliente_nasc_ano ( head cliente)))
  putStrLn $ "                          Telefone: " ++ ((cliente_telefone ( head cliente)))
  putStrLn $ "                             Conta: " ++  ((cliente_conta ( head cliente)))
  putStrLn $ "                             Saldo: " ++ show ((cliente_saldo ( head cliente))) ++ ",00 kz"
  putStrLn $ "                   Data de criação: " ++ show ((cliente_dta_dia ( head cliente))) ++"-"++ show ((cliente_dta_mes ( head cliente)))++"-"++ show ((cliente_dta_ano ( head cliente)))


-- Retornar cada campo da estrutura/tupla do cliente
cliente_nome (a,_,_,_,_,_,_,_,_,_,_) = a
cliente_nasc_dia (_,a,_,_,_,_,_,_,_,_,_) = a
cliente_nasc_mes (_,_,a,_,_,_,_,_,_,_,_) = a
cliente_nasc_ano (_,_,_,a,_,_,_,_,_,_,_) = a
cliente_id (_,_,_,_,a,_,_,_,_,_,_) = a
cliente_telefone (_,_,_,_,_,a,_,_,_,_,_) = a
cliente_conta (_,_,_,_,_,_,a,_,_,_,_) = a
cliente_saldo (_,_,_,_,_,_,_,a,_,_,_) = a
cliente_dta_dia (_,_,_,_,_,_,_,_,a,_,_) = a
cliente_dta_mes (_,_,_,_,_,_,_,_,_,a,_) = a
cliente_dta_ano (_,_,_,_,_,_,_,_,_,_,a) = a

-- Função que pesquisa se o usuário que deseja logar existe
pesquisar_usuario :: ([(String, String)], String) ->  [(String, String)]
pesquisar_usuario (xs, usuario) = [c | c <- xs, fst c == usuario]

-- Função que remove a senha (no ficheiro fila) do cliente, depois dele ter sido já atendido
remover_senha_cliente_atendido :: [Char] -> IO ()
remover_senha_cliente_atendido  dataStrFile = do 
    act_arquivo <- openFile "fila" WriteMode;
    hPutStr act_arquivo (substituir_espaco_por_virgula dataStrFile);
    hFlush act_arquivo;
    hClose act_arquivo;
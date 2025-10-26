-- | Módulo responsável por gerenciar toda a interface textual do sistema.
-- Ele apresenta menus, recebe entradas do usuário e chama as funções da
-- camada de negócio (definidas em Negocio.hs) e de relatórios.
-- Assim, o Menus.hs funciona como o "controlador" principal do programa.
module Menus (menuPrincipal) where

import Tipos
import Negocio
import Relatorios

-- ===============================================================
--                     FUNÇÕES DE UTILIDADE
-- ===============================================================

-- | Limpa a tela do terminal usando códigos de escape ANSI.
-- Isso garante que o menu apareça “limpo” a cada nova interação.
limparTela :: IO ()
limparTela = putStr "\ESC[2J\ESC[H"


-- ===============================================================
--                     MENU PRINCIPAL
-- ===============================================================

-- | Exibe o menu principal e redireciona o usuário
-- para o submenu correspondente, conforme a opção escolhida.
-- Cada submenu retorna o sistema atualizado (caso haja alterações),
-- o qual é repassado novamente ao menu principal para continuar o ciclo.
menuPrincipal :: Sistema -> IO Sistema
menuPrincipal sys = do
  limparTela
  putStrLn ""
  putStrLn "========== Aeroporto.hs =========="
  putStrLn "1) Passageiros"
  putStrLn "2) Companhias"
  putStrLn "3) Voos"
  putStrLn "4) Reservas"
  putStrLn "5) Relatorios"
  putStrLn "6) Salvar e Sair"
  putStr   "Escolha: "
  opc <- getLine
  case opc of
    "1" -> menuPassageiros sys >>= menuPrincipal
    "2" -> menuCompanhias sys >>= menuPrincipal
    "3" -> menuVoos sys >>= menuPrincipal
    "4" -> menuReservas sys >>= menuPrincipal
    "5" -> menuRelatorios sys >>= menuPrincipal
    "6" -> putStrLn "Saindo..." >> pure sys
    _   -> putStrLn "Opcao invalida." >> menuPrincipal sys


-- ===============================================================
--                     SUBMENU: PASSAGEIROS
-- ===============================================================

-- | Menu para gerenciar passageiros: cadastrar, listar ou voltar.
menuPassageiros :: Sistema -> IO Sistema
menuPassageiros sys = do
  limparTela
  putStrLn ""
  putStrLn "---- Passageiros ----"
  putStrLn "1) Cadastrar"
  putStrLn "2) Listar"
  putStrLn "3) Voltar"
  putStr   "Escolha: "
  opc <- getLine
  case opc of
    "1" -> do
      sys' <- acaoCadastrarPassageiro sys
      menuPassageiros sys'
    "2" -> do
      acaoListarPassageiros sys
      menuPassageiros sys
    "3" -> pure sys
    _   -> putStrLn "Opcao invalida." >> menuPassageiros sys

-- | Ação que lê os dados do passageiro e tenta cadastrá-lo.
-- Caso haja erro de validação (ex: campos vazios), exibe mensagem de erro.
acaoCadastrarPassageiro :: Sistema -> IO Sistema
acaoCadastrarPassageiro sys = do
  putStr   "Nome: "
  nome <- getLine
  putStr   "Documento: "
  doc  <- getLine
  case inserirPassageiroDados nome doc sys of
    Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
    Right sys' -> putStrLn "[OK] Passageiro cadastrado." >> pure sys'

-- | Lista todos os passageiros cadastrados.
acaoListarPassageiros :: Sistema -> IO ()
acaoListarPassageiros sys = do
  let ps = listarPassageiros sys
  if null ps
     then putStrLn "(vazio)"
     else mapM_ printPass ps

-- | Exibe um passageiro formatado.
printPass :: Passageiro -> IO ()
printPass p =
  putStrLn $ "- ID " ++ show (idPassageiro p)
          ++ " | Nome: " ++ nome p
          ++ " | Doc: "  ++ documento p


-- ===============================================================
--                     SUBMENU: COMPANHIAS
-- ===============================================================

menuCompanhias :: Sistema -> IO Sistema
menuCompanhias sys = do
  limparTela
  putStrLn ""
  putStrLn "---- Companhias ----"
  putStrLn "1) Cadastrar"
  putStrLn "2) Listar"
  putStrLn "3) Voltar"
  putStr   "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoCadastrarCompanhia sys >>= menuCompanhias
    "2" -> acaoListarCompanhias sys >> menuCompanhias sys
    "3" -> pure sys
    _   -> putStrLn "Opcao invalida." >> menuCompanhias sys

-- | Cadastro de uma nova companhia aérea.
acaoCadastrarCompanhia :: Sistema -> IO Sistema
acaoCadastrarCompanhia sys = do
  putStr   "Nome da Companhia: "
  nomeInput <- getLine
  case inserirCompanhiaNome nomeInput sys of
    Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
    Right sys' -> putStrLn "[OK] Companhia cadastrada." >> pure sys'

-- | Listagem das companhias existentes.
acaoListarCompanhias :: Sistema -> IO ()
acaoListarCompanhias sys = do
  let cs = listarCompanhias sys
  if null cs
     then putStrLn "(vazio)"
     else mapM_ printComp cs

printComp :: Companhia -> IO ()
printComp c =
  putStrLn $ "- ID " ++ show (idCompanhia c)
          ++ " | Nome: " ++ nomeCompanhia c


-- ===============================================================
--                     SUBMENU: VOOS
-- ===============================================================

menuVoos :: Sistema -> IO Sistema
menuVoos sys = do
  limparTela
  putStrLn ""
  putStrLn "---- Voos ----"
  putStrLn "1) Cadastrar"
  putStrLn "2) Listar"
  putStrLn "3) Buscar"
  putStrLn "4) Voltar"
  putStr   "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoCadastrarVoo sys >>= menuVoos
    "2" -> acaoListarVoos sys >> menuVoos sys
    "3" -> menuBuscaVoos sys >> menuVoos sys
    "4" -> pure sys
    _   -> putStrLn "Opcao invalida." >> menuVoos sys

-- | Criação de um voo, vinculando-o a uma companhia existente.
acaoCadastrarVoo :: Sistema -> IO Sistema
acaoCadastrarVoo sys = do
  putStr   "Origem: "
  origemInput <- getLine
  putStr   "Destino: "
  destinoInput <- getLine
  putStr   "Horario (ex: 12:30): "
  horarioInput <- getLine
  putStr   "ID da Companhia: "
  compInput <- getLine
  let maybeCompId = readIntSafe compInput
  case maybeCompId of
    Nothing     -> putStrLn "[ERRO] ID da Companhia inválido." >> pure sys
    Just compId -> case inserirVooDados origemInput destinoInput horarioInput compId sys of
      Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
      Right sys' -> putStrLn "[OK] Voo cadastrado." >> pure sys'

-- | Exibe todos os voos registrados no sistema.
acaoListarVoos :: Sistema -> IO ()
acaoListarVoos sys = do
  let vs = listarVoos sys
  if null vs
     then putStrLn "(vazio)"
     else mapM_ (printVoo sys) vs

-- | Formata um voo para exibição, mostrando inclusive a companhia associada.
printVoo :: Sistema -> Voo -> IO ()
printVoo sys v = do
  let compName = maybe "(desconhecida)" nomeCompanhia (buscarCompanhiaPorId (idComp v) sys)
  putStrLn $ "- ID " ++ show (idVoo v)
          ++ " | " ++ origem v ++ " -> " ++ destino v
          ++ " | " ++ horario v
          ++ " | Companhia: " ++ compName


-- ===============================================================
--                     SUBMENU: BUSCA DE VOOS
-- ===============================================================

-- | Permite pesquisar voos por origem, destino ou rota completa.
menuBuscaVoos :: Sistema -> IO ()
menuBuscaVoos sys = do
  limparTela
  putStrLn ""
  putStrLn "---- Buscar Voos ----"
  putStrLn "1) Por Origem"
  putStrLn "2) Por Destino"
  putStrLn "3) Por Rota (Origem e Destino)"
  putStrLn "4) Voltar"
  putStr   "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoBuscarVoosPorOrigem sys >> menuBuscaVoos sys
    "2" -> acaoBuscarVoosPorDestino sys >> menuBuscaVoos sys
    "3" -> acaoBuscarVoosPorRota sys >> menuBuscaVoos sys
    "4" -> pure ()
    _   -> putStrLn "Opcao invalida." >> menuBuscaVoos sys

-- As funções abaixo realizam buscas específicas e listam os resultados.
acaoBuscarVoosPorOrigem, acaoBuscarVoosPorDestino, acaoBuscarVoosPorRota :: Sistema -> IO ()
acaoBuscarVoosPorOrigem sys = do
  putStr "Origem: "
  origemInput <- getLine
  let resultados = buscarVoosPorOrigem origemInput sys
  if null resultados
    then putStrLn "(nenhum voo encontrado)"
    else mapM_ (printVoo sys) resultados

acaoBuscarVoosPorDestino sys = do
  putStr "Destino: "
  destinoInput <- getLine
  let resultados = buscarVoosPorDestino destinoInput sys
  if null resultados
    then putStrLn "(nenhum voo encontrado)"
    else mapM_ (printVoo sys) resultados

acaoBuscarVoosPorRota sys = do
  putStr "Origem: "
  origemInput <- getLine
  putStr "Destino: "
  destinoInput <- getLine
  let resultados = buscarVoosPorRota origemInput destinoInput sys
  if null resultados
    then putStrLn "(nenhum voo encontrado)"
    else mapM_ (printVoo sys) resultados


-- | Função auxiliar para converter String em Int de forma segura.
readIntSafe :: String -> Maybe Int
readIntSafe s = case reads s of
  [(n,"")] -> Just n
  _        -> Nothing

-- ===============================================================
--                     SUBMENU: RESERVAS
-- ===============================================================
-- Este menu é responsável por todo o gerenciamento das reservas,
-- desde a criação até a consulta por diferentes critérios.
-- Ele interage com as funções da camada de negócio (Negocio.hs),
-- garantindo que as operações sejam consistentes e seguras.
-- O fluxo segue o padrão de leitura -> validação -> execução -> feedback.

menuReservas :: Sistema -> IO Sistema
menuReservas sys = do
  limparTela
  putStrLn ""
  putStrLn "---- Reservas ----"
  putStrLn "1) Criar"
  putStrLn "2) Confirmar"
  putStrLn "3) Cancelar"
  putStrLn "4) Listar todas"
  putStrLn "5) Listar por Passageiro"
  putStrLn "6) Listar por Voo"
  putStrLn "7) Listar por Status"
  putStrLn "8) Voltar"
  putStr   "Escolha: "
  opc <- getLine
  case opc of
    -- Cada opção chama uma função específica de ação,
    -- e ao final retorna ao próprio menu (recursão interativa).
    "1" -> acaoCriarReserva sys >>= menuReservas
    "2" -> acaoConfirmarReserva sys >>= menuReservas
    "3" -> acaoCancelarReserva sys >>= menuReservas
    "4" -> acaoListarReservas sys >> menuReservas sys
    "5" -> acaoListarReservasPorPassageiro sys >> menuReservas sys
    "6" -> acaoListarReservasPorVoo sys >> menuReservas sys
    "7" -> menuListarReservasPorStatus sys >> menuReservas sys
    "8" -> pure sys
    _   -> putStrLn "Opcao invalida." >> menuReservas sys


-- ===============================================================
--                    AÇÃO: CRIAR RESERVA
-- ===============================================================
-- Cria uma nova reserva com base nos IDs do passageiro e do voo.
-- O status inicial da reserva é "Pendente".
-- São realizadas verificações de validade dos IDs e regras de negócio.
acaoCriarReserva :: Sistema -> IO Sistema
acaoCriarReserva sys = do
  putStr   "ID Passageiro: "
  sPid <- getLine
  putStr   "ID Voo: "
  sVid <- getLine
  case (readIntSafe sPid, readIntSafe sVid) of
    (Just pid, Just vid) ->
      -- Chama a função de negócio 'criarReserva' (Negocio.hs)
      case criarReserva pid vid sys of
        Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
        Right sys' -> putStrLn "[OK] Reserva criada (status: Pendente)." >> pure sys'
    _ -> putStrLn "[ERRO] IDs inválidos." >> pure sys


-- ===============================================================
--                    AÇÃO: CONFIRMAR RESERVA
-- ===============================================================
-- Atualiza o status de uma reserva para "Confirmada".
-- O ID da reserva é validado antes da alteração.
acaoConfirmarReserva :: Sistema -> IO Sistema
acaoConfirmarReserva sys = do
  putStr   "ID da Reserva: "
  sRid <- getLine
  case readIntSafe sRid of
    Just rid ->
      case confirmarReserva rid sys of
        Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
        Right sys' -> putStrLn "[OK] Reserva confirmada." >> pure sys'
    _ -> putStrLn "[ERRO] ID inválido." >> pure sys


-- ===============================================================
--                    AÇÃO: CANCELAR RESERVA
-- ===============================================================
-- Altera o status da reserva para "Cancelada".
-- Essa função é essencial para controle de disponibilidade de voos.
acaoCancelarReserva :: Sistema -> IO Sistema
acaoCancelarReserva sys = do
  putStr   "ID da Reserva: "
  sRid <- getLine
  case readIntSafe sRid of
    Just rid ->
      case cancelarReserva rid sys of
        Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
        Right sys' -> putStrLn "[OK] Reserva cancelada." >> pure sys'
    _ -> putStrLn "[ERRO] ID inválido." >> pure sys


-- ===============================================================
--                   LISTAGEM DE RESERVAS
-- ===============================================================
-- As funções abaixo exibem reservas segundo diferentes critérios:
-- todas, por passageiro, por voo e por status.
-- A função auxiliar 'printReserva' formata a exibição.

-- | Lista todas as reservas existentes.
acaoListarReservas :: Sistema -> IO ()
acaoListarReservas sys = do
  let rs = listarReservas sys
  if null rs
    then putStrLn "(vazio)"
    else mapM_ (printReserva sys) rs


-- | Lista reservas associadas a um determinado passageiro.
acaoListarReservasPorPassageiro :: Sistema -> IO ()
acaoListarReservasPorPassageiro sys = do
  putStr   "ID Passageiro: "
  sPid <- getLine
  case readIntSafe sPid of
    Just pid -> do
      let rs = listarReservasPorPassageiro pid sys
      if null rs
        then putStrLn "(vazio)"
        else mapM_ (printReserva sys) rs
    _ -> putStrLn "[ERRO] ID inválido."


-- | Lista reservas vinculadas a um voo específico.
acaoListarReservasPorVoo :: Sistema -> IO ()
acaoListarReservasPorVoo sys = do
  putStr   "ID Voo: "
  sVid <- getLine
  case readIntSafe sVid of
    Just vid -> do
      let rs = listarReservasPorVoo vid sys
      if null rs
        then putStrLn "(vazio)"
        else mapM_ (printReserva sys) rs
    _ -> putStrLn "[ERRO] ID inválido."


-- ===============================================================
--             LISTAGEM DE RESERVAS POR STATUS
-- ===============================================================
-- Este submenu permite filtrar as reservas por seu status:
-- Pendentes, Confirmadas ou Canceladas.
-- É útil para controle administrativo e geração de relatórios.
menuListarReservasPorStatus :: Sistema -> IO ()
menuListarReservasPorStatus sys = do
  limparTela
  putStrLn ""
  putStrLn "---- Listar por Status ----"
  putStrLn "1) Pendentes"
  putStrLn "2) Confirmadas"
  putStrLn "3) Canceladas"
  putStrLn "4) Voltar"
  putStr   "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoListarReservasPorStatus Pendente sys >> menuListarReservasPorStatus sys
    "2" -> acaoListarReservasPorStatus Confirmada sys >> menuListarReservasPorStatus sys
    "3" -> acaoListarReservasPorStatus Cancelada sys >> menuListarReservasPorStatus sys
    "4" -> pure ()
    _   -> putStrLn "Opcao invalida." >> menuListarReservasPorStatus sys


-- | Lista reservas conforme o status informado.
acaoListarReservasPorStatus :: StatusReserva -> Sistema -> IO ()
acaoListarReservasPorStatus st sys = do
  let rs = listarReservasPorStatus st sys
  if null rs
    then putStrLn "(nenhuma reserva encontrada)"
    else do
      putStrLn ""
      putStrLn ("Encontradas " ++ show (length rs)
                ++ " reserva(s) com status " ++ show st ++ ":")
      mapM_ (printReserva sys) rs


-- ===============================================================
--             EXIBIÇÃO DETALHADA DE UMA RESERVA
-- ===============================================================
-- A função abaixo formata os dados de uma reserva de forma legível,
-- mostrando informações do passageiro, voo (rota e horário)
-- e o status atual.
printReserva :: Sistema -> Reserva -> IO ()
printReserva sys r = do
  let pn = maybe "(?)" nome (buscarPassageiroPorId (idPass r) sys)
      v  = buscarVooPorId (idVooRef r) sys
      rota = maybe "(voo?)"
                  (\vv -> origem vv ++ " -> " ++ destino vv ++ " @ " ++ horario vv)
                  v
  putStrLn $ "- ID " ++ show (idReserva r)
          ++ " | Passageiro: " ++ pn
          ++ " | Voo: " ++ rota
          ++ " | Status: " ++ show (status r)


-- ===============================================================
--                    SUBMENU: RELATÓRIOS
-- ===============================================================
-- Este submenu permite gerar relatórios administrativos sobre
-- os dados armazenados. Cada opção chama uma função do módulo
-- Relatorios.hs, que imprime as informações de forma formatada.
-- São apenas funções IO (sem alteração de estado do sistema).
menuRelatorios :: Sistema -> IO Sistema
menuRelatorios sys = do
  limparTela
  putStrLn "\n---- Relatórios ----"
  putStrLn "1) Estatísticas gerais"
  putStrLn "2) Passageiros"
  putStrLn "3) Companhias"
  putStrLn "4) Voos"
  putStrLn "5) Reservas"
  putStrLn "6) Voltar"
  putStr "Escolha: "
  op <- getLine
  case op of
    "1" -> imprimirRelatorioEstatisticasGerais sys >> menuRelatorios sys
    "2" -> imprimirRelatorioPassageiros sys >> menuRelatorios sys
    "3" -> imprimirRelatorioCompanhias sys >> menuRelatorios sys
    "4" -> imprimirRelatorioVoos sys >> menuRelatorios sys
    "5" -> imprimirRelatorioReservas sys >> menuRelatorios sys
    "6" -> return sys
    _   -> putStrLn "Opção inválida!" >> menuRelatorios sys



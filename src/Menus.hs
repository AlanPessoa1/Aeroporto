-- | Módulo responsável por gerenciar toda a interface textual do sistema.
-- Ele apresenta menus, recebe entradas do usuário e chama as funções da
-- camada de negócio (definidas em Negocio.hs) e de relatórios.
-- Assim, o Menus.hs funciona como o "controlador" principal do programa.
module Menus (menuPrincipal) where

import Tipos
import Negocio
import Relatorios
import Reservas

-- ====== LOGIN ======

  login :: IO Usuario
  login = do
    putStrLn "Digite seu nome:"
    nomeUsuario <- getLine
    putStrLn "Você é administrador? (s/n)"
    resp <- getLine
    let tipoUsuario = if resp == "s" then Administrador else Usuario
    putStrLn $ "Bem-vindo, " ++ nomeUsuario ++ "!"
    return (Usuario nomeUsuario tipoUsuario)

-- ===============================================================
--                     FUNÇÕES DE UTILIDADE
-- ===============================================================

  menuPrincipal :: Usuario ->Sistema -> IO Sistema
  menuPrincipal usuario sys = do
    putStrLn ""
    putStrLn "========== Aeroporto.hs =========="

    if tipo usuario == Administrador
      then do
        putStrLn "1) Passageiros"
        putStrLn "2) Companhias"
        putStrLn "3) Voos"
        putStrLn "4) Reservas"
        putStrLn "5) Relatorios(Administrador)"
        putStrLn "6) Salvar e Sair"
      else do
        putStrLn "1) Passageiro"
        putStrLn "2) Companhias"
        putStrLn "3) Voos"
        putStrLn "4) Reservas"
        putStrLn "5) Salvar e Sair"

      putStr   "Escolha: "
      opc <- getLine
      case opc of
        "1" -> menuPassageiros sys >>= menuPrincipal usuario
        "2" -> menuCompanhias sys >>= menuPrincipal usuario
        "3" -> menuVoos sys >>= menuPrincipal usuario
        "4" -> menuReservas sys >>= menuPrincipal usuario   
        "5" -> if tipo usuario == Administrador
                 then menuRelatorios sys >>= menuPrincipal usuario
                 else putStrLn "Saindo..." >> pure sys
        "6" -> if tipo usuario == Administrador
                 then putStrLn "Saindo..." >> pure sys
                 else putStrLn "Opcao invalida." >> menuPrincipal usuario sys
        _   -> putStrLn "Opcao invalida." >> menuPrincipal usuario sys


--menuPrincipal :: Sistema -> IO Sistema
--menuPrincipal sys = do
  --putStrLn ""
  --putStrLn "========== Aeroporto.hs =========="
  --putStrLn "1) Passageiros"
  --putStrLn "2) Companhias"
  --putStrLn "3) Voos"
  --putStrLn "4) Reservas"
  --putStrLn "5) Relatorios"
  --putStrLn "6) Salvar e Sair"
  --putStr   "Escolha: "
  --opc <- getLine
  --case opc of
    --"1" -> menuPassageiros sys >>= menuPrincipal
    --"2" -> menuCompanhias sys >>= menuPrincipal
    --"3" -> menuVoos sys >>= menuPrincipal
    --"4" -> menuReservas sys >>= menuPrincipal   
    --"5" -> menuRelatorios sys >>= menuPrincipal
    --"6" -> putStrLn "Saindo..." >> pure sys
    --_   -> putStrLn "Opcao invalida." >> menuPrincipal sys

-- ====== SUBMENU: RESERVAS ======

menuReservas :: Sistema -> IO Sistema
menuReservas sys = do
  putStrLn ""
  putStrLn "---- Reservas ----"
  putStrLn "1) Criar Reserva"
  putStrLn "2) Listar Reservas"
  putStrLn "3) Confirmar Reserva"
  putStrLn "4) Cancelar Reserva"
  putStrLn "5) Voltar"
  putStr   "Escolha: "
  opc <- getLine
  case opc of
    "1" -> do
      sys' <- acaoCriarReserva sys
      menuReservas sys'
    "2" -> do
      acaoListarReservas sys
      menuReservas sys
    "3" -> do
      sys' <- acaoConfirmarReserva sys
      menuReservas sys'
    "4" -> do
      sys' <- acaoCancelarReserva sys
      menuReservas sys'
    "5" -> pure sys
    _   -> do
      putStrLn "Opcao invalida."
      menuReservas sys

-- ====== AÇÕES: RESERVAS ======

acaoCriarReserva :: Sistema -> IO Sistema
acaoCriarReserva sys = do
  putStr   "ID do Passageiro: "
  idPassInput <- getLine
  putStr   "ID do Voo: "
  idVooInput <- getLine

  case (readIntSafe idPassInput, readIntSafe idVooInput) of
    (Just idP, Just idV) -> criarReserva sys idP idV
    _ -> putStrLn "[ERRO] IDs inválidos." >> pure sys

acaoListarReservas :: Sistema -> IO ()
acaoListarReservas sys = listarReservas sys

acaoConfirmarReserva :: Sistema -> IO Sistema
acaoConfirmarReserva sys = do
  putStr "ID da reserva a confirmar: "
  idInput <- getLine
  case readIntSafe idInput of
    Just rid -> confirmarReserva sys rid
    _        -> putStrLn "[ERRO] ID inválido." >> pure sys

acaoCancelarReserva :: Sistema -> IO Sistema
acaoCancelarReserva sys = do
  putStr "ID da reserva a cancelar: "
  idInput <- getLine
  case readIntSafe idInput of
    Just rid -> cancelarReserva sys rid
    _        -> putStrLn "[ERRO] ID inválido." >> pure sys

-- ====== HELPER ======

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



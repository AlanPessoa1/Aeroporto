module Menus (menuPrincipal) where

import Tipos
import Negocio
import Relatorios

-- ====== UTILIDADES ======

-- | Limpa a tela usando sequências de escape ANSI
limparTela :: IO ()
limparTela = putStr "\ESC[2J\ESC[H"

-- ====== MENU PRINCIPAL ======

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

-- ====== SUBMENU: PASSAGEIROS ======

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
    _   -> do
      putStrLn "Opcao invalida."
      menuPassageiros sys

-- ====== AÇÕES: PASSAGEIROS ======

acaoCadastrarPassageiro :: Sistema -> IO Sistema
acaoCadastrarPassageiro sys = do
  putStr   "Nome: "
  nome <- getLine
  putStr   "Documento: "
  doc  <- getLine
  case inserirPassageiroDados nome doc sys of
    Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
    Right sys' -> putStrLn "[OK] Passageiro cadastrado." >> pure sys'

acaoListarPassageiros :: Sistema -> IO ()
acaoListarPassageiros sys = do
  let ps = listarPassageiros sys
  if null ps
     then putStrLn "(vazio)"
     else mapM_ printPass ps

printPass :: Passageiro -> IO ()
printPass p =
  putStrLn $ "- ID " ++ show (idPassageiro p)
          ++ " | Nome: " ++ nome p
          ++ " | Doc: "  ++ documento p

-- ====== SUBMENU: COMPANHIAS ======

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
    "1" -> do
      sys' <- acaoCadastrarCompanhia sys
      menuCompanhias sys'
    "2" -> do
      acaoListarCompanhias sys
      menuCompanhias sys
    "3" -> pure sys
    _   -> do
      putStrLn "Opcao invalida."
      menuCompanhias sys

acaoCadastrarCompanhia :: Sistema -> IO Sistema
acaoCadastrarCompanhia sys = do
  putStr   "Nome da Companhia: "
  nomeInput <- getLine
  case inserirCompanhiaNome nomeInput sys of
    Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
    Right sys' -> putStrLn "[OK] Companhia cadastrada." >> pure sys'

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

-- ====== SUBMENU: VOOS ======

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
    "1" -> do
      sys' <- acaoCadastrarVoo sys
      menuVoos sys'
    "2" -> do
      acaoListarVoos sys
      menuVoos sys
    "3" -> do
      menuBuscaVoos sys
      menuVoos sys
    "4" -> pure sys
    _   -> do
      putStrLn "Opcao invalida."
      menuVoos sys

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

acaoListarVoos :: Sistema -> IO ()
acaoListarVoos sys = do
  let vs = listarVoos sys
  if null vs
     then putStrLn "(vazio)"
     else mapM_ (printVoo sys) vs

printVoo :: Sistema -> Voo -> IO ()
printVoo sys v = do
  let compName = maybe "(desconhecida)" nomeCompanhia (buscarCompanhiaPorId (idComp v) sys)
  putStrLn $ "- ID " ++ show (idVoo v)
          ++ " | " ++ origem v ++ " -> " ++ destino v
          ++ " | " ++ horario v
          ++ " | Companhia: " ++ compName

-- ====== SUBMENU: BUSCA DE VOOS ======

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
    "1" -> do
      acaoBuscarVoosPorOrigem sys
      menuBuscaVoos sys
    "2" -> do
      acaoBuscarVoosPorDestino sys
      menuBuscaVoos sys
    "3" -> do
      acaoBuscarVoosPorRota sys
      menuBuscaVoos sys
    "4" -> pure ()
    _   -> do
      putStrLn "Opcao invalida."
      menuBuscaVoos sys

acaoBuscarVoosPorOrigem :: Sistema -> IO ()
acaoBuscarVoosPorOrigem sys = do
  putStr   "Origem: "
  origemInput <- getLine
  let resultados = buscarVoosPorOrigem origemInput sys
  if null resultados
    then putStrLn "(nenhum voo encontrado)"
    else do
      putStrLn ""
      putStrLn ("Encontrados " ++ show (length resultados) ++ " voo(s):")
      mapM_ (printVoo sys) resultados

acaoBuscarVoosPorDestino :: Sistema -> IO ()
acaoBuscarVoosPorDestino sys = do
  putStr   "Destino: "
  destinoInput <- getLine
  let resultados = buscarVoosPorDestino destinoInput sys
  if null resultados
    then putStrLn "(nenhum voo encontrado)"
    else do
      putStrLn ""
      putStrLn ("Encontrados " ++ show (length resultados) ++ " voo(s):")
      mapM_ (printVoo sys) resultados

acaoBuscarVoosPorRota :: Sistema -> IO ()
acaoBuscarVoosPorRota sys = do
  putStr   "Origem: "
  origemInput <- getLine
  putStr   "Destino: "
  destinoInput <- getLine
  let resultados = buscarVoosPorRota origemInput destinoInput sys
  if null resultados
    then putStrLn "(nenhum voo encontrado)"
    else do
      putStrLn ""
      putStrLn ("Encontrados " ++ show (length resultados) ++ " voo(s):")
      mapM_ (printVoo sys) resultados

-- Helper: leitura segura de Int
readIntSafe :: String -> Maybe Int
readIntSafe s = case reads s of
  [(n,"")] -> Just n
  _        -> Nothing


-- ====== SUBMENU: RESERVAS ======

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
    "1" -> acaoCriarReserva sys >>= menuReservas
    "2" -> acaoConfirmarReserva sys >>= menuReservas
    "3" -> acaoCancelarReserva sys >>= menuReservas
    "4" -> acaoListarReservas sys >> menuReservas sys
    "5" -> acaoListarReservasPorPassageiro sys >> menuReservas sys
    "6" -> acaoListarReservasPorVoo sys >> menuReservas sys
    "7" -> menuListarReservasPorStatus sys >> menuReservas sys
    "8" -> pure sys
    _   -> putStrLn "Opcao invalida." >> menuReservas sys

acaoCriarReserva :: Sistema -> IO Sistema
acaoCriarReserva sys = do
  putStr   "ID Passageiro: "
  sPid <- getLine
  putStr   "ID Voo: "
  sVid <- getLine
  case (readIntSafe sPid, readIntSafe sVid) of
    (Just pid, Just vid) ->
      case criarReserva pid vid sys of
        Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
        Right sys' -> putStrLn "[OK] Reserva criada (status: Pendente)." >> pure sys'
    _ -> putStrLn "[ERRO] IDs inválidos." >> pure sys

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

acaoListarReservas :: Sistema -> IO ()
acaoListarReservas sys = do
  let rs = listarReservas sys
  if null rs then putStrLn "(vazio)" else mapM_ (printReserva sys) rs

acaoListarReservasPorPassageiro :: Sistema -> IO ()
acaoListarReservasPorPassageiro sys = do
  putStr   "ID Passageiro: "
  sPid <- getLine
  case readIntSafe sPid of
    Just pid -> do
      let rs = listarReservasPorPassageiro pid sys
      if null rs then putStrLn "(vazio)" else mapM_ (printReserva sys) rs
    _ -> putStrLn "[ERRO] ID inválido."

acaoListarReservasPorVoo :: Sistema -> IO ()
acaoListarReservasPorVoo sys = do
  putStr   "ID Voo: "
  sVid <- getLine
  case readIntSafe sVid of
    Just vid -> do
      let rs = listarReservasPorVoo vid sys
      if null rs then putStrLn "(vazio)" else mapM_ (printReserva sys) rs
    _ -> putStrLn "[ERRO] ID inválido."

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
    "1" -> do
      acaoListarReservasPorStatus Pendente sys
      menuListarReservasPorStatus sys
    "2" -> do
      acaoListarReservasPorStatus Confirmada sys
      menuListarReservasPorStatus sys
    "3" -> do
      acaoListarReservasPorStatus Cancelada sys
      menuListarReservasPorStatus sys
    "4" -> pure ()
    _   -> do
      putStrLn "Opcao invalida."
      menuListarReservasPorStatus sys

acaoListarReservasPorStatus :: StatusReserva -> Sistema -> IO ()
acaoListarReservasPorStatus st sys = do
  let rs = listarReservasPorStatus st sys
  if null rs
    then putStrLn "(nenhuma reserva encontrada)"
    else do
      putStrLn ""
      putStrLn ("Encontradas " ++ show (length rs) ++ " reserva(s) com status " ++ show st ++ ":")
      mapM_ (printReserva sys) rs

printReserva :: Sistema -> Reserva -> IO ()
printReserva sys r = do
  let pn = maybe "(?)" nome (buscarPassageiroPorId (idPass r) sys)
      v  = buscarVooPorId (idVooRef r) sys
      rota = maybe "(voo?)" (\vv -> origem vv ++ " -> " ++ destino vv ++ " @ " ++ horario vv) v
  putStrLn $ "- ID " ++ show (idReserva r)
          ++ " | Passageiro: " ++ pn
          ++ " | Voo: " ++ rota
          ++ " | Status: " ++ show (status r)

-- ====== SUBMENU: RELATORIOS ======
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
    _   -> do
      putStrLn "Opção inválida!"
      menuRelatorios sys



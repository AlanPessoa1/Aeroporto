module Menus (menuPrincipal) where

import Tipos
import Negocio
import Relatorios
import Reservas

-- ====== MENU PRINCIPAL ======

menuPrincipal :: Sistema -> IO Sistema
menuPrincipal sys = do
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

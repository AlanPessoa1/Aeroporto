module Menus (menuPrincipal) where

import Tipos
import Negocio

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
    "3" -> putStrLn "[Menu] Voos (em breve)"        >> menuPrincipal sys
    "4" -> putStrLn "[Menu] Reservas (em breve)"     >> menuPrincipal sys
    "5" -> putStrLn "[Menu] Relatorios (em breve)"   >> menuPrincipal sys
    "6" -> putStrLn "Saindo..." >> pure sys
    _   -> putStrLn "Opcao invalida." >> menuPrincipal sys

-- ====== SUBMENU: PASSAGEIROS ======

menuPassageiros :: Sistema -> IO Sistema
menuPassageiros sys = do
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

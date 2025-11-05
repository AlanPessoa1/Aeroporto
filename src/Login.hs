module Login (menuInicial) where

import Tipos
import Negocio (inserirUsuario)
import System.IO (hFlush, stdout, hSetEcho)
import Data.Char (toLower)

-- Menu inicial: Registrar ou Entrar
menuInicial :: Sistema -> IO (Sistema, Usuario)
menuInicial sys = do
  putStrLn "\n===================================="
  putStrLn "   SISTEMA DE GERENCIAMENTO"
  putStrLn "         DE AEROPORTO"
  putStrLn "====================================\n"
  putStrLn "1) Registrar novo usuário"
  putStrLn "2) Entrar no sistema"
  putStrLn "3) Sair"
  putStr "\nEscolha: "
  hFlush stdout
  opcao <- getLine

  case opcao of
    "1" -> do
      sys' <- registrarUsuario sys
      menuInicial sys'
    "2" -> do
      usuario <- menuLogin (usuarios sys)
      return (sys, usuario)
    "3" -> error "Saindo do sistema..."
    _   -> do
      putStrLn "\nOpção inválida. Tente novamente."
      menuInicial sys

-- Tela de registro
registrarUsuario :: Sistema -> IO Sistema
registrarUsuario sys = do
  putStrLn "\n===================================="
  putStrLn "        REGISTRO DE USUARIO"
  putStrLn "====================================\n"

  putStr "Nome de usuário: "
  hFlush stdout
  nome <- getLine

  putStr "Email: "
  hFlush stdout
  email <- getLine

  putStr "Senha: "
  hFlush stdout
  hSetEcho stdout False
  senha <- getLine
  hSetEcho stdout True
  putStrLn ""

  case inserirUsuario nome email senha sys of
    Left err -> do
      putStrLn $ "\n[ERRO] " ++ err
      putStrLn "Pressione ENTER para tentar novamente..."
      _ <- getLine
      return sys
    Right sys' -> do
      putStrLn "\nUsuário registrado com sucesso!"
      putStrLn "Você já pode fazer login.\n"
      return sys'

-- Menu de login: escolher tipo de usuario
menuLogin :: [Usuario] -> IO Usuario
menuLogin usuarios = do
  putStrLn "\n===================================="
  putStrLn "        SISTEMA DE ACESSO"
  putStrLn "====================================\n"
  putStrLn "1) Login como Usuário Comum"
  putStrLn "2) Login como Administrador"
  putStrLn "3) Voltar"
  putStr "\nEscolha: "
  hFlush stdout
  opcao <- getLine

  case opcao of
    "1" -> loginPorTipo UsuarioComum usuarios
    "2" -> loginPorTipo Administrador usuarios
    "3" -> error "Voltando..."
    _   -> do
      putStrLn "\nOpção inválida."
      menuLogin usuarios

-- Tela de login por tipo de usuario
loginPorTipo :: TipoUsuario -> [Usuario] -> IO Usuario
loginPorTipo tipo usuarios = do
  putStrLn $ "\n---- Login como " ++ show tipo ++ " ----\n"

  putStr "Nome ou Email: "
  hFlush stdout
  nomeOuEmail <- getLine

  putStr "Senha: "
  hFlush stdout
  hSetEcho stdout False
  senha <- getLine
  hSetEcho stdout True
  putStrLn ""

  case autenticar nomeOuEmail senha tipo usuarios of
    Just u  -> do
      putStrLn $ "\nBem-vindo, " ++ nomeUsuario u ++ "!\n"
      return u
    Nothing -> do
      putStrLn "\nUsuário ou senha incorretos, ou tipo de usuário inválido."
      putStrLn "Tente novamente.\n"
      loginPorTipo tipo usuarios

-- Função de autenticação (verifica nome OU email, senha e tipo)
autenticar :: String -> String -> TipoUsuario -> [Usuario] -> Maybe Usuario
autenticar nomeOuEmail senha tipoEsperado usuarios =
  let nomeEmailLower = map toLower nomeOuEmail
      senhaLower = map toLower senha
  in  case filter (\u ->
                    (map toLower (nomeUsuario u) == nomeEmailLower ||
                     map toLower (emailUsuario u) == nomeEmailLower) &&
                    map toLower (senhaUsuario u) == senhaLower &&
                    tipoUsuario u == tipoEsperado) usuarios of
        []    -> Nothing
        (u:_) -> Just u





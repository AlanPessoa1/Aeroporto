module Login (login) where

import Tipos
import System.IO (hFlush, stdout, hSetEcho)
import Data.Char (toLower)

-- Tela de login
login :: [Usuario] -> IO Usuario
login usuarios = do
  putStrLn "\n===================================="
  putStrLn "        SISTEMA DE ACESSO"
  putStrLn "====================================\n"

  putStr "Nome: "
  hFlush stdout
  nome <- getLine

  putStr "Senha: "
  hFlush stdout
  -- desativa exibição da senha
  hSetEcho stdout False
  senha <- getLine
  hSetEcho stdout True
  putStrLn ""

  case autenticar nome senha usuarios of
    Just u  -> do
      putStrLn $ "\n✅ Bem-vindo, " ++ nomeUsuario u ++ " (" ++ show (tipoUsuario u) ++ ").\n"
      return u
    Nothing -> do
      putStrLn "\n❌ Usuário ou senha incorretos. Tente novamente.\n"
      login usuarios

-- Função de autenticação
autenticar :: String -> String -> [Usuario] -> Maybe Usuario
autenticar nome senha usuarios =
  let nomeLower  = map toLower nome
      senhaLower = map toLower senha
  in  case filter (\u -> map toLower (nomeUsuario u) == nomeLower
                      && map toLower (senhaUsuario u) == senhaLower) usuarios of
        []    -> Nothing
        (u:_) -> Just u





module Main where

import System.IO (hSetEncoding, stdout, stderr)
import GHC.IO.Encoding (utf8)
import Tipos
import Persistencia
import Menus
import Login (login)
import Control.Monad (when)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  putStrLn "Sistema de Gerenciamento de Aeroporto - Início"
  sys0 <- carregarSistema

  let sys =
        if null (usuarios sys0)
          then sys0 {
            usuarios =
              [ Usuario 1 "admin" "123" Administrador
              , Usuario 2 "joao"  "abc" UsuarioComum
              ]
          }
          else sys0

  -- Se criou usuários default, salva agora
  when (null (usuarios sys0)) $
    salvarSistema sys

  usuario <- login (usuarios sys)

  sys1 <- menuPrincipal usuario sys

  putStrLn "Salvando dados..."
  salvarSistema sys1

  putStrLn "Até logo!"



--Sempre rodar os dois antes do stack run para aparecer os acentos nas letras
--chcp 65001
-- $OutputEncoding = [Console]::OutputEncoding = [Text.UTF8Encoding]::UTF8







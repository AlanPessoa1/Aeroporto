module Main where

import System.IO (hSetEncoding, stdout, stderr)
import GHC.IO.Encoding (utf8)
import Tipos
import Persistencia
import Menus
import Login (menuInicial)
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
              [ Usuario 1 "admin" "admin@aeroporto.com" "123" Administrador
              , Usuario 2 "joao"  "joao@email.com" "abc" UsuarioComum
              ],
            companhias =
              [ Companhia 1 "Cia A"
              , Companhia 2 "Cia B"
              , Companhia 3 "Cia C"
              ],
            voos =
              [ Voo 1 "Recife" "Salvador" "08:00" 1 5
              , Voo 2 "Salvador" "Fortaleza" "10:00" 2 5
              , Voo 3 "Fortaleza" "Natal" "12:00" 3 5
              , Voo 4 "Natal" "Recife" "14:00" 1 5
              , Voo 5 "Recife" "Brasilia" "16:00" 2 5
              ]
          }
          else sys0

  -- Se criou dados default, salva agora
  when (null (usuarios sys0)) $
    salvarSistema sys

  (sysAtualizado, usuario) <- menuInicial sys

  sys1 <- menuPrincipal usuario sysAtualizado

  putStrLn "Salvando dados..."
  salvarSistema sys1

  putStrLn "Até logo!"



--Sempre rodar os dois antes do stack run para aparecer os acentos nas letras
--chcp 65001
-- $OutputEncoding = [Console]::OutputEncoding = [Text.UTF8Encoding]::UTF8







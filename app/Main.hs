module Main where

import System.IO (hSetEncoding, stdout, stderr)
import GHC.IO.Encoding (utf8)
import Tipos
import Persistencia
import Menus

main :: IO ()
main = do
  -- UTF-8 por ser Windows
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  putStrLn "Sistema de Gerenciamento de Aeroporto - Início"
  sys <- carregarSistema
  usuario <- login
  sys1 <- menuPrincipal usuario sys
  salvarSistema sys1
  putStrLn "Até logo!"

--main :: IO ()
--main = do
  -- UTF-8 por ser Windows
  --hSetEncoding stdout utf8
  --hSetEncoding stderr utf8

  --putStrLn "Sistema de Gerenciamento de Aeroporto - Início"
  --sys0 <- carregarSistema
  --sys1 <- menuPrincipal sys0
  --salvarSistema sys1
  --putStrLn "Até logo!"


--Sempre rodar os dois antes do stack run para aparecer os acentos nas letras
--chcp 65001
-- $OutputEncoding = [Console]::OutputEncoding = [Text.UTF8Encoding]::UTF8






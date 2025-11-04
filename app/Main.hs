module Main where

import System.IO (hSetEncoding, stdout, stderr)
import GHC.IO.Encoding (utf8)
import Tipos
import Persistencia
import Menus

-- | Função principal do programa
-- Inicializa o sistema, processa autenticação e executa menu principal
main :: IO ()
main = do
  -- Configura UTF-8 para Windows
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  putStrLn "Sistema de Gerenciamento de Aeroporto - Inicio"

  -- Carrega dados salvos
  sys <- carregarSistema

  -- Executa menu de autenticação (login ou registro)
  -- Retorna o usuário autenticado e o sistema (que pode ter sido atualizado com novo usuário)
  (usuario, sysAtualizado) <- menuAutenticacao sys

  -- Executa menu principal com o usuário autenticado
  sysRetorno <- menuPrincipal usuario sysAtualizado

  -- Salva todos os dados ao sair
  salvarSistema sysRetorno
  putStrLn "Ate logo!"

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






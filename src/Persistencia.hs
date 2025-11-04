{-Este módulo implementa a camada de persistência do projeto Aeroporto.
A ideia é que todas as informações (passageiros, companhias, voos e reservas)
sejam gravadas em arquivos de texto simples para que permaneçam disponíveis
mesmo após o programa ser encerrado.-}

module Persistencia
  ( carregarSistema
  , salvarSistema
  ) where

import System.Directory (doesFileExist, createDirectoryIfMissing)
import Text.Read (readMaybe)
import Tipos

-- Funções utilitárias genéricas (agora tolerantes a arquivos vazios/invalidos)
carregarLista :: Read a => FilePath -> IO [a]
carregarLista arq = do
  existe <- doesFileExist arq
  if not existe
     then return []
     else do
       conteudo <- readFile arq
       case readMaybe conteudo of
         Just xs -> return xs
         Nothing -> do
           putStrLn $ "[Aviso] Conteudo invalido em " ++ arq ++ " (usando lista vazia)."
           return []

salvarLista :: Show a => FilePath -> [a] -> IO ()
salvarLista arq xs = writeFile arq (show xs)

-- Caminhos dos arquivos de dados
-- Cada entidade tem seu próprio arquivo .db
-- Usuários são separados por tipo em arquivos diferentes
arquivoUsuariosAdmin, arquivoUsuariosComum, arquivoPassageiros, arquivoCompanhias, arquivoVoos, arquivoReservas :: FilePath
arquivoUsuariosAdmin = "dados/usuarios_admin.db"    -- Armazena usuários Administradores
arquivoUsuariosComum = "dados/usuarios_comum.db"    -- Armazena usuários Comuns
arquivoPassageiros   = "dados/passageiros.db"
arquivoCompanhias    = "dados/companhias.db"
arquivoVoos          = "dados/voos.db"
arquivoReservas      = "dados/reservas.db"

-- Carrega todo o sistema da persistência
-- Cria o diretório 'dados' se não existir
-- Carrega cada entidade de seu respectivo arquivo
-- Usuários são carregados de dois arquivos separados (admin e comum) e unidos
-- Se não houver administradores, cria um usuário admin padrão
carregarSistema :: IO Sistema
carregarSistema = do
  createDirectoryIfMissing True "dados"

  -- Carrega usuários de dois arquivos separados
  usAdmin <- carregarLista arquivoUsuariosAdmin  -- Carrega administradores
  usComum <- carregarLista arquivoUsuariosComum  -- Carrega usuários comuns

  -- Se não houver nenhum admin, cria o admin padrão
  let usAdminFinal = if null usAdmin
                       then [usuarioAdminPadrao]  -- Cria admin padrão
                       else usAdmin

  let us = usAdminFinal ++ usComum               -- Junta as duas listas

  ps <- carregarLista arquivoPassageiros
  cs <- carregarLista arquivoCompanhias
  vs <- carregarLista arquivoVoos
  rs <- carregarLista arquivoReservas
  return (Sistema us ps cs vs rs)

-- Usuário administrador padrão do sistema
-- Criado automaticamente na primeira execução se não houver nenhum admin
-- Credenciais: admin@gmail.com / adm123
usuarioAdminPadrao :: Usuario
usuarioAdminPadrao = Usuario
  { idUsuario = 1
  , nomeUsuario = "Administrador"
  , emailUsuario = "admin@gmail.com"
  , senhaUsuario = "adm123"
  , tipoUsuario = Administrador
  }

-- Salva todo o sistema na persistência
-- Cria o diretório 'dados' se não existir
-- Salva cada entidade em seu respectivo arquivo
-- Usuários são separados por tipo e salvos em arquivos diferentes
salvarSistema :: Sistema -> IO ()
salvarSistema sys = do
  createDirectoryIfMissing True "dados"

  -- Separa usuários por tipo
  let todosUsuarios = usuarios sys
      usuariosAdmin = filter (\u -> tipoUsuario u == Administrador) todosUsuarios
      usuariosComum = filter (\u -> tipoUsuario u == UsuarioComum) todosUsuarios

  -- Salva em arquivos separados
  salvarLista arquivoUsuariosAdmin usuariosAdmin  -- Salva administradores
  salvarLista arquivoUsuariosComum usuariosComum  -- Salva usuários comuns

  salvarLista arquivoPassageiros (passageiros sys)
  salvarLista arquivoCompanhias  (companhias  sys)
  salvarLista arquivoVoos        (voos        sys)
  salvarLista arquivoReservas    (reservas    sys)


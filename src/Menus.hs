-- | Módulo responsável pela interface textual do sistema.
-- Apresenta menus separados para Admin e Usuário Comum.
module Menus (menuPrincipal) where

import Tipos
import Negocio
import Relatorios
  ( imprimirRelatorioEstatisticasGerais
  , imprimirRelatorioPassageiros
  , imprimirRelatorioCompanhias
  , imprimirRelatorioVoos
  , imprimirRelatorioReservas
  )

-- ============================================================
--                 FUNÇÃO PRINCIPAL DE MENU
-- ============================================================

menuPrincipal :: Usuario -> Sistema -> IO Sistema
menuPrincipal usuario sys = do
  putStrLn ""
  putStrLn "========== Aeroporto.hs =========="
  putStrLn $ "Logado como: " ++ nomeUsuario usuario ++ " (" ++ show (tipoUsuario usuario) ++ ")"

  case tipoUsuario usuario of
    Administrador -> menuAdmin usuario sys
    UsuarioComum -> menuUsuarioComum usuario sys

-- ============================================================
--                    MENU ADMINISTRADOR
-- ============================================================

menuAdmin :: Usuario -> Sistema -> IO Sistema
menuAdmin usuario sys = do
  putStrLn ""
  putStrLn "1) Companhias"
  putStrLn "2) Voos"
  putStrLn "3) Passageiros"
  putStrLn "4) Reservas"
  putStrLn "5) Relatorios"
  putStrLn "6) Usuarios"
  putStrLn "7) Sair"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> menuCompanhiasAdmin sys >>= menuPrincipal usuario
    "2" -> menuVoosAdmin sys >>= menuPrincipal usuario
    "3" -> menuPassageirosAdmin sys >>= menuPrincipal usuario
    "4" -> menuReservasAdmin sys >>= menuPrincipal usuario
    "5" -> menuRelatorios sys >>= menuPrincipal usuario
    "6" -> menuUsuariosAdmin usuario sys >>= menuPrincipal usuario
    "7" -> putStrLn "Saindo..." >> pure sys
    _   -> putStrLn "Opcao invalida." >> menuPrincipal usuario sys

-- ============================================================
--                    MENU USUARIO COMUM
-- ============================================================

menuUsuarioComum :: Usuario -> Sistema -> IO Sistema
menuUsuarioComum usuario sys = do
  putStrLn ""
  putStrLn "1) Meu Perfil"
  putStrLn "2) Buscar Voos"
  putStrLn "3) Minhas Reservas"
  putStrLn "4) Configuracoes"
  putStrLn "5) Sair"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> menuPerfilUsuario usuario sys >>= menuPrincipal usuario
    "2" -> menuBuscarVoosUsuario sys >> menuPrincipal usuario sys
    "3" -> menuReservasUsuario usuario sys >>= menuPrincipal usuario
    "4" -> menuConfiguracoesUsuario usuario sys >>= \(s, u) -> menuPrincipal u s
    "5" -> putStrLn "Saindo..." >> pure sys
    _   -> putStrLn "Opcao invalida." >> menuPrincipal usuario sys

-- ============================================================
--                 MENUS ADMIN - COMPANHIAS
-- ============================================================

menuCompanhiasAdmin :: Sistema -> IO Sistema
menuCompanhiasAdmin sys = do
  putStrLn ""
  putStrLn "---- Companhias (Admin) ----"
  putStrLn "1) Cadastrar"
  putStrLn "2) Listar"
  putStrLn "3) Editar"
  putStrLn "4) Excluir"
  putStrLn "5) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoCadastrarCompanhia sys >>= menuCompanhiasAdmin
    "2" -> acaoListarCompanhias sys >> menuCompanhiasAdmin sys
    "3" -> acaoEditarCompanhia sys >>= menuCompanhiasAdmin
    "4" -> acaoExcluirCompanhia sys >>= menuCompanhiasAdmin
    "5" -> pure sys
    _   -> putStrLn "Opcao invalida." >> menuCompanhiasAdmin sys

acaoCadastrarCompanhia :: Sistema -> IO Sistema
acaoCadastrarCompanhia sys = do
  putStr "Nome da Companhia: "
  nome <- getLine
  case inserirCompanhiaNome nome sys of
    Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
    Right sys' -> putStrLn "[OK] Companhia cadastrada." >> pure sys'

acaoListarCompanhias :: Sistema -> IO ()
acaoListarCompanhias sys = do
  let cs = listarCompanhias sys
  if null cs
     then putStrLn "(vazio)"
     else mapM_ (\c -> putStrLn $ "- ID " ++ show (idCompanhia c)
                               ++ " | Nome: " ++ nomeCompanhia c) cs

acaoEditarCompanhia :: Sistema -> IO Sistema
acaoEditarCompanhia sys = do
  putStr "ID da Companhia: "
  sId <- getLine
  case readIntSafe sId of
    Nothing -> putStrLn "[ERRO] ID invalido." >> pure sys
    Just cid -> do
      putStr "Novo Nome: "
      novoNome <- getLine
      case editarCompanhia cid novoNome sys of
        Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
        Right sys' -> putStrLn "[OK] Companhia editada." >> pure sys'

acaoExcluirCompanhia :: Sistema -> IO Sistema
acaoExcluirCompanhia sys = do
  putStr "ID da Companhia: "
  sId <- getLine
  case readIntSafe sId of
    Nothing -> putStrLn "[ERRO] ID invalido." >> pure sys
    Just cid -> case excluirCompanhia cid sys of
        Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
        Right sys' -> putStrLn "[OK] Companhia excluida." >> pure sys'

-- ============================================================
--                    MENUS ADMIN - VOOS
-- ============================================================

menuVoosAdmin :: Sistema -> IO Sistema
menuVoosAdmin sys = do
  putStrLn ""
  putStrLn "---- Voos (Admin) ----"
  putStrLn "1) Cadastrar"
  putStrLn "2) Listar"
  putStrLn "3) Buscar"
  putStrLn "4) Editar"
  putStrLn "5) Excluir"
  putStrLn "6) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoCadastrarVoo sys >>= menuVoosAdmin
    "2" -> acaoListarVoos sys >> menuVoosAdmin sys
    "3" -> menuBuscaVoos sys >> menuVoosAdmin sys
    "4" -> acaoEditarVoo sys >>= menuVoosAdmin
    "5" -> acaoExcluirVoo sys >>= menuVoosAdmin
    "6" -> pure sys
    _   -> putStrLn "Opcao invalida." >> menuVoosAdmin sys

acaoCadastrarVoo :: Sistema -> IO Sistema
acaoCadastrarVoo sys = do
  putStr "Origem: "
  origemInput <- getLine
  putStr "Destino: "
  destinoInput <- getLine
  putStr "Horario (ex: 12:30): "
  horarioInput <- getLine
  putStr "ID da Companhia: "
  sCid <- getLine
  putStr "Capacidade (assentos): "
  sCap <- getLine
  case (readIntSafe sCid, readIntSafe sCap) of
    (Nothing, _) -> putStrLn "[ERRO] ID da companhia invalido." >> pure sys
    (_, Nothing) -> putStrLn "[ERRO] Capacidade invalida." >> pure sys
    (Just cid, Just cap) -> case inserirVooDados origemInput destinoInput horarioInput cid cap sys of
      Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
      Right sys' -> putStrLn "[OK] Voo cadastrado." >> pure sys'

acaoListarVoos :: Sistema -> IO ()
acaoListarVoos sys = do
  let vs = listarVoos sys
  if null vs
     then putStrLn "(vazio)"
     else mapM_ (printVoo sys) vs

acaoEditarVoo :: Sistema -> IO Sistema
acaoEditarVoo sys = do
  putStr "ID do Voo: "
  sId <- getLine
  case readIntSafe sId of
    Nothing -> putStrLn "[ERRO] ID invalido." >> pure sys
    Just vid -> do
      putStr "Nova Origem: "
      origemInput <- getLine
      putStr "Novo Destino: "
      destinoInput <- getLine
      putStr "Novo Horario: "
      horarioInput <- getLine
      putStr "Novo ID da Companhia: "
      sCid <- getLine
      putStr "Nova Capacidade: "
      sCap <- getLine
      case (readIntSafe sCid, readIntSafe sCap) of
        (Nothing, _) -> putStrLn "[ERRO] ID da companhia invalido." >> pure sys
        (_, Nothing) -> putStrLn "[ERRO] Capacidade invalida." >> pure sys
        (Just cid, Just cap) -> case editarVoo vid origemInput destinoInput horarioInput cid cap sys of
          Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
          Right sys' -> putStrLn "[OK] Voo editado." >> pure sys'

acaoExcluirVoo :: Sistema -> IO Sistema
acaoExcluirVoo sys = do
  putStr "ID do Voo: "
  sId <- getLine
  case readIntSafe sId of
    Nothing -> putStrLn "[ERRO] ID invalido." >> pure sys
    Just vid -> case excluirVoo vid sys of
      Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
      Right sys' -> putStrLn "[OK] Voo excluido." >> pure sys'

printVoo :: Sistema -> Voo -> IO ()
printVoo sys v = do
  let comp = maybe "(Desconhecida)" nomeCompanhia (buscarCompanhiaPorId (idComp v) sys)
      reservasConfirmadas = filter (\r -> idVooRef r == idVoo v && status r == Confirmada) (reservas sys)
      vagasOcupadas = length reservasConfirmadas
      vagasDisponiveis = capacidade v - vagasOcupadas
  putStrLn $ "- ID " ++ show (idVoo v)
          ++ " | " ++ origem v ++ " -> " ++ destino v
          ++ " | " ++ horario v
          ++ " | Companhia: " ++ comp
          ++ " | Vagas: " ++ show vagasDisponiveis ++ "/" ++ show (capacidade v)

menuBuscaVoos :: Sistema -> IO ()
menuBuscaVoos sys = do
  putStrLn ""
  putStrLn "---- Buscar Voos ----"
  putStrLn "1) Por Origem"
  putStrLn "2) Por Destino"
  putStrLn "3) Por Rota"
  putStrLn "4) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> putStr "Origem: " >> getLine >>= \o ->
             mapM_ (printVoo sys) (buscarVoosPorOrigem o sys) >> menuBuscaVoos sys
    "2" -> putStr "Destino: " >> getLine >>= \d ->
             mapM_ (printVoo sys) (buscarVoosPorDestino d sys) >> menuBuscaVoos sys
    "3" -> do
             putStr "Origem: "
             o <- getLine
             putStr "Destino: "
             d <- getLine
             mapM_ (printVoo sys) (buscarVoosPorRota o d sys)
             menuBuscaVoos sys
    "4" -> pure ()
    _   -> putStrLn "Opcao invalida." >> menuBuscaVoos sys

-- ============================================================
--                MENUS ADMIN - PASSAGEIROS
-- ============================================================

menuPassageirosAdmin :: Sistema -> IO Sistema
menuPassageirosAdmin sys = do
  putStrLn ""
  putStrLn "---- Passageiros (Admin) ----"
  putStrLn "1) Listar Todos"
  putStrLn "2) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoListarPassageiros sys >> menuPassageirosAdmin sys
    "2" -> pure sys
    _   -> putStrLn "Opcao invalida." >> menuPassageirosAdmin sys

acaoListarPassageiros :: Sistema -> IO ()
acaoListarPassageiros sys = do
  let ps = listarPassageiros sys
  if null ps
     then putStrLn "(vazio)"
     else mapM_ (\p -> putStrLn $ "- ID " ++ show (idPassageiro p)
                               ++ " | Nome: " ++ nome p
                               ++ " | Doc: "  ++ documento p) ps

-- ============================================================
--                 MENUS ADMIN - RESERVAS
-- ============================================================

menuReservasAdmin :: Sistema -> IO Sistema
menuReservasAdmin sys = do
  putStrLn ""
  putStrLn "---- Reservas (Admin) ----"
  putStrLn "1) Listar Todas"
  putStrLn "2) Confirmar Reserva"
  putStrLn "3) Cancelar Reserva"
  putStrLn "4) Listar por Status"
  putStrLn "5) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoListarReservas sys >> menuReservasAdmin sys
    "2" -> acaoConfirmarReserva sys >>= menuReservasAdmin
    "3" -> acaoCancelarReserva sys >>= menuReservasAdmin
    "4" -> menuListarReservasPorStatus sys >> menuReservasAdmin sys
    "5" -> pure sys
    _   -> putStrLn "Opcao invalida." >> menuReservasAdmin sys

acaoListarReservas :: Sistema -> IO ()
acaoListarReservas sys = do
  let rs = listarReservas sys
  if null rs
    then putStrLn "(vazio)"
    else mapM_ (printReserva sys) rs

acaoConfirmarReserva :: Sistema -> IO Sistema
acaoConfirmarReserva sys = do
  putStr "ID da Reserva: "
  sRid <- getLine
  case readIntSafe sRid of
    Just rid ->
      case confirmarReserva rid sys of
        Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
        Right sys' -> putStrLn "[OK] Reserva confirmada." >> pure sys'
    _ -> putStrLn "[ERRO] ID invalido." >> pure sys

acaoCancelarReserva :: Sistema -> IO Sistema
acaoCancelarReserva sys = do
  putStr "ID da Reserva: "
  sRid <- getLine
  case readIntSafe sRid of
    Just rid ->
      case cancelarReserva rid sys of
        Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
        Right sys' -> putStrLn "[OK] Reserva cancelada." >> pure sys'
    _ -> putStrLn "[ERRO] ID invalido." >> pure sys

menuListarReservasPorStatus :: Sistema -> IO ()
menuListarReservasPorStatus sys = do
  putStrLn ""
  putStrLn "---- Listar por Status ----"
  putStrLn "1) Pendentes"
  putStrLn "2) Confirmadas"
  putStrLn "3) Canceladas"
  putStrLn "4) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoListarReservasPorStatus Pendente sys >> menuListarReservasPorStatus sys
    "2" -> acaoListarReservasPorStatus Confirmada sys >> menuListarReservasPorStatus sys
    "3" -> acaoListarReservasPorStatus Cancelada sys >> menuListarReservasPorStatus sys
    "4" -> pure ()
    _   -> putStrLn "Opcao invalida." >> menuListarReservasPorStatus sys

acaoListarReservasPorStatus :: StatusReserva -> Sistema -> IO ()
acaoListarReservasPorStatus st sys = do
  let rs = listarReservasPorStatus st sys
  if null rs
    then putStrLn "(nenhuma reserva encontrada)"
    else do
      putStrLn ""
      putStrLn ("Encontradas " ++ show (length rs)
                ++ " reserva(s) com status " ++ show st ++ ":")
      mapM_ (printReserva sys) rs

printReserva :: Sistema -> Reserva -> IO ()
printReserva sys r = do
  let pn = maybe "(?)" nome (buscarPassageiroPorId (idPass r) sys)
      v  = buscarVooPorId (idVooRef r) sys
      rota = maybe "(voo?)"
                  (\vv -> origem vv ++ " -> " ++ destino vv ++ " @ " ++ horario vv)
                  v
  putStrLn $ "- ID " ++ show (idReserva r)
          ++ " | Passageiro: " ++ pn
          ++ " | Voo: " ++ rota
          ++ " | Status: " ++ show (status r)

-- ============================================================
--                    MENU RELATORIOS
-- ============================================================

menuRelatorios :: Sistema -> IO Sistema
menuRelatorios sys = do
  putStrLn ""
  putStrLn "---- Relatorios ----"
  putStrLn "1) Estatisticas Gerais"
  putStrLn "2) Passageiros"
  putStrLn "3) Companhias"
  putStrLn "4) Voos"
  putStrLn "5) Reservas"
  putStrLn "6) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> imprimirRelatorioEstatisticasGerais sys >> menuRelatorios sys
    "2" -> imprimirRelatorioPassageiros sys >> menuRelatorios sys
    "3" -> imprimirRelatorioCompanhias sys >> menuRelatorios sys
    "4" -> imprimirRelatorioVoos sys >> menuRelatorios sys
    "5" -> imprimirRelatorioReservas sys >> menuRelatorios sys
    "6" -> pure sys
    _   -> putStrLn "Opcao invalida." >> menuRelatorios sys

-- ============================================================
--                   MENUS ADMIN - USUARIOS
-- ============================================================

menuUsuariosAdmin :: Usuario -> Sistema -> IO Sistema
menuUsuariosAdmin _ sys = do
  putStrLn ""
  putStrLn "---- Usuarios (Admin) ----"
  putStrLn "1) Listar"
  putStrLn "2) Cadastrar Administrador"
  putStrLn "3) Excluir Usuario"
  putStrLn "4) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoListarUsuarios sys >> menuUsuariosAdmin undefined sys
    "2" -> acaoCadastrarAdmin sys >>= menuUsuariosAdmin undefined
    "3" -> acaoExcluirUsuario sys >>= menuUsuariosAdmin undefined
    "4" -> pure sys
    _   -> putStrLn "Opcao invalida." >> menuUsuariosAdmin undefined sys

acaoListarUsuarios :: Sistema -> IO ()
acaoListarUsuarios sys = do
  let us = listarUsuarios sys
  if null us
     then putStrLn "(vazio)"
     else mapM_ (\u -> putStrLn $ "- ID " ++ show (idUsuario u)
                               ++ " | Nome: " ++ nomeUsuario u
                               ++ " | Email: " ++ emailUsuario u
                               ++ " | Tipo: " ++ show (tipoUsuario u)) us

acaoCadastrarAdmin :: Sistema -> IO Sistema
acaoCadastrarAdmin sys = do
  putStr "Nome do Admin: "
  nomeInput <- getLine
  putStr "Email do Admin: "
  emailInput <- getLine
  putStr "Senha do Admin: "
  senhaInput <- getLine

  let nomeT = trim nomeInput
      emailT = trim emailInput
      senhaT = trim senhaInput

  if null nomeT || null emailT || null senhaT
    then putStrLn "[ERRO] Campos nao podem ser vazios." >> pure sys
    else if any (\u -> emailUsuario u == emailT) (usuarios sys)
      then putStrLn "[ERRO] Ja existe um usuario com esse email." >> pure sys
      else if any (\u -> nomeUsuario u == nomeT) (usuarios sys)
        then putStrLn "[ERRO] Ja existe um usuario com esse nome." >> pure sys
        else
          let novoId = proximoId (usuarios sys) idUsuario
              novoAdmin = Usuario novoId nomeT emailT senhaT Administrador
          in do
            putStrLn "[OK] Administrador cadastrado."
            pure sys { usuarios = usuarios sys ++ [novoAdmin] }

acaoExcluirUsuario :: Sistema -> IO Sistema
acaoExcluirUsuario sys = do
  putStr "ID do Usuario: "
  sId <- getLine
  case readIntSafe sId of
    Nothing -> putStrLn "[ERRO] ID invalido." >> pure sys
    Just uid -> case excluirUsuario uid sys of
      Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
      Right sys' -> putStrLn "[OK] Usuario excluido." >> pure sys'

-- ============================================================
--              MENUS USUARIO COMUM - PERFIL
-- ============================================================

menuPerfilUsuario :: Usuario -> Sistema -> IO Sistema
menuPerfilUsuario usuario sys = do
  putStrLn ""
  putStrLn "---- Meu Perfil ----"
  putStrLn "1) Cadastrar meu Passageiro"
  putStrLn "2) Ver meu Passageiro"
  putStrLn "3) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoCadastrarMeuPassageiro sys >>= menuPerfilUsuario usuario
    "2" -> acaoVerMeuPassageiro sys >> menuPerfilUsuario usuario sys
    "3" -> pure sys
    _   -> putStrLn "Opcao invalida." >> menuPerfilUsuario usuario sys

acaoCadastrarMeuPassageiro :: Sistema -> IO Sistema
acaoCadastrarMeuPassageiro sys = do
  putStr "Nome: "
  nomeInput <- getLine
  putStr "Documento: "
  docInput <- getLine
  case inserirPassageiroDados nomeInput docInput sys of
    Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
    Right sys' -> putStrLn "[OK] Passageiro cadastrado." >> pure sys'

acaoVerMeuPassageiro :: Sistema -> IO ()
acaoVerMeuPassageiro sys = do
  putStr "ID do seu Passageiro: "
  sId <- getLine
  case readIntSafe sId of
    Nothing -> putStrLn "[ERRO] ID invalido."
    Just pid -> case buscarPassageiroPorId pid sys of
      Nothing -> putStrLn "[ERRO] Passageiro nao encontrado."
      Just p -> putStrLn $ "Nome: " ++ nome p ++ " | Doc: " ++ documento p

-- ============================================================
--           MENUS USUARIO COMUM - BUSCAR VOOS
-- ============================================================

menuBuscarVoosUsuario :: Sistema -> IO ()
menuBuscarVoosUsuario sys = menuBuscaVoos sys

-- ============================================================
--          MENUS USUARIO COMUM - MINHAS RESERVAS
-- ============================================================

menuReservasUsuario :: Usuario -> Sistema -> IO Sistema
menuReservasUsuario usuario sys = do
  putStrLn ""
  putStrLn "---- Minhas Reservas ----"
  putStrLn "1) Criar Reserva"
  putStrLn "2) Listar Minhas Reservas"
  putStrLn "3) Cancelar Reserva"
  putStrLn "4) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoCriarReservaUsuario sys >>= menuReservasUsuario usuario
    "2" -> acaoListarMinhasReservas sys >> menuReservasUsuario usuario sys
    "3" -> acaoCancelarMinhaReserva sys >>= menuReservasUsuario usuario
    "4" -> pure sys
    _   -> putStrLn "Opcao invalida." >> menuReservasUsuario usuario sys

acaoCriarReservaUsuario :: Sistema -> IO Sistema
acaoCriarReservaUsuario sys = do
  putStr "ID do seu Passageiro: "
  sPid <- getLine
  putStr "ID do Voo: "
  sVid <- getLine
  case (readIntSafe sPid, readIntSafe sVid) of
    (Just pid, Just vid) ->
      case criarReserva pid vid sys of
        Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
        Right sys' -> do
          putStrLn "[OK] Reserva criada e confirmada automaticamente."
          pure sys'
    _ -> putStrLn "[ERRO] IDs invalidos." >> pure sys

acaoListarMinhasReservas :: Sistema -> IO ()
acaoListarMinhasReservas sys = do
  putStr "ID do seu Passageiro: "
  sPid <- getLine
  case readIntSafe sPid of
    Just pid -> do
      let rs = listarReservasPorPassageiro pid sys
      if null rs then putStrLn "(vazio)" else mapM_ (printReserva sys) rs
    _ -> putStrLn "[ERRO] ID invalido."

acaoCancelarMinhaReserva :: Sistema -> IO Sistema
acaoCancelarMinhaReserva sys = do
  putStr "ID da Reserva: "
  sRid <- getLine
  case readIntSafe sRid of
    Just rid ->
      case cancelarReserva rid sys of
        Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
        Right sys' -> putStrLn "[OK] Reserva cancelada." >> pure sys'
    _ -> putStrLn "[ERRO] ID invalido." >> pure sys

-- ============================================================
--         MENUS USUARIO COMUM - CONFIGURACOES
-- ============================================================

menuConfiguracoesUsuario :: Usuario -> Sistema -> IO (Sistema, Usuario)
menuConfiguracoesUsuario usuario sys = do
  putStrLn ""
  putStrLn "---- Configuracoes ----"
  putStrLn "1) Editar meu Nome"
  putStrLn "2) Editar meu Email"
  putStrLn "3) Editar minha Senha"
  putStrLn "4) Editar Tudo"
  putStrLn "5) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoEditarNome usuario sys >>= \(s, u) -> menuConfiguracoesUsuario u s
    "2" -> acaoEditarEmail usuario sys >>= \(s, u) -> menuConfiguracoesUsuario u s
    "3" -> acaoEditarSenha usuario sys >>= \(s, u) -> menuConfiguracoesUsuario u s
    "4" -> acaoEditarTudo usuario sys >>= \(s, u) -> menuConfiguracoesUsuario u s
    "5" -> pure (sys, usuario)
    _   -> putStrLn "Opcao invalida." >> menuConfiguracoesUsuario usuario sys

acaoEditarNome :: Usuario -> Sistema -> IO (Sistema, Usuario)
acaoEditarNome usuario sys = do
  putStr "Novo Nome: "
  novoNome <- getLine
  case editarUsuario (idUsuario usuario) novoNome (emailUsuario usuario) (senhaUsuario usuario) sys of
    Left err -> putStrLn ("[ERRO] " ++ err) >> pure (sys, usuario)
    Right sys' -> do
      let usuarioAtualizado = usuario { nomeUsuario = trim novoNome }
      putStrLn "[OK] Nome atualizado."
      pure (sys', usuarioAtualizado)

acaoEditarEmail :: Usuario -> Sistema -> IO (Sistema, Usuario)
acaoEditarEmail usuario sys = do
  putStr "Novo Email: "
  novoEmail <- getLine
  case editarUsuario (idUsuario usuario) (nomeUsuario usuario) novoEmail (senhaUsuario usuario) sys of
    Left err -> putStrLn ("[ERRO] " ++ err) >> pure (sys, usuario)
    Right sys' -> do
      let usuarioAtualizado = usuario { emailUsuario = trim novoEmail }
      putStrLn "[OK] Email atualizado."
      pure (sys', usuarioAtualizado)

acaoEditarSenha :: Usuario -> Sistema -> IO (Sistema, Usuario)
acaoEditarSenha usuario sys = do
  putStr "Nova Senha: "
  novaSenha <- getLine
  case editarUsuario (idUsuario usuario) (nomeUsuario usuario) (emailUsuario usuario) novaSenha sys of
    Left err -> putStrLn ("[ERRO] " ++ err) >> pure (sys, usuario)
    Right sys' -> do
      let usuarioAtualizado = usuario { senhaUsuario = trim novaSenha }
      putStrLn "[OK] Senha atualizada."
      pure (sys', usuarioAtualizado)

acaoEditarTudo :: Usuario -> Sistema -> IO (Sistema, Usuario)
acaoEditarTudo usuario sys = do
  putStr "Novo Nome: "
  novoNome <- getLine
  putStr "Novo Email: "
  novoEmail <- getLine
  putStr "Nova Senha: "
  novaSenha <- getLine
  case editarUsuario (idUsuario usuario) novoNome novoEmail novaSenha sys of
    Left err -> putStrLn ("[ERRO] " ++ err) >> pure (sys, usuario)
    Right sys' -> do
      let usuarioAtualizado = usuario { nomeUsuario = trim novoNome
                                       , emailUsuario = trim novoEmail
                                       , senhaUsuario = trim novaSenha }
      putStrLn "[OK] Dados atualizados."
      pure (sys', usuarioAtualizado)

-- ============================================================
--                    UTILITARIOS
-- ============================================================

readIntSafe :: String -> Maybe Int
readIntSafe s = case reads s of
  [(n,"")] -> Just n
  _        -> Nothing

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
        isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

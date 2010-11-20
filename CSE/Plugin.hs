{-# LANGUAGE TemplateHaskell #-}

module CSE.Plugin where

import GHCPlugins
import CSE.Pass ( cseProgram )


plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _options todos = do
    -- You should probably run this with -fno-cse !
    return $ CoreDoPasses [defaultGentleSimplToDo, cse_pass] : todos


cse_pass = CoreDoPluginPass "Plugged-in common sub-expression" (BindsToBindsPluginPass cseProgram)
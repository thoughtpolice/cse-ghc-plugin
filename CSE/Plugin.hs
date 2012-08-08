module CSE.Plugin
       ( plugin -- :: Plugin
       ) where

import CSE.Pass (cseProgram)
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

-- You should probably run this with -fno-cse !
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = do
    reinitializeGlobals
    return $ CoreDoPasses [cse] : todos
  where cse = CoreDoPluginPass "Common Subexpression Elimination" (bindsOnlyPass cseProgram)

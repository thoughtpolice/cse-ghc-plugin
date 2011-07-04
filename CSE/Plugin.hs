module CSE.Plugin where
import GhcPlugins
import CSE.Pass ( cseProgram )

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

-- You should probably run this with -fno-cse !
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ xs = return $ CoreDoPasses [defaultGentleSimplToDo, cse] : xs
  where cse = CoreDoPluginPass "Common Subexpression Elimination" (bindsOnlyPass cseProgram)

import Data.Map as Map
import Data.Maybe (fromMaybe)

type LEnv = [(String, String)]
-- data Lenv = [("t", "1+a"), ("a", "5")]

initLenv :: LEnv
initLenv = [("t", "1+a"), ("a", "5")]

findVar :: String -> LEnv
findVar vname = do
    let lenv = initLenv
    let m = Map.fromList lenv
    case Map.lookup vname m of
      Nothing -> lenv
      Just def -> lenv ++ [(vname, def)]
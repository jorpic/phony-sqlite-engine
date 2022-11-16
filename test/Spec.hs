
import Test.Hspec
import EngineSpec qualified

main :: IO ()
main = hspec $ do
  EngineSpec.spec

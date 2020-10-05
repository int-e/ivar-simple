import qualified Data.IVar.Simple as IVar
import Control.Concurrent

main :: IO ()
main = do
    -- create new `IVar`
    iv <- IVar.new
    -- spawn a thread that reads the `IVar` and prints the value
    forkIO $ print (IVar.read iv)
    -- the spawned thread will sleep while the `IVar` is empty
    threadDelay 1000000
    -- tentatively read the `IVar` -- it's still empty
    IVar.tryRead iv >>= print
    -- write a value to the `IVar`
    IVar.write iv 42
    -- now the thread will be woken up and print "42"
    threadDelay 1000000
    -- tentatively read the `IVar` -- now it's full
    IVar.tryRead iv >>= print
    -- further writes fail: `tryWrite` returns `False`
    IVar.tryWrite iv 42 >>= print
    -- and `write` throws an exception
    IVar.write iv 42

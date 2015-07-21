import Control.Monad.Reader

data State = State {
	rooms    :: String,
	purposes :: String
}

type Program = ReaderT State IO ()

myProgram :: Program 
myProgram = do
	-- blubb <- ask
	liftIO $ putStr "hej"
	liftIO $ putStr "hej"
	liftIO $ putStr "hej"

main = do
	runReaderT myProgram $ State {rooms = "hej", purposes = "då"}

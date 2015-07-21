import Control.Monad.Reader


myProgram :: Program 
myProgram = do
	-- blubb <- ask
	liftIO $ putStr "hej"
	liftIO $ putStr "hej"
	liftIO $ putStr "hej"

main = do
	runReaderT myProgram $ State {rooms = "hej", purposes = "då"}

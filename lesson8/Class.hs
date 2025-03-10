module Class where 

data Client = Client {sername :: String, name :: String, age :: Int}
    deriving (Show)

artyom :: Client
artyom = Client { sername = "Sergeev", name = "Artyom", age = 35}

setName :: Client -> String -> Client
setName (Client sn n a) nn = Client sn nn a 


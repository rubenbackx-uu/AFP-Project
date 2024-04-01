module Table where 

data Table a = Table { 
    tableName :: String,
    idCol :: String,
    definition :: String
}

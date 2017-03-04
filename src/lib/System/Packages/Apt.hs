module System.Packages.Apt where

data RepoType = Binary | Source | Invalid

instance Show RepoType where
  show Binary  = "deb"
  show Source  = "deb-src"
  show Invalid = "invalid"

data Repository = Repository 
    { repoType     :: RepoType
    , url          :: String
    , distribution :: String
    , components   :: [String]
    }

kyleRepo :: Repository
kyleRepo = Repository 
    { repoType = Binary
    , url = "https://apt.kyleisom.net"
    , distribution = "jessie"
    , components = ["main", "binary"]
    }

instance Show Repository where
  show (Repository t u d c) = unwords [show t, u, d, unwords c]

toSource :: Repository -> Maybe Repository
toSource (Repository Binary u d c) = Just $ Repository
    { repoType = Source
    , url = u
    , distribution = d
    , components = c
    }
toSource _ = Nothing

toSourceOnly :: Repository -> [String] -> Maybe Repository
toSourceOnly (Repository Binary u d _) comps = Just $ Repository
    { repoType = Source
    , url = u
    , distribution = d
    , components = comps
    }
toSourceOnly _ _ = Nothing


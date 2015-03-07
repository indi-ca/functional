module NBB where

import Data.List (elemIndex)



data SSHRequest = SSHRequest {
    command :: String,
    result :: [String]
} deriving (Show)

data SSHRequestAlt = SSHRequestAlt {
    commandAlt :: String
} deriving (Show)

data SSHResponse = SSHResponse {
    resultAlt :: [String]
} deriving (Show)


data SSHRequestType = StandardSSHRequest | NBBSSHRequest




-- A ss wrapper

successString = "Starting ssh connection"

isSuccess :: SSHRequest -> Bool
isSuccess request = successString `elem` results
    where results = result request

-- get's the contents of the command
inner :: SSHResponse -> Maybe [String]
inner request = case index of (Nothing) -> Nothing
                              (Just x) -> Just (drop (x+1) results)
    where results = resultAlt request
          index = elemIndex successString results

inner' :: Maybe [String] -> Maybe [String]
inner' (Nothing) = Nothing
inner' (Just xs) = case index of (Nothing) -> Nothing
                                 (Just x) -> Just (drop (x+1) xs)
    where index = elemIndex successString xs


-- Successful response is something like this:
raw_string = "connecting to lego.safenetbox.biz\nTrying 1 servers\n['10.107.11.189']\nIP address: 10.107.11.189\nGot port 4 from nbupdate SRV record\nStarting ssh connection\nNetbox release 29.6 (Final)\n"
sampleRequest = SSHRequest "cat /etc/redhat-release" (lines raw_string)


--test = inner sampleRequest



-- What I really need is,
-- something that takes a raw ss response
-- and returns an Either

--TODO: There is a validations library that Nick Patridge and Tony Morris are working on


{-# LANGUAGE OverloadedStrings #-}

module PasswordManager where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (cipherInit, ecbEncrypt, ecbDecrypt, KeySizeSpecifier(..))
import Crypto.Error (throwCryptoError)
import Crypto.Random (getRandomBytes)
import qualified Data.Text as T
import System.Console.Haskeline

import Config

getPasswordIO :: IO (Maybe String)
getPasswordIO = runInputT defaultSettings $ getPassword (Just '*') "Enter password: "

managePasswordIO :: Config -> IO String
managePasswordIO config = do
    let passwordFile = T.unpack $ password_file (credentials (lemmy config))
    let privateKeyFile = T.unpack $ private_key_file (credentials (lemmy config))

    -- Check if private key exists, generate if not
    keyExists <- doesFileExist privateKeyFile
    key <- if keyExists
        then BS.readFile privateKeyFile
        else do
            putStrLn "Generating new private key..."
            key <- getRandomBytes 32  -- AES256 needs 32-byte key
            BS.writeFile privateKeyFile key
            return key
    
    -- Initialize cipher with the key
    let cipher = throwCryptoError (cipherInit key) :: AES256
    
    -- Check if password file exists
    encryptedExists <- doesFileExist passwordFile
    if encryptedExists
        then do
            -- Decrypt and read existing password
            encrypted <- BS.readFile passwordFile
            let decrypted = ecbDecrypt cipher encrypted
            return (BSC.unpack decrypted)
        else do
            -- Get new password from user and encrypt it
            putStr "Enter new password: "
            hFlush stdout
            maybePassword <- getPasswordIO
            let password = BSC.pack $ fromMaybe "" maybePassword
            let encrypted = ecbEncrypt cipher password
            BS.writeFile passwordFile encrypted
            return (BSC.unpack password)
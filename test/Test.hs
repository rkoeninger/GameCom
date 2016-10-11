module Main where

import System.Exit (exitSuccess, exitFailure)
import Famihask
import Memory

testStoreWord = do
	ram <- malloc 2
	storeWord ram 0 0x8cf3
	b0 <- load ram 0
	b1 <- load ram 1
	return $ (b0 == 0xf3) && (b1 == 0x8c)

testLoadWord = do
	ram <- malloc 2
	store ram 0 0xf3
	store ram 1 0x8c
	w <- loadWord ram 0
	return $ w == 0x8cf3

main = do
	result0 <- testStoreWord
	result1 <- testLoadWord
	if result0 && result1 then exitSuccess else exitFailure

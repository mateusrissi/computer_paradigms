module Utils (fazTriplas,
	      pegaDimensoes,
	      removePadding,
	      adicionaPadding,
	      separaCabecalho,
	      rearranjaPixelList,
	      escreveEmASCII) where

import Char
import List
import IO


separaCabecalho :: [Char] -> ([Char],[Char])
separaCabecalho [] = ([],[])
separaCabecalho imagem = splitAt 54 imagem


fazTriplas :: [Char] -> [ [Char] ]
fazTriplas [] = []
fazTriplas arquivo = (take 3 arquivo):(fazTriplas (drop 3 arquivo) )


-- Funções para obter as dimensões da imagem a partir do cabecalho
pegaLargura :: [Char] -> Int
pegaLargura [] = 0
pegaLargura fstr = let lista = map ord (take 4 (drop 18 fstr))  in  (lista!!0) + (lista!!1) * 256


pegaAltura :: [Char] -> Int
pegaAltura [] = 0
pegaAltura fstr = let lista = map ord (take 4 (drop 22 fstr))  in  (lista!!0) + (lista!!1) * 256


pegaDimensoes :: [Char] -> (Int, Int)
pegaDimensoes fstr = ( (pegaLargura fstr), (pegaAltura fstr) )


-- Funções que removem e adicionam os bytes nulos de padding, para que cada linha
-- da imagem tenha um número de bytes múltiplo de 4
removePadding :: [Char] -> Int -> [Char]
removePadding [] _ = []
removePadding imagem largura = let (atual, resto) = splitAt (largura*3) imagem
				   pads = mod (largura*3) 4  in
					let proximasLinhas = drop pads resto  in
						atual ++ (removePadding proximasLinhas largura)


adicionaPadding :: Int -> [Char] -> [Char]
adicionaPadding _ [] = []
adicionaPadding largura imagem = let (atual, resto) = splitAt (largura*3) imagem 
				     pads = mod (largura*3) 4  in
					let proximasLinhas = (replicate pads (chr 0) ) ++ resto  in
						atual ++ (adicionaPadding largura proximasLinhas)


-- Funções para transformar a lista de pixels (única cor), quebrando-a por linha
-- da imagem, e colocando-a na ordem "visual" e não a do arquivo BMP						
separaPixelList ::  Int -> [Char] -> [ [Char] ]
separaPixelList largura [] = []
separaPixelList largura pl = let s = splitAt largura pl in
				(fst s):(separaPixelList largura (snd s) )


rearranjaPixelList ::  Int -> [Char] -> [ [Char] ]
rearranjaPixelList largura pl = reverse (separaPixelList largura pl)


-- Função para escrever uma representação da lista de linhas no arquivo
escreveEmASCII arquivo [] = return ()
escreveEmASCII arquivo cs = do
				hPrint arquivo (map ord (head cs))
				escreveEmASCII arquivo (tail cs)
